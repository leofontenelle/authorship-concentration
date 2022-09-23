# 1_summarize.R - calculates indexes

# Libraries and functions ####

source("0_functions.R")

library(data.table) |> suppressPackageStartupMessages()
library(stringi)


# Read journal data ####

tmp <- read_isis_title("data/title_all_drleonardo.iso")

indexing <- as.data.table(tmp$indexing, key = "issn") |> subset(
  # seemingly valid ISSNs
  grepl("^\\d{4}-\\d{3}[\\dxX]$", issn, perl = TRUE) &
    # in LILACS
    source == "LL" &
    # Not removed
    is.na(final_year)
)
stopifnot(!anyDuplicated(indexing$issn))
journals <- as.data.table(tmp$journals, key = "issn")[indexing$issn] |> subset(
  # keep journals only if the haven't been discontinued
  status == "C"
  ## while LILACS doesn't index journals dedicated to
  ## scientific dissemination, there are two Colombian journals
  ## recorded as "science dissemination" which are indexed, both
  ## according to the TITLE database and to this web page:
  ## https://lilacs.bvsalud.org/en/lilacs-journals/list-journals-indexed-in-lilacs/
  #
  # & level == "CT"
)
# keep journals only if the haven't been discontinued
indexing <- indexing[journals$issn]
subjects <- as.data.table(tmp$subjects, key = "issn")[indexing$issn]
rm(tmp)


# Clean journal data ####

stopifnot(identical(journals$issn, fix_issn(journals$issn)))
stopifnot(identical(subjects$issn, fix_issn(subjects$issn)))
subjects[decs %in% c("SINDROIMUNE IMUN ADQUIRIDAUIRIDAUIRIDA",
                     "S\u00eddrome de Imunodefici\u00eancia Adquirida"),
         decs := "S\u00edndrome de Imunodefici\u00eancia Adquirida"]
subjects[decs == "PRESTA\u252c\u00c7AO DE CUIDADOS DE SAUDE",
         decs := "Presta\u00e7\u00e3o de Cuidados de Sa\u00fade"]
# Some journals have subject headings in title case with diacritics,
# while others have them win upper case without diacritics.
subjects[, decs := normalize_jdescr(decs)]


# Write journal data ####

# sep = ";" and BOM for the sake of Brazilian users of MS Excel
subjects |>
  fwrite("data/journal_subjects.csv", sep = ";", bom = TRUE)
journals |>
  subset(select = c(issn, title, title_abbrev, country_code)) |>
  fwrite("data/journals.csv", sep = ";", bom = TRUE)


# Read data ####

years <- as.character(2015:2019)
articles <- authorships <- pubtypes <-
  vector(mode = "list", length = length(years)) |>
  setNames(years)
for (year in years) {
  path <- file.path("data", sprintf("lilacs_%s.isis", year))
  stopifnot(file.exists(path))
  if (interactive()) message(sprintf(fmt = "Reading %s", path))
  res <- read_isis(path)
  articles[[year]] <- res[["articles"]]
  authorships[[year]] <- res[["authorships"]]
  pubtypes[[year]] <- res[["pubtypes"]]
}
rm(year, years, path, res)
articles <- do.call("rbind", articles) |>
  as.data.table(key = "lilacs_id")
authorships <- do.call("rbind", authorships) |>
  as.data.table(key = "lilacs_id")
pubtypes <- do.call("rbind", pubtypes) |>
  as.data.table(key = "lilacs_id")


# Clean data ####

stopifnot(articles[, !any(duplicated(lilacs_id))])
stopifnot(articles[, all(lit_type == "S")])     # journal article
stopifnot(articles[, all(treat_level == "as")]) # journal article
articles |>
  set(j = c("lit_type", "treat_level"), value = NULL) |>
  # Journal titles in articles records are not always reliable
  set(j = "journal_title", value = NULL) |>
  # We are not going to use these
  set(j = c("pub_country", "n_references", "doi"), value = NULL)
articles[, issn := fix_issn(issn)]
articles[, pub_date := as.integer(substring(pub_date, first = 1, last = 4))]
stopifnot(all(!is.na(articles$pub_date))) # Any encoding issue?
setnames(articles, old = "pub_date", new = "year")

# Unneeded
authorships |>
  set(j = c("affiliation_1", "affiliation_2", "affiliation_3"), value = NULL) |>
  set(j = c("city", "state", "country"), value = NULL)
# Make author names great again
authorships[name == "Anon", name := NA_character_]
authorships[, name := decode_ncr(name)]
authorships[!is.na(name), name := stri_replace_all_fixed(name, ".", " ")]
authorships[!is.na(name), name := stri_replace_all_fixed(name, "  ", " ")]
authorships[!is.na(name), name := stri_replace_all_regex(
  str = name,
  pattern = "[,\\p{N}\\s*\u2020/+\u00ad\u2217-]+,",
  replacement = ",")]
authorships[!is.na(name), name := name |>
              stri_replace_all_regex(" ?\\([^)]*\\)", "")]
authorships[name == "memoriam), Dione Dias Torriani(in",
            name := "Torriani, Dione Dias"]
authorships[, name := fix_encoding(name)]
authorships[, ascii_name := name |>
              stri_trans_general("NFKC; Any-Latin; Latin-ASCII")]
stopifnot(authorships[
  !is.na(name),
  all(stri_enc_toascii(ascii_name) |>
        stri_detect_fixed("\032", negate = TRUE))
])
# "Vega-Luzuriaga, Patricio" == "Vega Luzuriaga, Patricio"
authorships[, ascii_name := stri_replace_all_regex(
  ascii_name, pattern = "\\s*-\\s*", replacement = " ")]
# ORCID id
authorships[, orcid_id := fix_orcid(orcid_id)]
stopifnot(authorships[
  !is.na(orcid_id) & !grepl("^0000-\\d{4}-\\d{4}-\\d{3}[0-9Xx]", orcid_id),
  .N == 0
])


# Put it together ####

authorships[, issn := articles[lilacs_id, issn]]
articles[, signed := lilacs_id %in%
           authorships[!is.na(name) | !is.na(orcid_id), lilacs_id]]
articles[, in_lilacs := issn %in% journals[, issn]]
stopifnot(all(!is.na((journals$issn))))
journals[, n_articles := fcoalesce(
  articles[, .N, keyby = issn][issn, N],
  0L
)]
journals[, n_signed := fcoalesce(
  articles[, .(N = sum(signed)), keyby = issn][issn, N],
  0L
)]
journals[, with_jdescr := issn %in% subjects[, issn]]
stopifnot(all(!is.na(subjects$issn)))
stopifnot(all(!is.na(subjects$decs)))
subjects[, in_articles := issn %in% articles[, issn]]
subjects[, in_lilacs := issn %in% journals[, issn]]


# Figure 1 ####

n_articles <- n_journals <-
  vector(mode = "integer", length = 3) |>
  setNames(nm = c("found", "lilacs", "50+"))
n_articles["found"] <- articles[, uniqueN(lilacs_id)]
# Some articles are missing their ISSN,
# (almost?) always because the journal isn't indexed
n_journals["found"] <- articles[, uniqueN(na.omit(issn))]
n_articles["lilacs"] <- articles[(in_lilacs), uniqueN(lilacs_id)]
n_journals["lilacs"] <- journals[n_articles > 0, .N]
n_articles["50+"] <- articles[
  issn %in% journals[n_signed >= 50, issn], uniqueN(lilacs_id)]
n_journals["50+"] <- journals[n_signed >= 50, .N]


# Drop irrelevant data ####

journals <- journals[n_signed >= 50]
articles <- articles[issn %in% journals$issn & signed]
authorships <- authorships[lilacs_id %in% articles[, lilacs_id] &
                             (!is.na(name) | !is.na(orcid_id))]
pubtypes <- pubtypes[lilacs_id %in% articles[, lilacs_id]] # See at the end
subjects <- subjects[issn %in% journals$issn]
stopifnot(all(journals$issn %in% authorships$issn))


# Distinguish authors ####

# author_id is orcid_id when we can, and ascii_name otherwise
authorships[, author_id := orcid_id]

# If name corresponds to a single ORCID id,
# all occurrences of name will have that id.
X <- authorships[
  !is.na(author_id) & !is.na(name),
  .(n = uniqueN(author_id), author_id = first(author_id)),
  keyby = name
][n == 1]
authorships[, author_id := fcoalesce(author_id, X[name, author_id])]

# Same with normalized name.
X <- authorships[
  !is.na(author_id) & !is.na(ascii_name),
  .(n = uniqueN(author_id), author_id = first(author_id)),
  keyby = ascii_name
][n == 1]
authorships[, author_id := fcoalesce(author_id, X[ascii_name, author_id])]

# Some names are associated with more than one ORCID id. That's OK!
# That's what ORCID ids are for. But sometimes the same name occurs with no
# ORCID id. Which ORCID id should we assign to these names?
# Here we might sophisticate, repeating the above methods for each country or
# institutions, or perhaps something even more fancy. However:
# (1) looking at how unique some names are, I believe most are
#     duplicate ORCID ids, not different persons; and
# (2) the probability of both publishing in the same journal is tiny; and
# (3) this study is based on another that compared names, period.
# So, we will simply treat these names as pertaining to the same person.
X <- authorships[
  !is.na(author_id) & !is.na(ascii_name),
  .(n = uniqueN(author_id)),
  keyby = "ascii_name"
][
  , any_missing_id := authorships[
    , any(is.na(author_id)), keyby = ascii_name][ascii_name, V1]
]
cant_choose_orcid <- c(n = format(X[n > 1 & any_missing_id, .N]),
                       "%" = format(100 * X[, mean(n > 1 & any_missing_id)]))
authorships[X[ascii_name, any_missing_id & n > 1], author_id := ascii_name]

# Any remaining authorship with no ORCID id
# will be simply identified by normalized name
authorships[is.na(author_id), author_id := ascii_name]
stopifnot(all(!is.na(authorships$author_id)))

# for eventual comparison with one_letter_surname_prop from
# https://osf.io/bvzfp/, although the latter's version 1 had its
# variable calculated with only a single pair of square brackets
# around :alpha:, see version 2 of https://osf.io/r3u5t/
authorships[, abbreviated_forename := !grepl(
  pattern = "^[^,]+,.*[[:alpha:]]{2}", x = ascii_name)]
stopifnot(all(!is.na(authorships$abbreviated_forename)))
X <- authorships[
  , .(abbreviated_forename_prop = mean(abbreviated_forename)), keyby = issn]
journals[, abbreviated_forename_prop := X[issn, abbreviated_forename_prop]]
rm(X)


# Calculate indexes ####

articles_per_author <- authorships[
  ,
  .(N = .N, abbreviated_forename = mean(abbreviated_forename)),
  by = .(issn, author_id)
] |>
  setorder(N) |>
  setkey(issn)
most_prolific_author <- articles_per_author[, .SD[.N], by = issn] |>
  setnames(old = c("author_id", "N", "abbreviated_forename"),
           new = c("most_prolific_author", "n_MPA", "abbreviated_forename_MPA"))
most_prolific_author[, n_ex_aequo_MPA := articles_per_author[
  , sum(N == max(N)), keyby = issn]$V1]
stopifnot(all.equal(most_prolific_author$issn, journals$issn))

journals <- journals[most_prolific_author]
journals[, PPMP := n_MPA / n_signed]
journals[, gini := articles_per_author[issn, gini(N)], by = issn]


# Save data ####

journals[, .(issn, n_articles, n_signed,
             abbreviated_forename_prop, abbreviated_forename_MPA,
             n_ex_aequo_MPA, n_MPA, PPMP, gini)] |>
  fwrite("data/authorship_concentration.csv", sep = ";", bom = TRUE)


# Output for writting the manuscript ####
print(cbind(n_articles, n_journals))
print(c(
  "ORCID id n" = format(sum(!is.na(authorships$orcid_id))),
  "ORCID id %" = format(100 * mean(!is.na(authorships$orcid_id)))
))
print(c("Can't choose which ORCID id to impute" = cant_choose_orcid))

print(paste(
  "Journals with at least 50 signed articles with a publication type:",
  articles[
    , .(N = sum(lilacs_id %in% unique(pubtypes$lilacs_id))), by = issn
  ][N >= 50, .N]
))
