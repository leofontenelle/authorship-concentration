# {stringi} saves us from inconsistencies between Windows and UNIX
# systems (Linux, macOS) on character encoding and transliteration

# The output can be used to reweight the source vector so that its
# distribution will be similar to that of the the target vector.
# The log argument makes the weights be calculated from the kernel
# density of the vectors' logarithms instead of their original values.
calculate_weights <- function(source, target, log = FALSE) {
  stopifnot(all(is.numeric(source)), all(is.numeric(target)))
  if (isTRUE(log)) {
    source <- log(source)
    target <- log(target)
  }
  minv <- min(source, target)
  maxv <- max(source, target)
  maxn <- max(length(source), length(target))
  w1 <- density(target, bw = "SJ", n = maxn, from = minv, to = maxv) |>
    with(approx(x, y, xout = source))
  w2 <- density(source, bw = "SJ", n = maxn, from = minv, to = maxv) |>
    with(approx(x, y, xout = source))
  w <- w1$y / w2$y
  w <- w / sum(w)
  w
}


# Replace numerical character references with as
# "&#913;" (uppercase alpha) in author names
decode_ncr <- function(x) {
  look <- stringi::stri_detect_fixed(x, "&#") %in% TRUE
  x_look <- x[look]
  ncr_pattern <- "&#([[:digit:]]+);"
  location <- stringi::stri_locate_all_regex(x_look, ncr_pattern)
  references <- stringi::stri_sub_all(x_look, location)
  characters <- lapply(references, function(x) {
    stringi::stri_sub(x, from = 3, to = -2) |>
      as.integer() |>
      intToUtf8(multiple = TRUE)
  })
  # This doesn't work with substring<-() because the letters
  # are shorter than the references they are replacing.
  stringi::stri_sub_all(x_look, location, omit_na = TRUE) <- characters
  x[look] <- x_look
  x
}


# Some names don't convert all the way to ASCII with {stringi}
# and iconv(to="ASCII//TRANSLIT") behaves differently in Windows.
fix_encoding <- function(name) {
  name |>
    # Apostrophe
    stringi::stri_replace_all_fixed("\u00b4", "'") |>
    stringi::stri_replace_all_fixed("D \u00cc\u0081", "D'") |>
    # Hyphen
    stringi::stri_replace_all_fixed("\u0096", "-") |>
    stringi::stri_replace_all_fixed("\u05be", "-") |>
    # Vowel + acute
    stringi::stri_replace_all_fixed("a\u00cc\u0081", "\u00e1") |>
    stringi::stri_replace_all_fixed("e\u00cc\u0081", "\u00e9") |>
    stringi::stri_replace_all_fixed("i\u00cc\u0081", "\u00ed") |>
    stringi::stri_replace_all_fixed("o\u00cc\u0081", "\u00f3") |>
    stringi::stri_replace_all_fixed("u\u00cc\u0081", "\u00fa") |>
    stringi::stri_replace_all_fixed("\u00cd\u0081", "\u00c1") |>
    stringi::stri_replace_all_fixed("\u{009f}lvarez", "\u{00c1}lvarez") |>
    # c + cedilha
    stringi::stri_replace_all_fixed("c\u00cc\u00a7", "\u00e7") |>
    # Miscellaneous
    stringi::stri_replace_all_fixed("Jo\u00c3\u00a3o", "Jo\u{00e3}o") |>
    stringi::stri_replace_all_fixed("Ru\u00ef\u00ac\u0081no", "Rufino") |>
    stringi::stri_replace_all_fixed("\u00c7eti\u00cc\u0087nkaya", "\u{00c7}eti\u{0307}nkaya") |>
    stringi::stri_replace_all_fixed("N\u02d9\u00d2ez", "N\u00fa\u00f1ez") |>
    stringi::stri_replace_all_fixed("Jad\u00b7n-Guerrero", "Jad\u00e1n-Guerrero") |>
    stringi::stri_replace_all_fixed("Lepe-Mart\u00ccnez", "Lepe-Mart\u{00ed}nez") |>
    stringi::stri_replace_all_fixed("G\u00dbmez-Garc\u00cca", "G\u{00f3}mez-Garc\u{00ed}a") |>
    stringi::stri_replace_all_fixed("Rea\u00a4o", "Rea\u00f1o") |>
    stringi::stri_replace_all_fixed("O\u0094Relly", "O'Relly") |>
    stringi::stri_replace_all_fixed("\u00a8O\u00a8", "O") |>
    stringi::stri_replace_all_fixed("Miguel\u25cb", "Miguelo") |>
    stringi::stri_replace_all_fixed("O\u0092Farrill", "O'Farrill") |>
    stringi::stri_replace_all_fixed("\u0412\u0430\u04bb\u0435na", "Bahena") |>
    stringi::stri_replace_all_fixed("\u00c7eti\u00cc\u2021nkaya", "\u00c7eti\u0307nkaya")
}


# Except for ISSNs missing an hyphen,
# these fixes are specific to the data at hand.
fix_issn <- function(x) {
  no_hyphen <- grepl("^(\\d{4})(\\d{3}(\\d|x))$", x,
                     ignore.case = TRUE, perl = TRUE)
  x[no_hyphen] <- x[no_hyphen] |>
    sub(pattern = "^(\\d{4})(\\d{3}(\\d|x))$",
        replacement = "\\1-\\2", perl = TRUE)
  # These two ISSN are from different journals,
  # with the same abbreviated title.
  # As far as I can tell, only one of them is in LILACS
  x[x == "0104-40600102-4698"] <- "0102-4698"
  x[x == "2317-269"] <- "2317-269X" # verified typo
  x
}


fix_orcid <- function(x) {

  # These are verified typos
  x[x == "000.003.3484.9638"] <- "0000-0003-3484-9638"
  x[x == "0300-0003-2352-3915"] <- "0000-0003-2352-3915"
  x[x == "0000.0002.1751.291"] <- "0000.0002.1751.2913"
  x[x == "2044-0000-0002-1273-2044"] <- "0000-0002-1273-2044"
  x[x == "0000.0002.0277.437"] <- "0000-0002-0277-4371"
  x[x == "0000-0002-3373-825"] <- "0000-0002-3373-8250"
  x[x == "0000-003-2842-2267"] <- "0000-0003-2842-2267"
  x[x == "0000-000l-8309-4003"] <- "0000-0001-8309-4003"
  x[x == "0000-0002-212-530X"] <- "0000-0002-2112-530X"
  x[x == "0000-0002-6215-09"] <- "0000-0002-6215-0923"
  x[x == "0000-0003-208+8-4870"] <- "0000-0003-2088-4870"
  x[x == "0000-0002-7745-071"] <- "0000-0002-7745-0718"
  x[x == "0000-0001-7457-772"] <- "0000-0001-7457-7724"
  x[x == "0000-003-0826-525x"] <- "0000-0003-0826-525x"
  x[x == "0000-0002-9631-190"] <- "0000-0002-9631-1908"
  x[x == "0000-002-0063-2011"] <- "0000-0002-0063-2011"
  x[x == "0000-0002-4259-3h655"] <- "0000-0002-4259-3655"
  x[x == "0000-0003-4467-193"] <- "0000-0003-4467-1933"
  x[x == "0000-0002-0151-361"] <- "0000-0002-0151-3612"
  x[x == "0000-0001-9402-561"] <- "0000-0001-9402-561X"
  x[x == "0000-0001-6383-"] <- "0000-0001-6383-7981"
  x[x == "0000-0001-6400-43"] <- "0000-0001-6400-4308"
  x[x == "0000-000-4890-7122"] <- "0000-0003-4890-7122"
  # See difference between the web page and the PDF file
  # https://revistas.usb.edu.co/index.php/Agora/article/view/3707
  x[x == "0000-0003-3286-870"] <- "0000-0002-1387-1708"
  # Not sure how to fix these ones
  x[endsWith(x, "67A8-RN00-0000-0000")] <- NA_character_
  x[endsWith(x, "0000-0002-5470-055")] <- NA_character_

  separator_pattern <- paste0(
    '^(\\d{4})(\xc2\xad|\\.| ||-)(\\d{4})(\xc2\xad|\\.| ||-)',
    '(\\d{4})(\xc2\xad|\\.| ||-)(\\d{3}[0-9Xx])$'
  )
  Encoding(separator_pattern) <- "UTF-8" # in case this is Windows

  # More general patterns of mistake
  x |>
    # URL or leading label
    sub(pattern = "^((http://)?orcid.org/|(ORCID)?: )", replacement = "") |>
    # extra leading 0
    sub(pattern = "^0(\\d{4}\\D)", replacement = "\\1") |>
    # missing first 0
    sub(pattern = "^(\\d{3}\\D)", replacement = "0\\1") |>
    # missing first four 0
    sub(pattern = "^(\\d{4})\\D(\\d{4})\\D(\\d{3}[0-9Xx])$",
        replacement = "0000-\\1-\\2-\\3") |>
    # Extraneous white space
    sub(pattern = "[[:blank:]]", replacement = "") |>
    # wrong or missing separator
    sub(pattern = separator_pattern, replacement = "\\1-\\3-\\5-\\7")
}


# Unbiased estimator of the population's Gini coefficient
# this is equation g in https://doi.org/10.2307/2094535
# also equation 45 in https://doi.org/10.1016/j.jeconom.2008.11.004
gini <- function(x) {
  if (is.unsorted(x)) x <- sort(x)
  n <- length(x)
  # sum(x) instead of mean(x) * n
  2 * sum(x * seq_along(x)) / (sum(x) * (n - 1)) - (n + 1) / (n - 1)
}

# In the TITLE database, some journals have their subject headings
# in upper case without diacritics, while others have them in
# title case with diacritics. The upper/title case should be dealt
# before.
normalize_jdescr <- function(str) {
  str <- stringi::stri_trans_totitle(str, locale = "pt_BR")
  uniqstr <- unique(str)
  uniqstr_ascii <- uniqstr |>
    stringi::stri_trans_general("NFKC; Any-Latin; Latin-ASCII")
  with_diacritics <- !stringi::stri_enc_isascii(uniqstr)
  # When two subject headings are the same except for diacritics,
  # make the one without diacritics equal to the other with them.
  for (i in which(with_diacritics)) {
    the_same <- which(uniqstr_ascii %in% uniqstr_ascii[i])
    str[str %in% uniqstr[the_same]] <- uniqstr[i]
  }
  str
}

# Page 12 of https://wiki.bireme.org/pt/img_auth.php/5/5f/2709BR.pdf
#
# "ISO files produced by CDS/ISIS are standard text files in the above
# described format. Because it is not practical to deal with text files
# with relatively long lines, specially when they have to be examined
# with line editors and/or transmitted by telecommunication lines,
# CDS/ISIS will divide each record in line segments of 80 characters,
# each followed by the standard line ending ctrl-M ctrl-J (Carriage
# Return / Line Feed). All segments, except for the last one, will
# contain exactly 80 text characters. This way, an ISO record with
# 835 characters will consist in 11 lines, the 10 first with 80
# characters and the last one with 35 characters."
#
# See also http://red.bvsalud.org/lilacs/wp-content/uploads/sites/2/2016/07/LILACS-2-ManualDescricao-en.pdf
#
# Subfields will not be separated.
read_isis <- function(filename, progress = interactive()) {

  file_size <- file.size(filename)

  if (file_size > 2^30) {
    warning("Large file size, consider rewriting to read in chunks.",
            immediate. = TRUE)
  }

  article_colnames <- c(
    lilacs_id = "002",
    lit_type = "005",
    treat_level = "006",
    journal_title = "030",
    issn = "035",
    pub_date = "065",
    pub_country = "067",
    n_references = "072",
    doi = "724"
  )

  # Names here are subfield tags
  author_colnames <- c(
    "_" = "name",
    "1" = "affiliation_1",
    "2" = "affiliation_2",
    "3" = "affiliation_3",
    "c" = "city",
    "s" = "state",         # non-standard
    "p" = "country",
    "k" = "orcid_id",      # non-standard
    "r" = "responsibility"
  )

  # Estimate of articles: 1 per 4.9 kB, but let's be conservative
  articles_estimate <- ceiling(file_size / 500)
  articles <- authorships <- pubtypes <-
    vector("list", length = articles_estimate)
  current_record <- 1

  d <- readLines(filename) |>
    # Better results than cp850 for this dataset
    stringi::stri_encode(from = "cp1252", to = "UTF-8")

  current_line <- 1
  if (progress) pb <- txtProgressBar(max = length(d), style = 3)

  while(current_line <= length(d)) {

    out <- read_record(d, current_line)

    # Field 030 is recurrent, but actually we expect only one value
    stopifnot(length(out[names(out) %in% "030"]) == 1)
    articles[[current_record]] <- matrix(out[article_colnames], nrow = 1)

    if (any(names(out) %in% "010")) {
      authorships[[current_record]] <- cbind(
        lilacs_id = out["002"],
        out[names(out) %in% "010"] |>
          lapply(split_subfields, names(author_colnames)) |>
          do.call(what = "rbind")
      )
    }
    if (any(names(out) %in% "071")) {
      pubtypes[[current_record]] <- cbind(
        lilacs_id = out["002"],
        pub_type  = out[names(out) %in% "071"]
      )
    }

    current_record <- current_record + 1
    current_line <- current_line + attr(out, "total_lines")
    if (progress) setTxtProgressBar(pb, value = current_line)
  }

  articles <- do.call("rbind", articles)
  colnames(articles) <- names(article_colnames)
  articles[articles == ""] <- NA_character_

  authorships <- do.call("rbind", authorships)
  # if the first article doesn't have some subfield, the whole
  # combined matrix would end up without the subfield column name.
  colnames(authorships) <- c("lilacs_id", author_colnames)
  rownames(authorships) <- NULL
  authorships[authorships == ""] <- NA_character_

  pubtypes <- do.call("rbind", pubtypes)
  rownames(pubtypes) <- NULL
  pubtypes[pubtypes == ""] <- NA_character_

  if (progress) close(pb)

  list(
    articles = articles,
    authorships = authorships,
    pubtypes = pubtypes
  )
}


# This is the same as read_isis(), but for the TITLE (journals) database.
# Data dictionary in: https://wiki.bireme.org/pt/index.php/Campos_da_base_Title
#
read_isis_title <- function(filename, progress = interactive()) {

  file_size <- file.size(filename)

  if (file_size > 2^30) {
    warning("Large file size, consider rewriting to read in chunks.",
            immediate. = TRUE)
  }

  journal_colnames <- c(
    status = "050",
    title = "100",
    title_abbrev = "150",
    country_code = "310",
    level = "330",
    script = "340",
    issn = "400"
  )
  # Names here are subfield tags
  indexing_colnames <- c(
    "_" = "source",
    "a" = "initial_volume",
    "b" = "initial_issue",
    "c" = "initial_year",
    "d" = "final_volume",
    "e" = "final_issue",
    "f" = "final_year"
  )

  # Actually each record is a hair above 1 kB, but let's be conservative
  estimated_records <- ceiling(file_size / 100)
  journals <- indexing <- subjects <-
    vector("list", estimated_records)

  current_record <- 1

  d <- readLines(filename) |>
    stringi::stri_encode(from = "cp850", to = "UTF-8")

  current_line <- 1
  if (progress) pb <- txtProgressBar(max = length(d), style = 3)

  while(current_line <= length(d)) {

    out <- read_record(d, current_line)

    journals[[current_record]] <- matrix(out[journal_colnames], nrow = 1)

    if (any(names(out) %in% "450")) {
      indexing[[current_record]] <- cbind(
        issn = journals[[current_record]][, names(journal_colnames) == "issn"],
        out[names(out) %in% "450"] |>
          lapply(split_subfields, names(indexing_colnames)) |>
          do.call(what = "rbind")
      )
    }
    if (any(names(out) %in% "440")) {
      subjects[[current_record]] <- cbind(
        issn = journals[[current_record]][, names(journal_colnames) == "issn"],
        decs  = out[names(out) %in% "440"]
      )
    }

    current_record <- current_record + 1
    current_line <- current_line + attr(out, "total_lines")
    if (progress) setTxtProgressBar(pb, value = current_line)
  }

  journals <- do.call("rbind", journals)
  colnames(journals) <- names(journal_colnames)

  indexing <- do.call("rbind", indexing)
  # if the first record doesn't have some subfield, the whole combined
  # matrix would end up without the subfield column name.
  colnames(indexing) <- c("issn", indexing_colnames)
  rownames(indexing) <- NULL

  subjects <- do.call("rbind", subjects)
  # A dozen journals don't have an ISSN. If any of them is the first one,
  # the column would go unnamed unlest we re-name it.
  colnames(subjects) <- c("issn", "decs")
  rownames(subjects) <- NULL

  if (progress) close(pb)

  list(
    journals = journals,
    indexing = indexing,
    subjects = subjects
  )
}


# See https://wiki.bireme.org/pt/img_auth.php/5/5f/2709BR.pdf
read_record <- function(d, current_line) {
  LINE_LENGTH <- 80
  FIELDTAG_LENGTH <- 3

  ### Leader
  first_line <- d[current_line]
  leader <- first_line |>
    substring(
      # ISO 2709 counts from 0, R counts from 1
      first = 1 + c(0, 5, 6, 10, 11, 12, 17, 20, 21, 22, 23),
      last = 1 + c(4, 5, 9, 10, 11, 16, 19, 20, 21, 22, 23)
    ) |>
    as.integer() |>
    setNames(nm = c(
      "register_size",
      "register_status",
      "implementation_codes",
      "indicator_size",
      "identificator_size",
      "base_address",
      "user_defined",
      "fieldsize_length",
      "fieldstart_length",
      "userdefined_length",
      "reserved"
    ))
  # Actually some fields don't need to be integer,
  # but in the case of our data they are always zero.
  stopifnot(
    leader["register_status"] == 0,
    leader["implementation_codes"] == 0,
    leader["indicator_size"] == 0,
    leader["identificator_size"] == 0,
    leader["user_defined"] == 0,
    leader["userdefined_length"] == 0,
    leader["reserved"] == 0
  )
  total_lines <- ceiling(unname(leader["register_size"]) / LINE_LENGTH)
  record <-
    d[seq.int(from = current_line, to = current_line + total_lines - 1)] |>
    paste0(collapse = "")

  ### Directory
  triple_length <-
    FIELDTAG_LENGTH +
    leader["fieldsize_length"] +
    leader["fieldstart_length"]
  nfields <- (leader["base_address"] - 24 - 1) / triple_length
  stopifnot(as.integer(nfields) == nfields)
  stopifnot(nfields >= 1)
  field_tags <- substring(
    record,
    # ISO 2709 counts from 0, R counts from 1
    first = 24 + 1 +
      (seq.int(nfields) - 1) * triple_length,
    last = 24 + FIELDTAG_LENGTH +
      (seq.int(nfields) - 1) * triple_length
  )
  field_lengths <- substring(
    record,
    # ISO 2709 counts from 0, R counts from 1
    first = 24 + 1 + FIELDTAG_LENGTH +
      (seq.int(nfields) - 1) * triple_length,
    last = 24 + FIELDTAG_LENGTH + leader["fieldsize_length"] +
      (seq.int(nfields) - 1) * triple_length
  ) |> as.integer()
  field_starts <- substring(
    record,
    # ISO 2709 counts from 0, R counts from 1
    first = 24 + 1 + FIELDTAG_LENGTH + leader["fieldsize_length"] +
      (seq.int(nfields) - 1) * triple_length,
    last = 24 + triple_length +
      (seq.int(nfields) - 1) * triple_length
  ) |> as.integer()

  ### Fields
  fields <- substring(
    # This is the performance bottleneck,
    # but regmatches() would be ~ 6x slower
    record,
    first = leader["base_address"] + field_starts + 1,
    last  = leader["base_address"] + field_starts + field_lengths
  ) |>
    # string the separator at the end
    {\(x) substring(x, 1, nchar(x) - 1)}() |>
    setNames(field_tags)

  attr(fields, "total_lines") <- total_lines

  fields
}

split_subfields <- function(x, tags) {
  caret_pos <- gregexpr("^", x, fixed = TRUE)[[1]]
  if (length(caret_pos) == 1 && caret_pos == -1) return(
    # There's no "^" in the string
    setNames(c(x, rep(NA_character_, length(tags) - 1)), tags)
  )
  # This assumes the undocumented rule that an untagged subfield
  # will be at the beginning of the field.
  actual_tags <- c("_", substring(x, caret_pos + 1, caret_pos + 1))
  # Position of each fields' first and last characters
  subfield_first <- c(-1, caret_pos + 2)
  subfield_last <- c(caret_pos - 1, nchar(x[[1]]))
  out <- x |>
    substring(first = subfield_first, last = subfield_last) |>
    # This assumes the undocumented rule that
    # there can be only one of each subfield.
    setNames(actual_tags)
  out[tags]
}


weighted.quantile <- function(x, w = 1, probs = seq(0, 1, 0.25),
                              na.rm = FALSE, names = TRUE) {

  if (any(probs > 1) | any(probs < 0)) stop("'probs' outside [0,1]")

  if (length(w) == 1) w <- rep(w, length(x))
  if (length(w) != length(x)) stop("w must have length 1 or be as long as x")

  if (isTRUE(na.rm)) {
    w <- x[!is.na(x)]
    x <- x[!is.na(x)]
  }

  w <- w[order(x)] / sum(w)
  x <- x[order(x)]

  if (length(x) == 1) {
    res <- rep(x, length(probs))
  } else {
    cum_w <- cumsum(w) - w * (1 - (seq_along(w) - 1) / (length(w) - 1))
    res <- approx(x = cum_w, y = x, xout = probs, ties = "ordered")$y
  }
  # Accessing non-exported functions is not great practice, generally,
  # but we want to ensure the labels are identical to those of
  # non-weighted quantiles.
  if (isTRUE(names)) names(res) <- stats:::format_perc(probs, "%")

  res
}
