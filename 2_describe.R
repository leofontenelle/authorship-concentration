# Libraries, functions and constants ####

source("0_functions.R")

library(data.table) |> suppressPackageStartupMessages()
library(ggplot2)
library(gridExtra)
library(scales)

journal_descriptors <- c(
  "ALLERGY AND IMMUNOLOGY", "ANATOMY", "ANESTHESIOLOGY", "ANTHROPOLOGY",
  "ANTI-BACTERIAL AGENTES", "AUDIOLOGY", "BACTERIOLOGY", "BEHAVIORAL SCIENCES",
  "BIOCHEMISTRY", "BIOETHICS", "BIOLOGY", "BIOMEDICAL ENGINEERING",
  "BIOPHYSICS", "BIOTECHNOLOGY", "BOTANY", "CARDIOLOGY", "CHEMISTRY",
  "COMMUNICABLE DISEASES", "COMPLEMENTARY THERAPIES", "CRITICAL CARE",
  "DELIVERY OF HEALTH CARE", "DENTISTRY", "DERMATOLOGY", "DIAGNOSTIC IMAGING",
  "EDUCATION", "EMERGENCY MEDICINE", "ENDOCRINOLOGY", "ENVIRONMENTAL HEALTH",
  "EPIDEMIOLOGY", "ETHICS", "FAMILY PRACTICE", "GASTROENTEROLOGY", "GENETICS",
  "GERIATRICS", "GERONTOLOGY", "GYNECOLOGY", "HEALTH SERVICES", "HEMATOLOGY",
  "HISTOLOGY", "HISTORY OF MEDICINE", "HOMEOPATHY", "HOSPITALS",
  "INTERNAL MEDICINE", "JURISPRUDENCE", "LIBRARY SCIENCE",
  "MEDICAL INFORMATICS", "MEDICINE", "METABOLISM", "MICROBIOLOGY",
  "MILITARY MEDICINE", "MOLECULAR BIOLOGY", "NEOPLASMS", "NEPHROLOGY",
  "NEUROLOGY", "NEUROSURGERY", "NUCLEAR MEDICINE", "NURSING",
  "NUTRITIONAL SCIENCES", "OBSTETRICS", "OCCUPATIONAL MEDICINE",
  "OPHTHALMOLOGY", "OPTOMETRY", "ORTHODONTICS", "ORTHOPEDICS",
  "OSTEOPATHIC MEDICINE", "OTOLARYNGOLOGY", "PARASITOLOGY", "PATHOLOGY",
  "PEDIATRICS", "PERINATOLOGY", "PHARMACOLOGY", "PHARMACY",
  "PHYSICAL EDUCATION", "PHYSIOLOGY", "PSYCHIATRY", "PSYCHOLOGY",
  "PSYCHOPHARMACOLOGY", "PSYCHOPHYSIOLOGY", "PUBLIC ADMINISTRATION",
  "PUBLIC HEALTH", "PULMONARY DISEASE (SPECIALTY)", "RADIOLOGY",
  "RADIOTHERAPY", "REPRODUCTIVE MEDICINE", "RHEUMATOLOGY", "SCIENCE",
  "SEXUALLY TRANSMITTED DISEASES", "SOCIAL MEDICINE", "SOCIAL SCIENCES",
  "SPEECH-LANGUAGE PATHOLOGY", "SPORTS MEDICINE", "SURGERY", "THERAPEUTICS",
  "TOXICOLOGY", "TRAUMATOLOGY", "TROPICAL MEDICINE", "UROLOGY",
  "VETERINARY MEDICINE", "VIROLOGY", "VITAL STATISTICS")
journal_descriptors <- setNames(
  # will map to these names
  object = journal_descriptors |> tolower() |> tools::toTitleCase(),
  # will map from these names
  nm = journal_descriptors
)
# c(`ALLERGY AND IMMUNOLOGY` = "Allergy and Immunology", ANATOMY = "Anatomy",
#   ANESTHESIOLOGY = "Anesthesiology", ANTHROPOLOGY = "Anthropology", ...)


# Read data ####

if (!dir.exists("data")) dir.create("data")
md5sums <- c(
  "data/journal_descriptors.csv" = "7ac81c8cb236287f66746478ecc8f0df",
  "data/journals_in_LILACS_2021-01-10.csv" = "1991bad0fb195d77361be8445e0b5031",
  "data/authorship_concentration.csv" = "aaf3b82df41389584c3224dfc9ce69fc",
  "data/journal_info_flat V2.csv" = "363e0931a4379656cca9642725463f55"
)
for (nm in head(names(md5sums), -1)) {
  if (file.exists(nm)) next
  message(sprintf("%s not found; dowloading.", nm))
  source_url <- basename(nm) |>
    sprintf(fmt = "https://zenodo.org/record/6126801/files/%s?download=1") |>
    URLencode()
  download.file(source_url, destfile = nm, mode = "wb")
}
for (nm in tail(names(md5sums), 1)) {
  if (file.exists(nm)) next
  message(sprintf("%s not found; dowloading.", nm))
  source_url <- "https://osf.io/bvzfp/download"
  download.file(source_url, destfile = nm, mode = "wb")
}
for (nm in names(md5sums)) {
  if (tools::md5sum(nm) != md5sums[nm])
    warning(sprintf(paste(
      "%s does not look like what it was expected to;",
      "ensure the data are correct to ensure reproducibility."),
      nm))
}

journals <- "data/journals_in_LILACS_2021-01-10.csv" |>
  fread(encoding = "UTF-8", key = "issn")
jdescr <- "data/journal_descriptors.csv" |>
  fread( encoding = "UTF-8", key = "issn")
jdescr[, journal_descriptor := journal_descriptors[journal_descriptor]]
authorship_concentration <- "data/authorship_concentration.csv" |>
  fread(encoding = "UTF-8", key = "issn")
journals <- journals[authorship_concentration]
rm(authorship_concentration, journal_descriptors)

medline <- fread("data/journal_info_flat V2.csv") |>
  subset(DP == "All" & On == "All" & `article number with authors` >= 50 &
           !is.na(PPMP) & !is.na(gini))
medline[, weight := calculate_weights(
  `article number with authors`,
  journals$n_signed,
  log = TRUE
)]


# Table 1 ####

# These two "reshape" functions format the quantiles for
# inclusion in Table 1
format_table1_integers <- function(x) {
  c(median_iiq = sprintf("%.0f [%.0f\u2013%.0f]", x["50%"], x["25%"], x["75%"]),
    range = sprintf("%.0f\u2013%.0f", x["0%"], x["100%"]))
}
format_table1_reals <- function(x) {
  c(median_iiq = sprintf("%.3f [%.3f\u2013%.3f]", x["50%"], x["25%"], x["75%"]),
    range = sprintf("%.3f\u2013%.3f", x["0%"], x["100%"]),
    p95 = sprintf("%.3f", x["95%"]))
}
# transforms a matrix to a vector, and names it with the merge of the
# matrix's column names and row names.
linearize <- function(m) {
  colnm <- colnames(m)
  rownm <- rownames(m)
  nm <- paste(rep(colnm, each = length(rownm)),
              rep(rownm, times = length(colnm)))
  setNames(as.vector(m), nm)
}
table1_lilacs_integer <- journals[
  , sapply(.SD, quantile, probs = c(0, .25, .50, .75, .95, 1)),
  .SDcols = c("n_articles", "n_signed", "n_MPA")] |>
  apply(2, format_table1_integers) |>
  linearize()
table1_medline_integer <- medline[, sapply(.SD, weighted.quantile,
  w = weight, probs = c(0, .25, .50, .75, .95, 1)),
  .SDcols = c(n_articles = "article number",
              n_signed = "article number with authors",
              n_MPA = "MP_article_number")]|>
  apply(2, format_table1_integers) |>
  linearize()
table1_lilacs_continuous <- journals[
  , sapply(.SD, quantile, probs = c(0, .25, .50, .75, .95, 1)),
  .SDcols = c("PPMP", "gini")] |>
  {\(x) {x[, "PPMP"] <- 100 * x[, "PPMP"]; x}}() |>
  apply(2, format_table1_reals) |>
  linearize()
table1_medline_continuous <- medline[, sapply(.SD, weighted.quantile,
  w = weight, probs = c(0, .25, .50, .75, .95, 1)),
  .SDcols = c("PPMP", "gini")] |>
  {\(x) {x[, "PPMP"] <- 100 * x[, "PPMP"]; x}}() |>
  apply(2, format_table1_reals) |>
  linearize()
table1_lilacs_tied <- sprintf(
  "%d (%.1f%%)",
  journals[, sum(n_ex_aequo_MPA > 1)],
  journals[, 100 * mean(n_ex_aequo_MPA > 1)]
)
table1_medline_tied <- sprintf(
  "%.1f (%.1f%%)",
  medline[, sum((`ex aequo number` > 1) * weight * .N)],
  medline[, 100 * weighted.mean(`ex aequo number` > 1, weight)]
)
table1 <- cbind(
  LILACS = c(
    n = as.character(journals[, .N]),
    table1_lilacs_integer,
    tied_as_MPA = table1_lilacs_tied,
    table1_lilacs_continuous
  ),
  MEDLINE = c(
    n = as.character(medline[, .N]),
    table1_medline_integer,
    tied_as_MPA = table1_medline_tied,
    table1_medline_continuous
  )
)
rm(table1_lilacs_integer, table1_lilacs_continuous, table1_lilacs_tied,
   table1_medline_integer, table1_medline_continuous, table1_medline_tied)


# Figure 1 ####

fig1a <- journals |>
  subset(PPMP < 0.3) |>
  ggplot(aes(n_signed, PPMP)) +
  geom_hline(yintercept = quantile(journals$PPMP, 0.95),
             # https://jfly.uni-koeln.de/color/
             colour = palette.colors()[2]) +
  annotate("text",
           x = max(journals$n_signed),
           y = quantile(journals$PPMP, 0.95),
           label= "95th percentile",
           colour = palette.colors()[2],
           size = 2,
           hjust = 1, vjust = -0.5) +
  geom_point(alpha = 0.25, shape = "bullet") +
  scale_x_log10() +
  scale_y_continuous(labels = percent, limits = c(0, 0.3)) +
  labs(x = "Authored articles", y = "By most profilic author", tag = "A") +
  theme_bw() +
  theme(text = element_text(size = 6), aspect.ratio = 1)

fig1b <- journals |>
  ggplot(aes(n_signed, gini)) +
  geom_hline(yintercept = quantile(journals$gini, 0.95),
             # https://jfly.uni-koeln.de/color/
             colour = palette.colors()[2]) +
  annotate("text",
           x = max(journals$n_signed),
           y = quantile(journals$gini, 0.95),
           label = "95th percentile",
           colour = palette.colors()[2],
           size = 2,
           hjust = 1, vjust = -0.5) +
  geom_point(alpha = 0.25, shape = "bullet") +
  scale_x_log10() +
  labs(x = "Authored articles", y = "Gini coefficient", tag = "B") +
  theme_bw() +
  theme(text = element_text(size = 6), aspect.ratio = 1)

fig1c <- journals |>
  subset(PPMP < 0.3) |>
  ggplot(aes(gini, PPMP, colour = log(n_articles))) +
  geom_vline(xintercept = quantile(journals$gini, 0.95),
             # https://jfly.uni-koeln.de/color/
             colour = palette.colors()[2]) +
  annotate("text",
           x = quantile(journals$gini, 0.95),
           y = round(sort(journals$PPMP, decreasing = TRUE)[2], 1),
           label = "95th percentile",
           angle = 90,
           colour = palette.colors()[2],
           size = 2,
           hjust = 1, vjust = -0.5) +
  geom_hline(yintercept = quantile(journals$PPMP, 0.95),
             # https://jfly.uni-koeln.de/color/
             colour = palette.colors()[2]) +
  annotate("text",
           x = min(journals$gini),
           y = quantile(journals$PPMP, 0.95),
           label = "95th percentile",
           colour = palette.colors()[2],
           size = 2,
           hjust = 0, vjust = -0.5) +
  geom_point(shape = "bullet") +
  scale_y_continuous(labels = percent, limits = c(0, 0.3)) +
  scale_colour_viridis_c(name = "Authored articles",
                         trans = "reverse",
                         breaks = ~ quantile(.x, 0:4/4),
                         labels = ~ quantile(.x, 0:4/4) |> exp() |> round()) +
  labs(x = "Gini coefficient", y = "By most profilic author", tag = "C") +
  theme_bw() +
  theme(text = element_text(size = 6),
        aspect.ratio = 1,
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.key.size = unit(6, "point"),
        legend.background = element_blank())

fig1 <- arrangeGrob(fig1a, fig1b, fig1c, nrow = 1)


# Figures 2 and 3 ####

predict_concentration <- function(n_signed, PPMP, gini, min.obs = 20) {
  stopifnot(length(n_signed) == length(PPMP),
            length(PPMP)== length(gini))
  if (length(n_signed) < 20) return(NULL)
  log_n <- log(n_signed)
  new_x <- unique(round(exp(seq(min(log_n), max(log_n), length.out = 101))))
  m1 <- smooth.spline(log(n_signed), qlogis(PPMP), df = 3)
  p1 <- plogis(predict(m1, log(new_x))$y)
  m2 <- smooth.spline(log(n_signed), qlogis(gini), df = 3)
  p2 <- plogis(predict(m2, log(new_x))$y)
  list(n_signed = new_x, PPMP = p1, gini = p2)
}
predictions_by_country <- journals[
  gini > 0,
  predict_concentration(n_signed, PPMP, gini),
  keyby = "country"
]
journal_by_descriptor <-
  journals[jdescr[, issn, by = .(descriptor = journal_descriptor)],
           .(issn, n_signed, n_MPA, PPMP, gini, descriptor),
           on = "issn"][!is.na(n_signed)] |>
  setorder(descriptor)
predictions_by_descriptor <- journal_by_descriptor[
  , predict_concentration(n_signed, PPMP, gini),
  keyby = "descriptor"
]

fig2a <- fig1a +
  geom_point(shape = "bullet") + # remove transparency for better contrast
  geom_line(aes(n_signed, PPMP, group = country),
            predictions_by_country,
            colour = palette.colors()[3])
fig2b <- fig1b +
  geom_point(shape = "bullet") + # remove transparency for better contrast
  geom_line(aes(n_signed, gini, group = country),
            predictions_by_country,
            colour = palette.colors()[3])
fig2 <- arrangeGrob(fig2a, fig2b, nrow = 1)

fig3a <- fig1a +
  geom_point(shape = "bullet") + # remove transparency for better contrast
  geom_line(aes(n_signed, PPMP, group = descriptor),
            predictions_by_descriptor,
            colour = palette.colors()[3])
fig3b <- fig1b +
  geom_point(shape = "bullet") + # remove transparency for better contrast
  geom_line(aes(n_signed, gini, group = descriptor),
            predictions_by_descriptor,
            colour = palette.colors()[3])
fig3 <- arrangeGrob(fig3a, fig3b, nrow = 1)


# Export tables and figures ####

fwrite(as.data.table(table1, keep.rownames = TRUE), "table_1.csv", bom = TRUE)
ggsave("Fig1.png", fig1, width = 16.5, height = 5.5, units = "cm")
ggsave("Fig2.png", fig2, width = 11, height = 5.5, units = "cm")
ggsave("Fig3.png", fig3, width = 11, height = 5.5, units = "cm")

print(sprintf("Countries (out of %d):", uniqueN(journals$country)))
print(journals[, .(N = uniqueN(issn)), keyby = country][N >= 20])

print(sprintf("Descriptors (out of %d):", uniqueN(jdescr$journal_descriptor)))
print(jdescr[, .(N = uniqueN(issn)), keyby = journal_descriptor][N >= 20])
