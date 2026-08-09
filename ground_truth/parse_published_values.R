# aggarwal_etal_2023/ground_truth/parse_published_values.R
# Output: ground_truth/published_appendix_values.csv,
#   ground_truth/published_maintext_tables.csv
# Depends on: the published article and Supplementary Information PDFs, and
#   pdftotext (poppler) on the PATH
# Description: Transcribe every published cell of the article's four large
#   floats. Supplementary Tables 1, 2 and 3 carry 701 numbers between them and
#   main-text Table 1 carries 95, none of which can be hand transcribed
#   reliably. Figures 1 and 2 print an estimate and a standard error on the face
#   of every point, 114 and 36 numbers, which makes them published tables in
#   disguise; they are read positionally from the PDF's text layer, never in
#   reading order, and they are read from the figures themselves rather than
#   from the Supplementary Tables the captions point at, so that agreement
#   between figure and table is a finding rather than an assumption.
#
#   Neither PDF is redistributed with this repository. Point
#   PUBLISHED_MATERIALS_DIR at a directory holding aggarwal_etal_2023.pdf and
#   aggarwal_etal_2023_appendix.pdf and re-run; the committed CSVs are the
#   output of that run and are what the ground truth reads.

library(tidyverse)
library(here)

here::i_am("ground_truth/parse_published_values.R")

materials_dir <- Sys.getenv("PUBLISHED_MATERIALS_DIR",
                            unset = here::here("published_materials"))
article_pdf <- file.path(materials_dir, "aggarwal_etal_2023.pdf")
appendix_pdf <- file.path(materials_dir, "aggarwal_etal_2023_appendix.pdf")

if (!file.exists(article_pdf) || !file.exists(appendix_pdf)) {
  stop("Set PUBLISHED_MATERIALS_DIR to a directory holding aggarwal_etal_2023.pdf ",
       "and aggarwal_etal_2023_appendix.pdf. Looked in: ", materials_dir)
}

# Typography, not arithmetic: the article prints U+2013 and U+2212 for the minus
# sign in different places, and a cell printed -0.000 is the same published
# claim as one printed 0.000. Both instruments normalise signed zero, so the
# transcription side must too.
normalise_number <- function(x) {
  x |>
    str_replace_all("−|–|—", "-") |>
    str_remove_all(",") |>
    str_replace("^-(0(\\.0+)?)$", "\\1")
}

pdf_layout_lines <- function(pdf, first = NULL, last = NULL) {
  out <- tempfile(fileext = ".txt")
  args <- c("-layout")
  if (!is.null(first)) args <- c(args, "-f", first)
  if (!is.null(last)) args <- c(args, "-l", last)
  status <- system2("pdftotext", c(args, shQuote(pdf), shQuote(out)))
  stopifnot(status == 0, file.exists(out))
  read_lines(out)
}

# Words with their bounding boxes, one row per word. This is the only honest way
# to read a figure that prints numbers on its face: reading order interleaves
# axis labels, facet strips and estimates.
pdf_words <- function(pdf, page) {
  out <- tempfile(fileext = ".xml")
  status <- system2("pdftotext",
                    c("-bbox-layout", "-f", page, "-l", page,
                      shQuote(pdf), shQuote(out)))
  stopifnot(status == 0, file.exists(out))
  raw <- read_file(out)
  m <- str_match_all(
    raw,
    '<word xMin="([0-9.]+)" yMin="([0-9.]+)" xMax="([0-9.]+)" yMax="([0-9.]+)">(.*?)</word>'
  )[[1]]
  tibble(
    x_min = as.numeric(m[, 2]),
    y_min = as.numeric(m[, 3]),
    x_max = as.numeric(m[, 4]),
    y_max = as.numeric(m[, 5]),
    text = m[, 6] |> str_replace_all("&gt;", ">") |> str_replace_all("&lt;", "<") |>
      str_replace_all("&amp;", "&")
  )
}

# Supplementary Tables 1, 2 and 3 ----
# Each data row ends in a fixed number of numeric columns, so the label columns
# are whatever precedes them. The label is split on the adjustment set, which is
# one of three known strings, rather than on whitespace, because both the
# covariate and the level can contain spaces.
appendix_lines <- pdf_layout_lines(appendix_pdf)

adjustment_labels <- c("Unadjusted", "PAP adjustment set", "Full adjustment set")

parse_numeric_tail <- function(line, n) {
  tokens <- str_split(str_squish(line), " ")[[1]]
  tail(tokens, n)
}

# Supplementary Table 1: 57 rows of Estimate, SE, df, t, p, CI lower, CI upper.
st1_start <- which(str_detect(appendix_lines, "^\\s*Supplementary Table 1:"))[1]
st2_start <- which(str_detect(appendix_lines, "^\\s*Supplementary Table 2:"))[1]
st3_start <- which(str_detect(appendix_lines, "^\\s*Supplementary Table 3:"))[1]
stopifnot(!is.na(st1_start), !is.na(st2_start), !is.na(st3_start),
          st1_start < st2_start, st2_start < st3_start)

quantities_7 <- c("estimate", "se", "df", "t", "p_value", "ci_lower", "ci_upper")
quantities_8 <- c("estimate", "se", "df", "t", "p_value", "p_value_bh",
                  "ci_lower", "ci_upper")

# digits is a property of the published cell and both instruments read it from
# here, so it is measured off the string rather than assumed per column.
printed_digits <- function(x) {
  if_else(str_detect(x, "\\."), str_length(str_remove(x, "^[^.]*\\.")), 0L)
}

parse_labelled_table <- function(lines, quantities, label_pattern) {
  n <- length(quantities)
  keep <- lines[str_detect(lines, label_pattern)]
  keep <- keep[str_count(keep, "-?[0-9]+\\.?[0-9]*") >= n]
  tibble(line = keep) |>
    mutate(
      values = map(line, function(l) parse_numeric_tail(l, n)),
      label = map2_chr(line, values, function(l, v) {
        str_squish(str_remove(str_squish(l), paste0(paste(v, collapse = " "), "$")))
      })
    ) |>
    mutate(row_index = row_number()) |>
    unnest_longer(values, indices_to = "position") |>
    mutate(quantity = quantities[position],
           value_paper = normalise_number(values)) |>
    select(row_index, label, quantity, value_paper) |>
    mutate(digits = printed_digits(value_paper))
}

st1 <- parse_labelled_table(
  appendix_lines[st1_start:(st2_start - 1)],
  quantities_7,
  paste0("(", paste(adjustment_labels, collapse = "|"), ")")
) |>
  mutate(
    adjustment = str_extract(label, paste(adjustment_labels, collapse = "|")),
    label_head = str_squish(str_remove(label, paste(adjustment_labels, collapse = "|"))),
    covariate = case_when(
      str_starts(label_head, "Trump support") ~ "Trump support",
      TRUE ~ word(label_head, 1)
    ),
    level = str_squish(str_remove(label_head, fixed(covariate))),
    float = "supplementary_table_1",
    row_key = str_glue("{covariate} | {level} | {adjustment}")
  )

st2 <- parse_labelled_table(
  appendix_lines[st2_start:(st3_start - 1)],
  quantities_7,
  paste0("(", paste(adjustment_labels, collapse = "|"), ")")
) |>
  mutate(
    adjustment = str_extract(label, paste(adjustment_labels, collapse = "|")),
    label_head = str_squish(str_remove(label, paste(adjustment_labels, collapse = "|"))),
    target = str_extract(label_head,
                         "Difference-in-CATEs TSS 60-70 versus TSS 30-40|Treatment\\*TSS interaction term from linear model"),
    outcome = str_squish(str_remove(label_head, fixed(target))),
    float = "supplementary_table_2",
    row_key = str_glue("{target} | {outcome} | {adjustment}")
  )

st3 <- parse_labelled_table(
  appendix_lines[st3_start:length(appendix_lines)],
  quantities_8,
  "^\\s*(Party|TSS|Trump support score|Turnout score|Ideology score|Partisanship score|Voted in)"
) |>
  mutate(float = "supplementary_table_3", row_key = label)

stopifnot(n_distinct(st1$row_key) == 57, n_distinct(st2$row_key) == 18,
          n_distinct(st3$row_key) == 22)

appendix_values <-
  bind_rows(st1, st2, st3) |>
  select(float, row_key, quantity, value_paper, digits) |>
  arrange(float, row_key, quantity, .locale = "en")

stopifnot(nrow(appendix_values) == 57 * 7 + 18 * 7 + 22 * 8)
write_csv(appendix_values, here::here("ground_truth", "published_appendix_values.csv"))

# Main-text Table 1 ----
# A genuine typeset table with fixed columns, so it is read from the layout
# text. Every row ends in five numbers and begins with three labels; the total
# row has two empty label cells and is matched on its own word.
article_lines <- pdf_layout_lines(article_pdf)
t1_start <- which(str_detect(article_lines, "^Table 1 \\| Experimental strata"))[1]
stopifnot(!is.na(t1_start))
t1_block <- article_lines[t1_start:(t1_start + 30)]

t1_quantities <- c("control_n", "treatment_n", "p_treat",
                   "voting_rate_control", "voting_rate_treatment")

t1_rows <- t1_block[str_detect(t1_block, "^\\s+(Female|Other|Total)\\s")]
stopifnot(length(t1_rows) == 19)

table_1 <-
  tibble(line = t1_rows) |>
  mutate(
    values = map(line, function(l) parse_numeric_tail(l, 5)),
    label = map2_chr(line, values, function(l, v) {
      str_squish(str_remove(str_squish(l), paste0(paste(v, collapse = " "), "$")))
    })
  ) |>
  unnest_longer(values, indices_to = "position") |>
  mutate(
    float = "table_1",
    row_key = label,
    quantity = t1_quantities[position],
    value_paper = normalise_number(values),
    digits = printed_digits(value_paper)
  ) |>
  select(float, row_key, quantity, value_paper, digits)

stopifnot(nrow(table_1) == 95)

# Figures 1 and 2 ----
# Read positionally. Within a row the three estimate/standard error pairs are
# ranked left to right, which is the panel order the column headers give. Rows
# are grouped into facet bands by the gap between them, and a band takes the
# facet strip words that fall inside it. Nearest-strip-word assignment was
# tried first and is wrong: Figure 1's Black row sits 21.21 points below the
# Race strip and 21.18 above the Margin strip, so it lands in the wrong facet by
# three hundredths of a point. Nothing here is taken from the Supplementary
# Tables, so that agreement between the figures and the tables is a finding.
figure_face_values <- function(pdf, page, strip_x_min, label_x_max,
                               n_rows, n_panels = 3) {
  all_words <- pdf_words(pdf, page)

  # The figure's own region is everything above its caption. Both pages carry
  # body text below the figure, and page 3's prose prints numbers in the same
  # three-decimal form as the estimates on the plot.
  caption_top <- all_words |> filter(text == "Fig.") |> pull(y_min) |> min()
  words <- all_words |> filter(y_max < caption_top)

  estimates <- words |>
    filter(str_detect(text, "^[−–-]?[0-9]\\.[0-9]{3}$")) |>
    arrange(y_min, x_min)
  ses <- words |>
    filter(str_detect(text, "^\\([0-9]\\.[0-9]{3}\\)$")) |>
    arrange(y_min, x_min)
  stopifnot(nrow(estimates) == n_rows * n_panels, nrow(ses) == n_rows * n_panels)

  # Rows: cluster on y. Points within a facet row share a y to within a point.
  band <- function(y, tol = 3) cumsum(c(1, diff(sort(unique(round(y, 1)))) > tol))
  ykeys <- sort(unique(round(estimates$y_min, 1)))
  row_of <- tibble(ykey = ykeys, row_band = band(ykeys))

  estimates <- estimates |>
    mutate(ykey = round(y_min, 1)) |>
    left_join(row_of, by = "ykey")
  ses <- ses |>
    mutate(ykey = round(y_min, 1)) |>
    left_join(row_of, by = "ykey")
  stopifnot(max(estimates$row_band) == n_rows)

  paired <- estimates |>
    group_by(row_band) |>
    arrange(x_min, .by_group = TRUE) |>
    mutate(panel = row_number()) |>
    ungroup() |>
    select(row_band, panel, y_row = y_min, x_min, estimate = text) |>
    left_join(
      ses |>
        group_by(row_band) |>
        arrange(x_min, .by_group = TRUE) |>
        mutate(panel = row_number()) |>
        ungroup() |>
        select(row_band, panel, se = text),
      by = c("row_band", "panel")
    )
  stopifnot(nrow(paired) == n_rows * n_panels, !any(is.na(paired$se)))

  # Row labels: the words to the left of the plotting region sitting just below
  # each row of values.
  row_y <- paired |> group_by(row_band) |> summarize(y_row = min(y_row), .groups = "drop")
  labels <- words |>
    filter(x_max <= label_x_max) |>
    mutate(ykey = round(y_min, 1)) |>
    group_by(ykey) |>
    summarize(label = paste(text[order(x_min)], collapse = " "), .groups = "drop")
  row_labels <- row_y |>
    mutate(label = map_chr(y_row, function(y) {
      candidates <- labels |> filter(ykey > y, ykey < y + 14)
      stopifnot(nrow(candidates) >= 1)
      candidates$label[which.min(candidates$ykey)]
    }))

  # Facet bands: rows within one facet are evenly spaced and the step between
  # facets is larger, so the break is any gap more than a quarter above the
  # median. The strip label is the rotated text down the right-hand edge lying
  # inside the band, read across its columns from the outer one inwards.
  row_gaps <- diff(row_y$y_row)
  facet_break <- row_gaps > 1.25 * median(row_gaps)
  banded <- row_y |>
    mutate(facet = cumsum(c(1, as.integer(facet_break))),
           pad = median(row_gaps) / 2)

  facet_extent <- banded |>
    group_by(facet) |>
    summarize(y_lo = min(y_row) - first(pad), y_hi = max(y_row) + first(pad),
              .groups = "drop")

  strip_words <- words |> filter(x_min >= strip_x_min)
  facet_extent <- facet_extent |>
    mutate(strip = map2_chr(y_lo, y_hi, function(lo, hi) {
      inside <- strip_words |>
        filter(y_min > lo, y_min < hi) |>
        arrange(desc(x_min), y_min)
      stopifnot(nrow(inside) >= 1)
      paste(inside$text, collapse = " ")
    }))

  panel_names <- c("Unadjusted", "PAP adjustment set", "Full adjustment set")

  paired |>
    left_join(row_labels |> select(row_band, label), by = "row_band") |>
    left_join(banded |> select(row_band, facet), by = "row_band") |>
    left_join(facet_extent |> select(facet, strip), by = "facet") |>
    mutate(estimator = panel_names[panel]) |>
    select(strip, label, estimator, estimate, se) |>
    pivot_longer(c(estimate, se), names_to = "quantity", values_to = "value_paper") |>
    mutate(
      row_key = str_glue("{strip} | {label} | {estimator}"),
      value_paper = normalise_number(str_remove_all(value_paper, "[()]")),
      digits = printed_digits(value_paper)
    ) |>
    select(row_key, quantity, value_paper, digits)
}

figure_1 <- figure_face_values(article_pdf, page = 2, strip_x_min = 470,
                               label_x_max = 190, n_rows = 19) |>
  mutate(float = "figure_1")
figure_2 <- figure_face_values(article_pdf, page = 3, strip_x_min = 470,
                               label_x_max = 185, n_rows = 6) |>
  mutate(float = "figure_2")

stopifnot(nrow(figure_1) == 114, nrow(figure_2) == 36)

maintext_values <-
  bind_rows(table_1, figure_1, figure_2) |>
  select(float, row_key, quantity, value_paper, digits) |>
  arrange(float, row_key, quantity, .locale = "en")

write_csv(maintext_values, here::here("ground_truth", "published_maintext_tables.csv"))

print(appendix_values |> count(float))
print(maintext_values |> count(float))
