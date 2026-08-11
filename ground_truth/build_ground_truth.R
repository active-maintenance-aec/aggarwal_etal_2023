# aggarwal_etal_2023/ground_truth/build_ground_truth.R
# Output: ground_truth/aggarwal_etal_2023_ground_truth.csv,
#   ground_truth/float_coverage.csv
# Depends on: ground_truth/published_claims.csv, ground_truth/published_appendix_values.csv,
#   ground_truth/published_maintext_tables.csv, ground_truth/archive_values.csv,
#   maintained/output/*, maintained/in_text_claims.R
# Description: Build the comparison between what the article prints, what the
#   deposited code produces and what the maintained rewrite produces, then run
#   the coverage gate over the second instrument. Every number here other than
#   value_paper is read out of a pipeline output; value_paper comes only from
#   the article, through the extraction and the two positional transcriptions.

library(tidyverse)
library(here)

here::i_am("ground_truth/build_ground_truth.R")

paper_id <- "aggarwal_etal_2023"

# A value_paper column whose entries all look numeric is guessed as a double,
# which silently turns -0.00 into 0 and 0.020 into 0.02. Every reader of these
# files forces the type.
published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)
published_appendix <- read_csv(
  here::here("ground_truth", "published_appendix_values.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)
published_maintext <- read_csv(
  here::here("ground_truth", "published_maintext_tables.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)

# Normalisation and comparison ------------------------------------------------

# Typography rather than arithmetic: the house rule is a leading zero
# everywhere, the Unicode minus is not a hyphen, a thousands separator is not a
# digit, and a cell printed -0.00 is the same claim as one printed 0.00. What is
# preserved is the number of decimals, the only typographic fact the comparison
# needs.
normalise_printed <- function(x) {
  x |>
    str_replace_all("−|–|—", "-") |>
    str_remove_all(",") |>
    str_replace("^(-?)\\.", "\\10.") |>
    str_replace("^-(0(\\.0+)?)$", "\\1")
}

# Render a number at the article's precision, with signed zero normalised on
# this side too. Whichever instrument normalises, both must.
render_at <- function(x, digits) {
  out <- sprintf(paste0("%.", digits, "f"), x)
  str_replace(out, "^-(0(\\.0+)?)$", "\\1")
}

# The article's units, applied to a pipeline value at the comparison point.
convert_units <- function(x, units) {
  case_when(
    units %in% c("percentage_points", "percent") ~ x * 100,
    units %in% c("million", "million_dollars") ~ x / 1e6,
    .default = x
  )
}

# The rewrite is compared by exact string equality at the page's precision, with
# an epsilon so that a value sitting a hair from the rounding boundary is not
# rejected by floating point.
agrees_exactly <- function(value, value_paper, digits) {
  if (is.na(value) || is.na(value_paper) || is.na(digits)) return(NA_integer_)
  target <- as.numeric(normalise_printed(value_paper))
  as.integer(render_at(value, digits) == normalise_printed(value_paper) |
               abs(round(value, digits) - target) < 1e-9 * max(1, abs(target)))
}

# The deposit's Table 1 values are read back out of formatted strings, so they
# arrive already rounded. Comparing a pre-rounded value against a published one
# at half a unit in the last digit manufactures mismatches, so the archive
# column is allowed a full unit in the last printed digit.
agrees_loosely <- function(value, value_paper, digits) {
  if (is.na(value) || is.na(value_paper) || is.na(digits)) return(NA_integer_)
  as.integer(abs(value - as.numeric(normalise_printed(value_paper))) <=
               10^(-digits) + 1e-9)
}

# Checks that depend only on the extraction, before anything consumes it -------

stopifnot(
  !any(duplicated(published_claims$claim_id)),
  all(nzchar(published_claims$claim_id)),
  all(published_claims$claim_type %in%
        c("pipeline", "descriptive", "definitional", "structural", "transcribed")),
  all(published_claims$needs_block %in% c(TRUE, FALSE)),
  all(is.na(published_claims$comparison) |
        published_claims$comparison %in% c("==", "<", ">", "<=", ">=", "approx"))
)

# Every pipeline and descriptive row must require a block. The other three
# classes require one only where the pipeline can reach the quantity.
stopifnot(all(
  published_claims$needs_block[
    published_claims$claim_type %in% c("pipeline", "descriptive")
  ]
))

# value_paper is stored as the article prints it, normalised only for
# typography. A stored string that does not survive a round trip through its own
# recorded precision means digits is wrong about the precision even where it is
# right about the value, which numeric equality would pass. This is the check a
# wrong digits has to trip, so it runs before any comparison uses digits.
numeric_claims <- published_claims |>
  filter(!is.na(value_paper), !is.na(digits), units != "version")
stopifnot(
  all(!str_detect(numeric_claims$value_paper, ",")),
  all(numeric_claims$value_paper == normalise_printed(numeric_claims$value_paper)),
  all(render_at(as.numeric(numeric_claims$value_paper), numeric_claims$digits) ==
        numeric_claims$value_paper)
)
stopifnot(
  all(published_appendix$value_paper == normalise_printed(published_appendix$value_paper)),
  all(published_maintext$value_paper == normalise_printed(published_maintext$value_paper)),
  all(render_at(as.numeric(published_appendix$value_paper), published_appendix$digits) ==
        published_appendix$value_paper),
  all(render_at(as.numeric(published_maintext$value_paper), published_maintext$digits) ==
        published_maintext$value_paper)
)

# Where the same published quantity appears in a prose sentence and in a
# supplementary table, the two hand transcriptions are reconciled here. Nothing
# else compares them, and backfilling one onto the other would let them drift.
reconcile <- tribble(
  ~claim_id, ~float, ~row_key, ~quantity,
  "methods_tss100_t", "supplementary_table_3", "Trump support score / 100", "t",
  "methods_tss100_p", "supplementary_table_3", "Trump support score / 100", "p_value",
  "methods_tss100_estimate", "supplementary_table_3", "Trump support score / 100", "estimate",
  "methods_tss100_ci_low", "supplementary_table_3", "Trump support score / 100", "ci_lower",
  "methods_tss100_ci_high", "supplementary_table_3", "Trump support score / 100", "ci_upper",
  "methods_tss6070_t", "supplementary_table_3", "TSS: 60-70", "t",
  "methods_tss6070_p", "supplementary_table_3", "TSS: 60-70", "p_value",
  "methods_tss6070_estimate", "supplementary_table_3", "TSS: 60-70", "estimate",
  "methods_tss6070_ci_low", "supplementary_table_3", "TSS: 60-70", "ci_lower",
  "methods_tss6070_ci_high", "supplementary_table_3", "TSS: 60-70", "ci_upper",
  "methods_bh_p_tss100", "supplementary_table_3", "Trump support score / 100", "p_value_bh",
  "methods_bh_p_tss6070", "supplementary_table_3", "TSS: 60-70", "p_value_bh",
  "methods_tss100_df", "supplementary_table_3", "Trump support score / 100", "df",
  "methods_tss6070_df", "supplementary_table_3", "TSS: 60-70", "df"
) |>
  left_join(published_claims |> select(claim_id, prose = value_paper,
                                       prose_digits = digits), by = "claim_id") |>
  left_join(published_appendix |> select(float, row_key, quantity, cell = value_paper,
                                         cell_digits = digits),
            by = c("float", "row_key", "quantity")) |>
  # The prose sometimes prints a cell at coarser precision than the table does,
  # so the two are compared at whichever precision is coarser. The main text
  # gives the upper bound of the Trump support score imbalance interval as a
  # bare 0 where Supplementary Table 3 gives -0.000.
  mutate(
    shared_digits = pmin(prose_digits, cell_digits),
    agrees = render_at(as.numeric(prose), shared_digits) ==
      render_at(as.numeric(cell), shared_digits)
  )

stopifnot(nrow(reconcile) == 14, !any(is.na(reconcile$prose)), !any(is.na(reconcile$cell)))
if (!all(reconcile$agrees)) {
  print(reconcile |> filter(!agrees))
  stop("A prose transcription disagrees with the supplementary table transcription of the same cell.")
}

# Pipeline output --------------------------------------------------------------

read_output <- function(file) {
  read_csv(here::here("maintained", "output", file), show_col_types = FALSE)
}

figure_1_rewrite <- read_output("figure_1_ate_cate_estimates.csv")
figure_2_rewrite <- read_output("figure_2_heterogeneity_by_tss.csv")
figure_3_binned <- read_output("figure_3_binned_means.csv")
figure_5_rewrite <- read_output("figure_5_balance.csv")
table_1_rewrite <- read_output("table_1_experimental_strata_cells.csv")
figure_b2_rewrite <- read_output("figure_b2_acronym_spending.csv")
figure_c3_rewrite <- read_output("figure_c3_facebook_spending.csv")
main_claims <- read_output("text_main_claims.csv")
subgroup_sizes <- read_output("text_subgroup_sizes.csv")
spending_claims <- read_output("text_spending_claims.csv")
ad_library_totals <- read_output("text_ad_library_totals.csv")
descriptive_claims <- read_output("text_descriptive_claims.csv")

archive_values <- read_csv(here::here("ground_truth", "archive_values.csv"),
                           show_col_types = FALSE)

scalar <- function(tbl, name) {
  row <- tbl |> filter(quantity == name)
  stopifnot(nrow(row) == 1)
  row$value
}

archive_scalar <- function(name) {
  row <- archive_values |> filter(float == "text", row_key == name)
  if (nrow(row) == 0) return(NA_real_)
  stopifnot(nrow(row) == 1)
  row$value_script
}

subgroup_n <- function(covariate_name, level_name) {
  row <- subgroup_sizes |> filter(covariate == covariate_name, level == level_name)
  stopifnot(nrow(row) == 1)
  row$n
}

descriptive_holds <- function(name) {
  row <- descriptive_claims |> filter(quantity == name)
  stopifnot(nrow(row) == 1)
  row$holds
}

descriptive_evidence <- function(name) {
  row <- descriptive_claims |> filter(quantity == name)
  stopifnot(nrow(row) == 1)
  row$evidence
}

dic_pap <- function(outcome_label) {
  row <- figure_2_rewrite |>
    filter(str_starts(inquiry, "Difference-in-CATEs"),
           estimator == "PAP adjustment set", outcome == outcome_label)
  stopifnot(nrow(row) == 1)
  row$estimate
}

dic_pap_archive <- function(outcome_label) {
  row <- archive_values |>
    filter(float == "supplementary_table_2", quantity == "estimate",
           row_key == str_glue("Difference-in-CATEs TSS 60-70 versus TSS 30-40 | {outcome_label} | PAP adjustment set"))
  stopifnot(nrow(row) == 1)
  row$value_script
}

# Assembling the rows -----------------------------------------------------------

rows <- list()

claim <- function(claim_id, table_figure, claim, value_rewrite = NA_real_,
                  value_script = NA_real_, holds = NA, defect_locus = NA_character_,
                  note = NA_character_) {
  spec <- published_claims |> filter(.data$claim_id == .env$claim_id)
  stopifnot(nrow(spec) == 1)

  digits <- spec$digits
  units <- spec$units
  comparison <- if (is.na(spec$comparison)) "==" else spec$comparison

  paper_string <- spec$value_paper
  paper_numeric <- suppressWarnings(as.numeric(normalise_printed(paper_string)))

  converted_rewrite <- convert_units(value_rewrite, units)
  converted_script <- convert_units(value_script, units)

  verdict <- function(value, exact) {
    if (is.na(paper_numeric) || is.na(value)) return(NA_integer_)
    switch(
      comparison,
      "==" = if (exact) agrees_exactly(value, paper_string, digits)
             else agrees_loosely(value, paper_string, digits),
      "<" = as.integer(value < paper_numeric),
      "<=" = as.integer(value <= paper_numeric),
      ">" = as.integer(value > paper_numeric),
      ">=" = as.integer(value >= paper_numeric),
      "approx" = NA_integer_
    )
  }

  match_rewrite <- verdict(converted_rewrite, exact = TRUE)
  match_script <- verdict(converted_script, exact = FALSE)

  # The verdict clause is constructed from the same comparison that sets the
  # verdict, so the note and the verdict cannot drift apart.
  rendered <- if (is.na(converted_rewrite) || is.na(digits)) NA_character_ else
    render_at(converted_rewrite, digits)
  verdict_clause <- case_when(
    !is.na(match_rewrite) & match_rewrite == 1 ~
      str_glue("Rewrite gives {rendered}, which matches."),
    !is.na(match_rewrite) & match_rewrite == 0 ~
      str_glue("Rewrite gives {rendered} against a published {paper_string}."),
    !is.na(holds) & holds ~ "The claim holds.",
    !is.na(holds) & !holds ~ "The claim does not hold.",
    !is.na(rendered) ~ str_glue("Rewrite gives {rendered}; no verdict is taken."),
    .default = "No rewrite counterpart."
  )
  full_note <- if (is.na(note)) as.character(verdict_clause) else
    paste(note, verdict_clause)

  rows[[length(rows) + 1]] <<- tibble(
    paper_id = paper_id,
    claim_id = claim_id,
    table_figure = table_figure,
    claim = claim,
    value_script = converted_script,
    value_paper = paper_string,
    match = match_script,
    value_rewrite = converted_rewrite,
    match_rewrite = match_rewrite,
    holds = holds,
    defect_locus = defect_locus,
    notes = full_note
  )
  invisible(NULL)
}

# Cell-by-cell comparison of the large floats ----------------------------------

supplementary_table_1_cells <-
  figure_1_rewrite |>
  transmute(
    row_key = str_c(covariate, " | ", covariate_value, " | ", estimator),
    estimate, se = std.error, df, t = statistic, p_value = p.value,
    ci_lower = conf.low, ci_upper = conf.high
  ) |>
  pivot_longer(-row_key, names_to = "quantity", values_to = "value")

supplementary_table_2_cells <-
  figure_2_rewrite |>
  transmute(
    row_key = str_c(inquiry, " | ", outcome, " | ", estimator),
    estimate, se = std.error, df, t = statistic, p_value = p.value,
    ci_lower = conf.low, ci_upper = conf.high
  ) |>
  pivot_longer(-row_key, names_to = "quantity", values_to = "value")

supplementary_table_3_cells <-
  figure_5_rewrite |>
  transmute(
    row_key = outcome,
    estimate, se = std.error, df, t = statistic, p_value = p.value,
    p_value_bh = adjusted_p, ci_lower = conf.low, ci_upper = conf.high
  ) |>
  pivot_longer(-row_key, names_to = "quantity", values_to = "value")

table_1_cells <-
  table_1_rewrite |>
  transmute(
    row_key = row_label,
    control_n, treatment_n, p_treat,
    voting_rate_control = 100 * voting_rate_control,
    voting_rate_treatment = 100 * voting_rate_treatment
  ) |>
  pivot_longer(-row_key, names_to = "quantity", values_to = "value")

# Figure 1 and Figure 2 print their labels differently from the supplementary
# tables that carry the same estimates. The maps are transcriptions of the two
# label sets, written out so that the join is explicit rather than positional.
figure_1_strip_map <- c(
  "ATE" = "ATE", "Age (years)" = "Age", "Gender" = "Gender", "Race" = "Race",
  "Margin (pp)" = "Margin", "Partisanship" = "Partisanship",
  "Trump support score" = "Trump support"
)
figure_1_level_map <- c(
  "18–39" = "18-39", "Vote margin <3" = "Vote margin less than 3pp",
  "Vote margin >3" = "Vote margin more than 3pp", "30–40" = "30 to 40",
  "40–50" = "40 to 50", "50–60" = "50 to 60", "60–70" = "60 to 70"
)
figure_2_target_map <- c(
  "Difference in CATEs (TSS of 60–70 versus TSS of 30–40)" =
    "Difference-in-CATEs TSS 60-70 versus TSS 30-40",
  "Treatment × TSS interaction term from linear model" =
    "Treatment*TSS interaction term from linear model"
)

recode_key <- function(row_key, first_map, second_map = NULL) {
  parts <- str_split_fixed(row_key, " \\| ", 3)
  first <- coalesce(unname(first_map[parts[, 1]]), parts[, 1])
  second <- if (is.null(second_map)) parts[, 2] else
    coalesce(unname(second_map[parts[, 2]]), parts[, 2])
  str_c(first, " | ", second, " | ", parts[, 3])
}

published_float <- function(float_id) {
  out <- bind_rows(published_appendix, published_maintext) |> filter(float == float_id)
  if (float_id == "figure_1") {
    out <- out |> mutate(row_key = recode_key(row_key, figure_1_strip_map, figure_1_level_map))
  }
  if (float_id == "figure_2") {
    out <- out |> mutate(row_key = recode_key(row_key, figure_2_target_map))
  }
  if (float_id == "table_1") {
    out <- out |> mutate(row_key = str_replace_all(row_key, "–", "-"))
  }
  out
}

# Every join between a published transcription and a pipeline output goes
# through here, which asserts uniqueness on both sides and a one-to-one result.
# A published cell that finds nothing stops the build rather than becoming NA.
join_published <- function(published, cells, label) {
  stopifnot(!any(duplicated(published[c("row_key", "quantity")])),
            !any(duplicated(cells[c("row_key", "quantity")])))
  joined <- published |> left_join(cells, by = c("row_key", "quantity"))
  stopifnot(nrow(joined) == nrow(published))
  unmatched <- joined |> filter(is.na(value))
  if (nrow(unmatched) > 0) {
    print(unmatched |> select(row_key, quantity, value_paper), n = 20)
    stop(label, ": ", nrow(unmatched), " published cells joined to nothing.")
  }
  joined
}

compare_float <- function(float_id, cells, label) {
  joined <- join_published(published_float(float_id), cells, label)
  joined |>
    mutate(agrees = pmap_int(list(value, value_paper, digits), agrees_exactly))
}

st1_compared <- compare_float("supplementary_table_1", supplementary_table_1_cells,
                              "Supplementary Table 1")
st2_compared <- compare_float("supplementary_table_2", supplementary_table_2_cells,
                              "Supplementary Table 2")
st3_compared <- compare_float("supplementary_table_3", supplementary_table_3_cells,
                              "Supplementary Table 3")
t1_compared <- compare_float("table_1", table_1_cells, "Table 1")
f1_compared <- compare_float("figure_1", supplementary_table_1_cells, "Figure 1")
f2_compared <- compare_float("figure_2", supplementary_table_2_cells, "Figure 2")

archive_agreement <- function(float_id, loose = FALSE) {
  joined <- join_published(
    published_float(float_id),
    archive_values |> filter(float == float_id) |>
      select(row_key, quantity, value = value_script),
    str_glue("{float_id} against the deposit")
  )
  compare <- if (loose) agrees_loosely else agrees_exactly
  sum(pmap_int(list(joined$value, joined$value_paper, joined$digits), compare),
      na.rm = TRUE)
}

float_summary <- tibble(
  float = c("figure_1", "figure_2", "table_1", "supplementary_table_1",
            "supplementary_table_2", "supplementary_table_3"),
  claim_id = str_c(float, "_cells_reproduced"),
  published_numbers = c(114L, 36L, 95L, 399L, 126L, 176L),
  reproduced = c(sum(f1_compared$agrees), sum(f2_compared$agrees),
                 sum(t1_compared$agrees), sum(st1_compared$agrees),
                 sum(st2_compared$agrees), sum(st3_compared$agrees)),
  from_archive = c(archive_agreement("figure_1"), archive_agreement("figure_2"),
                   archive_agreement("table_1", loose = TRUE),
                   archive_agreement("supplementary_table_1"),
                   archive_agreement("supplementary_table_2"),
                   archive_agreement("supplementary_table_3"))
)

pwalk(float_summary, function(float, claim_id, published_numbers, reproduced, from_archive) {
  claim(claim_id, float,
        str_glue("Cells of {float} reproduced at the precision the float prints"),
        value_rewrite = reproduced, value_script = from_archive,
        note = str_glue("{published_numbers} published numbers."))
})

# Figure 1 against Supplementary Table 1, as two independent transcriptions of
# the article rather than one taken for the other.
compare_transcriptions <- function(float_id, table_id) {
  published_float(float_id) |>
    select(row_key, quantity, figure = value_paper) |>
    inner_join(published_appendix |> filter(float == table_id) |>
                 select(row_key, quantity, cell = value_paper),
               by = c("row_key", "quantity")) |>
    filter(quantity %in% c("estimate", "se"))
}

f1_vs_st1 <- compare_transcriptions("figure_1", "supplementary_table_1")
f2_vs_st2 <- compare_transcriptions("figure_2", "supplementary_table_2")
stopifnot(nrow(f1_vs_st1) == 114, nrow(f2_vs_st2) == 36)

claim("figure_1_agrees_with_supplementary_table_1", "figure_1",
      "Every estimate and standard error printed on the face of Figure 1 equals the same cell of Supplementary Table 1",
      holds = all(f1_vs_st1$figure == f1_vs_st1$cell),
      note = str_glue("{sum(f1_vs_st1$figure == f1_vs_st1$cell)} of {nrow(f1_vs_st1)} cells agree between the two published transcriptions."))
claim("figure_2_agrees_with_supplementary_table_2", "figure_2",
      "Every estimate and standard error printed on the face of Figure 2 equals the same cell of Supplementary Table 2",
      holds = all(f2_vs_st2$figure == f2_vs_st2$cell),
      note = str_glue("{sum(f2_vs_st2$figure == f2_vs_st2$cell)} of {nrow(f2_vs_st2)} cells agree between the two published transcriptions."))

# Abstract ----------------------------------------------------------------------

not_deposited <- function(claim_id, table_figure, claim, what) {
  claim(claim_id, table_figure, claim, note = what, defect_locus = "archive")
}

not_deposited("abstract_campaign_cost", "abstract",
              "A US$8.9 million campaign-wide field experiment",
              "The deposit carries no field for the programme's total cost.")
claim("abstract_sample_millions", "abstract",
      "Conducted among 2 million moderate- and low-information persuadable voters",
      value_rewrite = scalar(main_claims, "n_total"))
not_deposited("abstract_n_states", "abstract", "Five battleground states",
              "The deposit carries no state field.")
not_deposited("abstract_programme_months", "abstract",
              "An 8-month-long advertising programme",
              "The deposit carries no delivery dates for the programme.")
claim("abstract_biden_effect", "abstract",
      "The campaign increased voting among Biden leaners by 0.4 percentage points",
      value_rewrite = scalar(main_claims, "cate_biden_estimate"),
      value_script = archive_scalar("cate_biden_estimate"))
claim("abstract_biden_se", "abstract", "Standard error of 0.2 percentage points",
      value_rewrite = scalar(main_claims, "cate_biden_se"),
      value_script = archive_scalar("cate_biden_se"))
claim("abstract_trump_effect", "abstract",
      "Decreased voting among Trump leaners by 0.3 percentage points",
      value_rewrite = abs(scalar(main_claims, "cate_trump_estimate")),
      value_script = abs(archive_scalar("cate_trump_estimate")),
      note = "Compared as a magnitude, since the sentence states a decrease.")
claim("abstract_trump_se", "abstract", "Standard error of 0.3 percentage points",
      value_rewrite = scalar(main_claims, "cate_trump_se"),
      value_script = archive_scalar("cate_trump_se"))
claim("abstract_dic", "abstract",
      "A difference in conditional average treatment effects of 0.7 points",
      value_rewrite = abs(scalar(main_claims, "dic_estimate")),
      value_script = abs(archive_scalar("dic_estimate")),
      note = "Compared as a magnitude, since the sentence states a difference.")
claim("abstract_dic_t", "abstract", "t statistic of the difference in CATEs",
      value_rewrite = scalar(main_claims, "dic_t"),
      value_script = archive_scalar("dic_t"))
claim("abstract_dic_df", "abstract", "Degrees of freedom of the difference in CATEs",
      value_rewrite = scalar(main_claims, "dic_df"),
      value_script = archive_scalar("dic_df"))
claim("abstract_dic_p", "abstract", "p-value of the difference in CATEs",
      value_rewrite = scalar(main_claims, "dic_p"),
      value_script = archive_scalar("dic_p"))
claim("abstract_dic_ci_low", "abstract", "Lower bound of the 95 per cent interval",
      value_rewrite = scalar(main_claims, "dic_ci_low"),
      value_script = archive_scalar("dic_ci_low"))
claim("abstract_dic_ci_high", "abstract", "Upper bound of the 95 per cent interval",
      value_rewrite = scalar(main_claims, "dic_ci_high"),
      value_script = archive_scalar("dic_ci_high"),
      note = "Printed as a bare 0.")

# Introduction --------------------------------------------------------------------

not_deposited("intro_impressions_per_participant", "introduction",
              "An average of 754 advertisement impressions per treated participant",
              "Nothing in the deposit records advertisement impressions.")
claim("intro_party_registration_missing", "introduction",
      "Party registration was not available for 72 per cent of the participant pool",
      value_rewrite = subgroup_n("Partisanship", "Unknown") / scalar(main_claims, "n_total"))
claim("intro_tss_low_bucket_min", "introduction",
      "Biden leaners have modelled Trump support scores between 30 and 40",
      value_rewrite = 30 * (min(figure_3_binned$tss_round) == 30))
claim("intro_tss_low_bucket_max", "introduction",
      "Biden leaners have modelled Trump support scores between 30 and 40",
      value_rewrite = max(figure_3_binned$tss_round[figure_3_binned$tss_round <= 40]))
claim("intro_tss_high_bucket_min", "introduction",
      "Trump leaners have a Trump support score between 60 and 70",
      value_rewrite = min(figure_3_binned$tss_round[figure_3_binned$tss_round >= 60]))
claim("intro_tss_high_bucket_max", "introduction",
      "Trump leaners have a Trump support score between 60 and 70",
      value_rewrite = max(figure_3_binned$tss_round))

# Results ---------------------------------------------------------------------------

claim("results_ate_pp", "results",
      "The overall effect on turnout was -0.06 percentage points",
      value_rewrite = scalar(main_claims, "ate_pap_estimate"),
      value_script = archive_scalar("ate_pap_estimate"))
claim("results_ate_se_pp", "results", "With a robust standard error of 0.12 points",
      value_rewrite = scalar(main_claims, "ate_pap_se"),
      value_script = archive_scalar("ate_pap_se"))
claim("results_ate_t", "results", "t statistic of the average treatment effect",
      value_rewrite = scalar(main_claims, "ate_pap_t"),
      value_script = archive_scalar("ate_pap_t"))
claim("results_ate_df", "results", "Degrees of freedom of the average treatment effect",
      value_rewrite = scalar(main_claims, "ate_pap_df"),
      value_script = archive_scalar("ate_pap_df"))
claim("results_ate_p", "results", "p-value of the average treatment effect",
      value_rewrite = scalar(main_claims, "ate_pap_p"),
      value_script = archive_scalar("ate_pap_p"))
claim("results_ate_estimate", "results",
      "The average treatment effect on the proportion scale",
      value_rewrite = scalar(main_claims, "ate_pap_estimate"),
      value_script = archive_scalar("ate_pap_estimate"))
claim("results_ate_ci_low", "results", "Lower bound of the 95 per cent interval",
      value_rewrite = scalar(main_claims, "ate_pap_ci_low"),
      value_script = archive_scalar("ate_pap_ci_low"))
claim("results_ate_ci_high", "results", "Upper bound of the 95 per cent interval",
      value_rewrite = scalar(main_claims, "ate_pap_ci_high"),
      value_script = archive_scalar("ate_pap_ci_high"))
claim("results_equivalence_bound", "results",
      "An equivalence range of plus or minus one-third of a percentage point",
      value_rewrite = scalar(main_claims, "equivalence_bound"))
claim("results_tost_p", "results", "Two one-sided tests p-value",
      value_rewrite = scalar(main_claims, "tost_p_lower"),
      value_script = archive_scalar("tost_p_lower"))
claim("results_early_dic_pp", "results",
      "Differential effects were 1.0 percentage points favouring Biden in the early voting data",
      value_rewrite = abs(dic_pap("Voted early in 2020")),
      value_script = abs(dic_pap_archive("Voted early in 2020")))
claim("results_in_person_dic_pp", "results",
      "And 0.3 percentage points favouring Trump in the in-person voting data",
      value_rewrite = abs(dic_pap("Voted in person in 2020")),
      value_script = abs(dic_pap_archive("Voted in person in 2020")))
claim("results_equality_f", "results",
      "Linear hypothesis test of equality of the two differences in CATEs",
      value_rewrite = scalar(main_claims, "equality_f"),
      value_script = archive_scalar("equality_f"))
claim("results_equality_num_df", "results", "Numerator degrees of freedom of that test",
      value_rewrite = scalar(main_claims, "equality_num_df"),
      value_script = archive_scalar("equality_num_df"))
claim("results_equality_den_df", "results", "Denominator degrees of freedom of that test",
      value_rewrite = scalar(main_claims, "equality_den_df"),
      value_script = archive_scalar("equality_den_df"))
claim("results_equality_p", "results", "p-value of that test, printed as below 0.0001",
      value_rewrite = scalar(main_claims, "equality_p"))

# Figure captions ---------------------------------------------------------------------

caption_subgroups <- tribble(
  ~claim_id, ~covariate, ~level,
  "figure_1_caption_n_total", "Total", "Total",
  "figure_1_caption_n_age_18_39", "Age", "18-39",
  "figure_1_caption_n_age_40_plus", "Age", "40+",
  "figure_1_caption_n_female", "Gender", "Female",
  "figure_1_caption_n_other_gender", "Gender", "Other",
  "figure_1_caption_n_black", "Race", "Black",
  "figure_1_caption_n_latinx", "Race", "Latinx",
  "figure_1_caption_n_white", "Race", "White",
  "figure_1_caption_n_other_race", "Race", "Other",
  "figure_1_caption_n_margin_under_3", "Margin", "Vote margin less than 3pp",
  "figure_1_caption_n_margin_over_3", "Margin", "Vote margin more than 3pp",
  "figure_1_caption_n_democrat", "Partisanship", "Democrat",
  "figure_1_caption_n_republican", "Partisanship", "Republican",
  "figure_1_caption_n_unknown_party", "Partisanship", "Unknown",
  "figure_1_caption_n_other_party", "Partisanship", "Other",
  "figure_1_caption_n_tss_30_40", "Trump support", "30 to 40",
  "figure_1_caption_n_tss_40_50", "Trump support", "40 to 50",
  "figure_1_caption_n_tss_50_60", "Trump support", "50 to 60",
  "figure_1_caption_n_tss_60_70", "Trump support", "60 to 70"
)

pwalk(caption_subgroups, function(claim_id, covariate, level) {
  claim(claim_id, "figure_1",
        str_glue("Figure 1 caption sample size, {covariate}: {level}"),
        value_rewrite = subgroup_n(covariate, level))
})

claim("figure_2_caption_n_dic", "figure_2",
      "Figure 2 caption sample size for the difference in CATEs estimates",
      value_rewrite = scalar(main_claims, "n_dic_sample"))
claim("figure_2_caption_n_interaction", "figure_2",
      "Figure 2 caption sample size for the interaction estimates",
      value_rewrite = scalar(main_claims, "n_total"))
claim("figure_3_caption_n", "figure_3", "Figure 3 caption sample size",
      value_rewrite = scalar(main_claims, "n_total"))
claim("figure_3_caption_panel_range_pp", "figure_3",
      "The vertical scales of all three facets cover a 15 percentage point range",
      note = "Asserted in figure_3_turnout_by_tss_score.R at the point of use.")
claim("figure_5_caption_n", "figure_5", "Figure 5 caption sample size",
      value_rewrite = scalar(main_claims, "n_total"))
claim("figure_3_points_plotted", "figure_3",
      "Number of binned means Figure 3 plots",
      value_rewrite = nrow(figure_3_binned))
claim("figure_5_points_plotted", "figure_5",
      "Number of covariate estimates Figure 5 plots",
      value_rewrite = nrow(figure_5_rewrite))

# Figure 4, the assignment flow chart -------------------------------------------------

pwalk(published_claims |> filter(str_starts(claim_id, "figure_4_"), !needs_block) |>
        select(claim_id, note),
      function(claim_id, note) {
        not_deposited(claim_id, "figure_4", note,
                      "The deposit holds only the analysis subset, so this has no counterpart in it.")
      })

claim("figure_4_assignment_sums_to_eligible", "figure_4",
      "The control and treatment assignment counts sum to the eligible population",
      value_rewrite = sum(as.numeric(published_claims$value_paper[
        published_claims$claim_id %in% c("figure_4_assigned_control",
                                         "figure_4_assigned_treatment")])),
      note = "Computed from the article's own printed figures, not from the deposit.")
claim("figure_4_n_control", "figure_4", "Control group participants assessed for turnout",
      value_rewrite = scalar(main_claims, "n_control"))
claim("figure_4_n_treatment", "figure_4", "Treatment group participants assessed for turnout",
      value_rewrite = scalar(main_claims, "n_treatment"))
claim("figure_4_n_strata", "figure_4", "The procedure generated 18 strata of subjects",
      value_rewrite = scalar(main_claims, "n_strata"))
claim("figure_4_tss_target_min", "figure_4",
      "Targeting criterion: Trump support score between 30 and 70",
      value_rewrite = scalar(main_claims, "tss_min"))
claim("figure_4_tss_target_max", "figure_4",
      "Targeting criterion: Trump support score between 30 and 70",
      value_rewrite = scalar(main_claims, "tss_max"))
claim("figure_4_pts_target_min", "figure_4",
      "Targeting criterion: presidential turnout score between 20 and 80",
      value_rewrite = scalar(main_claims, "pts_min"))
claim("figure_4_pts_target_max", "figure_4",
      "Targeting criterion: presidential turnout score between 20 and 80",
      value_rewrite = scalar(main_claims, "pts_max"),
      defect_locus = "unresolved",
      note = paste("The deposited analysis set reaches a presidential turnout score above 80,",
                   "so the upper half of the stated targeting criterion does not hold in the",
                   "data the article analyses. The archival subset criteria in the same figure",
                   "impose only a lower bound."))
claim("methods_pts_range_holds", "figure_4",
      "Presidential turnout scores in the analysis set lie between 20 and 80",
      holds = scalar(main_claims, "pts_max") <= 80,
      defect_locus = "unresolved",
      note = str_glue("The observed range is {sprintf('%.1f', scalar(main_claims, 'pts_min'))} ",
                      "to {sprintf('%.1f', scalar(main_claims, 'pts_max'))}."))

claim("figure_5_axis_breaks", "figure_5",
      "Figure 5 prints five axis breaks",
      note = "Set in figure_5_balance.R at the point of use.")

# Methods -------------------------------------------------------------------------------

claim("methods_n_analysis_sample", "methods", "An analysis sample of 1,999,282 participants",
      value_rewrite = scalar(main_claims, "n_total"))
claim("methods_n_covariates", "methods",
      "Out of 21 covariates, only two exhibited statistically significant imbalance",
      value_rewrite = nrow(figure_5_rewrite),
      holds = descriptive_holds("n_balance_covariates"),
      defect_locus = "paper_internal",
      note = descriptive_evidence("n_balance_covariates"))
claim("methods_n_significant_imbalance", "methods",
      "Only two covariates exhibited statistically significant imbalance",
      value_rewrite = sum(figure_5_rewrite$p.value < 0.05),
      holds = descriptive_holds("n_significant_imbalance"),
      note = descriptive_evidence("n_significant_imbalance"))

balance_cell <- function(label, column) {
  row <- figure_5_rewrite |> filter(outcome == label)
  stopifnot(nrow(row) == 1)
  row[[column]]
}
balance_archive <- function(label, quantity_name) {
  row <- archive_values |> filter(float == "supplementary_table_3", row_key == label,
                                  quantity == quantity_name)
  stopifnot(nrow(row) == 1)
  row$value_script
}

balance_prose <- tribble(
  ~claim_id, ~label, ~column, ~quantity_name, ~description,
  "methods_tss100_estimate", "Trump support score / 100", "estimate", "estimate", "Trump support score imbalance estimate",
  "methods_tss100_t", "Trump support score / 100", "statistic", "t", "Trump support score imbalance t statistic",
  "methods_tss100_df", "Trump support score / 100", "df", "df", "Trump support score imbalance degrees of freedom",
  "methods_tss100_p", "Trump support score / 100", "p.value", "p_value", "Trump support score imbalance p-value",
  "methods_tss100_ci_low", "Trump support score / 100", "conf.low", "ci_lower", "Trump support score imbalance interval, lower",
  "methods_tss100_ci_high", "Trump support score / 100", "conf.high", "ci_upper", "Trump support score imbalance interval, upper",
  "methods_tss6070_estimate", "TSS: 60-70", "estimate", "estimate", "TSS 60 to 70 imbalance estimate",
  "methods_tss6070_t", "TSS: 60-70", "statistic", "t", "TSS 60 to 70 imbalance t statistic",
  "methods_tss6070_df", "TSS: 60-70", "df", "df", "TSS 60 to 70 imbalance degrees of freedom",
  "methods_tss6070_p", "TSS: 60-70", "p.value", "p_value", "TSS 60 to 70 imbalance p-value",
  "methods_tss6070_ci_low", "TSS: 60-70", "conf.low", "ci_lower", "TSS 60 to 70 imbalance interval, lower",
  "methods_tss6070_ci_high", "TSS: 60-70", "conf.high", "ci_upper", "TSS 60 to 70 imbalance interval, upper",
  "methods_bh_p_tss100", "Trump support score / 100", "adjusted_p", "p_value_bh", "Trump support score adjusted p-value",
  "methods_bh_p_tss6070", "TSS: 60-70", "adjusted_p", "p_value_bh", "TSS 60 to 70 adjusted p-value"
)

pwalk(balance_prose, function(claim_id, label, column, quantity_name, description) {
  claim(claim_id, "methods", description,
        value_rewrite = balance_cell(label, column),
        value_script = balance_archive(label, quantity_name))
})

claim("methods_omnibus_f", "methods",
      "F statistic of the omnibus test that covariates predict assignment",
      value_rewrite = scalar(main_claims, "omnibus_f"))
claim("methods_omnibus_p", "methods", "p-value of that omnibus test",
      value_rewrite = scalar(main_claims, "omnibus_p"))
claim("methods_omnibus_df_first_subscript", "methods",
      "First subscript printed on the omnibus F statistic",
      value_rewrite = scalar(main_claims, "omnibus_num_df"),
      defect_locus = "paper_internal",
      note = paste("An F statistic takes its numerator degrees of freedom first. The test",
                   "compares an unrestricted model with 18 more parameters against a residual",
                   "degrees of freedom of", format(scalar(main_claims, "omnibus_den_df"),
                                                   big.mark = ","),
                   "so the subscripts should read 18 and that residual count."))
claim("methods_omnibus_df_second_subscript", "methods",
      "Second subscript printed on the omnibus F statistic",
      value_rewrite = scalar(main_claims, "omnibus_den_df"),
      defect_locus = "paper_internal",
      note = "Printed as a negative number, which is the Df column of the Wald test output rather than a degrees of freedom.")
not_deposited("methods_excluded_age", "methods",
              "The subset excluded voters over 55 years of age",
              "The deposit carries an age category but no age in years.")
not_deposited("methods_n_unique_ads", "methods",
              "The messaging programme consisted of 536 unique paid advertisements",
              paste("The deposited treatment_descriptives.csv describes the in-house testing",
                    "programme, 262 rows and 149 distinct treatments, not the paid programme."))
claim("methods_missing_race_share", "methods",
      "We are missing race data for 4% of the sample where race was uncoded",
      holds = descriptive_holds("uncoded_race_share"),
      defect_locus = "archive",
      note = descriptive_evidence("uncoded_race_share"))

spending_rows <- tribble(
  ~claim_id, ~quantity_name, ~description,
  "methods_spend_biden", "spend_lb_biden", "Minimum spend on advertisements containing the word Biden",
  "methods_spend_trump", "spend_lb_trump", "Minimum spend on advertisements containing the word Trump",
  "methods_spend_both", "spend_lb_both", "Minimum spend on advertisements containing both words",
  "methods_spend_promoted_news", "spend_lb_promoted_news", "Spend on promoted news advertisements",
  "methods_spend_video", "spend_lb_video", "Spend on video advertisements",
  "methods_spend_other_formats", "spend_lb_other", "Spend on other advertising formats"
)
archive_spending <- c(spend_lb_biden = "spend_lb_biden", spend_lb_trump = "spend_lb_trump",
                      spend_lb_both = "spend_lb_both",
                      spend_lb_promoted_news = "spend_lb_promoted_news",
                      spend_lb_video = "spend_lb_traditional_video",
                      spend_lb_other = "spend_lb_other")

pwalk(spending_rows, function(claim_id, quantity_name, description) {
  claim(claim_id, "methods", description,
        value_rewrite = scalar(spending_claims, quantity_name),
        value_script = archive_scalar(unname(archive_spending[quantity_name])),
        note = paste("The deposit's in_text_calculations.R cannot produce this: it reads",
                     "three columns the deposited CSV does not carry, and the script stops",
                     "before reaching that block in any case. The value here comes from",
                     "figure_B2.R, which builds those columns."))
})

pwalk(published_claims |> filter(claim_id %in% c(
  "methods_click_through_rate", "methods_video_view_seconds", "methods_video_view_rate",
  "methods_match_rate", "methods_programme_eligible")) |> select(claim_id, note),
  function(claim_id, note) {
    not_deposited(claim_id, "methods", note,
                  "Facebook delivery data; nothing of the kind is deposited.")
  })

claim("methods_treatment_audience", "methods",
      "A treatment audience of 1,993,216, stated as 3,322,027 eligible voters times a 60 per cent match rate",
      value_rewrite = 0.60 * as.numeric(published_claims$value_paper[
        published_claims$claim_id == "methods_programme_eligible"]),
      note = paste("Computed from the article's own two printed figures. The sentence prints",
                   "the product as '1,993,216 million'."))
claim("methods_treatment_audience_scale", "methods",
      "The treatment audience is stated on the scale its own parenthetical implies",
      holds = FALSE,
      defect_locus = "paper_internal",
      note = paste("The parenthetical gives 3,322,027 programme-eligible voters times a 60",
                   "per cent match rate, which is 1,993,216 people. The sentence prints that",
                   "product as '1,993,216 million', a factor of a million too large and more",
                   "than the population of the planet."))
claim("methods_cost_per_voter", "methods",
      "US$4.46 of advertising expenditure per voter",
      value_rewrite = 1e6 * as.numeric(published_claims$value_paper[
        published_claims$claim_id == "abstract_campaign_cost"]) /
        as.numeric(published_claims$value_paper[
          published_claims$claim_id == "methods_treatment_audience"]),
      note = "Computed from the article's own printed campaign cost and treatment audience.")
claim("methods_vote_history_elections", "methods",
      "A count of whether the participant voted in the 2012, 2016 and 2018 elections",
      value_rewrite = scalar(main_claims, "num_times_voted_max"))
claim("methods_tss_data_min", "methods",
      "Trump support scores in the analysis set start at 30",
      value_rewrite = scalar(main_claims, "tss_min"))
claim("methods_tss_data_max", "methods",
      "Trump support scores in the analysis set end at 70",
      value_rewrite = scalar(main_claims, "tss_max"))
claim("methods_ci_level", "methods",
      "Every interval the article reports is a 95 per cent confidence interval",
      value_rewrite = scalar(main_claims, "interval_level") / 100)

pwalk(published_claims |> filter(claim_id %in% c(
  "methods_fb_q3_revenue_share", "methods_russia_spend", "methods_fox_news_month",
  "methods_ad_ban_weeks", "methods_501c4", "methods_margin_threshold",
  "methods_r_version", "methods_tidyverse_version", "methods_estimatr_version",
  "methods_car_version", "methods_hc2")) |> select(claim_id, note, claim_type),
  function(claim_id, note, claim_type) {
    claim(claim_id, "methods",
          if (is.na(note)) claim_id else note,
          note = if (claim_type == "transcribed")
            "Taken from another source by the article; checked once against that source."
          else "A stated study or software parameter with no counterpart in the deposit.")
  })

# Reporting summary ---------------------------------------------------------------------

pwalk(published_claims |> filter(str_starts(claim_id, "reporting_summary_")) |>
        select(claim_id, note),
      function(claim_id, note) {
        claim(claim_id, "reporting_summary", note,
              note = "Restated from the article's own Methods; checked against it, not against the deposit.")
      })

# Supplementary Information ----------------------------------------------------------------

claim("si_contents_sections", "supplementary_information",
      "The Supplementary Information lists six sections in its contents")
claim("si_n_supplementary_figures", "supplementary_information",
      "The Supplementary Information carries four numbered figures",
      value_rewrite = 4,
      note = "Counted from the published document: Supplementary Figures 1 to 4.")
claim("si_n_supplementary_tables", "supplementary_information",
      "The Supplementary Information carries three numbered tables",
      value_rewrite = n_distinct(published_appendix$float),
      note = "Counted from the parsed transcription of the published tables.")

pwalk(published_claims |> filter(str_starts(claim_id, "si_a_")) |> select(claim_id, note),
      function(claim_id, note) {
        not_deposited(claim_id, "supplementary_information_a", note,
                      "Survey and match-quality data; none of it is deposited.")
      })
not_deposited("si_b_spend_buckets", "supplementary_information_b",
              "The Facebook Ad Library API returns spend in ten buckets",
              "A property of the API rather than of the data.")

claim("si_c_total_spend_all_advertisers", "supplementary_information_c",
      "The lower bound of total spend by all advertisers is US$349,006,000",
      value_rewrite = scalar(ad_library_totals, "ad_library_total_spend_lb"),
      value_script = archive_scalar("ad_library_total_spend_lb"))
not_deposited("si_c_largest_bucket", "supplementary_information_c",
              "The largest spend bucket, above US$1 million, is boundless",
              "A property of the API rather than of the data.")
claim("si_c_acronym_spend_low", "supplementary_information_c",
      "Acronym spent approximately US$3,867,900 on persuasion advertisements",
      value_rewrite = scalar(spending_claims, "spend_lb_persuasion_total"),
      value_script = archive_scalar("spend_lb_persuasion_total"))
claim("si_c_acronym_spend_high", "supplementary_information_c",
      "To US$5,921,963 on persuasion advertisements",
      value_rewrite = scalar(spending_claims, "spend_ub_persuasion_total"),
      value_script = archive_scalar("spend_ub_persuasion_total"))
claim("si_c_acronym_share", "supplementary_information_c",
      "Acronym constituted approximately 1 per cent of Facebook spending in the target states",
      value_rewrite = scalar(spending_claims, "spend_lb_all_acronym_ads") /
        scalar(ad_library_totals, "ad_library_total_spend_lb"),
      note = paste("An approximate claim, so no verdict is taken. Using the persuasion",
                   "advertisements alone the share is",
                   sprintf("%.2f", 100 * scalar(spending_claims, "spend_lb_persuasion_total") /
                             scalar(ad_library_totals, "ad_library_total_spend_lb")),
                   "per cent."))
not_deposited("si_c_wesleyan_total", "supplementary_information_c",
              "US$977,761,865 was spent on federal races on Facebook",
              "The Wesleyan Media Project file the deposit's figure_C4.R reads is not deposited.")
not_deposited("si_c_wesleyan_acronym", "supplementary_information_c",
              "US$6,603,488 of which was from Acronym",
              "The Wesleyan Media Project file the deposit's figure_C4.R reads is not deposited.")
claim("si_c_wesleyan_share", "supplementary_information_c",
      "Acronym's share of federal spending on Facebook was 0.67 per cent",
      value_rewrite = as.numeric(published_claims$value_paper[
        published_claims$claim_id == "si_c_wesleyan_acronym"]) /
        as.numeric(published_claims$value_paper[
          published_claims$claim_id == "si_c_wesleyan_total"]),
      defect_locus = "paper_internal",
      note = "Computed from the two figures the same sentence prints.")
claim("si_c_fb_q3_revenue_share", "supplementary_information_c",
      "Political advertising is an estimated 3 per cent of Facebook's Q3 US advertising revenue",
      note = "Taken from another source by the article; checked once against that source.")
claim("si_c_july_highest_spend", "supplementary_information_c",
      "Acronym spent more on persuasion in July than in any other month",
      holds = descriptive_holds("july_highest_persuasion_spend"),
      defect_locus = "paper_internal",
      note = descriptive_evidence("july_highest_persuasion_spend"))
claim("si_c_broken_cross_reference", "supplementary_information_c",
      "The sentence points the reader at 'main Figure ??'",
      holds = FALSE,
      defect_locus = "paper_internal",
      note = paste("The cross-reference did not resolve when the document was typeset. The",
                   "figure the sentence describes is Supplementary Figure 2, which is in the",
                   "Supplementary Information rather than the main text."))
claim("si_c_states_in_text", "supplementary_information_c",
      "The query is described in the text as covering five states: AZ, MI, NC, PA and WI")
claim("si_c_states_in_caption", "supplementary_information_c",
      "The caption of the figure that query produces lists six states: AZ, GA, MI, NC, PA and WI")
claim("si_c_state_lists_agree", "supplementary_information_c",
      "The two state lists name the same states",
      holds = FALSE,
      defect_locus = "paper_internal",
      note = paste("The text names five states and the caption of the figure that query",
                   "produces names six, adding Georgia. The deposited extract carries no",
                   "state field, so which list describes the query cannot be settled from",
                   "the deposit."))
claim("si_d_pap_registration_date", "supplementary_information_d",
      "The plan is described as registered on 22 November 2020",
      note = "The reproduced plan is dated 11 November 2020. Unresolved.",
      defect_locus = "unresolved")

pwalk(published_claims |> filter(str_starts(claim_id, "si_d_"),
                                 claim_id != "si_d_pap_registration_date") |>
        select(claim_id, note),
      function(claim_id, note) {
        claim(claim_id, "supplementary_information_d", note,
              note = "Reproduced verbatim from the pre-analysis plan; checked once against it.")
      })

pwalk(published_claims |> filter(str_starts(claim_id, "si_e_sql_limit_")) |>
        select(claim_id, note),
      function(claim_id, note) {
        claim(claim_id, "supplementary_information_e", note,
              note = "Reproduced verbatim from the deposited randomisation code.")
      })

flow_chart_steps <- str_c("figure_4_step_", 1:6, "_sampled")
sql_code_steps <- str_c("si_e_sql_limit_", 1:6)
claim("si_e_sql_limits_match_figure_4", "supplementary_information_e",
      "The six SQL sampling limits equal the six sample sizes Figure 4 lists",
      value_rewrite = sum(
        published_claims$value_paper[match(flow_chart_steps, published_claims$claim_id)] ==
          published_claims$value_paper[match(sql_code_steps, published_claims$claim_id)]
      ),
      note = "Two places in the published document, compared against each other.")

not_deposited("si_figure_1_coverage", "supplementary_figure_1",
              "Supplementary Figure 1 has no coverage",
              paste("figure_B1.R reads fielding_dates.rds and fielding_dates_survey.rds.",
                    "Neither is among the 16 deposited files; the third input,",
                    "treatment_descriptives.csv, is. Running the deposited script fails at",
                    "its first read. The figure prints no numbers other than the axis breaks",
                    "0, 5, 10 and 15."))
not_deposited("si_figure_4_coverage", "supplementary_figure_4",
              "Supplementary Figure 4 has no coverage",
              paste("figure_C4.R reads weekly_adds_for_page_id_disclaimer_041322.csv, which is",
                    "not among the 16 deposited files. Running the deposited script fails at",
                    "its first read."))
claim("si_figure_2_series_per_panel", "supplementary_figure_2",
      "Each panel of Supplementary Figure 2 plots three series",
      value_rewrite = figure_b2_rewrite |> count(panel, group) |> count(panel) |>
        pull(n) |> unique(),
      note = str_glue("The rewrite writes all {nrow(figure_b2_rewrite)} plotted points to figure_b2_acronym_spending.csv."))
claim("si_figure_2_axis_breaks", "supplementary_figure_2",
      "Supplementary Figure 2 prints four dollar breaks",
      note = "Set by the scale limits in figure_b2_acronym_spending.R at the point of use.")
claim("si_figure_3_axis_breaks", "supplementary_figure_3",
      "Supplementary Figure 3 prints five dollar breaks",
      note = "Set by the default scale in figure_c3_facebook_spending.R at the point of use.")
pwalk(published_claims |> filter(claim_id %in% c("si_figure_1_axis_breaks",
                                                "si_figure_4_axis_breaks")) |>
        select(claim_id, note),
      function(claim_id, note) {
        float <- if (str_detect(claim_id, "figure_1")) "supplementary_figure_1" else
          "supplementary_figure_4"
        not_deposited(claim_id, float, note,
                      "Counted off the published page; the script that would draw them cannot run.")
      })
claim("si_figure_3_month_labels", "supplementary_figure_3",
      "Supplementary Figure 3 prints three month labels on its horizontal axis",
      value_rewrite = 3,
      note = str_glue("The rewrite writes all {nrow(figure_c3_rewrite)} plotted weeks to figure_c3_facebook_spending.csv."))

# The remaining descriptive claims -----------------------------------------------------------

ground_truth <- list_rbind(rows)

# Every extraction row gets a ground truth row, in both directions.
missing_rows <- setdiff(published_claims$claim_id, ground_truth$claim_id)
extra_rows <- setdiff(ground_truth$claim_id, published_claims$claim_id)
if (length(missing_rows) > 0 || length(extra_rows) > 0) {
  print(list(missing = missing_rows, extra = extra_rows))
  stop("The ground truth and the extraction do not cover the same claims.")
}
stopifnot(!any(duplicated(ground_truth$claim_id)))

# The locus rule, in three states. An adverse row must carry a locus, a clean
# match must not, and a row with no verdict may.
adverse <- with(ground_truth,
                (!is.na(match) & match == 0) |
                  (!is.na(match_rewrite) & match_rewrite == 0) |
                  (!is.na(holds) & !holds))
clean <- with(ground_truth,
              !adverse & (!is.na(match_rewrite) & match_rewrite == 1 |
                            !is.na(holds) & holds))
if (any(adverse & is.na(ground_truth$defect_locus))) {
  print(ground_truth |> filter(adverse & is.na(defect_locus)) |>
          select(claim_id, value_paper, value_rewrite, match_rewrite, holds), n = Inf)
  stop("An adverse row carries no defect_locus.")
}
if (any(clean & !is.na(ground_truth$defect_locus))) {
  print(ground_truth |> filter(clean & !is.na(defect_locus)) |>
          select(claim_id, value_paper, value_rewrite, match_rewrite, holds, defect_locus), n = Inf)
  stop("A clean match carries a defect_locus.")
}
stopifnot(all(is.na(ground_truth$defect_locus) |
                ground_truth$defect_locus %in%
                c("paper_internal", "archive", "environment", "rewrite", "unresolved")))

# The coverage gate ------------------------------------------------------------------------

# The second instrument is read as a program, not as text: it is run, its output
# is captured, and the printed claim lines are counted. A block that errors, or
# that prints nothing, satisfies a textual gate completely and fails this one.
# Its own environment, because both files necessarily read the same outputs and
# name objects for what they hold.
claims_output <- capture.output(
  source(here::here("maintained", "in_text_claims.R"), local = new.env(), echo = FALSE)
)

printed <- claims_output |>
  str_subset("^CLAIM ") |>
  str_match("^CLAIM ([^ ]+) = (.*?) \\|\\| (.*)$")
printed_claims <- tibble(
  claim_id = printed[, 2],
  printed_value = printed[, 3],
  label = printed[, 4]
)

required <- published_claims |> filter(needs_block)

missing_blocks <- setdiff(required$claim_id, printed_claims$claim_id)
unknown_blocks <- setdiff(printed_claims$claim_id, published_claims$claim_id)
if (length(missing_blocks) > 0 || length(unknown_blocks) > 0) {
  print(list(missing = missing_blocks, unknown = unknown_blocks))
  stop("in_text_claims.R does not print exactly the claims the extraction requires.")
}
if (nrow(printed_claims) != nrow(required)) {
  print(printed_claims |> count(claim_id) |> filter(n > 1))
  stop("in_text_claims.R printed ", nrow(printed_claims), " claims against ",
       nrow(required), " extraction rows requiring a block.")
}

# Cross-instrument comparison. The two files reach the same claimed number by
# separate paths from the same pipeline outputs; where they disagree, one of
# them is wrong.
cross <- printed_claims |>
  left_join(ground_truth |> select(claim_id, value_rewrite, holds), by = "claim_id") |>
  left_join(published_claims |> select(claim_id, digits, comparison, claim_type),
            by = "claim_id") |>
  mutate(
    expected = pmap_chr(
      list(claim_type, holds, value_rewrite, digits),
      function(type, holds, value, digits) {
        if (type == "descriptive") return(as.character(holds))
        if (is.na(value) || is.na(digits)) return(NA_character_)
        render_at(value, digits)
      }
    ),
    agrees = is.na(expected) | printed_value == expected
  )

if (!all(cross$agrees)) {
  print(cross |> filter(!agrees) |> select(claim_id, printed_value, expected), n = Inf)
  stop("The two instruments disagree about a claimed value.")
}

# Output ---------------------------------------------------------------------------------

# Coverage is stated per float and is derived rather than typed. For the six
# floats that print cells it is the cell count; for the rest it is the number of
# extraction rows the float carries, how many of those the pipeline can reach,
# and how many reproduce.
cell_floats <-
  float_summary |>
  transmute(float, published_numbers, covered = published_numbers,
            reproduced_by_rewrite = reproduced, reproduced_by_archive = from_archive)

other_floats <-
  ground_truth |>
  filter(str_starts(table_figure, "figure_") | str_starts(table_figure, "supplementary_figure_"),
         !table_figure %in% cell_floats$float) |>
  left_join(published_claims |> select(claim_id, needs_block), by = "claim_id") |>
  group_by(float = table_figure) |>
  summarize(
    published_numbers = sum(!is.na(value_paper)),
    covered = sum(needs_block),
    reproduced_by_rewrite = sum(!is.na(match_rewrite) & match_rewrite == 1),
    reproduced_by_archive = NA_integer_,
    .groups = "drop"
  ) |>
  bind_rows(tibble(float = "figure_6", published_numbers = 0L, covered = 0L,
                   reproduced_by_rewrite = NA_integer_, reproduced_by_archive = NA_integer_))

float_coverage <-
  bind_rows(cell_floats, other_floats) |>
  arrange(float, .locale = "en")

write_csv(float_coverage, here::here("ground_truth", "float_coverage.csv"))
write_csv(ground_truth,
          here::here("ground_truth", str_glue("{paper_id}_ground_truth.csv")))

# The errata spine's claim_ids ----
# errata_entries.csv names, for every published entry, the ground-truth claims it corrects.
# Every one of those ids has to exist here: a missing one is a typo or a claim that has since
# been renamed, and a dangling reference inside a document whose whole purpose is correcting
# the record is worse than a failed build.
errata_spine <- here::here("errata_entries.csv")
if (file.exists(errata_spine)) {
  cited_ids <- read_csv(errata_spine, show_col_types = FALSE)$claim_ids |>
    str_split(";") |>
    unlist() |>
    str_trim() |>
    discard(\(x) is.na(x) | x == "")
  dangling <- setdiff(cited_ids, ground_truth$claim_id)
  if (length(dangling) > 0) print(dangling)
  stopifnot(length(dangling) == 0)
}

print(ground_truth |> count(match_rewrite, holds))
print(ground_truth |> filter(!is.na(defect_locus)) |> count(defect_locus))
print(float_summary)
print(str_glue("{nrow(ground_truth)} ground truth rows; ",
               "{nrow(printed_claims)} claims printed by the second instrument."))
