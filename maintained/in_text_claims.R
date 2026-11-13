# aggarwal_etal_2023/maintained/in_text_claims.R
# Output: console only: one CLAIM line per block and a CLAIM SUMMARY line
# Depends on: maintained/output/*, ground_truth/published_claims.csv,
#   ground_truth/published_appendix_values.csv, ground_truth/published_maintext_tables.csv,
#   errata_entries.csv, excheckr
# Description: The second instrument. Every number in the article, its figure
#   captions, its Methods and its Supplementary Information is recomputed here
#   from the pipeline's committed output, by a path of its own, and printed
#   beside the sentence that states it. Nothing here fits a model and nothing
#   here reads the ground truth: where this file and build_ground_truth.R
#   disagree, one of them is wrong and that disagreement is the finding.
#
#   It reads the extraction and the two published transcriptions, which are
#   readings of the article rather than the comparison against it. A block
#   cannot print "the article states 0.036" without the article's own string.
#   It also reads the errata spine, which says which published numbers a
#   quantity erratum corrects and to what.
#
#   Scoring comes from excheckr. Every block prints a line of the form
#
#       CLAIM <claim_id> = <value> || [verdict] <label>
#
#   at the precision the article prints, on the article's own scale: a share
#   the article prints in per cent is multiplied by 100 at the call site, and a
#   count it prints in millions is divided by a million there. The verdict is
#   derived from the extraction and the spine, never typed here. [match] means
#   the article's number and this one agree; [corrected] means a quantity
#   erratum names the claim and this value equals the corrected value it
#   publishes; [shape] means the article prints no number and the line records
#   a truth value (1 or 0) or a count; [hedged] means a descriptive claim that
#   does not agree, scored as holds in build_ground_truth.R; [derived] is
#   declared at the call site where the block computes something other than the
#   article's own token in order to test it, such as the observed maximum of a
#   stated bound. MISMATCH, STALE, DRIFT and MISSING fail the run.
#
#   It loads its own packages rather than sourcing helpers.R, so that nothing
#   the analysis scripts define can reach it.
#
#   The quoted sentences are transcribed from the published pages, with the
#   article's Unicode minus and en dash written as ASCII hyphens and its
#   subscripted degrees of freedom written inline. Nothing else is changed.

library(here)
library(tidyverse)
library(excheckr)

here::i_am("maintained/in_text_claims.R")

options(width = 200)

# Every column is read as character: value_paper carries "0.60" and a reader left
# to guess returns 0.6, destroying the precision the comparison depends on.
published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(.default = col_character())
)
errata <- read_csv(here::here("errata_entries.csv"),
                   col_types = cols(.default = col_character()))
published_appendix <- read_csv(
  here::here("ground_truth", "published_appendix_values.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)
published_maintext <- read_csv(
  here::here("ground_truth", "published_maintext_tables.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)

read_out <- function(file) {
  read_csv(here::here("maintained", "output", file), show_col_types = FALSE)
}

f1 <- read_out("figure_1_ate_cate_estimates.csv")
f2 <- read_out("figure_2_heterogeneity_by_tss.csv")
f3_binned <- read_out("figure_3_binned_means.csv")
f5 <- read_out("figure_5_balance.csv")
t1 <- read_out("table_1_experimental_strata_cells.csv")
b2 <- read_out("figure_b2_acronym_spending.csv")
c3 <- read_out("figure_c3_facebook_spending.csv")
main <- read_out("text_main_claims.csv")
sizes <- read_out("text_subgroup_sizes.csv")
spend <- read_out("text_spending_claims.csv")
ad_library <- read_out("text_ad_library_totals.csv")
descriptive <- read_out("text_descriptive_claims.csv")

# The expectation of every claim comes from these two frames: the extraction
# says what the article prints, the spine says which of those a quantity
# erratum corrects and to what. format = "id" is the line build_ground_truth.R
# parses.
claim_start(published = published_claims, errata = errata, format = "id")

tidy_number <- function(x) str_replace(x, "^-(0(\\.0+)?)$", "\\1")

paper_value <- function(id) {
  spec <- published_claims |> filter(.data$claim_id == .env$id)
  stopifnot(nrow(spec) == 1)
  as.numeric(spec$value_paper)
}

quantity <- function(tbl, name) {
  row <- tbl |> filter(quantity == name)
  stopifnot(nrow(row) == 1)
  row$value
}

holds_for <- function(name) {
  row <- descriptive |> filter(quantity == name)
  stopifnot(nrow(row) == 1)
  row$holds
}

# Abstract ----

# "We present the results of a large, US$8.9 million campaign-wide field
#  experiment, conducted among 2 million moderate- and low-information
#  persuadable voters in five battleground states during the 2020 US
#  presidential election."
claim("abstract_sample_millions", (sum(t1$control_n[t1$row_label != "Total"]) +
        sum(t1$treatment_n[t1$row_label != "Total"])) / 1e6,
      "Analysis sample, in millions, summed over the 18 strata")

# "Treatment group participants were exposed to an 8-month-long advertising
#  programme delivered via social media, designed to persuade people to vote
#  against Donald Trump and for Joe Biden."
# The cost, the state count and the programme length are properties of the
# campaign, not of the data; nothing in the deposit records them.

# "We found evidence of differential turnout effects by modelled level of Trump
#  support: the campaign increased voting among Biden leaners by 0.4 percentage
#  points (s.e. = 0.2 pp) and decreased voting among Trump leaners by 0.3
#  percentage points (s.e. = 0.3 pp) for a difference in conditional average
#  treatment effects of 0.7 points (t1,035,571 = -2.09; P = 0.036; DIC = 0.7
#  points; 95% confidence interval = -0.014 to 0)."
claim("abstract_biden_effect", 100 * quantity(main, "cate_biden_estimate"),
      "CATE, Trump support score 30 to 40, PAP specification")
claim("abstract_biden_se", 100 * quantity(main, "cate_biden_se"),
      "Standard error of that CATE")
claim("abstract_trump_effect", 100 * abs(quantity(main, "cate_trump_estimate")),
      "CATE magnitude, Trump support score 60 to 70, PAP specification")
claim("abstract_trump_se", 100 * quantity(main, "cate_trump_se"),
      "Standard error of that CATE")
claim("abstract_dic", 100 * abs(quantity(main, "dic_estimate")),
      "Difference in CATEs, magnitude")
claim("abstract_dic_t", quantity(main, "dic_t"), "t statistic")
claim("abstract_dic_df", quantity(main, "dic_df"), "Degrees of freedom")
claim("abstract_dic_p", quantity(main, "dic_p"), "p-value")
claim("abstract_dic_ci_low", quantity(main, "dic_ci_low"), "Interval, lower bound")
claim("abstract_dic_ci_high", quantity(main, "dic_ci_high"), "Interval, upper bound")

# Introduction ----

# "Unfortunately, party registration was not available for a large proportion of
#  the participant pool (72%), so we estimated heterogeneous effects by Trump
#  support score as well."
claim("intro_party_registration_missing",
      100 * sizes$n[sizes$level == "Unknown"] / sizes$n[sizes$level == "Total"],
      "Share of the analysis sample with unknown party registration")

# "According to our pre-registered regression specification to estimate
#  conditional average treatment effects (CATEs), we find that the campaign
#  increased voting among Biden leaners (those with modelled Trump support
#  scores between 30 and 40) by 0.4 percentage points and decreased voting among
#  Trump leaners (those with a Trump support score between 60 and 70) by 0.3
#  percentage points."
tss_low <- f3_binned |> filter(tss_round <= 40) |> pull(tss_round)
tss_high <- f3_binned |> filter(tss_round >= 60) |> pull(tss_round)
claim("intro_tss_low_bucket_min", min(tss_low), "Lowest Trump support score in the low bucket")
claim("intro_tss_low_bucket_max", max(tss_low), "Highest Trump support score in the low bucket")
claim("intro_tss_high_bucket_min", min(tss_high), "Lowest Trump support score in the high bucket")
claim("intro_tss_high_bucket_max", max(tss_high), "Highest Trump support score in the high bucket")

# Results ----

# "Focusing on the pre-registered specification (middle column of facets), we
#  found that the overall effect on turnout (ATE) was -0.06 percentage points,
#  with a robust standard error of 0.12 points (t1,999,277 = -0.52; P = 0.60;
#  ATE = -0.0006; 95% CI = -0.0030 to 0.0017)."
ate_row <- f1 |> filter(covariate == "ATE", estimator == "PAP adjustment set")
stopifnot(nrow(ate_row) == 1)
claim("results_ate_pp", 100 * ate_row$estimate, "Average treatment effect, percentage points")
claim("results_ate_se_pp", 100 * ate_row$std.error, "Standard error, percentage points")
claim("results_ate_t", ate_row$statistic, "t statistic")
claim("results_ate_df", ate_row$df, "Degrees of freedom")
claim("results_ate_p", ate_row$p.value, "p-value")
claim("results_ate_estimate", ate_row$estimate, "Average treatment effect, proportion scale")
claim("results_ate_ci_low", ate_row$conf.low, "Interval, lower bound")
claim("results_ate_ci_high", ate_row$conf.high, "Interval, upper bound")

# "Using a very narrow equivalence range (plus or minus one-third of a
#  percentage point), we can affirm that our overall estimate is effectively
#  equivalent to zero using the two one-sided tests procedure (P = 0.013)."
claim("results_equivalence_bound", 100 * quantity(main, "equivalence_bound"),
      "Equivalence bound, percentage points")
claim("results_tost_p", quantity(main, "tost_p_lower"),
      "Larger of the two one-sided p-values")

# "Here, we see that differential effects of the programme were stronger in our
#  early voting data (1.0 percentage points favouring Biden) than in the
#  in-person voting data (0.3 percentage points favouring Trump) (linear
#  hypothesis test of equality of the differences in CATEs, accounting for the
#  covariance of the estimates: F1,1035571) = 20.99; P < 0.0001)."
dic <- f2 |> filter(str_starts(inquiry, "Difference-in-CATEs"),
                    estimator == "PAP adjustment set")
claim("results_early_dic_pp",
      100 * abs(dic$estimate[dic$outcome == "Voted early in 2020"]),
      "Difference in CATEs for early voting, magnitude")
claim("results_in_person_dic_pp",
      100 * abs(dic$estimate[dic$outcome == "Voted in person in 2020"]),
      "Difference in CATEs for in-person voting, magnitude")
claim("results_equality_f", quantity(main, "equality_f"), "F statistic")
claim("results_equality_num_df", quantity(main, "equality_num_df"),
      "Numerator degrees of freedom")
claim("results_equality_den_df", quantity(main, "equality_den_df"),
      "Denominator degrees of freedom")
# The sentence states a bound, P < 0.0001, so the line records the p-value against it.
claim("results_equality_p", quantity(main, "equality_p"),
      "p-value of that test, against a stated bound of 0.0001", expect = "derived")

# Figure 1 and Supplementary Table 1 ----

# "Fig. 1 | Average treatment effects and CATEs of treatment on 2020 turnout
#  under three inverse probability-weighted regression specifications. ... The
#  numbers were as follows: 1,999,282 (total), 1,379,017 (aged 18-39 years),
#  620,265 (aged 40+ years), 978,041 (female), 1,021,241 (other gender), 233,546
#  (Black), 179,036 (Latinx), 1,531,129 (White), 55,571 (other race), 1,337,057
#  (margin of <3 percentage points), 662,225 (margin of >3 percentage points),
#  182,945 (democratic partisanship), 71,875 (republican partisanship),
#  1,442,071 (unknown partisanship), 302,391 (other partisanship), 522,918
#  (Trump support score of 30-40), 485,371 (Trump support score of 40-50),
#  478,333 (Trump support score of 50-60) and 512,660 (Trump support score of
#  60-70)."
caption_sizes <- tribble(
  ~id, ~covariate, ~level,
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
pwalk(caption_sizes, function(id, covariate, level) {
  n <- sizes |> filter(.data$covariate == .env$covariate, .data$level == .env$level)
  stopifnot(nrow(n) == 1)
  claim(id, n$n, str_glue("Sample size, {covariate}: {level}"))
})

# The large floats are compared cell by cell, by this file's own join. Figures 1
# and 2 print an estimate and a standard error above every point, which makes
# them published tables; they are read from their own transcription rather than
# from the supplementary tables the captions point at.
render_cell <- function(x, digits) tidy_number(sprintf(paste0("%.", digits, "f"), x))

count_agreeing <- function(published, cells) {
  joined <- published |> left_join(cells, by = c("row_key", "quantity"))
  stopifnot(nrow(joined) == nrow(published), !any(is.na(joined$value)))
  sum(render_cell(joined$value, joined$digits) == joined$value_paper |
        abs(round(joined$value, joined$digits) -
              as.numeric(joined$value_paper)) < 1e-9 * pmax(1, abs(as.numeric(joined$value_paper))))
}

st1_cells <-
  f1 |>
  transmute(
    row_key = str_c(covariate, " | ", covariate_value, " | ", estimator),
    estimate, se = std.error, df, t = statistic, p_value = p.value,
    ci_lower = conf.low, ci_upper = conf.high
  ) |>
  pivot_longer(-row_key, names_to = "quantity", values_to = "value")

st2_cells <-
  f2 |>
  transmute(
    row_key = str_c(inquiry, " | ", outcome, " | ", estimator),
    estimate, se = std.error, df, t = statistic, p_value = p.value,
    ci_lower = conf.low, ci_upper = conf.high
  ) |>
  pivot_longer(-row_key, names_to = "quantity", values_to = "value")

st3_cells <-
  f5 |>
  transmute(
    row_key = outcome,
    estimate, se = std.error, df, t = statistic, p_value = p.value,
    p_value_bh = adjusted_p, ci_lower = conf.low, ci_upper = conf.high
  ) |>
  pivot_longer(-row_key, names_to = "quantity", values_to = "value")

t1_cells <-
  t1 |>
  transmute(
    row_key = row_label,
    control_n, treatment_n, p_treat,
    voting_rate_control = 100 * voting_rate_control,
    voting_rate_treatment = 100 * voting_rate_treatment
  ) |>
  pivot_longer(-row_key, names_to = "quantity", values_to = "value")

figure_key <- function(tbl, replacements) {
  tbl |> mutate(row_key = reduce(names(replacements), function(x, from) {
    str_replace_all(x, fixed(from), replacements[[from]])
  }, .init = row_key))
}

figure_1_published <- published_maintext |>
  filter(float == "figure_1") |>
  figure_key(list("Age (years)" = "Age", "Margin (pp)" = "Margin",
                  "Trump support score |" = "Trump support |",
                  "18–39" = "18-39", "Vote margin <3" = "Vote margin less than 3pp",
                  "Vote margin >3" = "Vote margin more than 3pp",
                  "30–40" = "30 to 40", "40–50" = "40 to 50",
                  "50–60" = "50 to 60", "60–70" = "60 to 70"))
figure_2_published <- published_maintext |>
  filter(float == "figure_2") |>
  figure_key(list(
    "Difference in CATEs (TSS of 60–70 versus TSS of 30–40)" =
      "Difference-in-CATEs TSS 60-70 versus TSS 30-40",
    "Treatment × TSS interaction term from linear model" =
      "Treatment*TSS interaction term from linear model"
  ))
table_1_published <- published_maintext |>
  filter(float == "table_1") |>
  mutate(row_key = str_replace_all(row_key, "–", "-"))

claim("figure_1_cells_reproduced", count_agreeing(figure_1_published, st1_cells),
      "Cells of Figure 1 reproduced of 114")
claim("figure_2_cells_reproduced", count_agreeing(figure_2_published, st2_cells),
      "Cells of Figure 2 reproduced of 36")
claim("table_1_cells_reproduced", count_agreeing(table_1_published, t1_cells),
      "Cells of Table 1 reproduced of 95")
claim("supplementary_table_1_cells_reproduced",
      count_agreeing(published_appendix |> filter(float == "supplementary_table_1"), st1_cells),
      "Cells of Supplementary Table 1 reproduced of 399")
claim("supplementary_table_2_cells_reproduced",
      count_agreeing(published_appendix |> filter(float == "supplementary_table_2"), st2_cells),
      "Cells of Supplementary Table 2 reproduced of 126")
claim("supplementary_table_3_cells_reproduced",
      count_agreeing(published_appendix |> filter(float == "supplementary_table_3"), st3_cells),
      "Cells of Supplementary Table 3 reproduced of 176")

# "Inferential statistics for each estimate are reported in Supplementary Table
#  1." The figure and the table are two published transcriptions of the same
# estimates, so they are compared to each other rather than one taken for the
# other.
figure_versus_table <- function(figure_published, table_id) {
  figure_published |>
    filter(quantity %in% c("estimate", "se")) |>
    select(row_key, quantity, figure = value_paper) |>
    inner_join(published_appendix |> filter(float == table_id) |>
                 select(row_key, quantity, cell = value_paper),
               by = c("row_key", "quantity"))
}
f1_pairs <- figure_versus_table(figure_1_published, "supplementary_table_1")
f2_pairs <- figure_versus_table(figure_2_published, "supplementary_table_2")
stopifnot(nrow(f1_pairs) == 114, nrow(f2_pairs) == 36)
claim("figure_1_agrees_with_supplementary_table_1",
      all(f1_pairs$figure == f1_pairs$cell),
      "Figure 1's printed cells equal Supplementary Table 1's", digits = 0)
claim("figure_2_agrees_with_supplementary_table_2",
      all(f2_pairs$figure == f2_pairs$cell),
      "Figure 2's printed cells equal Supplementary Table 2's", digits = 0)

# "Fig. 2 ... The numbers were as follows: 1,035,578 (difference in CATEs
#  estimates) and 1,999,282 (interaction estimates)."
claim("figure_2_caption_n_dic", quantity(main, "n_dic_sample"),
      "Participants in the two extreme Trump support buckets")
claim("figure_2_caption_n_interaction", quantity(main, "n_total"),
      "Participants in the interaction estimates")

# "Fig. 3 | 2020 turnout rates by one-point bins of Trump support score and
#  condition. ... n = 1,999,282."
claim("figure_3_caption_n", quantity(main, "n_total"), "Participants in Figure 3")
claim("figure_3_points_plotted", nrow(f3_binned),
      "Binned means plotted: 41 bins by 2 conditions by 3 outcomes")

# "Fig. 5 | Balance on pre-treatment covariates. ... n = 1,999,282."
claim("figure_5_caption_n", quantity(main, "n_total"), "Participants in Figure 5")
claim("figure_5_points_plotted", nrow(f5), "Covariate estimates plotted")

# Figure 4, the assignment flow chart ----

# "Voter turnout in 2020 was assessed for all 287,735 remaining control group
#  participants" and "1,711,547 remaining treatment group participants".
claim("figure_4_n_control", sum(t1$control_n[t1$row_label != "Total"]),
      "Control group participants, summed over the 18 strata")
claim("figure_4_n_treatment", sum(t1$treatment_n[t1$row_label != "Total"]),
      "Treatment group participants, summed over the 18 strata")

# "This procedure generated 18 strata of subjects, each with a different
#  probability of assignment to treatment."
claim("figure_4_n_strata", sum(t1$row_label != "Total"), "Strata in Table 1")

# "(1) Trump support score between 30 and 70 (2) Presidential turnout score
#  between 20 and 80"
claim("figure_4_tss_target_min", quantity(main, "tss_min"),
      "Lowest Trump support score in the analysis set")
claim("figure_4_tss_target_max", quantity(main, "tss_max"),
      "Highest Trump support score in the analysis set")
claim("figure_4_pts_target_min", quantity(main, "pts_min"),
      "Lowest presidential turnout score in the analysis set")
# The criterion is a bound and the line records the observed maximum against it.
# The analysis set exceeds it; build_ground_truth.R records that as unresolved.
claim("figure_4_pts_target_max", quantity(main, "pts_max"),
      "Highest presidential turnout score in the analysis set, against a stated bound of 80",
      expect = "derived")
claim("methods_pts_range_holds", quantity(main, "pts_max") <= 80,
      "Presidential turnout scores lie inside the stated targeting range", digits = 0)

# "All 31,146,729 voters registered as of February 2020 in five states ... were
#  initially eligible", against "2,486,157 voters were assigned to the control
#  group" and "28,660,572 voters were assigned to the treatment group".
claim("figure_4_assignment_sums_to_eligible",
      paper_value("figure_4_assigned_control") + paper_value("figure_4_assigned_treatment"),
      "Assignment counts summed, against the eligible population")

# Table 1 ----

# "Table 1 | Experimental strata": 19 rows of group sizes, assignment
# probabilities and voting rates, compared cell by cell above.

# Methods ----

# "By starting from the voter file and filtering on all of the relevant
#  variables, we were able to recover all treatment and control identities in
#  this subset, resulting in an analysis sample of 1,999,282 participants."
claim("methods_n_analysis_sample", sum(t1$control_n[t1$row_label != "Total"]) +
        sum(t1$treatment_n[t1$row_label != "Total"]),
      "Analysis sample, summed over the 18 strata")

# "Out of 21 covariates, only two exhibited statistically significant imbalance:
#  Trump support score (t1,999,280 = -2.260; P = 0.024; ATE = -0.001; 95% CI =
#  -0.001 to 0) and Trump support scores between 60 and 70 (t1,999,280 = -2.609;
#  P = 0.009; ATE = -0.003; 95% CI = -0.006 to -0.001)."
claim("methods_n_covariates", nrow(f5), "Covariates in Figure 5 and Supplementary Table 3")
claim("methods_n_significant_imbalance", sum(f5$p.value < 0.05),
      "Covariates with an unadjusted p below 0.05")

balance_row <- function(label) {
  row <- f5 |> filter(outcome == label)
  stopifnot(nrow(row) == 1)
  row
}
tss100 <- balance_row("Trump support score / 100")
tss6070 <- balance_row("TSS: 60-70")
claim("methods_tss100_estimate", tss100$estimate, "Trump support score imbalance")
claim("methods_tss100_t", tss100$statistic, "t statistic")
claim("methods_tss100_df", tss100$df, "Degrees of freedom")
claim("methods_tss100_p", tss100$p.value, "p-value")
claim("methods_tss100_ci_low", tss100$conf.low, "Interval, lower bound")
claim("methods_tss100_ci_high", tss100$conf.high, "Interval, upper bound")
claim("methods_tss6070_estimate", tss6070$estimate, "TSS 60 to 70 imbalance")
claim("methods_tss6070_t", tss6070$statistic, "t statistic")
claim("methods_tss6070_df", tss6070$df, "Degrees of freedom")
claim("methods_tss6070_p", tss6070$p.value, "p-value")
claim("methods_tss6070_ci_low", tss6070$conf.low, "Interval, lower bound")
claim("methods_tss6070_ci_high", tss6070$conf.high, "Interval, upper bound")

# "Neither of these estimates remained significant after a Benjamini-Hochberg
#  correction to control for the false discovery rate (P = 0.262 and 0.200,
#  respectively)."
claim("methods_bh_p_tss100", tss100$adjusted_p, "Adjusted p-value")
claim("methods_bh_p_tss6070", tss6070$adjusted_p, "Adjusted p-value")

# "An F-test comparing a regression of treatment status on covariates with
#  strata fixed effects versus a restricted model predicting treatment status
#  from strata fixed effects was non-significant (F1999264,-18 = 1.0266; P =
#  0.425)."
claim("methods_omnibus_f", quantity(main, "omnibus_f"), "Omnibus F statistic")
claim("methods_omnibus_p", quantity(main, "omnibus_p"), "Omnibus p-value")
claim("methods_omnibus_df_first_subscript", quantity(main, "omnibus_num_df"),
      "Numerator degrees of freedom, which the first subscript should carry")
claim("methods_omnibus_df_second_subscript", quantity(main, "omnibus_den_df"),
      "Denominator degrees of freedom, which the second subscript should carry")

# "Moreover, we are missing race data for 4% of the sample where race was
#  uncoded."
# The share describes the assignment population, which the deposit does not
# carry, so the block has no value to print.
claim("methods_missing_race_share", holds_for("uncoded_race_share"),
      "The deposit reaches the population the sentence describes", digits = 0,
      expect = "absent")

# "Over the experimental period, Acronym spent a minimum of US$368,800 on
#  advertisements containing the word Biden, US$3,254,600 on advertisements
#  containing the word Trump and US$244,500 on advertisements containing both
#  words. Turning to advertisement formats, Acronym spent US$1,275,000 on
#  promoted news advertisements, US$2,288,900 on video advertisements and
#  US$304,000 on other advertising formats (for example, images)."
by_keyword <- b2 |> filter(panel == "Keyword") |> group_by(group) |>
  summarize(spend_lb = sum(spend_lb), .groups = "drop")
by_format <- b2 |> filter(panel == "Ad Format") |> group_by(group) |>
  summarize(spend_lb = sum(spend_lb), .groups = "drop")
group_spend <- function(tbl, name) {
  row <- tbl |> filter(group == name)
  stopifnot(nrow(row) == 1)
  row$spend_lb
}
claim("methods_spend_biden", group_spend(by_keyword, "Biden"), "Spend on Biden advertisements")
claim("methods_spend_trump", group_spend(by_keyword, "Trump"), "Spend on Trump advertisements")
claim("methods_spend_both", group_spend(by_keyword, "Both"), "Spend on advertisements naming both")
claim("methods_spend_promoted_news", group_spend(by_format, "Promoted news"),
      "Spend on promoted news")
claim("methods_spend_video", group_spend(by_format, "Traditional video"),
      "Spend on traditional video")
claim("methods_spend_other_formats", group_spend(by_format, "Other"),
      "Spend on other formats")

# "The full cost of the advertising campaign was US$8.9 million, spread out over
#  a treatment audience of 1,993,216 million (3,322,027 programme-eligible
#  voters x our 60% match rate), amounting to US$4.46 of advertising expenditure
#  per voter."
claim("methods_treatment_audience",
      paper_value("methods_programme_eligible") * paper_value("methods_match_rate") / 100,
      "Programme-eligible voters times the match rate")
# The printed figure carries "million" and the parenthetical's product does not,
# so the sentence as printed fails this test and the corrected sentence, which
# prints the audience as a count, passes it. Entry 2 of the errata publishes that
# correction as 1. The extraction stores no value for a truth-value claim, so the
# sentence's own result, 0, is read off the page and passed here; without it the
# line could only score [shape] and the erratum would go unchecked.
claim("methods_treatment_audience_scale",
      round(paper_value("methods_programme_eligible") * paper_value("methods_match_rate") / 100) ==
        paper_value("methods_treatment_audience"),
      "The parenthetical's product equals the audience read as a count",
      published = "0", digits = 0)
# The campaign cost is printed to two significant figures, so the quotient is
# determined only to about half a cent and the line records it without a verdict.
claim("methods_cost_per_voter",
      1e6 * paper_value("abstract_campaign_cost") / paper_value("methods_treatment_audience"),
      "Campaign cost divided by the treatment audience", expect = "derived")

# "The second specification (pre-analysis plan (PAP) adjustment set) includes
#  control variables described in the PAP: the Trump support score, the
#  presidential turnout score and a count of whether the participant voted in
#  the 2012, 2016 and 2018 elections."
claim("methods_vote_history_elections", quantity(main, "num_times_voted_max"),
      "Maximum of the vote history count")

# "In particular, they directed advertising to voters modelled to have mid-range
#  Trump support and turnout scores."
claim("methods_tss_data_min", quantity(main, "tss_min"), "Lowest Trump support score")
claim("methods_tss_data_max", quantity(main, "tss_max"), "Highest Trump support score")

# "The error bars represent 95% CIs", stated in every figure caption.
claim("methods_ci_level", quantity(main, "interval_level"),
      "Interval level implied by the reported half widths")

# Supplementary Information ----

# "A Matching treatment voters to Facebook users ... B Description of Ad Content
#  ... C Analysis of Overall Facebook Ad Environment ... D Pre-analysis Plan ...
#  E Randomization code ... F Regression tables"
claim("si_n_supplementary_figures", 4, "Numbered supplementary figures")
claim("si_n_supplementary_tables", n_distinct(published_appendix$float),
      "Numbered supplementary tables, counted from the transcription")

# "The lower bound of total spend by all advertisers that meet this criteria is
#  $349,006,000; unfortunately, we cannot calculate an upper bound since the
#  largest bucket (>$1 million) is boundless."
claim("si_c_total_spend_all_advertisers",
      quantity(ad_library, "ad_library_total_spend_lb"),
      "Lower bound of total spend in the Ad Library extract")

# "Acronym spent approximately $3,867,900 to $5,921,963 on persuasion ads
#  containing the words 'Biden' or 'Trump' during this period."
claim("si_c_acronym_spend_low", quantity(spend, "spend_lb_persuasion_total"),
      "Lower bound of Acronym persuasion spend")
claim("si_c_acronym_spend_high", quantity(spend, "spend_ub_persuasion_total"),
      "Upper bound of Acronym persuasion spend")

# "Comparing both the lower bound of Acronym spend and overall spend as
#  specified above ($349,006,000), we find that Acronym constituted
#  approximately 1% of spending on Facebook on the presidential campaign in our
#  target states."
claim("si_c_acronym_share",
      100 * quantity(spend, "spend_lb_all_acronym_ads") /
        quantity(ad_library, "ad_library_total_spend_lb"),
      "Acronym's share of the extract's total spend")

# "According to this data, there was a total of approximately $977,761,865 spent
#  on federal races on Facebook from 2/1/2020 to 11/3/2020, $6,603,488 (0.67%) of
#  which was from Acronym."
claim("si_c_wesleyan_share",
      100 * paper_value("si_c_wesleyan_acronym") / paper_value("si_c_wesleyan_total"),
      "Acronym's share, computed from the two figures the sentence prints")

# "In fact, we observe significant early spending (Acronym spent more on
#  persuasion in July than in any other month, see main Figure ??), and we find
#  much stronger effects on early voting than day of voting."
claim("si_c_july_highest_spend", holds_for("july_highest_persuasion_spend"),
      "July carries the largest monthly lower-bound persuasion spend", digits = 0)
claim("si_c_broken_cross_reference", FALSE,
      "The cross-reference 'main Figure ??' resolves to a figure", digits = 0)

# "We queried the Facebook Ad API for all ads containing 'Biden' or 'Trump' that
#  ran from 2/1/2020 - 11/3/2020 on Facebook, where the ads were targeted to at
#  least one of Acronym's program's states: AZ MI, NC, PA, or WI", against
# "Supplementary Figure 3: ... targeted to at least one of AZ, GA, MI, NC, PA, or
#  WI."
claim("si_c_state_lists_agree", FALSE,
      "The query description and the figure caption name the same states", digits = 0)

# "Here we report the SQL code used to randomly sample units from the voter file
#  in to the holdout control group."
claim("si_e_sql_limits_match_figure_4",
      sum(map_dbl(str_c("figure_4_step_", 1:6, "_sampled"), paper_value) ==
            map_dbl(str_c("si_e_sql_limit_", 1:6), paper_value)),
      "SQL sampling limits equal to the Figure 4 sample sizes")

# The listing's two race recodes, as published p. 15 prints them:
#   ", case when vb_voterbase_race = 'African-American' or (vb_voterbase_race =
#    'Uncoded' and civis_race"
#   ", case when vb_voterbase_race = 'Hispanic' or (vb_voterbase_race =
#    'Uncoded' and civis_race = 'HISPA"
# Both lines are drawn past the right edge of a 612-point page, to x 610.6 and
# x 615.8, and the text layer ends where the drawing does. Each loses the whole
# of its "then ... end as ..." clause, which is the definition of the variable,
# so neither can be checked against the deposited code and the listing as
# printed cannot be run. FALSE is read off the published page, not computed:
# there is no pipeline quantity for a line that is missing.
claim("si_e_sql_black_definition", FALSE,
      "The listing's black recode reaches its 'end as black'", digits = 0)
claim("si_e_sql_hispanic_definition", FALSE,
      "The listing's hispanic recode reaches its 'end as hispanic'", digits = 0)

# "Supplementary Figure 2: Lower bound of spending by Acronym on ads containing
#  the words 'Biden' or 'Trump' on Facebook, over time, by format and keyword"
claim("si_figure_2_series_per_panel",
      b2 |> distinct(panel, group) |> count(panel) |> pull(n) |> unique(),
      "Series per panel of Supplementary Figure 2")

# "Supplementary Figure 3: Lower bound of spending by all advertisers on
#  Facebook on ads with the words 'Biden' or 'Trump'"
claim("si_figure_3_month_labels", 3, "Month labels on the horizontal axis")

print(str_glue("Weeks plotted in Supplementary Figure 3: {nrow(c3)}"))
print(str_glue("Points plotted in Supplementary Figure 2: {nrow(b2)}"))
print(descriptive |> select(quantity, holds, evidence), n = Inf)

# Gates ----
# assert_claims() stops on any MISMATCH, STALE, DRIFT or MISSING, on a claim a
# quantity erratum names that did not score [corrected], and on any difference
# between the claims printed here and the claims the extraction says need a
# block. claim_summary() then prints the verdict counts, so an exemption cannot
# grow in silence.
assert_claims()
invisible(claim_summary())
