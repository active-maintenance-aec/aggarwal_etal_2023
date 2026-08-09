# aggarwal_etal_2023/maintained/text_descriptive_claims.R
# Output: output/text_descriptive_claims.csv
# Depends on: output/figure_1_ate_cate_estimates.csv, output/figure_2_heterogeneity_by_tss.csv,
#   output/figure_5_balance.csv, output/figure_b2_acronym_spending.csv,
#   output/text_main_claims.csv, original/aggregated_analysis_set.rds, helpers.R
# Description: Truth values for the claims the article makes about shape, sign
#   and count rather than about a number. Each row carries the statement, the
#   evidence behind it and whether it holds. Thresholds that appear here are
#   comparison targets rather than inputs to any estimate: nothing in this file
#   is fitted, and every estimate it reads was produced by another script.

source(here::here("maintained", "helpers.R"))

figure_1 <- read_csv(here::here("maintained", "output", "figure_1_ate_cate_estimates.csv"),
                     show_col_types = FALSE)
figure_2 <- read_csv(here::here("maintained", "output", "figure_2_heterogeneity_by_tss.csv"),
                     show_col_types = FALSE)
figure_5 <- read_csv(here::here("maintained", "output", "figure_5_balance.csv"),
                     show_col_types = FALSE)
figure_b2 <- read_csv(here::here("maintained", "output", "figure_b2_acronym_spending.csv"),
                      show_col_types = FALSE)
main_claims <- read_csv(here::here("maintained", "output", "text_main_claims.csv"),
                        show_col_types = FALSE)

claim_value <- function(name) {
  row <- main_claims |> filter(quantity == name)
  stopifnot(nrow(row) == 1)
  row$value
}

pap <- figure_1 |> filter(estimator == "PAP adjustment set")

# Party registration coverage, which the article reports as a share of the
# participant pool.
main_analysis_set <- load_main_analysis_set()
unknown_party_share <- mean(main_analysis_set$party == "Unknown")
other_race_share <- mean(main_analysis_set$race == "Other")

# Which month carries the largest lower-bound persuasion spend. The panels of
# Supplementary Figure 2 split the same advertisements two ways, so either gives
# the same monthly total.
monthly_spend <-
  figure_b2 |>
  filter(panel == "Keyword") |>
  group_by(month) |>
  summarize(spend_lb = sum(spend_lb), .groups = "drop") |>
  arrange(desc(spend_lb))

dic_pap <-
  figure_2 |>
  filter(str_starts(inquiry, "Difference-in-CATEs"),
         estimator == "PAP adjustment set")

dic_of <- function(outcome_label) {
  row <- dic_pap |> filter(outcome == outcome_label)
  stopifnot(nrow(row) == 1)
  row$estimate
}

null_cate_covariates <- c("Age", "Gender", "Race", "Margin")
null_cates <- pap |> filter(covariate %in% null_cate_covariates)

significant_imbalance <- figure_5 |> filter(p.value < 0.05)

descriptive <- tribble(
  ~quantity, ~statement, ~evidence, ~holds,

  "n_balance_covariates",
  "Out of 21 covariates, only two exhibited statistically significant imbalance",
  str_glue("Figure 5 and Supplementary Table 3 report {nrow(figure_5)} covariates"),
  nrow(figure_5) == 21,

  "n_significant_imbalance",
  "only two exhibited statistically significant imbalance",
  str_glue("{nrow(significant_imbalance)} covariates have an unadjusted p below 0.05: ",
           "{paste(significant_imbalance$outcome, collapse = '; ')}"),
  nrow(significant_imbalance) == 2,

  "imbalance_not_significant_after_bh",
  "Neither of these estimates remained significant after a Benjamini-Hochberg correction",
  str_glue("smallest adjusted p among the two is ",
           "{sprintf('%.3f', min(significant_imbalance$adjusted_p))}"),
  all(significant_imbalance$adjusted_p > 0.05),

  "omnibus_not_significant",
  "An F-test ... was non-significant",
  str_glue("omnibus p is {sprintf('%.3f', claim_value('omnibus_p'))}"),
  claim_value("omnibus_p") > 0.05,

  "biden_leaners_increase",
  "the campaign increased voting among Biden leaners",
  str_glue("CATE for Trump support 30 to 40 is ",
           "{sprintf('%.5f', claim_value('cate_biden_estimate'))}"),
  claim_value("cate_biden_estimate") > 0,

  "trump_leaners_decrease",
  "and decreased voting among Trump leaners",
  str_glue("CATE for Trump support 60 to 70 is ",
           "{sprintf('%.5f', claim_value('cate_trump_estimate'))}"),
  claim_value("cate_trump_estimate") < 0,

  "cates_by_age_gender_race_margin_null",
  "We also observed small conditional average effect estimates by age, gender, race and vote margin in 2016 that were not statistically significant",
  str_glue("largest absolute PAP estimate across those {nrow(null_cates)} subgroups is ",
           "{sprintf('%.4f', max(abs(null_cates$estimate)))}; ",
           "smallest p is {sprintf('%.3f', min(null_cates$p.value))}"),
  all(null_cates$p.value > 0.05),

  "equivalent_to_zero",
  "we can affirm that our overall estimate is effectively equivalent to zero using the two one-sided tests procedure",
  str_glue("larger of the two one-sided p-values is ",
           "{sprintf('%.5f', max(claim_value('tost_p_lower'), claim_value('tost_p_upper')))}"),
  max(claim_value("tost_p_lower"), claim_value("tost_p_upper")) < 0.05,

  "early_stronger_than_in_person",
  "differential effects of the programme were stronger in our early voting data than in the in-person voting data",
  str_glue("difference in CATEs is {sprintf('%.4f', dic_of('Voted early in 2020'))} for early ",
           "voting and {sprintf('%.4f', dic_of('Voted in person in 2020'))} for in-person voting"),
  abs(dic_of("Voted early in 2020")) > abs(dic_of("Voted in person in 2020")),

  "early_favours_biden_in_person_favours_trump",
  "1.0 percentage points favouring Biden ... 0.3 percentage points favouring Trump",
  str_glue("early difference in CATEs is negative ({sprintf('%.4f', dic_of('Voted early in 2020'))}) ",
           "and in-person is positive ({sprintf('%.4f', dic_of('Voted in person in 2020'))})"),
  dic_of("Voted early in 2020") < 0 && dic_of("Voted in person in 2020") > 0,

  "party_registration_missing_share",
  "party registration was not available for a large proportion of the participant pool (72%)",
  str_glue("share of the analysis sample with unknown party registration is ",
           "{sprintf('%.1f', 100 * unknown_party_share)} per cent"),
  round(100 * unknown_party_share) == 72,

  "uncoded_race_share",
  "we are missing race data for 4% of the sample where race was uncoded",
  str_glue("the deposit holds only the analysis subset, whose other-race share is ",
           "{sprintf('%.1f', 100 * other_race_share)} per cent; the sentence describes the ",
           "31.1 million voters eligible at assignment, which the deposit does not carry"),
  NA,

  "july_highest_persuasion_spend",
  "Acronym spent more on persuasion in July than in any other month",
  str_glue("the largest monthly lower-bound persuasion spend is ",
           "{format(monthly_spend$spend_lb[1], big.mark = ',')} in ",
           "{format(monthly_spend$month[1], '%B %Y')}; July 2020 is ",
           "{format(monthly_spend$spend_lb[monthly_spend$month == as.Date('2020-07-01')], big.mark = ',')}"),
  monthly_spend$month[1] == as.Date("2020-07-01")
)

write_csv(descriptive, here::here("maintained", "output", "text_descriptive_claims.csv"))

options(width = 200)
print(descriptive, n = Inf)
print(monthly_spend, n = Inf)
