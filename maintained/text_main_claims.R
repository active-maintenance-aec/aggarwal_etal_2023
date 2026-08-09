# aggarwal_etal_2023/maintained/text_main_claims.R
# Output: output/text_main_claims.csv, output/text_subgroup_sizes.csv
# Depends on: original/aggregated_analysis_set.rds, helpers.R
# Description: Every quantity the article states in prose that comes from the
#   campaign-level experiment: the pre-registered average effect and its
#   equivalence test, the two conditional effects the abstract names, the
#   difference in conditional effects, the two party effects, the test that
#   early and in-person differences are equal, the omnibus balance test, and the
#   subgroup sizes Figure 1's caption lists.

source(here::here("maintained", "helpers.R"))

main_analysis_set <- load_main_analysis_set()

pap_formula <- formula("voted_in_2020 ~ condition + num_times_voted + tss_100 + pts_100")

treatment_row <- function(fit) {
  fit |> tidy() |> filter(term == "conditiontreat")
}

ate_pap <- treatment_row(lm_robust(pap_formula, weights = ipw,
                                   data = main_analysis_set))

cate_of <- function(rows) {
  treatment_row(lm_robust(pap_formula, weights = ipw, data = rows))
}

cate_biden <- cate_of(filter(main_analysis_set, tss_bucket == "30 to 40"))
cate_trump <- cate_of(filter(main_analysis_set, tss_bucket == "60 to 70"))
cate_republican <- cate_of(filter(main_analysis_set, party == "Republican"))
cate_democrat <- cate_of(filter(main_analysis_set, party == "Democrat"))

# Two one-sided tests against an equivalence range of a third of a percentage
# point either side of zero.
equivalence_bound <- 1 / 300
tost_lower <- pnorm(ate_pap$estimate, mean = -equivalence_bound,
                    sd = ate_pap$std.error, lower.tail = FALSE)
tost_upper <- pnorm(ate_pap$estimate, mean = equivalence_bound,
                    sd = ate_pap$std.error, lower.tail = TRUE)

extreme_buckets <- filter(main_analysis_set, tss_bucket %in% c("30 to 40", "60 to 70"))

dic_fit <-
  lm_robust(
    voted_in_2020 ~ condition * tss_bucket + num_times_voted + tss_100 + pts_100,
    weights = ipw, data = extreme_buckets
  ) |>
  tidy() |>
  filter(term == "conditiontreat:tss_bucket60 to 70")

# Are the difference in CATEs for early voting and for in-person voting equal?
# Fitted with lm() because car::linearHypothesis needs a multivariate lm, with
# the HC2 covariance passed in explicitly so the test matches every other
# standard error in the paper.
early_in_person_fit <-
  lm(
    cbind(voted_in_person_2020, voted_early_2020) ~
      condition * tss_bucket + num_times_voted + tss_100 + pts_100,
    weights = ipw, data = extreme_buckets
  )

equality_test <-
  car::linearHypothesis(
    early_in_person_fit,
    vcov = hccm(early_in_person_fit, type = "hc2"),
    hypothesis.matrix = c(0, 0, 0, 0, 0, 0, 1),
    P = matrix(c(1, -1))
  )

# car returns the sums of squares and prints the F, with no accessor in
# between, so the statistic is formed here and then checked against the number
# car itself prints. Both paths have to agree before it is written down.
equality_f <- (equality_test$SSPH[1, 1] / equality_test$df) /
  (equality_test$SSPE[1, 1] / equality_test$df.residual)
equality_num_df <- equality_test$df * equality_test$r
equality_den_df <- equality_test$df.residual
equality_p <- pf(equality_f, equality_num_df, equality_den_df, lower.tail = FALSE)

equality_printed <-
  capture.output(print(equality_test)) |>
  str_subset("^Pillai") |>
  str_squish() |>
  str_split(" ") |>
  pluck(1)
stopifnot(
  length(equality_printed) >= 6,
  abs(as.numeric(equality_printed[4]) - equality_f) < 1e-4,
  as.numeric(equality_printed[5]) == equality_num_df,
  as.numeric(equality_printed[6]) == equality_den_df
)

# Omnibus test that the covariates jointly predict treatment assignment, over
# and above the strata fixed effects.
omnibus_full <-
  lm_robust(
    formula(paste(
      "treat_num ~ party_dem + party_rep + party_unknown +",
      "tss_40_50 + tss_50_60 + tss_60_70 + tss_100 + pts_100 +",
      vote_history_terms, "+ strata"
    )),
    data = main_analysis_set
  )
omnibus_restricted <- lm_robust(treat_num ~ strata, data = main_analysis_set)
omnibus <- waldtest(omnibus_full, omnibus_restricted, test = "F")

# Design parameters the article states and the deposited data can be checked
# against: the score ranges the targeting criteria name, the vote history count,
# and the interval level every reported interval uses.
interval_level <- 100 * (2 * pt(
  abs(ate_pap$conf.low - ate_pap$estimate) / ate_pap$std.error,
  df = ate_pap$df
) - 1)

claims <- tibble(
  quantity = c(
    "n_total", "n_control", "n_treatment", "n_dic_sample",
    "tss_min", "tss_max", "pts_min", "pts_max",
    "num_times_voted_max", "n_strata", "interval_level",
    "ate_pap_estimate", "ate_pap_se", "ate_pap_t", "ate_pap_df", "ate_pap_p",
    "ate_pap_ci_low", "ate_pap_ci_high",
    "tost_p_lower", "tost_p_upper", "equivalence_bound",
    "cate_biden_estimate", "cate_biden_se", "cate_biden_t", "cate_biden_df",
    "cate_biden_p", "cate_biden_ci_low", "cate_biden_ci_high",
    "cate_trump_estimate", "cate_trump_se", "cate_trump_t", "cate_trump_df",
    "cate_trump_p", "cate_trump_ci_low", "cate_trump_ci_high",
    "dic_estimate", "dic_se", "dic_t", "dic_df", "dic_p",
    "dic_ci_low", "dic_ci_high",
    "cate_republican_estimate", "cate_republican_se",
    "cate_democrat_estimate", "cate_democrat_se",
    "equality_f", "equality_num_df", "equality_den_df", "equality_p",
    "omnibus_f", "omnibus_num_df", "omnibus_den_df", "omnibus_p"
  ),
  value = c(
    nrow(main_analysis_set),
    sum(main_analysis_set$condition == "control"),
    sum(main_analysis_set$condition == "treat"),
    nrow(extreme_buckets),
    min(main_analysis_set$tss), max(main_analysis_set$tss),
    100 * min(main_analysis_set$pts_100), 100 * max(main_analysis_set$pts_100),
    max(main_analysis_set$num_times_voted),
    n_distinct(main_analysis_set$strata),
    interval_level,
    ate_pap$estimate, ate_pap$std.error, ate_pap$statistic, ate_pap$df,
    ate_pap$p.value, ate_pap$conf.low, ate_pap$conf.high,
    tost_lower, tost_upper, equivalence_bound,
    cate_biden$estimate, cate_biden$std.error, cate_biden$statistic,
    cate_biden$df, cate_biden$p.value, cate_biden$conf.low, cate_biden$conf.high,
    cate_trump$estimate, cate_trump$std.error, cate_trump$statistic,
    cate_trump$df, cate_trump$p.value, cate_trump$conf.low, cate_trump$conf.high,
    dic_fit$estimate, dic_fit$std.error, dic_fit$statistic, dic_fit$df,
    dic_fit$p.value, dic_fit$conf.low, dic_fit$conf.high,
    cate_republican$estimate, cate_republican$std.error,
    cate_democrat$estimate, cate_democrat$std.error,
    equality_f, equality_num_df, equality_den_df, equality_p,
    omnibus$F[2], abs(omnibus$Df[2]), omnibus$Res.Df[1], omnibus$`Pr(>F)`[2]
  )
)

write_csv(claims, here::here("maintained", "output", "text_main_claims.csv"))

# The subgroup sizes Figure 1's caption lists, one row per level.
subgroup_sizes <-
  c(agecat = "Age", gender = "Gender", race = "Race",
    close_margins_2016 = "Margin", party = "Partisanship",
    tss_bucket = "Trump support") |>
  imap(function(covariate, column) {
    main_analysis_set |>
      count(level = .data[[column]], name = "n") |>
      mutate(covariate = covariate)
  }) |>
  list_rbind() |>
  bind_rows(tibble(level = "Total", n = nrow(main_analysis_set),
                   covariate = "Total")) |>
  select(covariate, level, n) |>
  arrange(covariate, level, .locale = "en")

write_csv(subgroup_sizes,
          here::here("maintained", "output", "text_subgroup_sizes.csv"))

print(claims, n = Inf)
print(subgroup_sizes, n = Inf)
