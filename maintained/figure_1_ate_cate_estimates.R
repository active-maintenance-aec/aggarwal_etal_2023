# aggarwal_etal_2023/maintained/figure_1_ate_cate_estimates.R
# Output: output/figure_1_ate_cate_estimates.{pdf,png,csv},
#   output/table_s1_ate_cate_estimates.{csv,tex}
# Depends on: original/aggregated_analysis_set.rds, helpers.R
# Description: Average and conditional average treatment effects on 2020 turnout
#   under three inverse probability weighted specifications. Figure 1 and
#   Supplementary Table 1 are the same 57 fits at two precisions, so they are
#   written from one set of model objects rather than fitted twice.

source(here::here("maintained", "helpers.R"))

main_analysis_set <- load_main_analysis_set()

formula_unadjusted <- formula("voted_in_2020 ~ condition")
formula_pap <- formula("voted_in_2020 ~ condition + num_times_voted + tss_100 + pts_100")
formula_full <- formula(paste(
  "voted_in_2020 ~ condition + pts_100 + tss_100 +", vote_history_terms,
  "+ strata + party_dem + party_rep + party_unknown"
))

# Grouping by party and then adjusting for party indicators would be collinear,
# so the full adjustment set drops them in the partisanship panel. The deposit
# does the same, in its formula_3_party.
formula_full_party <- formula(paste(
  "voted_in_2020 ~ condition + pts_100 + tss_100 +", vote_history_terms, "+ strata"
))

specifications <- list(
  Unadjusted = list(main = formula_unadjusted, party = formula_unadjusted),
  `PAP adjustment set` = list(main = formula_pap, party = formula_pap),
  `Full adjustment set` = list(main = formula_full, party = formula_full_party)
)

subgroups <- c(
  Gender = "gender",
  Race = "race",
  Age = "agecat",
  Margin = "close_margins_2016",
  `Trump support` = "tss_bucket",
  Partisanship = "party"
)

fit_within_groups <- function(group_var, fml) {
  main_analysis_set |>
    group_by(across(all_of(group_var))) |>
    reframe(tidy(lm_robust(formula = fml, weights = ipw, data = pick(everything()))))
}

estimates_for <- function(spec) {
  cates <- imap(subgroups, function(group_var, covariate) {
    fml <- if (covariate == "Partisanship") spec$party else spec$main
    fit_within_groups(group_var, fml)
  })
  bind_rows(
    c(list(ATE = tidy(lm_robust(formula = spec$main, weights = ipw,
                                data = main_analysis_set))),
      cates),
    .id = "covariate"
  )
}

estimates_df <-
  map(specifications, estimates_for) |>
  list_rbind(names_to = "estimator") |>
  filter(term == "conditiontreat") |>
  mutate(
    covariate_value = coalesce(gender, race, agecat, close_margins_2016,
                               tss_bucket, party),
    covariate_value = replace_na(covariate_value, "ATE"),
    covariate = factor(
      covariate,
      levels = c("ATE", "Age", "Gender", "Race", "Margin", "Partisanship",
                 "Trump support")
    ),
    estimator = factor(estimator, levels = estimator_levels),
    entry = str_glue("{sprintf('%.3f', estimate)} ({sprintf('%.3f', std.error)})")
  ) |>
  arrange(covariate, covariate_value, estimator, .locale = "en")

g <-
  ggplot(estimates_df, aes(estimate, covariate_value)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dotted", alpha = 0.5) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_text(
    aes(label = entry, x = estimate + sign(estimate) * 0.004),
    size = 2,
    nudge_y = 0.25,
    color = gray(0.45)
  ) +
  facet_grid(
    rows = vars(covariate),
    cols = vars(estimator),
    scales = "free_y",
    space = "free"
  ) +
  coord_cartesian(xlim = c(-0.02, 0.02)) +
  scale_x_continuous(breaks = c(-0.01, 0.01)) +
  theme_bw() +
  theme(axis.title.y = element_blank(), panel.grid.minor.x = element_blank()) +
  labs(x = "Average and conditional average treatment effect estimates")

ggsave(here::here("maintained", "output", "figure_1_ate_cate_estimates.pdf"),
       plot = g, width = 6.5, height = 6)
ggsave(here::here("maintained", "output", "figure_1_ate_cate_estimates.png"),
       plot = g, width = 6.5, height = 6, dpi = 300)

# The unrounded cells, so that the ground truth never reads a value back out of
# a formatted table and rounds it a second time.
estimates_df |>
  select(covariate, covariate_value, estimator, estimate, std.error, df,
         statistic, p.value, conf.low, conf.high, entry) |>
  write_csv(here::here("maintained", "output", "figure_1_ate_cate_estimates.csv"))

table_s1 <-
  estimates_df |>
  transmute(
    Covariate = covariate,
    Level = covariate_value,
    Adjustment = estimator,
    Estimate = sprintf("%.3f", estimate),
    SE = sprintf("%.3f", std.error),
    df = df,
    t = sprintf("%.3f", statistic),
    `p-value` = sprintf("%.3f", p.value),
    `95%CI lower` = sprintf("%.3f", conf.low),
    `95%CI upper` = sprintf("%.3f", conf.high)
  )

write_csv(table_s1, here::here("maintained", "output", "table_s1_ate_cate_estimates.csv"))

table_s1 |>
  kable(format = "latex", booktabs = TRUE, linesep = "", align = "lllrrrrrrr",
        caption = "Average and conditional average treatment effects.") |>
  kable_styling(latex_options = "striped", font_size = 8) |>
  write_lines(here::here("maintained", "output", "table_s1_ate_cate_estimates.tex"))
