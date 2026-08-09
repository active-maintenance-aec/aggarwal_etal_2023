# aggarwal_etal_2023/maintained/figure_2_heterogeneity_by_tss.R
# Output: output/figure_2_heterogeneity_by_tss.{pdf,png,csv},
#   output/table_s2_heterogeneity_by_tss.{csv,tex}
# Depends on: original/aggregated_analysis_set.rds, helpers.R
# Description: Treatment effect heterogeneity by Trump support score, as a
#   difference in conditional average treatment effects between the 60 to 70 and
#   30 to 40 buckets and as a treatment by score interaction, for three outcomes
#   under three specifications. Figure 2 and Supplementary Table 2 are the same
#   18 estimates at two precisions.

source(here::here("maintained", "helpers.R"))

main_analysis_set <-
  load_main_analysis_set() |>
  mutate(condition = factor(condition, levels = c("control", "treat")))

outcomes <- "cbind(voted_in_2020, voted_in_person_2020, voted_early_2020)"

interaction_formulas <- list(
  Unadjusted = formula(paste(outcomes, "~ condition*tss_100")),
  `PAP adjustment set` = formula(paste(
    outcomes, "~ condition*tss_100 + num_times_voted + pts_100"
  )),
  `Full adjustment set` = formula(paste(
    outcomes, "~ condition*tss_100 + pts_100 +", vote_history_terms,
    "+ strata + party_dem + party_rep + party_unknown"
  ))
)

difference_formulas <- list(
  Unadjusted = formula(paste(outcomes, "~ condition*tss_bucket")),
  `PAP adjustment set` = formula(paste(
    outcomes, "~ condition*tss_bucket + num_times_voted + tss_100 + pts_100"
  )),
  `Full adjustment set` = formula(paste(
    outcomes, "~ condition*tss_bucket + pts_100 + tss_100 +", vote_history_terms,
    "+ strata + party_dem + party_rep + party_unknown"
  ))
)

extreme_buckets <- filter(main_analysis_set, tss_bucket %in% c("30 to 40", "60 to 70"))

interaction_estimates <-
  map(interaction_formulas,
      function(fml) tidy(lm_robust(formula = fml, weights = ipw,
                                   data = main_analysis_set))) |>
  list_rbind(names_to = "estimator") |>
  filter(term == "conditiontreat:tss_100")

difference_estimates <-
  map(difference_formulas,
      function(fml) tidy(lm_robust(formula = fml, weights = ipw,
                                   data = extreme_buckets))) |>
  list_rbind(names_to = "estimator") |>
  filter(term == "conditiontreat:tss_bucket60 to 70")

gg_df <-
  bind_rows(
    `Treatment*TSS interaction term\nfrom linear model` = interaction_estimates,
    `Difference-in-CATEs\nTSS 60-70 versus TSS 30-40` = difference_estimates,
    .id = "inquiry"
  ) |>
  mutate(
    entry = str_glue("{sprintf('%.3f', estimate)} ({sprintf('%.3f', std.error)})"),
    estimator = factor(estimator, levels = estimator_levels),
    outcome = factor(
      outcome,
      levels = c("voted_early_2020", "voted_in_person_2020", "voted_in_2020"),
      labels = c("Voted early in 2020", "Voted in person in 2020", "Voted in 2020")
    )
  ) |>
  arrange(inquiry, outcome, estimator, .locale = "en")

g <-
  ggplot(gg_df, aes(estimate, outcome)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dotted", alpha = 0.5) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_text(
    aes(label = entry, x = estimate + sign(estimate) * 0.004),
    size = 2,
    nudge_y = 0.25,
    color = gray(0.45)
  ) +
  facet_grid(inquiry ~ estimator) +
  coord_cartesian(xlim = c(-0.05, 0.05)) +
  scale_x_continuous(breaks = c(-0.02, 0.02)) +
  theme_bw() +
  theme(axis.title.y = element_blank(), panel.grid.minor.x = element_blank()) +
  labs(x = "Treatment effect heterogeneity estimates by Trump support")

ggsave(here::here("maintained", "output", "figure_2_heterogeneity_by_tss.pdf"),
       plot = g, width = 6.5, height = 4.5)
ggsave(here::here("maintained", "output", "figure_2_heterogeneity_by_tss.png"),
       plot = g, width = 6.5, height = 4.5, dpi = 300)

gg_df |>
  mutate(inquiry = str_replace_all(inquiry, "\\n", " ")) |>
  select(inquiry, outcome, estimator, estimate, std.error, df, statistic,
         p.value, conf.low, conf.high, entry) |>
  write_csv(here::here("maintained", "output", "figure_2_heterogeneity_by_tss.csv"))

table_s2 <-
  gg_df |>
  transmute(
    Target = str_replace_all(inquiry, "\\n", " "),
    Outcome = outcome,
    Adjustment = estimator,
    Estimate = sprintf("%.3f", estimate),
    SE = sprintf("%.3f", std.error),
    df = df,
    t = sprintf("%.3f", statistic),
    `p-value` = sprintf("%.3f", p.value),
    `95%CI lower` = sprintf("%.3f", conf.low),
    `95%CI upper` = sprintf("%.3f", conf.high)
  )

write_csv(table_s2,
          here::here("maintained", "output", "table_s2_heterogeneity_by_tss.csv"))

table_s2 |>
  kable(format = "latex", booktabs = TRUE, linesep = "", align = "lllrrrrrrr",
        caption = "The heterogeneous effects of treatment by Trump support.") |>
  kable_styling(latex_options = "striped", font_size = 8) |>
  write_lines(here::here("maintained", "output", "table_s2_heterogeneity_by_tss.tex"))
