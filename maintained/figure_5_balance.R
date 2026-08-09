# aggarwal_etal_2023/maintained/figure_5_balance.R
# Output: output/figure_5_balance.{pdf,png,csv}, output/table_s3_balance.{csv,tex}
# Depends on: original/aggregated_analysis_set.rds, helpers.R
# Description: Differences in mean pre-treatment covariate values by condition,
#   for the 22 covariates the article reports. Figure 5 plots the estimates and
#   Supplementary Table 3 prints them with a Benjamini-Hochberg adjusted p-value.

source(here::here("maintained", "helpers.R"))

main_analysis_set <- load_main_analysis_set()

balance_fit <-
  lm_robust(
    formula = formula(paste0(
      "cbind(", paste(balance_covariates, collapse = ", "), ") ~ condition"
    )),
    weights = ipw,
    data = main_analysis_set
  )

balance_df <-
  tidy(balance_fit) |>
  filter(term == "conditiontreat") |>
  mutate(
    outcome = factor(outcome, levels = balance_covariates,
                     labels = balance_covariate_labels),
    adjusted_p = p.adjust(p.value, method = "BH")
  )

# The published figure prints five axis breaks. Asserted here, where they are set.
axis_breaks <- seq(-0.01, 0.01, 0.005)
stopifnot(length(axis_breaks) == 5)

g <-
  ggplot(balance_df, aes(estimate, outcome)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dotted", alpha = 0.5) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_x_continuous(breaks = axis_breaks) +
  coord_cartesian(xlim = c(-0.01, 0.01)) +
  theme_bw() +
  theme(axis.title.y = element_blank(), panel.grid.minor.x = element_blank()) +
  labs(x = "Differences in mean pre-treatment covariate values")

ggsave(here::here("maintained", "output", "figure_5_balance.pdf"),
       plot = g, width = 6.5, height = 3.5)
ggsave(here::here("maintained", "output", "figure_5_balance.png"),
       plot = g, width = 6.5, height = 3.5, dpi = 300)

balance_df |>
  select(outcome, estimate, std.error, df, statistic, p.value, adjusted_p,
         conf.low, conf.high) |>
  write_csv(here::here("maintained", "output", "figure_5_balance.csv"))

table_s3 <-
  balance_df |>
  transmute(
    Covariate = outcome,
    Estimate = sprintf("%.3f", estimate),
    SE = sprintf("%.3f", std.error),
    df = df,
    t = sprintf("%.3f", statistic),
    `p-value` = sprintf("%.3f", p.value),
    `p-value (BH correction)` = sprintf("%.3f", adjusted_p),
    `95%CI lower` = sprintf("%.3f", conf.low),
    `95%CI upper` = sprintf("%.3f", conf.high)
  )

write_csv(table_s3, here::here("maintained", "output", "table_s3_balance.csv"))

table_s3 |>
  kable(format = "latex", booktabs = TRUE, linesep = "", align = "lrrrrrrrr",
        caption = "Balance estimates.") |>
  kable_styling(latex_options = "striped", font_size = 8) |>
  write_lines(here::here("maintained", "output", "table_s3_balance.tex"))
