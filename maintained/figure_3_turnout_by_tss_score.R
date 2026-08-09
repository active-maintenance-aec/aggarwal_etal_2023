# aggarwal_etal_2023/maintained/figure_3_turnout_by_tss_score.R
# Output: output/figure_3_turnout_by_tss_score.{pdf,png},
#   output/figure_3_binned_means.csv, output/figure_3_linear_predictions.csv
# Depends on: original/aggregated_analysis_set.rds, helpers.R
# Description: 2020 turnout rates by one-point bins of Trump support score and
#   condition, for early, in-person and any voting, with the unadjusted linear
#   predictions overlaid. The figure prints no numbers on its face, so the two
#   CSVs of what it plots are the only thing a reader can diff against it.

source(here::here("maintained", "helpers.R"))

main_analysis_set <-
  load_main_analysis_set() |>
  mutate(condition = factor(condition, levels = c("control", "treat")))

outcome_labels <- c("Voted early", "Voted in person", "Voted")
outcome_levels <- c("voted_early_2020", "voted_in_person_2020", "voted_in_2020")

fit_early <- lm_robust(voted_early_2020 ~ condition * tss, weights = ipw,
                       data = main_analysis_set)
fit_in_person <- lm_robust(voted_in_person_2020 ~ condition * tss, weights = ipw,
                           data = main_analysis_set)
fit_any <- lm_robust(voted_in_2020 ~ condition * tss, weights = ipw,
                     data = main_analysis_set)

newdata <- expand_grid(condition = c("control", "treat"), tss = seq(30, 70))

predict_over_grid <- function(fit) {
  fitted <- predict(fit, newdata = newdata, se.fit = TRUE,
                    interval = "confidence")$fit
  bind_cols(newdata, as_tibble(fitted))
}

preds_df <-
  list(
    voted_early_2020 = predict_over_grid(fit_early),
    voted_in_person_2020 = predict_over_grid(fit_in_person),
    voted_in_2020 = predict_over_grid(fit_any)
  ) |>
  list_rbind(names_to = "outcome") |>
  mutate(outcome_label = factor(outcome, levels = outcome_levels,
                                labels = outcome_labels))

binned_means <-
  main_analysis_set |>
  group_by(condition, tss_round) |>
  reframe(tidy(lm_robust(
    cbind(voted_in_person_2020, voted_early_2020, voted_in_2020) ~ 1,
    weights = ipw, data = pick(everything())
  ))) |>
  mutate(outcome_label = factor(outcome, levels = outcome_levels,
                                labels = outcome_labels))

# The published figure clips the upper confidence bound of the Voted panel at
# 0.60, which is the top of that panel's range. The clipped column is what the
# plot draws; conf.high is what the estimate actually is.
gg_df <- binned_means |> mutate(conf.high.clipped = pmin(conf.high, 0.60))

label_df <-
  tibble(
    label = c("Control group", "Treatment group"),
    tss_round = c(60, 60),
    estimate = c(0.31, 0.24),
    condition = c("control", "treat"),
    outcome_label = factor("Voted early", levels = outcome_labels)
  )

# Each panel covers a 15 percentage point range, on ranges chosen per panel.
# geom_blank fixes them without clipping the data.
blank_df <-
  tibble(
    outcome_label = factor(rep(outcome_labels, each = 2), levels = outcome_labels),
    estimate = c(0.35, 0.20, 0.35, 0.20, 0.60, 0.45),
    tss_round = 50,
    condition = "treat"
  )

# "The vertical scales of all three facets cover a 15 percentage point range,
# but the ranges differ across facets." Asserted here, where the ranges are set.
stopifnot(
  blank_df |>
    group_by(outcome_label) |>
    summarize(span = diff(range(estimate)), .groups = "drop") |>
    pull(span) |>
    (\(x) all(abs(x - 0.15) < 1e-9))()
)

g <-
  ggplot(gg_df, aes(tss_round, estimate, shape = condition)) +
  geom_blank(data = blank_df) +
  geom_linerange(
    data = filter(gg_df, condition == "control"),
    aes(ymin = conf.low, ymax = conf.high.clipped),
    color = gray(0.9)
  ) +
  geom_point(data = filter(gg_df, condition == "control"), color = gray(0.7)) +
  geom_line(
    data = filter(preds_df, condition == "control"),
    aes(y = fit, x = tss), color = gray(0.7)
  ) +
  geom_ribbon(
    data = filter(preds_df, condition == "control"),
    aes(y = fit, ymin = lwr, ymax = upr, x = tss),
    color = gray(0.7), fill = gray(0.7), alpha = 0.5
  ) +
  geom_linerange(
    data = filter(gg_df, condition == "treat"),
    aes(ymin = conf.low, ymax = conf.high.clipped),
    color = gray(0.5)
  ) +
  geom_point(data = filter(gg_df, condition == "treat"), color = gray(0.2)) +
  geom_line(
    data = filter(preds_df, condition == "treat"),
    aes(y = fit, x = tss), color = gray(0.2)
  ) +
  geom_ribbon(
    data = filter(preds_df, condition == "treat"),
    aes(y = fit, ymin = lwr, ymax = upr, x = tss),
    color = gray(0.2), fill = gray(0.1), alpha = 0.5
  ) +
  geom_label(data = filter(label_df, condition == "treat"),
             aes(label = label), color = "black") +
  geom_label(data = filter(label_df, condition == "control"),
             aes(label = label), color = gray(0.6)) +
  facet_wrap(~outcome_label, scales = "free_y") +
  theme_bw() +
  labs(y = "Fraction voting", x = "Trump support score") +
  scale_shape_manual(name = "Condition", labels = c("Control", "Treatment"),
                     values = c(19, 17)) +
  theme(legend.position = "none")

ggsave(here::here("maintained", "output", "figure_3_turnout_by_tss_score.pdf"),
       plot = g, width = 8.5, height = 4.5)
ggsave(here::here("maintained", "output", "figure_3_turnout_by_tss_score.png"),
       plot = g, width = 8.5, height = 4.5, dpi = 300)

gg_df |>
  select(outcome, outcome_label, condition, tss_round, estimate, std.error,
         conf.low, conf.high, conf.high.clipped, df) |>
  arrange(outcome, condition, tss_round, .locale = "en") |>
  write_csv(here::here("maintained", "output", "figure_3_binned_means.csv"))

preds_df |>
  select(outcome, outcome_label, condition, tss, fit, lwr, upr) |>
  arrange(outcome, condition, tss, .locale = "en") |>
  write_csv(here::here("maintained", "output", "figure_3_linear_predictions.csv"))
