# aggarwal_etal_2023/ground_truth/extract_archive_values.R
# Output: ground_truth/archive_values.csv
# Depends on: original/ (fetched by download_original.R)
# Description: Read out of the deposited code every published quantity it can
#   answer, so that value_script in the ground truth is generated rather than
#   typed. Each deposited script is sourced in its own environment inside a
#   scratch copy of the archive and the objects it leaves behind are read; four
#   of them stop on an error part way through, and the objects built before the
#   error are still what the deposit computed, so they are read too.
#
#   print.eval is on because source() prints nothing without it, and the
#   deposit's equivalence test is printed rather than assigned.

library(tidyverse)
library(here)

here::i_am("ground_truth/extract_archive_values.R")

archive_run_dir <- Sys.getenv("ARCHIVE_RUN_DIR",
                              unset = file.path(tempdir(), "aggarwal_etal_2023_archive"))
scratch <- file.path(archive_run_dir, "extract")
original_dir <- here::here("original")
stopifnot(dir.exists(original_dir))

unlink(scratch, recursive = TRUE)
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
file.copy(list.files(original_dir, full.names = TRUE), scratch)

run_deposited <- function(script) {
  env <- new.env(parent = globalenv())
  old <- setwd(scratch)
  on.exit(setwd(old), add = TRUE)
  # A printed tibble wraps at the console width, which splits a row across
  # lines and makes the last token on the row line something other than the
  # last column. Widening it is what makes the stdout readable by position.
  old_width <- options(width = 300)
  on.exit(options(old_width), add = TRUE)
  printed <- capture.output(
    outcome <- try(source(script, local = env, echo = FALSE, print.eval = TRUE),
                   silent = TRUE)
  )
  list(env = env, printed = printed, failed = inherits(outcome, "try-error"))
}

figure_1_run <- run_deposited("figure_1.R")
figure_2_run <- run_deposited("figure_2.R")
figure_5_run <- run_deposited("figure_5.R")
table_1_run <- run_deposited("table_1.R")
in_text_run <- run_deposited("in_text_calculations.R")
figure_b2_run <- run_deposited("figure_B2.R")
figure_c3_run <- run_deposited("figure_C3.R")

# The deposit's Figure 1 and Supplementary Table 1 come off one object, and so
# do Figure 2 and Supplementary Table 2. The keys are built to match the
# published transcription rather than the deposit's own column names.
estimator_from <- function(x) as.character(x)

figure_1_values <-
  figure_1_run$env$gg_df |>
  as_tibble() |>
  transmute(
    row_key = str_glue("{covariate} | {covariate_value} | {estimator_from(estimator)}"),
    estimate, se = std.error, df, t = statistic, p_value = p.value,
    ci_lower = conf.low, ci_upper = conf.high
  ) |>
  pivot_longer(-row_key, names_to = "quantity", values_to = "value_script") |>
  mutate(source = "figure_1.R")

figure_2_values <-
  figure_2_run$env$gg_df |>
  as_tibble() |>
  transmute(
    row_key = str_glue("{str_replace_all(inquiry, '\\\\n', ' ')} | {outcome} | {estimator_from(estimator)}"),
    estimate, se = std.error, df, t = statistic, p_value = p.value,
    ci_lower = conf.low, ci_upper = conf.high
  ) |>
  pivot_longer(-row_key, names_to = "quantity", values_to = "value_script") |>
  mutate(source = "figure_2.R")

figure_5_values <-
  figure_5_run$env$balance_df |>
  as_tibble() |>
  transmute(
    row_key = as.character(outcome),
    estimate, se = std.error, df, t = statistic, p_value = p.value,
    p_value_bh = adjusted_p, ci_lower = conf.low, ci_upper = conf.high
  ) |>
  pivot_longer(-row_key, names_to = "quantity", values_to = "value_script") |>
  mutate(source = "figure_5.R")

# table_1.R stops at its first print.xtable call, which writes into a directory
# the deposit does not ship. Both summary objects exist by then. They are
# already formatted strings, which is why they are kept as strings here.
table_1_values <-
  bind_rows(
    table_1_run$env$summary_table |> as_tibble(),
    table_1_run$env$summary_table_overall |> as_tibble()
  ) |>
  transmute(
    row_key = str_squish(str_c(Gender, Race, Age, sep = " ")),
    control_n = Control,
    treatment_n = Treatment,
    p_treat = `Assignment Probability`,
    voting_rate_control = voting_rate_2020_control,
    voting_rate_treatment = voting_rate_2020_treatment
  ) |>
  pivot_longer(-row_key, names_to = "quantity", values_to = "value_script_chr") |>
  mutate(
    value_script = parse_number(value_script_chr),
    source = "table_1.R"
  ) |>
  select(-value_script_chr)

# in_text_calculations.R stops at its waldtest call, which needs a package the
# deposit comments out of its own library block. Everything above that line ran.
in_text_env <- in_text_run$env
treatment_row <- function(x) x |> filter(term == "conditiontreat")

# The deposit prints its equivalence test rather than assigning it, so the two
# one-sided p-values exist only in the script's stdout. source() prints nothing
# without print.eval, which is why it is set above. The printed value is then
# checked against the same expression evaluated on the deposit's own objects.
tost_printed <-
  in_text_run$printed |>
  str_subset("conditiontreat") |>
  tail(1) |>
  str_squish() |>
  str_split(" ") |>
  pluck(1)

tost_from_objects <- pnorm(
  treatment_row(in_text_env$fit_ATE)$estimate,
  mean = -1 * in_text_env$eq_bound,
  sd = treatment_row(in_text_env$fit_ATE)$std.error,
  lower.tail = FALSE
)
stopifnot(
  length(tost_printed) >= 11,
  abs(as.numeric(tost_printed[length(tost_printed) - 1]) - tost_from_objects) < 1e-6
)

scalar_values <- tibble(
  row_key = c(
    "ate_pap_estimate", "ate_pap_se", "ate_pap_t", "ate_pap_df", "ate_pap_p",
    "ate_pap_ci_low", "ate_pap_ci_high",
    "cate_biden_estimate", "cate_biden_se",
    "cate_trump_estimate", "cate_trump_se",
    "dic_estimate", "dic_se", "dic_t", "dic_df", "dic_p",
    "dic_ci_low", "dic_ci_high",
    "cate_republican_estimate", "cate_republican_se",
    "cate_democrat_estimate", "cate_democrat_se",
    "equality_f", "equality_num_df", "equality_den_df",
    "tost_p_lower"
  ),
  quantity = "value",
  value_script = c(
    treatment_row(in_text_env$fit_ATE)$estimate,
    treatment_row(in_text_env$fit_ATE)$std.error,
    treatment_row(in_text_env$fit_ATE)$statistic,
    treatment_row(in_text_env$fit_ATE)$df,
    treatment_row(in_text_env$fit_ATE)$p.value,
    treatment_row(in_text_env$fit_ATE)$conf.low,
    treatment_row(in_text_env$fit_ATE)$conf.high,
    treatment_row(in_text_env$fit_biden)$estimate,
    treatment_row(in_text_env$fit_biden)$std.error,
    treatment_row(in_text_env$fit_trump)$estimate,
    treatment_row(in_text_env$fit_trump)$std.error,
    in_text_env$difference_in_cates |>
      filter(term == "conditiontreat:tss_bucket60 to 70") |> pull(estimate),
    in_text_env$difference_in_cates |>
      filter(term == "conditiontreat:tss_bucket60 to 70") |> pull(std.error),
    in_text_env$difference_in_cates |>
      filter(term == "conditiontreat:tss_bucket60 to 70") |> pull(statistic),
    in_text_env$difference_in_cates |>
      filter(term == "conditiontreat:tss_bucket60 to 70") |> pull(df),
    in_text_env$difference_in_cates |>
      filter(term == "conditiontreat:tss_bucket60 to 70") |> pull(p.value),
    in_text_env$difference_in_cates |>
      filter(term == "conditiontreat:tss_bucket60 to 70") |> pull(conf.low),
    in_text_env$difference_in_cates |>
      filter(term == "conditiontreat:tss_bucket60 to 70") |> pull(conf.high),
    treatment_row(in_text_env$fit_Republicans)$estimate,
    treatment_row(in_text_env$fit_Republicans)$std.error,
    treatment_row(in_text_env$fit_Democrats)$estimate,
    treatment_row(in_text_env$fit_Democrats)$std.error,
    (in_text_env$out$SSPH[1, 1] / in_text_env$out$df) /
      (in_text_env$out$SSPE[1, 1] / in_text_env$out$df.residual),
    in_text_env$out$df * in_text_env$out$r,
    in_text_env$out$df.residual,
    as.numeric(tost_printed[length(tost_printed) - 1])
  )
) |>
  mutate(source = "in_text_calculations.R")

# The deposit's spending block is inside in_text_calculations.R, below the line
# that stops it, and it reads three columns the deposited CSV does not carry, so
# it could not run even if the script reached it. figure_B2.R builds those
# columns, so the spending figures are read from there.
acronym_ads <- figure_b2_run$env$df_acronym |> as_tibble()
persuasion_ads <- acronym_ads |> filter(!is_turnout)

spending_values <-
  bind_rows(
    persuasion_ads |>
      group_by(row_key = str_c("spend_lb_", str_to_lower(str_replace_all(ad_type, "\\n", "_")))) |>
      summarize(value_script = sum(spend_lb), .groups = "drop"),
    persuasion_ads |>
      group_by(row_key = str_c("spend_lb_", str_to_lower(str_replace_all(ad_format, "[\\n ]", "_")))) |>
      summarize(value_script = sum(spend_lb), .groups = "drop"),
    tibble(
      row_key = c("spend_lb_persuasion_total", "spend_ub_persuasion_total",
                  "spend_lb_all_acronym_ads", "n_acronym_ads", "n_persuasion_ads"),
      value_script = c(sum(persuasion_ads$spend_lb), sum(persuasion_ads$spend_ub),
                       sum(acronym_ads$spend_lb), nrow(acronym_ads),
                       nrow(persuasion_ads))
    )
  ) |>
  mutate(quantity = "value", source = "figure_B2.R")

ad_library_values <- tibble(
  row_key = c("ad_library_total_ads", "ad_library_total_spend_lb",
              "ad_library_acronym_spend_lb"),
  quantity = "value",
  value_script = c(
    nrow(figure_c3_run$env$df),
    sum(figure_c3_run$env$df$spend_lb),
    sum(figure_c3_run$env$df$spend_lb[figure_c3_run$env$df$is_acronym])
  ),
  source = "figure_C3.R"
)

archive_values <-
  bind_rows(
    figure_1_values |> mutate(float = "figure_1"),
    figure_1_values |> mutate(float = "supplementary_table_1"),
    figure_2_values |> mutate(float = "figure_2"),
    figure_2_values |> mutate(float = "supplementary_table_2"),
    figure_5_values |> mutate(float = "supplementary_table_3"),
    table_1_values |> mutate(float = "table_1"),
    scalar_values |> mutate(float = "text"),
    spending_values |> mutate(float = "text"),
    ad_library_values |> mutate(float = "text")
  ) |>
  select(float, row_key, quantity, value_script, source) |>
  arrange(float, row_key, quantity, .locale = "en")

stopifnot(!any(duplicated(archive_values[c("float", "row_key", "quantity")])))
write_csv(archive_values, here::here("ground_truth", "archive_values.csv"))

print(archive_values |> count(float, source))
print(str_glue("Scripts that stopped part way: ",
               "{paste(c('figure_1.R', 'figure_2.R', 'figure_5.R', 'table_1.R', ",
               "'in_text_calculations.R', 'figure_B2.R', 'figure_C3.R')",
               "[c(figure_1_run$failed, figure_2_run$failed, figure_5_run$failed, ",
               "table_1_run$failed, in_text_run$failed, figure_b2_run$failed, ",
               "figure_c3_run$failed)], collapse = ', ')}"))
