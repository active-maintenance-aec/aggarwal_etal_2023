# aggarwal_etal_2023/maintained/text_spending_claims.R
# Output: output/text_spending_claims.csv
# Depends on: original/ad_library_2020_acronym.csv, helpers.R
# Description: The advertising spending figures the Methods section and
#   Supplementary Information section C state. The deposit's own
#   in_text_calculations.R computes these from three columns the deposited CSV
#   does not carry, so it cannot run; the classification comes from
#   figure_B2.R, which does build them, and lives in helpers.R.

source(here::here("maintained", "helpers.R"))

acronym_ads <- load_acronym_ads()
persuasion_ads <- filter(acronym_ads, !is_turnout)

by_keyword <-
  persuasion_ads |>
  group_by(ad_type) |>
  summarize(spend_lb = sum(spend_lb), spend_ub = sum(spend_ub), .groups = "drop")

by_format <-
  persuasion_ads |>
  group_by(ad_format) |>
  summarize(spend_lb = sum(spend_lb), spend_ub = sum(spend_ub), .groups = "drop")

pick_group <- function(tbl, column, level, quantity) {
  row <- tbl |> filter(.data[[column]] == level)
  stopifnot(nrow(row) == 1)
  row[[quantity]]
}

claims <- tibble(
  quantity = c(
    "n_acronym_ads", "n_persuasion_ads",
    "spend_lb_biden", "spend_lb_trump", "spend_lb_both",
    "spend_lb_promoted_news", "spend_lb_video", "spend_lb_other",
    "spend_lb_persuasion_total", "spend_ub_persuasion_total",
    "spend_lb_all_acronym_ads"
  ),
  value = c(
    nrow(acronym_ads),
    nrow(persuasion_ads),
    pick_group(by_keyword, "ad_type", "Biden", "spend_lb"),
    pick_group(by_keyword, "ad_type", "Trump", "spend_lb"),
    pick_group(by_keyword, "ad_type", "Both", "spend_lb"),
    pick_group(by_format, "ad_format", "Promoted\nnews", "spend_lb"),
    pick_group(by_format, "ad_format", "Traditional\nvideo", "spend_lb"),
    pick_group(by_format, "ad_format", "Other", "spend_lb"),
    sum(persuasion_ads$spend_lb),
    sum(persuasion_ads$spend_ub),
    sum(acronym_ads$spend_lb)
  )
)

write_csv(claims, here::here("maintained", "output", "text_spending_claims.csv"))

print(claims, n = Inf)
