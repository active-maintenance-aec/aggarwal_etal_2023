# aggarwal_etal_2023/maintained/table_1_experimental_strata.R
# Output: output/table_1_experimental_strata.{csv,tex},
#   output/table_1_experimental_strata_cells.csv
# Depends on: original/aggregated_analysis_set.rds, helpers.R
# Description: Group sizes, assignment probabilities and 2020 voting rates for
#   the 18 experimental strata, plus a total row. The published table rounds the
#   voting rates to one decimal and the assignment probabilities to three, so
#   the unrounded cells are written alongside the formatted table.

source(here::here("maintained", "helpers.R"))

main_analysis_set <- load_main_analysis_set()

strata_cells <-
  main_analysis_set |>
  group_by(strata) |>
  summarize(
    control_n = sum(condition == "control"),
    treatment_n = sum(condition == "treat"),
    p_treat = mean(treatment_prob),
    voting_rate_control = mean(voted_in_2020[condition == "control"]),
    voting_rate_treatment = mean(voted_in_2020[condition == "treat"]),
    .groups = "drop"
  ) |>
  separate_wider_delim(strata, delim = "_", names = c("gender", "race", "age")) |>
  mutate(across(c(gender, race, age), str_to_title))

# The total row is not the column total of the rows above it. The strata rows
# report unweighted means within a stratum, where assignment probability is
# constant; the total row weights by the inverse probability of assignment, and
# the assignment probability it reports is the realised treated fraction rather
# than the mean of treatment_prob.
total_cells <-
  main_analysis_set |>
  summarize(
    gender = "",
    race = "",
    age = "Total",
    control_n = sum(condition == "control"),
    treatment_n = sum(condition == "treat"),
    p_treat = mean(condition == "treat"),
    voting_rate_control = weighted.mean(voted_in_2020[condition == "control"],
                                        w = ipw[condition == "control"]),
    voting_rate_treatment = weighted.mean(voted_in_2020[condition == "treat"],
                                          w = ipw[condition == "treat"])
  )

# The row label is written out rather than reassembled by a reader. An empty
# gender cell in the total row comes back from read_csv as NA, and pasting the
# three label columns then yields NA rather than "Total".
table_1_cells <-
  bind_rows(strata_cells, total_cells) |>
  mutate(row_label = str_squish(str_c(gender, " ", race, " ", age))) |>
  relocate(row_label)

write_csv(table_1_cells,
          here::here("maintained", "output", "table_1_experimental_strata_cells.csv"))

table_1 <-
  table_1_cells |>
  transmute(
    Gender = gender,
    Race = race,
    `Age (years)` = age,
    Control = label_comma()(control_n),
    Treatment = label_comma()(treatment_n),
    Ptreat = sprintf("%.3f", p_treat),
    `2020 voting rate, control` = sprintf("%.1f", 100 * voting_rate_control),
    `2020 voting rate, treatment` = sprintf("%.1f", 100 * voting_rate_treatment)
  )

write_csv(table_1, here::here("maintained", "output", "table_1_experimental_strata.csv"))

table_1 |>
  kable(format = "latex", booktabs = TRUE, linesep = "", align = "lllrrrrr",
        caption = "Experimental strata.") |>
  kable_styling(latex_options = "striped", font_size = 9) |>
  write_lines(here::here("maintained", "output", "table_1_experimental_strata.tex"))
