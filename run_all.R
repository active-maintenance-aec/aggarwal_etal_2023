# aggarwal_etal_2023/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive,
# then the figures and tables, then the in-text quantities, then the deposited
# archive as a program, then the ground truth and the second instrument, then
# the archive check again. Every script is self-contained and can be run alone.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way.
source(here::here("download_original.R"))

# Main text figures and tables ----
source(here::here("maintained", "figure_1_ate_cate_estimates.R"))
source(here::here("maintained", "figure_2_heterogeneity_by_tss.R"))
source(here::here("maintained", "figure_3_turnout_by_tss_score.R"))
source(here::here("maintained", "figure_5_balance.R"))
source(here::here("maintained", "table_1_experimental_strata.R"))

# Supplementary figures ----
# figure_c3 reads the 433 MB Ad Library extract and is the slowest script here.
source(here::here("maintained", "figure_b2_acronym_spending.R"))
source(here::here("maintained", "figure_c3_facebook_spending.R"))

# In-text quantities ----
# text_descriptive_claims reads the figure outputs, so it runs after them.
source(here::here("maintained", "text_main_claims.R"))
source(here::here("maintained", "text_spending_claims.R"))
source(here::here("maintained", "text_descriptive_claims.R"))

# The deposited archive as a program ----
# Runs every deposited script in a scratch copy, as shipped and again stripped
# to data plus code, and records where each one stopped. Then reads out of the
# deposit every published quantity it can answer, which is what value_script in
# the ground truth is built from. Nothing is ever run inside original/.
source(here::here("ground_truth", "run_archive.R"))
source(here::here("ground_truth", "extract_archive_values.R"))

# Ground truth ----
# Rebuilds the comparison table from the outputs above, so it cannot go stale.
# The build also runs in_text_claims.R under capture.output for the coverage
# gate; the call below is the human-readable pass.
source(here::here("ground_truth", "build_ground_truth.R"))
source(here::here("maintained", "in_text_claims.R"))

# Deposited archive, again ----
# The gate inside download_original.R is a precondition: sourcing it first
# proves original/ was intact when the run began and says nothing about what the
# run did to it. Re-sourcing it here is what catches a script that damaged the
# deposit mid-run.
source(here::here("download_original.R"))
