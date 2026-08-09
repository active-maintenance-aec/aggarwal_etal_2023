# aggarwal_etal_2023/maintained/helpers.R
# Output: none
# Depends on: nothing
# Description: Packages and shared helpers sourced by every script in maintained/.

library(here)
library(tidyverse)
library(estimatr)
library(scales)
library(patchwork)
library(knitr)
library(kableExtra)
library(car)
library(lmtest)

here::i_am("maintained/helpers.R")

# The deposit ships one aggregated row per distinct covariate profile with a
# count, and every deposited analysis script expands it before estimating.
# Reading it is the first line of most scripts here, so it lives in one place.
#
# The deposited object is a grouped tibble, grouped by all 41 of its covariate
# columns, which is what a count() leaves behind. A bare summarize() over it
# therefore returns two million rows rather than one, so the grouping is dropped
# on load. Every group_by() downstream replaces it and is unaffected.
load_main_analysis_set <- function() {
  here::here("original", "aggregated_analysis_set.rds") |>
    read_rds() |>
    ungroup() |>
    uncount(weights = n)
}

# The Facebook Ad Library extract for Acronym's own advertisements. The
# deposited CSV carries no ad_type, ad_format or is_turnout column: those are
# built in figure_B2.R and the deposit's in_text_calculations.R uses them
# without building them, which is why the deposited script cannot produce the
# published spending figures. The classification is reproduced here from
# figure_B2.R so that both the appendix figure and the spending claims read one
# definition.
load_acronym_ads <- function() {
  acronym_domains <- c(
    "www.showuptovote.com",
    "action.fourisenough.org",
    "www.trumpcoronavirusplan.com",
    "trumpcoronavirusplan.com",
    "cheatsheetforthevotingbooth.com",
    "action.prjctsunshine.org",
    "",
    NA
  )
  turnout_phrases <- paste0(
    "(make a plan|ilana|vote biden to get it done|vote to ditch trump|",
    "use your power and vote|running out to vote|make your plan|",
    "cheat sheet for the voting)"
  )
  here::here("original", "ad_library_2020_acronym.csv") |>
    read_csv(col_types = cols(page_id = col_character())) |>
    mutate(
      external_domain = str_trim(str_remove_all(ad_creative_link_captions, ",")),
      external_domain = if_else(external_domain %in% acronym_domains,
                                NA_character_, external_domain),
      ad_type = case_when(
        is_trump & is_biden ~ "Both",
        is_trump ~ "Trump",
        is_biden ~ "Biden"
      ),
      month = as.Date(floor_date(ad_delivery_start_time, unit = "month")),
      spend_ub = if_else(spend_ub == -1, 1000000, spend_ub),
      ad_format = case_when(
        is_video_acronym ~ "Traditional\nvideo",
        !is.na(external_domain) ~ "Promoted\nnews",
        TRUE ~ "Other"
      ),
      is_turnout = (
        ad_creative_link_captions %in%
          c("www.showuptovote.com", "cheatsheetforthevotingbooth.com")
      ) | str_detect(tolower(ad_creative_bodies), turnout_phrases)
    )
}

# The three regression specifications, written once. The article names them in
# every figure and in both supplementary regression tables.
estimator_levels <- c("Unadjusted", "PAP adjustment set", "Full adjustment set")

vote_history_terms <- paste0("voted_in_", seq(2000, 2018, by = 2), collapse = " + ")

# The 22 pre-treatment covariates of Figure 5 and Supplementary Table 3, and the
# labels the article prints for them. Used by the balance figure and again by
# the text script that counts significant imbalances.
balance_covariates <- c(
  "party_dem", "party_rep", "party_unknown", "party_other",
  "tss_30_40", "tss_40_50", "tss_50_60", "tss_60_70",
  "tss_100", "pts_100",
  "ideology_score_100", "partisan_score_100",
  paste0("voted_in_", seq(2000, 2018, by = 2))
)

balance_covariate_labels <- c(
  "Party: Democrat", "Party: Republican", "Party: Unknown", "Party: Other",
  "TSS: 30-40", "TSS: 40-50", "TSS: 50-60", "TSS: 60-70",
  "Trump support score / 100", "Turnout score / 100",
  "Ideology score / 100", "Partisanship score / 100",
  paste("Voted in", seq(2000, 2018, by = 2))
)

# Blank a figure PDF's embedded timestamps ----
# R's pdf() device stamps /CreationDate and /ModDate with the wall clock, so an
# otherwise deterministic pipeline writes a different file on every run. The epoch
# string is the same width as what it replaces, which keeps the cross-reference byte
# offsets valid, and a file with no timestamp is left alone.
blank_pdf_timestamps <- function(path) {
  epoch <- charToRaw("D:19700101000000")
  raw_pdf <- readBin(path, "raw", file.size(path))
  hits <- grepRaw("D:[0-9]{14}", raw_pdf, all = TRUE)
  if (length(hits) == 0) return(invisible(path))
  for (h in hits) raw_pdf[h:(h + length(epoch) - 1L)] <- epoch
  writeBin(raw_pdf, path)
  invisible(path)
}
