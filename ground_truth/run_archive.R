# aggarwal_etal_2023/ground_truth/run_archive.R
# Output: ground_truth/archive_run_status.csv, ground_truth/archive_overwrites.csv,
#   ground_truth/archive_write_calls.csv
# Depends on: original/ (fetched by download_original.R)
# Description: Run every deposited script in a scratch copy of the archive and
#   record where each one stopped. The deposit is run twice, once exactly as
#   shipped and once stripped to data plus code, so that a script passing only
#   because the deposit ships the object it reads is visible as a failure. The
#   scratch copy also answers a question the checksum gate cannot: which
#   deposited files the deposit's own code writes over when it is run in place.
#   Nothing is ever run inside original/.

library(tidyverse)
library(here)

here::i_am("ground_truth/run_archive.R")

# Where the deposit is copied to. An environment variable so the script is
# usable by anyone with the repo.
archive_run_dir <- Sys.getenv("ARCHIVE_RUN_DIR",
                              unset = file.path(tempdir(), "aggarwal_etal_2023_archive"))
stopifnot(nzchar(archive_run_dir))

original_dir <- here::here("original")
stopifnot(dir.exists(original_dir))

manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

# The eleven deposited analysis scripts, in the order README.txt introduces
# them: the seven main-analysis scripts, then the four appendix ones.
analysis_scripts <- c(
  "helper_file.R", "in_text_calculations.R", "figure_1.R", "figure_2.R",
  "figure_3.R", "figure_5.R", "table_1.R",
  "figure_B1.R", "figure_B2.R", "figure_C3.R", "figure_C4.R"
)
stopifnot(all(analysis_scripts %in% manifest$file))

# What the deposit writes ----
# Read before running: an archive that writes nothing cannot produce an artifact
# anyone could diff, which decides whether a comparison against the deposit is
# possible at all. Comment lines are stripped first, since a commented-out write
# is a claim about the published artifact and not a write.
write_calls <-
  tibble(script = analysis_scripts) |>
  mutate(line = map(script, function(s) {
    lines <- read_lines(file.path(original_dir, s))
    tibble(line_number = seq_along(lines), text = str_squish(lines))
  })) |>
  unnest(line) |>
  filter(!str_starts(text, "#")) |>
  filter(str_detect(text, "ggsave\\(|write_csv\\(|write_rds\\(|saveRDS\\(|sink\\(|file *= *\"")) |>
  mutate(writes_to = str_extract(text, '(?<=")[^"]*\\.(pdf|png|tex|csv|rds|RData)(?=")')) |>
  filter(!is.na(writes_to)) |>
  # read_rds(file = ) matches the file = "..." pattern and is a read.
  filter(!str_detect(text, "read_rds|read_csv|read\\.csv|readRDS")) |>
  select(script, line_number, writes_to, text)

write_csv(write_calls, here::here("ground_truth", "archive_write_calls.csv"))

# Is the stripped pass distinguishable from the as-shipped pass? It is only if
# some deposited file is itself written by a deposited script. Asserted rather
# than assumed: "the deposit ships no derived objects" and "every script failed
# before reaching a write" are the same measurement and opposite findings.
derived_members <- intersect(write_calls$writes_to, manifest$file)

scrub <- function(x) {
  x |>
    str_replace_all("(/private)?/+[^ '\"]*?/(shipped|stripped)/+", "<scratch>/") |>
    str_replace_all("(/private)?/+[^ '\"]*?aggarwal_etal_2023_archive/*", "<scratch>/")
}

make_copy <- function(dest, strip) {
  unlink(dest, recursive = TRUE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(original_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
            dest, recursive = TRUE)
  if (strip && length(derived_members) > 0) unlink(file.path(dest, derived_members))
  invisible(dest)
}

# Run one script in its own R session, from the copy's root, and record where it
# stopped. The timeout is tested before the error text: a killed script's dying
# message is not the reason it stopped, and filing it as one turns a resource
# limit into a code defect.
run_script <- function(script, pass, timeout = 3600) {
  started <- Sys.time()
  result <- system2("Rscript", c("--vanilla", shQuote(script)),
                    stdout = TRUE, stderr = TRUE, timeout = timeout)
  status <- attr(result, "status")
  status <- if (is.null(status)) 0L else as.integer(status)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  timed_out <- status == 124L || elapsed >= timeout
  error_line <- result[str_detect(result, "^Error")] |> head(1)
  tibble(
    pass = pass,
    script = script,
    exit_status = status,
    outcome = case_when(
      timed_out ~ "timeout",
      status == 0 ~ "clean",
      TRUE ~ "error"
    ),
    stopped_at = if (length(error_line) == 0) NA_character_ else scrub(str_squish(error_line)),
    log = paste(result, collapse = "\n")
  )
}

run_pass <- function(dir, scripts, pass) {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  map(scripts, function(s) run_script(s, pass)) |> list_rbind()
}

# As shipped ----
shipped_dir <- file.path(archive_run_dir, "shipped")
make_copy(shipped_dir, strip = FALSE)

deposited <- manifest$file
mtime_before <- file.mtime(file.path(shipped_dir, deposited))

shipped <- run_pass(shipped_dir, analysis_scripts, "as_shipped")

mtime_after <- file.mtime(file.path(shipped_dir, deposited))
overwritten <- deposited[!is.na(mtime_after) & mtime_after > mtime_before]
added <- setdiff(
  list.files(shipped_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
  list.files(original_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
)

overwrites <- bind_rows(
  tibble(file = overwritten, effect = "overwritten"),
  tibble(file = added, effect = "added")
) |>
  arrange(effect, file, .locale = "en")

# Stripped to data plus code ----
stripped_dir <- file.path(archive_run_dir, "stripped")
make_copy(stripped_dir, strip = TRUE)
stripped <- run_pass(stripped_dir, analysis_scripts, "stripped")

status <- bind_rows(shipped, stripped)

walk(which(status$outcome != "clean"), function(i) {
  print(str_glue("[{status$pass[i]}] {status$script[i]}: {status$outcome[i]}"))
  print(status$stopped_at[i])
})

# The log carries scratch paths and is a property of the run, so it is printed
# and never committed.
status |>
  select(pass, script, exit_status, outcome, stopped_at) |>
  write_csv(here::here("ground_truth", "archive_run_status.csv"))

write_csv(overwrites, here::here("ground_truth", "archive_overwrites.csv"))

print(str_glue("Deposited files a deposited script writes over: {length(derived_members)}. ",
               "The stripped pass is the as-shipped pass where that count is zero, ",
               "and the deposit then ships no derived objects."))
print(status |> count(pass, outcome))
print(str_glue("Scratch copy: {archive_run_dir}"))
