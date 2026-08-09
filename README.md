# Maintained reproduction of Aggarwal, Allen, Coppock, Frankowski, Messing, Zhang, Barnes, Beasley, Hantman and Zheng (2023)


- [What this repository is](#what-this-repository-is)
  - [Folder layout](#folder-layout)
  - [How to reproduce](#how-to-reproduce)
- [The paper](#the-paper)
- [Does the deposited archive run?](#does-the-deposited-archive-run)
- [Errata](#errata)
  - [Findings that are not errata](#findings-that-are-not-errata)
- [The ground truth](#the-ground-truth)
  - [Coverage, float by float](#coverage-float-by-float)
- [The extraction and the two
  instruments](#the-extraction-and-the-two-instruments)
  - [What the second instrument found that a float-shaped ground truth
    would
    not](#what-the-second-instrument-found-that-a-float-shaped-ground-truth-would-not)
- [The maintained rewrite](#the-maintained-rewrite)
- [Figure verification](#figure-verification)
- [Rewrite verification](#rewrite-verification)
- [R environment](#r-environment)

*Drafted by Claude Opus 5 under the supervision of Alex Coppock.*

## What this repository is

This repository re-runs the analysis behind a published article from its
deposited data, in current R, and lays every number the article prints
against what that analysis produces. It holds three things: a script
that fetches and verifies the authors’ replication archive, a rewrite of
the analysis using current packages, and a ground truth that records,
claim by claim, whether the published number reproduces.

|  |  |
|----|----|
| Article | Aggarwal M, Allen J, Coppock A, Frankowski D, Messing S, Zhang K, Barnes J, Beasley A, Hantman H and Zheng S (2023). “A 2 million-person, campaign-wide field experiment shows how digital advertising affects voter turnout.” *Nature Human Behaviour* 7, 332–341. <https://doi.org/10.1038/s41562-022-01487-4> |
| Replication archive | <https://doi.org/10.7910/DVN/YMKVA1> |
| Pre-analysis plans | <https://osf.io/3evfp> and <https://osf.io/jkush> |
| Treatment stimuli | <https://osf.io/ex3kq> |

### Folder layout

    download_original.R    fetches and verifies the deposited archive
    run_all.R              runs everything, in order
    original/              the deposited archive (not redistributed; fetched on demand)
    original_manifest.csv  every deposited file, its size and its published checksum
    maintained/            the rewrite; maintained/output/ holds every number it produces
    ground_truth/          the extraction, the comparison and the gates
    errata.qmd             corrections to the published article

### How to reproduce

Download the repository, open `aggarwal_etal_2023.Rproj`, and run:

``` r
source("run_all.R")
```

`run_all.R` fetches the deposited archive from Harvard Dataverse,
verifies every file against its published checksum and byte size, writes
every figure and table, computes every in-text quantity, runs the
deposited scripts in a scratch copy, rebuilds the ground truth and
re-verifies the archive. Nothing writes into `original/`. The archive is
494 MB, most of it one Facebook Ad Library extract, so the first run
spends several minutes downloading.

## The paper

A campaign-level field experiment run by Acronym, a left-leaning
non-profit, during the 2020 US presidential election. Of the registered
voters in Arizona, Michigan, North Carolina, Pennsylvania and Wisconsin
who met the campaign’s targeting criteria, a randomly sampled holdout
saw none of Acronym’s advertising for eight months while everyone else
saw the full programme. Turnout was then read from the TargetSmart voter
file for the 1,999,282 participants whose assignment could be recovered.

The headline results are an average effect on turnout indistinguishable
from zero and a difference in conditional average treatment effects by
modelled Trump support of 0.7 percentage points: turnout rose 0.4 points
among Biden leaners and fell 0.3 points among Trump leaners.

## Does the deposited archive run?

The deposit is 16 files: eleven analysis scripts, four data files and a
README. Every script was run in a scratch copy, twice, once as the
archive ships and once stripped to data plus code.

| pass       | clean | error |
|:-----------|------:|------:|
| as_shipped |     5 |     6 |
| stripped   |     5 |     6 |

Deposited scripts by pass and outcome.

The two passes give the same answer, and that is a finding rather than
an omission. Stripping the copy to data plus code removes nothing,
because no deposited file is written by any deposited script: the
write-call scan below returns 3 uncommented writes and all 3 of them
target an `output/` subdirectory that is not part of the deposit. The
archive ships no derived objects, so the stripped-copy test is vacuous
here and is reported as such rather than as a pass.

6 of 11 scripts stop on an error, for four distinct reasons.

| script | stopped_at |
|:---|:---|
| in_text_calculations.R | Error in waldtest(omni_fit, omni_fit_restricted, test = “… |
| figure_2.R | Error in xtable(arrange(mutate(select(gg_df, inquiry, out… |
| figure_3.R | Error in `ggsave()`: |
| table_1.R | Error in file(file, ifelse(append, “a”, “w”)) : |
| figure_B1.R | Error in readRDS(con, refhook = refhook) : cannot open th… |
| figure_C4.R | Error: |

Where each failing script stopped, as shipped.

- **Two scripts fail because the deposit does not ship the directory
  they write into.** `figure_3.R` calls
  `ggsave("output/figure_3.pdf", ...)` and `table_1.R` opens
  `output/table_1a.tex` for writing. Neither `output/` nor any
  `dir.create()` is in the deposit, so on a clean copy both stop at
  their first write. Every estimate each script computes is already in
  memory when it stops.
- **Two scripts fail on data files the deposit does not contain.**
  `figure_B1.R` reads `fielding_dates.rds` and
  `fielding_dates_survey.rds`; `figure_C4.R` reads
  `weekly_adds_for_page_id_disclaimer_041322.csv`. The deposit’s own
  README lists all three among the files the appendix scripts depend on.
  They are the reason Supplementary Figures 1 and 4 have no coverage
  here.
- **`figure_2.R` fails on a missing package.** It calls `xtable()` after
  opening with `rm(list = ls())` and without `library(xtable)`, so the
  call resolves only if a previous script attached the package in the
  same session.
- **`in_text_calculations.R` fails on a commented-out package.** Its
  `library(lmtest)` line is commented out and it calls `waldtest()`.
  Everything above that line runs.

**One further defect in the deposit is not visible as a failure**,
because the script stops before reaching it. Below the `waldtest()`
call, `in_text_calculations.R` computes the article’s six advertising
spending figures with `filter(!is_turnout)` and `group_by(ad_format)`
and `group_by(ad_type)`. The deposited `ad_library_2020_acronym.csv`
carries none of those three columns; they are built in `figure_B2.R`. So
the script that is named for the in-text calculations cannot produce the
in-text spending figures even after the `lmtest` line is restored. All
six reproduce from `figure_B2.R`’s classification, which is what this
rewrite uses.

Running the deposit in place would add 1 file to the archive directory,
an `Rplots.pdf` from the scripts that print a plot to the active device,
and would overwrite 0 deposited files.

**The archive reproduces the article.** Every one of the 946 numbers the
article’s six large floats print is recovered from the deposited code,
and none of the 53 ground truth rows with an archive counterpart
disagrees with the page.

## Errata

Five sentences in the published article are wrong, and they are
corrected in
[`aggarwal_etal_2023_errata.pdf`](aggarwal_etal_2023_errata.pdf), whose
values are computed at render time from this repository’s output. None
of them changes a conclusion. In summary: the Supplementary Information
names July as the month of heaviest persuasion spending where the figure
it points at makes it February, and that cross-reference does not
resolve; the Methods count 21 pre-treatment covariates where Figure 5
and Supplementary Table 3 both carry 22; the treatment audience is given
as “1,993,216 million” where the sentence’s own parenthetical makes it
1,993,216; the balance F test’s degrees of freedom are printed as the
residual count first and a negative parameter difference second; and one
share is rounded down where it rounds up.

### Findings that are not errata

**The deposited scripts are not quite the scripts that made the
published figures.** `figure_3.R` draws two `geom_label` annotations
reading “Control group” and “Treatment group”; the published Figure 3
carries neither. `figure_5.R` labels its axis rows “Trump support score
/ 100”, “Turnout score / 100”, “Ideology score / 100” and “Partisanship
score / 100”; the published Figure 5 prints “TSS/100”, “Turnout
score/100”, “Ideology score/100” and “Partisanship score/100”. Every
estimate in both figures reproduces exactly. The rewrite follows the
deposited code, so its Figure 3 carries the two labels.

**One targeting criterion does not hold in the data the article
analyses.** Figure 4 gives the targeting criteria as a Trump support
score between 30 and 70 and a presidential turnout score between 20 and
80. The Trump support score in the deposited analysis set runs exactly
30 to 70. The presidential turnout score runs 20.0 to 99.2, so the upper
half of the criterion is not satisfied. The same figure states the
archival subset’s own criteria as a presidential turnout score above 20
with no upper bound, and the analysis sample is that subset, so the two
descriptions in one figure are not consistent with each other. Which is
right cannot be settled from the deposit, and no published estimate
depends on it, so this is recorded as unresolved rather than corrected.

**The state list of the Facebook Ad Library query appears twice and
differs.** Supplementary Information section C describes the query as
covering AZ, MI, NC, PA and WI; the caption of Supplementary Figure 3,
which that query produces, lists AZ, GA, MI, NC, PA and WI. The
deposited extract carries no state field, so neither list can be
checked. The total the section states, US\$349,006,000, is the
lower-bound sum over the whole deposited extract and reproduces exactly.

**The Reporting Summary gives the study’s timing as July to November
2020** where the Methods describe an eight-month programme running from
March 2020 to election day. The Reporting Summary form does not say
which activity it is timing, so this is left as an inconsistency rather
than corrected.

**The registration date of the pre-analysis plan appears twice and
differs.** Supplementary Information section D says the plan was
registered on 22 November 2020; the plan it reproduces is dated 11
November 2020. A registration can postdate a document, so this is not
necessarily an error.

## The ground truth

`ground_truth/aggarwal_etal_2023_ground_truth.csv` carries one row per
published claim, 216 of them. `value_paper` is the string the article
prints and is the only column anyone typed; `value_script` is read out
of the deposited code and `value_rewrite` out of `maintained/output/`,
both by script. A value agrees when the computed number, printed to the
page’s own precision, gives the same digits.

| claim_type   | does not reproduce | no verdict | reproduces | does not hold | holds |
|:-------------|-------------------:|-----------:|-----------:|--------------:|------:|
| definitional |                  2 |         56 |         13 |             0 |     0 |
| descriptive  |                  0 |          1 |          0 |             4 |     2 |
| pipeline     |                  3 |          1 |         88 |             0 |     0 |
| structural   |                  0 |          7 |          6 |             1 |     0 |
| transcribed  |                  0 |         32 |          0 |             0 |     0 |

Published claims by type and verdict.

| defect_locus   | rows |
|:---------------|-----:|
| archive        |   47 |
| paper_internal |    8 |
| unresolved     |    3 |

Where the fault lies, on every row that is not a clean match.

Every adverse row carries a `defect_locus` and no clean match carries
one. The five values are `paper_internal` (the article disagrees with
its own tables or its own data), `archive` (the deposit cannot support
the claim), `environment` (R or a package moved underneath it),
`rewrite` (ours) and `unresolved` (cause not established). The `archive`
rows here are almost all of one kind: the deposit is the analysis subset
and one Ad Library extract, and the article also describes an assignment
population of 31 million voters, a Facebook match rate, an impressions
count and a Wesleyan Media Project spending series, none of which the
deposit was ever going to carry.

### Coverage, float by float

| float | published numbers | covered | fraction | reproduced by rewrite | reproduced by archive |
|:---|---:|---:|:---|---:|---:|
| figure_1 | 114 | 114 | 100% | 114 | 114 |
| figure_2 | 36 | 36 | 100% | 36 | 36 |
| figure_3 | 3 | 2 | 67% | 2 | NA |
| figure_4 | 27 | 9 | 33% | 7 | NA |
| figure_5 | 3 | 2 | 67% | 2 | NA |
| figure_6 | 0 | 0 | NA | NA | NA |
| supplementary_figure_1 | 1 | 0 | 0% | 0 | NA |
| supplementary_figure_2 | 2 | 1 | 50% | 1 | NA |
| supplementary_figure_3 | 2 | 1 | 50% | 1 | NA |
| supplementary_figure_4 | 1 | 0 | 0% | 0 | NA |
| supplementary_table_1 | 399 | 399 | 100% | 399 | 399 |
| supplementary_table_2 | 126 | 126 | 100% | 126 | 126 |
| supplementary_table_3 | 176 | 176 | 100% | 176 | 176 |
| table_1 | 95 | 95 | 100% | 95 | 95 |

Coverage per published float. Published numbers are what the float puts
on its own face, or the countable quantities it asserts where it prints
none.

Six floats carry the bulk of the published record. Supplementary Tables
1, 2 and 3 print 701 numbers between them, main-text Table 1 prints 95,
and Figures 1 and 2 print an estimate and a standard error above every
point, 114 and 36 numbers, which makes them published tables in
disguise. All 946 are parsed from the published PDFs into
`ground_truth/published_appendix_values.csv` and
`ground_truth/published_maintext_tables.csv` and compared cell by cell.
Every one reproduces.

Figures 1 and 2 are read positionally from the text layer, never in
reading order, and they are read from the figures themselves rather than
from the supplementary tables their captions point at. That makes the
agreement between the two published transcriptions a finding: all 114 of
Figure 1’s cells and all 36 of Figure 2’s equal the corresponding
supplementary table cell.

Figures 3 and 5 print no numbers on their faces. For those the checkable
published quantity is what they plot: Figure 3 plots 246 binned means,
which is 41 one-point bins by two conditions by three outcomes, and
Figure 5 plots 22 covariate estimates, one per axis label. Both counts
are verified and both figures’ underlying estimates are written to CSV.

**Two floats have zero coverage and the reason is the same in both
cases.** Supplementary Figure 1 needs `fielding_dates.rds` and
`fielding_dates_survey.rds`; Supplementary Figure 4 needs
`weekly_adds_for_page_id_disclaimer_041322.csv`. None of the three is
among the 16 deposited files, the deposited scripts that read them fail
at their first read, and neither figure prints any number other than its
axis breaks. Figure 6 is two photographs of advertisements and asserts
no quantity at all.

## The extraction and the two instruments

`ground_truth/published_claims.csv` is the extraction: 216 rows, one per
quantitative claim in the article, its figure captions, its Methods, its
Reporting Summary and its Supplementary Information, each classified by
hand as `pipeline`, `descriptive`, `definitional`, `structural` or
`transcribed`, and each carrying the string the page prints and the
precision it prints it at.

| claim_type   | needs a block: FALSE | needs a block: TRUE |
|:-------------|---------------------:|--------------------:|
| definitional |                   55 |                  16 |
| descriptive  |                    0 |                   7 |
| pipeline     |                    0 |                  92 |
| structural   |                    7 |                   7 |
| transcribed  |                   32 |                   0 |

The extraction by claim type and whether the pipeline can reach the
quantity.

It was built by extracting every numeric token from the text layer of
both PDFs with its surrounding context, then classifying each by hand.
Numbered citation markers, the reference lists, calendar years used only
as dates and the article’s own DOI are excluded, and the six floats
above are carried as one row each with their cells compared separately
rather than as 946 rows. A companion pass swept for numbers written as
words, which no token scan sees: “five battleground states”,
“8-month-long”, “plus or minus one-third of a percentage point”, “only
two exhibited statistically significant imbalance” and “greater than
three points” all entered the extraction that way.

Two instruments read the same pipeline output by separate paths.
`ground_truth/build_ground_truth.R` builds the comparison table.
`maintained/in_text_claims.R` carries the article’s own sentence in a
block comment beside code that recomputes the number, and prints 122
claims in the form `CLAIM <id> = <value> || <label>`. It reads
`maintained/output/` and the extraction, never the ground truth, so the
two derivations are independent and a disagreement between them is a
finding.

The build asserts all of the following and stops if any fails.

- Every `pipeline` and `descriptive` claim has a block and a row.
- Re-rendering `value_paper` from its own number at its own recorded
  precision returns the string the extraction stores. This runs before
  anything consumes the precision, because it is the check that catches
  a precision right about the value and wrong about the digits.
- Where the same published quantity is printed both in prose and in a
  supplementary table, the two hand transcriptions agree at whichever
  precision is coarser.
- The number of printed claims equals the number of rows requiring one,
  and the two sets of ids are equal in both directions.
- Every value the two instruments both produce agrees exactly.
- Every published cell joins to exactly one pipeline value; a published
  cell that finds nothing stops the build rather than becoming missing.
- Every adverse row carries a `defect_locus` and no clean match does.
- The ground truth and the extraction cover the same claims, in both
  directions.

Three of these were tested by breaking them, and the checks are ordered
so that each break reaches its own: deleting a block fails the count and
the id comparison, perturbing a printed value fails the cross-instrument
comparison, and changing one claim’s recorded precision fails the string
check before the value comparison can absorb it.

### What the second instrument found that a float-shaped ground truth would not

Every number in the article’s six large floats reproduces, so a ground
truth built around floats would have returned a clean sweep. All five
errata, and every finding above, are prose, captions or the document’s
own structure.

| claim | holds |
|:---|:---|
| Out of 21 covariates, only two exhibited statistically significant imbalance | FALSE |
| only two exhibited statistically significant imbalance | TRUE |
| Neither of these estimates remained significant after a Benjamini-Hochberg correction | TRUE |
| An F-test … was non-significant | TRUE |
| the campaign increased voting among Biden leaners | TRUE |
| and decreased voting among Trump leaners | TRUE |
| We also observed small conditional average effect estimates by age, gender, race and vo… | TRUE |
| we can affirm that our overall estimate is effectively equivalent to zero using the two… | TRUE |
| differential effects of the programme were stronger in our early voting data than in th… | TRUE |
| 1.0 percentage points favouring Biden … 0.3 percentage points favouring Trump | TRUE |
| party registration was not available for a large proportion of the participant pool (72%) | TRUE |
| we are missing race data for 4% of the sample where race was uncoded | NA |
| Acronym spent more on persuasion in July than in any other month | FALSE |

Descriptive claims: what the article asserts about shape, sign and
count, and whether it holds. The evidence behind each verdict is written
out beside it in the pipeline output.

## The maintained rewrite

`maintained/` is 12 scripts. Every figure and table script writes the
unrounded estimates it plots or prints alongside the figure itself, so
nothing downstream reads a number back out of a formatted cell.

The substitutions are the usual ones for code of this age: the native
pipe for `magrittr`, `reframe(tidy(...))` for `do(tidy(...))`,
`pick(everything())` for the implicit `.` inside `do()`, `if_else()` for
`ifelse()`, `separate_wider_delim()` for `separate()`, `knitr::kable()`
written to a file for `xtable` printed through a connection, and
`label_dollar()`/`label_date()` for the `scales::` formatter aliases.
`rm(list = ls())` is dropped, packages are loaded once in `helpers.R`,
and every path goes through `here::here()`.

**One deposited behaviour had to be preserved deliberately.**
`aggregated_analysis_set.rds` is a grouped tibble, grouped by all 41 of
its covariate columns, which is what a `count()` leaves behind. A bare
`summarize()` over it returns two million rows rather than one, which is
why the deposit’s `table_1.R` calls `ungroup()` before its total row.
The rewrite drops the grouping when the file is read, where it cannot be
forgotten.

**The rewrite adds three published tables the deposit computes and never
writes.** The deposited `figure_1.R`, `figure_2.R` and `figure_5.R` each
assemble an `xtable` object holding exactly the cells of Supplementary
Tables 1, 2 and 3, and each has its `print.xtable()` call commented out.
Those three tables are 701 of the article’s published numbers, and
without writing them there is no artifact to compare against.

## Figure verification

Every rendered figure was laid beside the published one. Numbers alone
cannot catch a transposed axis or a mislabelled series, and two of this
article’s four plotted figures print their estimates on the face of the
plot.

![Figure 1 as the rewrite draws it. All 114 printed labels, the facet
order and the row order within each facet reproduce the published
figure.](maintained/output/figure_1_ate_cate_estimates.png)

![Figure 2 as the rewrite draws it. All 36 printed labels
reproduce.](maintained/output/figure_2_heterogeneity_by_tss.png)

![Figure 3 as the rewrite draws it. The two group labels come from the
deposited script; the published figure does not carry
them.](maintained/output/figure_3_turnout_by_tss_score.png)

![Figure 5 as the rewrite draws it, with the deposited script’s own
covariate labels.](maintained/output/figure_5_balance.png)

![Supplementary Figure 2 as the rewrite draws
it.](maintained/output/figure_b2_acronym_spending.png)

![Supplementary Figure 3 as the rewrite draws
it.](maintained/output/figure_c3_facebook_spending.png)

## Rewrite verification

`run_all.R` was run to completion twice from clean sessions and the
whole tree diffed. Every CSV and TeX file is byte-identical between
runs. The figure PDFs differ, because a PDF records the time it was
written.

`original/` is verified twice per run. `download_original.R` is sourced
first, which checks every file against the MD5 Dataverse serves for
`?format=original` and against its deposited byte size, and then refuses
to continue if `original/` holds any file the manifest does not list,
dotfiles included. It is sourced again as the last step, because the
first pass proves only that the archive was intact when the run began.
All 16 files match, and 0 carry a published checksum that disagrees with
the bytes Dataverse serves. Two of the sixteen are ingested as tabular
data and are served under a `.tab` name unless the original format is
requested; the manifest records both names. Checksums for files this
repository writes are not quoted anywhere: a PDF records its write time,
so its hash changes on every run.

## R environment

| component | version |
|:----------|:--------|
| R         | 4.6.0   |
| tidyverse | 2.0.0   |
| estimatr  | 1.0.6   |
| car       | 3.1.5   |
| lmtest    | 0.9.40  |
| ggplot2   | 4.0.3   |

The article reports R 4.1.1 with tidyverse 1.3.1, estimatr 0.30.6 and
car 3.1.0. Nothing in this analysis draws at random: no script calls
`sample()`, `rnorm()` or a bootstrap, so the results are bit-identical
at any seed and the sampler change in R 3.6 is not in play. The one
place where composing two packages could have moved a published number
is the omnibus balance test, which calls `lmtest::waldtest()` on two
`estimatr::lm_robust` objects. Current `estimatr` exports a `vcov`
method that the version of the day did not, which is the mechanism that
silently changed an entire published table in another archive in this
programme. Here it does not bite: the test returns 1.0266 on 18 and
1,999,246 degrees of freedom, which is the published statistic exactly.
