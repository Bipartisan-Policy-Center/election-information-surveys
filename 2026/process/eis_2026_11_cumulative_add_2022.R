

########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####
####  Title: Adding 2022 to the EIS cumulative file
####
####  Author: Jack Friedman
####
####  Overview: Extends the 2024<->2026 cumulative file built by 'eis_2026_04_cumulative_data.R' with a
####            third year, 2022, adding only the content confirmed comparable in
####            'eis_2026_11_cumulative_2022_2023_design.md': demographics and the PROSPECTIVE vote-count
####            confidence items (conf_own_vote/conf_local_votes/conf_state_votes/conf_national_votes).
####            2022's RETROSPECTIVE confidence items (BPC9-12, "were counted in 2020") are a different
####            construct (past fact vs. future expectation) and are deliberately excluded, per Jack's
####            explicit instruction. Run this AFTER 'eis_2026_04_cumulative_data.R': it reads that
####            script's output as its 2024/2026 base and OVERWRITES it with the combined 3-year file. If
####            04 is re-run later (e.g. because 2026 data changes), re-run this script afterward or 2022
####            will silently disappear from the file again.
####
####  File Description: 2022 contributes only its national sample (AUD == 1, n = 2,002) - the raw file
####            also bundles three non-national state oversamples (Colorado/Georgia/Wisconsin) which
####            would bias national trend estimates and are dropped. 2022 has no respondent-ID column, so
####            resp_id is synthesized from row position. 2022 has no weight_common (the composition-free
####            re-raked weight from script 03 covers only 2024/2026 - see the design doc's "out of
####            scope" section); weighted comparisons involving 2022 should use weight_native.
####
####  Output: output/eis_cumulative.rds and output/eis_cumulative.csv (now 2022+2024+2026)
####
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


library(tidyverse)

out.dir <- "output"


##### #
#### #
### ################################################################################################################################################# #
# Part A. Loading inputs ----------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# The existing 2024/2026 cumulative file, built by eis_2026_04_cumulative_data.R - read as this
# script's base rather than re-deriving 2024/2026 from scratch, the same "consume an earlier script's
# derived output" pattern eis_2026_04_cumulative_data.R itself uses for eis_2026_clean.rds.
cumulative.existing <- readRDS(file.path(out.dir, "eis_cumulative.rds"))

# 2022 has no cleaning script of its own (the same situation eis_2026_04_cumulative_data.R documents for
# 2024) - read raw and build only the columns needed here.
data22.orig <- read.csv("input/eis_2022_data.csv")


##### #
#### #
### ################################################################################################################################################# #
# Part B. Filtering to the national sample, and the backbone ------------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# AUD: 1 = national GENPOP (n=2,002), 2/3/4 = Colorado/Georgia/Wisconsin oversamples (n=805/805/501).
# The oversamples are not part of a national probability sample and would bias national trend
# estimates, so they're dropped before anything else - the same shape of decision
# eis_2026_04_cumulative_data.R made for 2024's field1/field2 overwrite.
data22.orig <- data22.orig %>% filter(as.integer(AUD) == 1)

# lvl() only exists to avoid retyping levels(cumulative.existing$...) repeatedly, so 2022's factor
# labels are guaranteed identical to the existing file's rather than hand-typed twice with a chance of a
# mismatched string - identical purpose to eis_2026_04_cumulative_data.R's own lvl().
lvl <- function(var) levels(cumulative.existing[[var]])

# 2022 has no respondent-ID column at all (confirmed against the full raw header) - every row is still
# unique, just not vendor-identified, so a synthetic id from row position is used instead of a
# row-position "fallback" (there is nothing to fall back FROM, unlike 2024/2026's ResponseID/resp_id).
backbone22 <- tibble(
  year          = 2022L,
  resp_id       = paste0("2022_", seq_len(nrow(data22.orig))),
  weight_native = data22.orig$wts
)


##### #
#### #
### ################################################################################################################################################# #
# Part C. Demographics ------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# Every code->label mapping below was verified against 2022/raw/levels_codebook.csv and
# 2022/survey_instrument.md this session - see eis_2026_11_cumulative_2022_2023_design.md for the full
# audit. All of these use the identical Morning Consult coding scheme eis_2026_04_cumulative_data.R
# already relied on for 2024, since 2022 and 2024 share a vendor and item bank.
demog22 <- tibble(
  educ3              = factor(data22.orig$xeduc3,        levels = 1:3, labels = lvl("educ3"),        ordered = TRUE),
  race_white         = factor(as.integer(coalesce(data22.orig$xdemWhite, 0)), levels = c(0, 1)),
  age4               = factor(data22.orig$age,           levels = 1:4, labels = lvl("age4"),         ordered = TRUE),
  gender             = factor(data22.orig$xdemGender,    levels = 1:2, labels = lvl("gender")),
  region4            = factor(data22.orig$xreg4,         levels = 1:4, labels = lvl("region4")),
  generation         = factor(data22.orig$demAgeGeneration, levels = 1:4, labels = lvl("generation"), ordered = TRUE),
  ideo3              = factor(data22.orig$xdemIdeo3,     levels = 1:3, labels = lvl("ideo3"),         ordered = TRUE),
  income3            = factor(data22.orig$xdemInc3,      levels = 1:3, labels = lvl("income3"),       ordered = TRUE),
  rural_urban3       = factor(data22.orig$xdemUsr,       levels = 1:3, labels = lvl("rural_urban3"),  ordered = TRUE),
  employment         = factor(data22.orig$xdemEmploy,    levels = 1:8, labels = lvl("employment")),
  evangelical        = factor(data22.orig$xdemEvang,     levels = 1:2, labels = lvl("evangelical")),
  pid3               = factor(data22.orig$xpid3,         levels = 1:3, labels = lvl("pid3")),
  recalled_vote      = factor(data22.orig$xsubVote20O,   levels = 1:4, labels = c("Dem", "Rep", "Other", "Did not vote")),
  religion           = factor(data22.orig$xdemReligion,  levels = 1:5,
                               labels = c("All Christian", "All Non-Christian", "Atheist",
                                          "Agnostic/Nothing in particular", "Something Else"))
)

# race4: 2022 delivers the same four overlapping single-value flags 2024 does (Hispanic origin asked
# separately from race), so a Hispanic respondent also carries a race flag. Giving Hispanic origin
# precedence, identical to eis_2026_04_cumulative_data.R Part C.3, is the only ordering that partitions
# the sample.
race4_22 <- case_when(coalesce(data22.orig$xdemHispBin, 0) == 1 ~ "Hispanic",
                      coalesce(data22.orig$xdemWhite,   0) == 1 ~ "White, non-Hispanic",
                      coalesce(data22.orig$demBlackBin, 0) == 1 ~ "Black, non-Hispanic",
                      coalesce(data22.orig$demRaceOther,0) == 1 ~ "Other, non-Hispanic",
                      TRUE                                      ~ NA_character_)
demog22$race4 <- factor(race4_22, levels = lvl("race4"))

# --- Temporary checkpoint for Task 2 - replaced by Part F/G in Task 4 ---
message("Task 2 checkpoint: religion NAs = ", sum(is.na(demog22$religion)),
        ", race4 NAs = ", sum(is.na(demog22$race4)),
        ", religion levels match existing = ", identical(levels(demog22$religion), lvl("religion")),
        ", race4 levels match existing = ", identical(levels(demog22$race4), lvl("race4")))
