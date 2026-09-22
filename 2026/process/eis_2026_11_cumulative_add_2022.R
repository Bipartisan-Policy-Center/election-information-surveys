

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


##### #
#### #
### ################################################################################################################################################# #
# Part D. Confidence votes will be counted as intended - PROSPECTIVE items only ------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# 2022 asks this both retrospectively (BPC9-12: "were... counted accurately in the 2020 election") and
# prospectively (BPC13-16: "will be... counted accurately in the 2022 midterm election"). Only the
# prospective items match conf_own_vote/etc.'s construct (a forward-looking expectation) - the
# retrospective items ask about a past fact instead and are deliberately excluded, per Jack's explicit
# instruction and eis_2026_11_cumulative_2022_2023_design.md's callout not to confuse the two.
#
# Recoding to label text first, then applying the existing file's level order via `levels = lvl(...)`,
# the same safer pattern eis_2026_04_cumulative_data.R Part E.5 uses rather than a positional remap -
# even though this four-item scale happens to be a clean end-to-end reversal of the raw codes, spelling
# it out avoids relying on that happening to be true.
recode_conf4 <- function(x) case_when(x == 1 ~ "Very confident",
                                      x == 2 ~ "Somewhat confident",
                                      x == 3 ~ "Not too confident",
                                      x == 4 ~ "Not confident at all",
                                      TRUE   ~ NA_character_)   # code 5 (Don't know) and anything else -> NA

conf22 <- tibble(
  conf_own_vote       = factor(recode_conf4(data22.orig$BPC13), levels = lvl("conf_own_vote"),       ordered = TRUE),
  conf_local_votes    = factor(recode_conf4(data22.orig$BPC14), levels = lvl("conf_local_votes"),    ordered = TRUE),
  conf_state_votes    = factor(recode_conf4(data22.orig$BPC15), levels = lvl("conf_state_votes"),    ordered = TRUE),
  conf_national_votes = factor(recode_conf4(data22.orig$BPC16), levels = lvl("conf_national_votes"), ordered = TRUE)
)


##### #
#### #
### ################################################################################################################################################# #
# Part E. Filling every other column, then stacking --------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# 2022 does not populate country_direction/top_issue (out of the scope Jack asked for - demographics and
# confidence only, even though both are available - see the design doc), state/county/town (no ZIP
# delivered), union_member/sexual_orientation/insured/insurance_type/married (not delivered, same gap
# 2024/2026 already have), weight_common (the composition-free re-raked weight from script 03 covers
# only 2024/2026 - see the design doc's "out of scope" section), and every source-seeking/AI/concern/
# vote-experience substantive item (2022 either lacks them entirely or lacks a clean match - see the
# design doc).
#
# na_like() fills each of those columns with the correctly-typed NA - same class, same factor levels and
# level ORDER as the existing file - by indexing the reference column with an out-of-range index rather
# than hand-declaring ~90 columns' levels a second time. Same idea as eis_2026_04_cumulative_data.R's own
# na_binary(), generalized from one hardcoded 2-level factor to any column type, because this script
# needs it for far more columns than that one did.
na_like <- function(x, n) x[rep(NA_integer_, n)]

data22.built <- bind_cols(backbone22, demog22, conf22)

missing.cols <- setdiff(names(cumulative.existing), names(data22.built))
for (col in missing.cols) {
  data22.built[[col]] <- na_like(cumulative.existing[[col]], nrow(data22.built))
}

# Column ORDER also has to match exactly - bind_rows() matches by name regardless of order, but the
# stopifnot below checks names() with order-sensitive identical(), matching eis_2026_04_cumulative_data.R
# Part F's own check, so the columns are put in the existing file's order here rather than relying on
# bind_rows() to reconcile it silently.
data22.built <- data22.built %>% select(all_of(names(cumulative.existing)))

# Checking level identity BEFORE stacking, not after - bind_rows() does not require two factor columns to
# share levels, it silently unions them - so a mismatch here would not throw an error later, it would
# just produce a column with more levels than either side actually has and no error to catch it.
# Identical check to eis_2026_04_cumulative_data.R Part F, extended to compare 2022 against the existing
# (already-validated) 2024/2026 file instead of comparing 2024 against 2026.
factor.cols <- names(data22.built)[sapply(data22.built, is.factor)]
level.check.cols <- setdiff(factor.cols, c("state", "county", "town"))
level.mismatches <- level.check.cols[!sapply(level.check.cols, function(v)
  identical(levels(data22.built[[v]]), levels(cumulative.existing[[v]])))]

stopifnot("2022 and the existing cumulative file have the same column names, in the same order" =
            identical(names(data22.built), names(cumulative.existing)),
          "every factor column in 2022, other than state/county/town, has identical levels, in the same order, to the existing file" =
            length(level.mismatches) == 0)

cumulative <- bind_rows(cumulative.existing, data22.built)


##### #
#### #
### ################################################################################################################################################# #
# Part F. Validation --------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

stopifnot(
  "7,037 rows total (5,035 existing + 2,002 from 2022)" = nrow(cumulative) == 7037,
  "2,002 rows are 2022" = sum(cumulative$year == 2022) == 2002,
  "resp_id is unique within year" = cumulative %>% count(year, resp_id) %>% pull(n) %>% max() == 1,
  "2022's weight_native sums to its own n" = cumulative %>% filter(year == 2022) %>%
    pull(weight_native) %>% sum() %>% {abs(. - 2002) < 1},
  "2022's weight_common is entirely missing (no common weight exists for 2022 yet)" =
    cumulative %>% filter(year == 2022) %>% pull(weight_common) %>% is.na() %>% all(),
  "2022's demographics are never entirely missing" =
    cumulative %>% filter(year == 2022) %>% select(educ3, race4, age4, gender, region4, pid3) %>%
      summarise(across(everything(), ~ !all(is.na(.x)))) %>% unlist() %>% all(),
  "2022's prospective confidence items are never entirely missing" =
    cumulative %>% filter(year == 2022) %>%
      select(conf_own_vote, conf_local_votes, conf_state_votes, conf_national_votes) %>%
      summarise(across(everything(), ~ !all(is.na(.x)))) %>% unlist() %>% all(),
  "2022's country_direction/top_issue and source-seeking/AI/concern/seek items are entirely missing (out of scope for this pass)" =
    cumulative %>% filter(year == 2022) %>%
      select(country_direction, top_issue, matches("^(reg|run|won)_src_|^src_|^ai_ok_|^concern_|^seek_|^vote_exp_positive")) %>%
      summarise(across(everything(), ~ all(is.na(.x)))) %>% unlist() %>% all()
)

message("eis_2026_11_cumulative_add_2022.R: all validation checks passed. n = ", nrow(cumulative),
        " (", sum(cumulative$year == 2022), " from 2022, ", sum(cumulative$year == 2024), " from 2024, ",
        sum(cumulative$year == 2026), " from 2026), ", ncol(cumulative), " columns.")


##### #
#### #
### ################################################################################################################################################# #
# Part G. Saving -------------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

saveRDS(object = cumulative, file = file.path(out.dir, "eis_cumulative.rds"))
write.csv(x = cumulative, file = file.path(out.dir, "eis_cumulative.csv"), row.names = FALSE, na = "")

message("\nCumulative file written to ", out.dir, "/eis_cumulative.rds and .csv (", nrow(cumulative),
        " rows, ", ncol(cumulative), " columns).")

# The end.
