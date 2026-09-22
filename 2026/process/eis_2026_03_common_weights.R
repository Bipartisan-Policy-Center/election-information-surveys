

# EIS_2026_common_weights



########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####
####  Title: Making 2022, 2024, and 2026 weighted estimates comparable
####
####  Author: Jack Friedman
####
####  Overview: The 2026 Election Information Survey is cleaned in three companion .R files. The first,
####            'eis_2026_01_combine_codebooks.R', builds the combined codebook. The second,
####            'eis_2026_02_clean_data.R', cleans the raw data. This file, 'eis_2026_03_common_weights.R', re-weights
####            the 2022, 2024, and 2026 registered-voter files to one common demographic target so that a
####            year-over-year difference is free of composition effects. Run it after scripts 01 and 02.
####
####  File Description:
####
####  THE PROBLEM. Morning Consult weighted the three registered-voter files to different demographic targets, most
####  severely between 2024 and 2026: 40.6% versus 57.6% without a bachelor's degree (a 17.0-point gap) and 58.6%
####  versus 76.4% White (17.8 points) — even though the raw, unweighted samples were near-identical across all three
####  years (60.8% / 59.0% / 62.2% without a bachelor's degree and 71.5% / 71.1% / 77.7% White, for 2024 / 2026 / 2022
####  respectively; verified directly against the raw 2022 delivery). 2022's own delivered weight moves its raw
####  numbers only a little (62.2 -> 60.4% without a bachelor's degree, 77.7 -> 76.7% White) — nothing close to 2024's
####  severe swing — but it still targets a composition a few points off the other two years, and "a few points off"
####  still contaminates a difference the way a bigger gap does, just less of it. A weighted year-over-year difference
####  therefore mixes real opinion change with the change in the target, and the delivered weights give no way to
####  separate the two.
####
####  THE FIX. Re-rake all three files to ONE common demographic target. A difference between estimates computed at
####  the same demographic composition cannot contain a composition effect, because it has been differenced out.
####
####  WHAT STAYS YEAR-SPECIFIC. Each file is also raked to its own recalled presidential vote margin, because that
####  margin is correct in each year and legitimately differs between them — 2022 and 2024 both recall the 2020
####  election (the most recent one at either fielding date), the 2026 file recalls 2024. 2024 and 2026's delivered
####  files hit their recalled election almost exactly (2024: Biden 51.3 / Trump 46.8 against an actual 51.3 / 46.8;
####  2026: Harris 48.3 / Trump 49.8 against an actual 48.3 / 49.8). 2022's own delivered weight is looser on this
####  dimension (Biden 47.3 / Trump 43.3 against the same actual 2020 result) but is held at its own observed share
####  here for the same reason as the other two years: whatever political calibration the vendor built into each
####  year's own weight should stay untouched by a re-rake aimed at demographic composition, not overwritten by an
####  external target it was never trying to hit in the first place.
####
####  Output: output/eis_common_weights.rds, holding all three years' common weights plus a worked comparison of the
####          two confidence items the 2024 and 2026 instruments share.
####
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# Loading packages
library(tidyverse)
library(survey)   # for svydesign() and rake(), which do the iterative proportional fitting


##### #
#### #
### ################################################################################################################################################# #
# Part A. Loading both years and harmonizing the weighting margins ----------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.1. File paths and data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Relative to 2026/ — the R project's working directory whenever its .Rproj is open, regardless of which subfolder a
# script itself is saved in (this file lives in 2026/process/, but that is irrelevant to where relative paths resolve).
out.dir <- "output"

# The 2026 file, cleaned by script 02
data26 <- readRDS(file.path(out.dir, "eis_2026_clean.rds"))

# The 2024 registered-voter file, as delivered. Read raw rather than cleaned, because no cleaning script exists for it
# and only its six weighting margins are needed here. Lives alongside the 2026 vendor delivery in input/ since it is
# only ever read as a comparison input to the 2026 pipeline, not as its own year-folder project.
data24 <- read.csv("input/eis_2024_field2_rvoter_data.csv")

# The 2022 file, same reasoning as 2024 - read raw, only its six weighting margins are needed. AUD == 1 restricts to
# the national sample (the same copy and the same filter eis_2026_11_cumulative_add_2022.R uses) - the raw file also
# bundles three non-national state oversamples, which have no place in a national demographic target.
data22 <- read.csv("input/eis_2022_data.csv") %>% filter(as.integer(AUD) == 1)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.2. The six weighting margins, harmonized across the two years ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Both files code these variables identically — xeduc3 1/2/3, age 1:4, xdemGender 1/2, xreg4 1:4, xdemWhite 1 or
# missing, and the recalled-vote variable 1:4 — so the two blocks below differ only in where the columns come from.
# In the 2024 file they are raw numeric codes; in the 2026 file script 02 has already made them labelled factors, so
# as.integer() recovers the same codes.
#
# Using the same level LABELS in both years is what makes the two comparable: rake() matches a margin by level name.

margins24 <- tibble(educ   = factor(data24$xeduc3,    levels = 1:3, labels = c("No bachelor's degree", "Bachelor's degree", "Post-grad")),
                    white  = factor(coalesce(data24$xdemWhite, 0), levels = 0:1, labels = c("Not White", "White")),
                    age    = factor(data24$age,       levels = 1:4, labels = c("18-34", "35-44", "45-64", "65+")),
                    gender = factor(data24$xdemGender, levels = 1:2, labels = c("Male", "Female")),
                    region = factor(data24$xreg4,     levels = 1:4, labels = c("Northeast", "Midwest", "South", "West")),
                    # 2024 recalled the 2020 presidential vote
                    vote   = factor(data24$xsubVote20O, levels = 1:4, labels = c("Dem", "Rep", "Other", "Did not vote")),
                    weight = data24$wts)

margins26 <- tibble(educ   = factor(as.integer(data26$educ3),   levels = 1:3, labels = c("No bachelor's degree", "Bachelor's degree", "Post-grad")),
                    white  = factor(as.integer(data26$race_white == "1"), levels = 0:1, labels = c("Not White", "White")),
                    age    = factor(as.integer(data26$age4),    levels = 1:4, labels = c("18-34", "35-44", "45-64", "65+")),
                    gender = factor(as.integer(data26$gender),  levels = 1:2, labels = c("Male", "Female")),
                    region = factor(as.integer(data26$region4), levels = 1:4, labels = c("Northeast", "Midwest", "South", "West")),
                    # 2026 recalled the 2024 presidential vote
                    vote   = factor(as.integer(data26$vote_2024), levels = 1:4, labels = c("Dem", "Rep", "Other", "Did not vote")),
                    weight = data26$weight)

# 2022 codes these six variables identically to 2024 (same vendor, same raw numeric codes, confirmed against
# 2022/raw/levels_codebook.csv) - so this block is margins24's block verbatim, just reading from data22.
margins22 <- tibble(educ   = factor(data22$xeduc3,    levels = 1:3, labels = c("No bachelor's degree", "Bachelor's degree", "Post-grad")),
                    white  = factor(coalesce(data22$xdemWhite, 0), levels = 0:1, labels = c("Not White", "White")),
                    age    = factor(data22$age,       levels = 1:4, labels = c("18-34", "35-44", "45-64", "65+")),
                    gender = factor(data22$xdemGender, levels = 1:2, labels = c("Male", "Female")),
                    region = factor(data22$xreg4,     levels = 1:4, labels = c("Northeast", "Midwest", "South", "West")),
                    # 2022 recalled the 2020 presidential vote too - the same referent 2024 uses
                    vote   = factor(data22$xsubVote20O, levels = 1:4, labels = c("Dem", "Rep", "Other", "Did not vote")),
                    weight = data22$wts)

# A respondent missing a margin variable would silently drop out of that margin during raking, which would quietly
# change the denominator. Moving missing values into their own level instead, so every respondent stays in every margin.
# Part B then gives that level its own observed share as its target, which leaves it untouched by the raking.
margins24 <- margins24 %>% mutate(across(where(is.factor), ~ fct_na_value_to_level(.x, level = "(missing)") %>% fct_drop()))
margins26 <- margins26 %>% mutate(across(where(is.factor), ~ fct_na_value_to_level(.x, level = "(missing)") %>% fct_drop()))
margins22 <- margins22 %>% mutate(across(where(is.factor), ~ fct_na_value_to_level(.x, level = "(missing)") %>% fct_drop()))

# Stacking all three years into one long table of one row per (year, respondent, margin, level). Everything in Part B
# is computed off this, so the three years cannot be treated inconsistently by accident.
margins.long <- bind_rows(margins24 %>% mutate(year = 2024, row = row_number()),
                          margins26 %>% mutate(year = 2026, row = row_number()),
                          margins22 %>% mutate(year = 2022, row = row_number())) %>%
  pivot_longer(-c(year, row, weight), names_to = "margin", values_to = "level",
               values_transform = as.character)




##### #
#### #
### ################################################################################################################################################# #
# Part B. Building the population targets ------------------------------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.1. The common demographic target ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# DEFAULT: the 2026 delivered margins, on the grounds that 2026's demographics are the plausible ones. 40.6% without a
# bachelor's degree — the 2024 target — implies that 59.4% of registered voters hold one, roughly 1.5 times the rate
# among all adults, which is not credible.
#
# For correct LEVELS as well as correct differences, replace these with an external benchmark: the Census CPS Voting and
# Registration Supplement, Table 5, "Reported Voting and Registration by Age, Sex, and Educational Attainment"
# (P20-590 for November 2024), restricted to the reported-registered column.
# Available at: https://www.census.gov/data/tables/2024/demo/voting-and-registration/p20-590.html
#
# Any common target makes the DIFFERENCE between the two years composition-free. Only a correct target also makes each
# year's LEVEL right.

target <- tribble(
  ~margin,  ~level,                  ~share,
  "educ",   "No bachelor's degree",   0.576,
  "educ",   "Bachelor's degree",      0.265,
  "educ",   "Post-grad",              0.159,
  "white",  "Not White",              0.236,
  "white",  "White",                  0.764,
  "age",    "18-34",                  0.246,
  "age",    "35-44",                  0.161,
  "age",    "45-64",                  0.326,
  "age",    "65+",                    0.268,
  "gender", "Male",                   0.475,
  "gender", "Female",                 0.525,
  "region", "Northeast",              0.172,
  "region", "Midwest",                0.224,
  "region", "South",                  0.375,
  "region", "West",                   0.228
)

# Guarding against a typo in the literals above: each margin must sum to 1 before it is normalized
target.sums <- target %>% group_by(margin) %>% summarise(total = sum(share), .groups = "drop") %>% filter(abs(total - 1) > 0.01)

if (nrow(target.sums) > 0) {
  stop("Target margin does not sum to 1: ", paste(target.sums$margin, collapse = ", "))
}

# The levels named in the target must be exactly the levels present in the data, or rake() would silently ignore a
# margin. Checking both directions, for both years, and allowing only the "(missing)" level to be extra in the data.
target.mismatch <- margins.long %>%
  distinct(year, margin, level) %>%
  filter(margin != "vote", level != "(missing)") %>%
  anti_join(target, by = c("margin", "level"))

if (nrow(target.mismatch) > 0) {
  stop("Levels present in the data but absent from the target:\n",
       paste0("  ", target.mismatch$year, " ", target.mismatch$margin, ": ", target.mismatch$level, collapse = "\n"))
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.2. Turning the target into what rake() expects, for each year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# rake() wants a population total per level, with the totals for a margin summing to the sample size. Three groups of
# level have to be handled differently, which is what the three blocks of the bind_rows() below are:
#
#   1. The five demographic margins take the COMMON target, and this is what makes the two years comparable.
#   2. A "(missing)" level takes its own observed share, so raking neither inflates nor deletes those respondents. The
#      five demographic shares then have to be scaled down by that share, so the margin still sums to 1.
#   3. The vote margin takes its own observed share IN THAT YEAR, which is what keeps each year's political
#      calibration intact.

# The share each level currently holds under the delivered weights, by year and margin
observed <- margins.long %>%
  group_by(year, margin, level) %>%
  summarise(weight = sum(weight), .groups = "drop_last") %>%      # total delivered weight on this level
  mutate(share = weight / sum(weight)) %>%                        # as a share of its own margin
  ungroup() %>%
  select(year, margin, level, share)

# How much of each margin sits in its "(missing)" level. complete() fills in a zero for the margins that have none, so
# the join below never produces an NA.
missing.share <- observed %>%
  filter(level == "(missing)") %>%
  select(year, margin, share_missing = share) %>%
  complete(year = c(2022, 2024, 2026), margin = unique(target$margin), fill = list(share_missing = 0))

# The number of respondents in each year, used to convert shares into the counts rake() wants
year.n <- margins.long %>% distinct(year, row) %>% count(year, name = "n_resp")

population <- bind_rows(
  # 1. The common demographic target, scaled down to leave room for any "(missing)" level
  # The join is intentionally one-to-many: each target level is expanded to one row per year, because the amount to
  # scale it down by depends on how much of that margin is missing IN THAT YEAR.
  target %>%
    left_join(missing.share, by = "margin", relationship = "many-to-many") %>%
    mutate(share = share * (1 - share_missing)) %>%
    select(year, margin, level, share),
  # 2. The "(missing)" levels, held at their observed share
  observed %>% filter(level == "(missing)"),
  # 3. The vote margin, held at its own observed share within each year
  observed %>% filter(margin == "vote")
) %>%
  # Converting shares to counts. Re-normalizing within margin first, so rounding in the literals cannot make a margin
  # miss the sample size.
  left_join(year.n, by = "year") %>%
  group_by(year, margin) %>%
  mutate(Freq = share / sum(share) * first(n_resp)) %>%
  ungroup() %>%
  select(year, margin, level, Freq) %>%
  arrange(year, margin, level)

# The margin names, sorted once. rake() matches its sample.margins formulas to its population.margins list BY POSITION,
# so both must be built from this same order or the wrong target would be applied to the wrong variable.
margin.names <- sort(unique(population$margin))

# The formulas, one per margin
sample.margins <- map(margin.names, ~ as.formula(paste0("~", .x)))

# The population tables, one data frame per margin, whose first column must be NAMED AFTER THE VARIABLE it describes.
# Splitting the stacked table back out into that shape, one year at a time.
population24 <- population %>%
  filter(year == 2024) %>%
  split(.$margin) %>%
  map(~ data.frame(level = .x$level, Freq = .x$Freq) %>% set_names(c(unique(.x$margin), "Freq")))

population26 <- population %>%
  filter(year == 2026) %>%
  split(.$margin) %>%
  map(~ data.frame(level = .x$level, Freq = .x$Freq) %>% set_names(c(unique(.x$margin), "Freq")))

population22 <- population %>%
  filter(year == 2022) %>%
  split(.$margin) %>%
  map(~ data.frame(level = .x$level, Freq = .x$Freq) %>% set_names(c(unique(.x$margin), "Freq")))

# split() orders its output by the splitting variable, which must match margin.names
stopifnot("the population tables and the margin formulas are in different orders" =
            identical(names(population24), margin.names),
          identical(names(population26), margin.names),
          identical(names(population22), margin.names))




##### #
#### #
### ################################################################################################################################################# #
# Part C. Raking ------------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.1. Running the rake for each year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# rake() does iterative proportional fitting: it cycles through the margins, rescaling each level so its weighted share
# matches the target, and repeats until nothing moves. Starting from the DELIVERED weights rather than from 1, so that
# whatever the vendor weighted on and did not tell us is partly retained rather than thrown away. (The delivered
# demographics explain only R-squared = 0.22 of log(weight) in 2026, so the vendor's model used variables that are not
# in the file; this is what preserves their effect.)

design24 <- svydesign(ids = ~1, weights = ~weight, data = margins24)
design26 <- svydesign(ids = ~1, weights = ~weight, data = margins26)
design22 <- svydesign(ids = ~1, weights = ~weight, data = margins22)

raked24 <- rake(design24,
                sample.margins     = sample.margins,
                population.margins = population24,
                control            = list(maxit = 200, epsilon = 1e-10))

raked26 <- rake(design26,
                sample.margins     = sample.margins,
                population.margins = population26,
                control            = list(maxit = 200, epsilon = 1e-10))

raked22 <- rake(design22,
                sample.margins     = sample.margins,
                population.margins = population22,
                control            = list(maxit = 200, epsilon = 1e-10))

# Pulling the new weights out and normalizing each to sum to its own sample size, matching the convention the vendor
# used for the delivered weights
weight24 <- weights(raked24)
weight24 <- weight24 / sum(weight24) * length(weight24)

weight26 <- weights(raked26)
weight26 <- weight26 / sum(weight26) * length(weight26)

weight22 <- weights(raked22)
weight22 <- weight22 / sum(weight22) * length(weight22)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.2. Checking that the rake converged on every margin ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Recomputing each margin from the new weights and comparing it against the target it was supposed to hit. This is the
# check that matters: if a margin is off, the difference between years is not composition-free after all.
achieved <- bind_rows(margins24 %>% mutate(year = 2024, weight_new = weight24, row = row_number()),
                      margins26 %>% mutate(year = 2026, weight_new = weight26, row = row_number()),
                      margins22 %>% mutate(year = 2022, weight_new = weight22, row = row_number())) %>%
  pivot_longer(-c(year, row, weight, weight_new), names_to = "margin", values_to = "level",
               values_transform = as.character) %>%
  group_by(year, margin, level) %>%
  summarise(weight_new = sum(weight_new), .groups = "drop_last") %>%
  mutate(share_achieved = weight_new / sum(weight_new)) %>%
  ungroup() %>%
  # Bringing the target back in as a share, to compare like with like
  left_join(population %>%
              group_by(year, margin) %>%
              mutate(share_target = Freq / sum(Freq)) %>%
              ungroup() %>%
              select(year, margin, level, share_target),
            by = c("year", "margin", "level")) %>%
  mutate(deviation_pts = 100 * abs(share_achieved - share_target))

rake.failures <- achieved %>% filter(deviation_pts > 0.01)

if (nrow(rake.failures) > 0) {
  stop("The rake did not converge on every margin (deviation above 0.01 points):\n",
       paste0("  ", rake.failures$year, " ", rake.failures$margin, " ", rake.failures$level, ": ",
              round(rake.failures$deviation_pts, 4), " pts", collapse = "\n"))
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.3. What the re-weighting did to each year's design ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The design effect is 1 + the squared coefficient of variation of the weights, and Kish's effective n is the size a
# simple random sample of the same precision would have to be. Both are worth reporting, because a common target that
# happened to make the weights far more variable would buy comparability at the cost of precision.
design.comparison <- bind_rows(tibble(year = 2024, weight_old = data24$wts,   weight_new = weight24),
                               tibble(year = 2026, weight_old = data26$weight, weight_new = weight26),
                               tibble(year = 2022, weight_old = data22$wts,   weight_new = weight22)) %>%
  group_by(year) %>%
  summarise(n             = n(),
            deff_old      = 1 + (sd(weight_old) / mean(weight_old))^2,
            deff_new      = 1 + (sd(weight_new) / mean(weight_new))^2,
            n_eff_old     = sum(weight_old)^2 / sum(weight_old^2),
            n_eff_new     = sum(weight_new)^2 / sum(weight_new^2),
            max_weight_old = max(weight_old),
            max_weight_new = max(weight_new),
            .groups = "drop")

cat("\n\nWHAT THE COMMON TARGET DID TO EACH YEAR'S DESIGN\n\n")
print(design.comparison %>% mutate(across(where(is.numeric), ~ round(.x, 2))) %>% as.data.frame())
cat("\nNote the side benefit: the common target IMPROVES 2024's design effect, which is further evidence that the\n",
    "delivered 2024 target was straining that sample.\n")




##### #
#### #
### ################################################################################################################################################# #
# Part D. Comparing an item across the two years ----------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.1. The comparison helper ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This is the one function this script defines, because it is the thing the script exists to hand over: a year-over-year
# comparison has to be computed the same way every time it is run, on whatever item is being compared, and the
# arithmetic is not something any existing function does.
#
# `indicator24` and `indicator26` are LOGICAL vectors over their year's respondents:
#   TRUE   the respondent is in the numerator
#   FALSE  in the denominator but not the numerator
#   NA     out of base, so in neither
#
# That convention is what puts every percentage on the right denominator without passing one in.
#
# Returns the naive difference (delivered weights), the composition-free difference (common weights), and the gap
# between them — which is the part of the naive difference that was the target change rather than opinion change.
compare_years <- function(label, indicator24, indicator26) {

  # The weighted percentage in the numerator, over the respondents who were in base at all
  pct <- function(indicator, weight) 100 * sum(weight[indicator], na.rm = TRUE) / sum(weight[!is.na(indicator)])

  naive  <- pct(indicator26, data26$weight) - pct(indicator24, data24$wts)
  common <- pct(indicator26, weight26)      - pct(indicator24, weight24)

  tibble(item          = label,
         y2024_as_del  = pct(indicator24, data24$wts),
         y2026_as_del  = pct(indicator26, data26$weight),
         diff_naive    = naive,
         y2024_common  = pct(indicator24, weight24),
         y2026_common  = pct(indicator26, weight26),
         diff_common   = common,
         artifact      = naive - common)
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.2. The worked example ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The only two items the 2024 and 2026 instruments share with an identical scale and identical wording apart from the
# election named: confidence that votes will be counted as intended, at state and at national level.
#   2024 BPC22 / BPC23  ->  2026 conf_state_votes / conf_national_votes
# Both years code 1 = Very confident through 4 = Not confident at all, with 5 = Don't know. The don't-knows stay in the
# denominator, which is how BPC reports them.
#
# On the 2026 side the _i columns are used, which script 02 built running the OTHER WAY — 4 = Very confident — so that
# higher always means more confident. Hence >= 3 for "confident" and == 4 for "very confident", against 1:2 and == 1 in
# the raw 2024 codes.
comparison <- bind_rows(
  compare_years("Confident votes counted: state",
                data24$BPC22 %in% 1:2, data26$conf_state_votes_i >= 3),
  compare_years("Confident votes counted: nationwide",
                data24$BPC23 %in% 1:2, data26$conf_national_votes_i >= 3),
  compare_years("Very confident only: state",
                data24$BPC22 == 1, data26$conf_state_votes_i == 4),
  compare_years("Very confident only: nationwide",
                data24$BPC23 == 1, data26$conf_national_votes_i == 4)
)

cat("\n\nWORKED COMPARISON (percentage points)\n\n")
cat("  diff_naive   what the delivered weights say changed\n")
cat("  diff_common  what changed at a fixed demographic composition\n")
cat("  artifact     the part of the naive difference that was the target change, not opinion change\n\n")
print(comparison %>% mutate(across(where(is.numeric), ~ round(.x, 1))) %>% as.data.frame())


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.3. How much 2022 itself moves under the common target ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Not a year-over-year difference like D.2's worked example - 2022 has no BPC22/BPC23-style matching wording to pair
# it against in a two-year diff. Just 2022 on its own, delivered weight vs. common weight, on the same two confidence
# items (2022's BPC15 state / BPC16 nationwide), to show directly how much this year's own re-rake actually moves it
# - answering the question this script's header raises: since 2022's own delivered weight was already fairly close
# to the common target (unlike 2024's), how much does re-raking it actually change.
pct22 <- function(indicator, weight) 100 * sum(weight[indicator], na.rm = TRUE) / sum(weight[!is.na(indicator)])

comparison22 <- tibble(
  item      = c("Confident votes counted: state", "Confident votes counted: nationwide"),
  delivered = c(pct22(data22$BPC15 %in% 1:2, data22$wts), pct22(data22$BPC16 %in% 1:2, data22$wts)),
  common    = c(pct22(data22$BPC15 %in% 1:2, weight22),   pct22(data22$BPC16 %in% 1:2, weight22))
) %>% mutate(moved_pts = common - delivered)

cat("\n\n2022 ALONE: DELIVERED WEIGHT VS. COMMON WEIGHT (percentage points)\n\n")
print(comparison22 %>% mutate(across(where(is.numeric), ~ round(.x, 1))) %>% as.data.frame())




##### #
#### #
### ################################################################################################################################################# #
# Part E. Saving ------------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# resp_id_26 is saved alongside the 2026 weights so they can be joined rather than assumed to be in the same row
# order. w2022, like w2024, is saved positionally instead - 2022 has no respondent-ID column at all (confirmed
# against its full raw header), and data22 above and eis_2026_11_cumulative_add_2022.R's own data22.orig both derive
# from the identical read.csv("input/eis_2022_data.csv") %>% filter(AUD == 1) sequence, so their row order is
# guaranteed identical without a join.
saveRDS(object = list(w2024        = as.numeric(weight24),
                      w2026        = as.numeric(weight26),
                      w2022        = as.numeric(weight22),
                      resp_id_26   = data26$resp_id,
                      target       = target,
                      design       = design.comparison,
                      comparison   = comparison,
                      comparison22 = comparison22),
        file = file.path(out.dir, "eis_common_weights.rds"))

message("\nCommon weights written to ", out.dir, "/eis_common_weights.rds")
message("Use w2024 with eis_2024_field2_rvoter_data.csv (row order preserved), w2026 with eis_2026_clean.rds (join by resp_id),")
message("and w2022 with input/eis_2022_data.csv filtered to AUD == 1 (row order preserved, same reasoning as w2024).")




# The end.
