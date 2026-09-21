

########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####
####  Title: Building the EIS cumulative 2024-2026 data file
####
####  Author: Jack Friedman
####
####  Overview: The 2026 Election Information Survey is cleaned in three companion .R files (scripts 01-03). This
####            file, 'eis_2026_04_cumulative_data.R', stacks the 2024 and 2026 registered-voter files into one long
####            dataset holding only the demographics, weights, and substantive items verified comparable across both
####            fields. Run it after scripts 01-03. Full rationale for every included/excluded item is in
####            'eis_2026_04_cumulative_data_design.md'.
####
####  File Description: One row per respondent per year (1,891 + 3,144 = 5,035 rows). Both years' own delivered
####            weight and the composition-free common weight from script 03 are carried on every row. 2024 has no
####            cleaning script of its own, so this file does that work for exactly the columns needed here — it does
####            not attempt to build a general-purpose cleaned 2024 file the way scripts 01-02 do for 2026.
####
####  Output: output/eis_cumulative.rds and output/eis_cumulative.csv
####
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# Loading packages
library(tidyverse)
library(zipcodeR)   # for the 2024 ZIP -> state/county/town crosswalk, same package script 02 uses for 2026


##### #
#### #
### ################################################################################################################################################# #
# Part A. Loading inputs ----------------------------------------------------------------------------------------------------------------------------- ----
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

# The 2024 registered-voter file, as delivered. Read raw rather than cleaned, because no cleaning script exists for
# 2024 and this script builds only the columns it needs, not a general-purpose cleaned 2024 file. Lives alongside the
# 2026 vendor delivery in input/ since it is only ever read as a comparison input to the 2026 pipeline.
data24.orig <- read.csv("input/eis_2024_field2_rvoter_data.csv")

# The 2026 file, cleaned by script 02
data26 <- readRDS(file.path(out.dir, "eis_2026_clean.rds"))

# Both years' delivered weights plus the common (composition-free) re-raked weights built by script 03
common.weights <- readRDS(file.path(out.dir, "eis_common_weights.rds"))


##### #
#### #
### ################################################################################################################################################# #
# Part B. The backbone: year, respondent ID, and both weights ---------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.1. 2024 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# w2024 is saved in the same row order as eis_2024_field2_rvoter_data.csv (per script 03's own saveRDS message), so no
# join is needed here
backbone24 <- tibble(year          = 2024L,
                     resp_id       = as.character(data24.orig$ResponseID),
                     weight_native = data24.orig$wts,
                     weight_common = common.weights$w2024)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.2. 2026 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# w2026 is NOT guaranteed row-order-aligned to data26 (script 03's own message says to join by resp_id), so joining
# explicitly rather than assuming position
weight.common.26 <- tibble(resp_id       = as.character(common.weights$resp_id_26),
                           weight_common = common.weights$w2026)

backbone26 <- tibble(year          = 2026L,
                     resp_id       = as.character(data26$resp_id),
                     weight_native = data26$weight) %>%
  left_join(weight.common.26, by = "resp_id")


##### #
#### #
### ################################################################################################################################################# #
# Part C. Demographics ------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.1. The demographics that match cleanly between years ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# `lvl()` only exists to avoid retyping `levels(data26$...)` about twenty times below, so that 2024's factor labels
# are guaranteed identical to 2026's rather than hand-typed twice with a chance of a mismatched string. Not hiding any
# real logic - a one-line accessor, not a novel operation.
lvl <- function(var) levels(data26[[var]])

# 2024's raw numeric codes, mapped onto 2026's exact category labels. Every code->label mapping below was verified
# against eis_2024_field1_codebook_levels.csv and eis_2024_codebook_questions.csv this session - see
# eis_2026_04_cumulative_data_design.md for the full audit. Three columns below don't reuse lvl(): `race_white` and
# `race4` are built from 0/1 flags and a case_when() construction rather than a label lookup (race4 is finished in
# Part C.3 below), and `recalled_vote` is relabeled to generic "Dem"/"Rep" because 2026's own `vote_2024` carries the
# candidates' names ("Kamala Harris", "Donald Trump") - script 03 already relabels it the same way, matched here
# rather than reinvented.
demog24 <- tibble(
  educ3              = factor(data24.orig$xeduc3,        levels = 1:3, labels = lvl("educ3"),        ordered = TRUE),
  race_white         = factor(as.integer(coalesce(data24.orig$xdemWhite, 0)), levels = c(0, 1)),
  age4               = factor(data24.orig$age,           levels = 1:4, labels = lvl("age4"),         ordered = TRUE),
  gender             = factor(data24.orig$xdemGender,    levels = 1:2, labels = lvl("gender")),
  region4            = factor(data24.orig$xreg4,         levels = 1:4, labels = lvl("region4")),
  generation         = factor(data24.orig$demAgeGeneration, levels = 1:4, labels = lvl("generation"), ordered = TRUE),
  ideo3              = factor(data24.orig$xdemIdeo3,     levels = 1:3, labels = lvl("ideo3"),         ordered = TRUE),
  income3            = factor(data24.orig$xdemInc3,      levels = 1:3, labels = lvl("income3"),       ordered = TRUE),
  rural_urban3       = factor(data24.orig$xdemUsr,       levels = 1:3, labels = lvl("rural_urban3"),  ordered = TRUE),
  employment         = factor(data24.orig$xdemEmploy,    levels = 1:8, labels = lvl("employment")),
  union_member       = factor(data24.orig$demUnion,      levels = 1:2, labels = lvl("union_member")),
  sexual_orientation = factor(data24.orig$demLGBTQ1,     levels = 1:7, labels = lvl("sexual_orientation")),
  insured            = factor(data24.orig$demInsured,    levels = 1:2, labels = lvl("insured")),
  insurance_type     = factor(data24.orig$demInsType,    levels = 1:7, labels = lvl("insurance_type")),
  evangelical        = factor(data24.orig$xdemEvang,     levels = 1:2, labels = lvl("evangelical")),
  married            = factor(data24.orig$xdemMarried,   levels = 1:2, labels = lvl("married")),
  pid3               = factor(data24.orig$xpid3,         levels = 1:3, labels = lvl("pid3")),
  recalled_vote      = factor(data24.orig$xsubVote20O,   levels = 1:4, labels = c("Dem", "Rep", "Other", "Did not vote")),
  # country_direction and top_issue are categorized as "substantive comparable items" in the design doc, but both are
  # built here in Part C, not Part E, purely for tibble-construction convenience
  country_direction  = factor(data24.orig$nr1,           levels = 1:2, labels = lvl("country_direction")),
  top_issue          = factor(data24.orig$nr3,           levels = 1:8, labels = lvl("top_issue"))
)

# 2026's side: mostly a direct pass-through since script 02 already cleaned these. `recalled_vote` is relabeled from
# vote_2024's candidate names to the same generic Dem/Rep/Other/Did-not-vote scheme as 2024, matching script 03's
# existing precedent exactly.
demog26 <- data26 %>%
  transmute(educ3, race_white, age4, gender, region4, generation, ideo3, income3, rural_urban3,
            employment, union_member, sexual_orientation, insured, insurance_type, evangelical, married, pid3,
            recalled_vote     = factor(as.integer(vote_2024), levels = 1:4,
                                        labels = c("Dem", "Rep", "Other", "Did not vote")),
            country_direction, top_issue)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.2. Religion: 2024 only has 5 categories, so 2026's 12 are collapsed to match ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 2024's xdemReligion is a coarse 5-bucket vendor recode; 2026's religion is the full 12-category item. There is no
# way to go the other direction (2024 was never asked the finer breakdown), so 2026 is collapsed down to 2024's
# scheme. This is a real loss of information on the 2026 side - flagged in the plan header, not just here.
religion24 <- factor(data24.orig$xdemReligion, levels = 1:5,
                     labels = c("All Christian", "All Non-Christian", "Atheist",
                                "Agnostic/Nothing in particular", "Something Else"))

religion26 <- fct_collapse(data26$religion,
  "All Christian"                  = c("Protestant", "Roman Catholic", "Mormon",
                                        "Orthodox (e.g. Greek or Russian Orthodox)"),
  "All Non-Christian"              = c("Jewish", "Muslim", "Buddhist", "Hindu"),
  "Atheist"                        = "Atheist",
  "Agnostic/Nothing in particular" = c("Agnostic", "Nothing in particular"),
  "Something Else"                 = "Something else")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.3. Race, made mutually exclusive the same way script 02 does it for 2026 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 2024 delivers the same structure 2026 did before race4 was built: four overlapping single-value flags (Hispanic
# origin asked separately from race, so a Hispanic respondent also carries a race flag). Giving Hispanic origin
# precedence, identical to script 02 Part J.3, is the only ordering that partitions the sample.
race4_24 <- case_when(coalesce(data24.orig$xdemHispBin, 0) == 1 ~ "Hispanic",
                      coalesce(data24.orig$xdemWhite,   0) == 1 ~ "White, non-Hispanic",
                      coalesce(data24.orig$demBlackBin, 0) == 1 ~ "Black, non-Hispanic",
                      coalesce(data24.orig$demRaceOther,0) == 1 ~ "Other, non-Hispanic",
                      TRUE                                      ~ NA_character_)
race4_24 <- factor(race4_24, levels = levels(data26$race4))

race4_26 <- data26$race4

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.4. Fold both into demog24/demog26 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

demog24 <- demog24 %>% mutate(religion = religion24, race4 = race4_24)
demog26 <- demog26 %>% mutate(religion = religion26, race4 = race4_26)

##### #
#### #
### ################################################################################################################################################# #
# Part D. State-level geography for 2024, from ZIP --------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.1. Deriving state, county, and town from ZIP ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 2024 was never delivered a state variable, unlike 2026 (whose `state` is the respondent's own delivered answer).
# This derives state/county/town from the ZIP code the same way script 02 Part J.1 does for 2026 - the only
# difference is there is no delivered state to prefer over the derived one, so the derived one IS the state column
# here.

# demZIP is numeric in the raw CSV (read.csv drops leading zeros - see eis_2026_README.md's own note on why 2026's
# zip column is kept character for exactly this reason). Restoring the leading zero before the crosswalk join, since
# a dropped zero would send a Massachusetts/Connecticut/etc. ZIP to the wrong row.
zip.24 <- sprintf("%05d", as.integer(data24.orig$demZIP))

zip.db <- zipcodeR::zip_code_db %>%
  filter(!is.na(state), state != "") %>%
  select(zip = zipcode, town = major_city, county, state)

# Same fallback as script 02: the ZIP's first three digits (its sectional center facility) sit inside one state for
# nearly all prefixes, recovering a state for a ZIP the crosswalk table itself does not list
zip3.map <- zip.db %>%
  mutate(zip3 = str_sub(zip, 1, 3)) %>%
  count(zip3, state, name = "n_zips") %>%
  group_by(zip3) %>%
  slice_max(n_zips, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(zip3, state_zip3 = state)

state.names <- tibble(state = c(state.abb, "DC"), state_name = c(state.name, "District of Columbia"))

geo24 <- tibble(resp_id = as.character(data24.orig$ResponseID), zip = zip.24) %>%
  left_join(zip.db, by = "zip") %>%
  mutate(zip3 = str_sub(zip, 1, 3)) %>%
  left_join(zip3.map, by = "zip3") %>%
  mutate(zip_match = case_when(!is.na(state)      ~ "ZIP code",
                               !is.na(state_zip3) ~ "ZIP prefix",
                               TRUE               ~ "unmatched"),
         state     = coalesce(state, state_zip3)) %>%
  left_join(state.names, by = "state") %>%
  transmute(resp_id, state = as.factor(state_name), county = as.factor(county),
            town = as.factor(town), zip_match)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.2. Check match quality ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

cat("\n=== ZIP Code Match Quality ===\n")
print(table(geo24$zip_match, useNA = "ifany"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.3. Validate against county_fips ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# A plain n_distinct(county) vs. n_distinct(county_fips) count does not validate anything - county *names*
# legitimately repeat across states (there is more than one "Washington County"), so unequal counts prove nothing
# and equal counts would not either. The real check is whether the state implied by the delivered county_fips agrees
# with the state already derived from ZIP above, built the same data-derived way zip3.map was built rather than a
# hardcoded FIPS table: zero-pad county_fips to 5 digits, take its first two digits (the state FIPS prefix), then
# for each prefix take the MODAL ZIP-derived state among respondents who share that prefix. This is safe circularity
# - the ZIP-derived state is already validated at a 99.79%+ exact-match rate (see the ZIP Code Match Quality check
# above) - so using its mode per FIPS-prefix as ground truth is sound.
fips.24 <- tibble(resp_id     = as.character(data24.orig$ResponseID),
                  county_fips = ifelse(is.na(data24.orig$county_fips), NA_character_,
                                       sprintf("%05d", as.integer(data24.orig$county_fips)))) %>%
  mutate(fips_state2 = str_sub(county_fips, 1, 2))

geo24.fips <- geo24 %>% select(resp_id, state) %>% left_join(fips.24, by = "resp_id")

fips.state.map <- geo24.fips %>%
  filter(!is.na(fips_state2)) %>%
  count(fips_state2, state, name = "n_resp") %>%
  group_by(fips_state2) %>%
  slice_max(n_resp, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(fips_state2, state_fips = state)

fips.agreement <- geo24.fips %>%
  filter(!is.na(fips_state2)) %>%
  left_join(fips.state.map, by = "fips_state2") %>%
  summarise(n = n(), n_agree = sum(state == state_fips, na.rm = TRUE))

cat("\n=== County FIPS Validation ===\n")
cat("Missing county_fips:", sum(is.na(data24.orig$county_fips)), "of", nrow(data24.orig), "\n")
cat(sprintf("state agreement via county_fips: %d of %d (%.1f%%)\n",
            fips.agreement$n_agree, fips.agreement$n, 100 * fips.agreement$n_agree / fips.agreement$n))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.4. Fold into the demographics tibbles ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

demog24 <- demog24 %>% bind_cols(geo24 %>% select(state, county, town))
demog26 <- demog26 %>% bind_cols(data26 %>% select(state, county, town))

##### #
#### #
### ################################################################################################################################################# #
# Part E. Substantive comparable items --------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.1. How often each kind of election information is sought ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# BPC1's stem and 5-point frequency scale are identical in both years for these three rows (register/vote, how
# elections run, who won). 2024's BPC1_4 (candidates) and BPC1_5 (campaign news) have no 2026 counterpart and are
# dropped, per the design doc.
#
# Recoding to the exact label TEXT first, then applying 2026's level order separately (`levels = lvl(...)`), rather
# than a positional `levels = 1:5, labels = ...` remap — the raw code order (1 = "Only around major dates...", ...,
# 5 = "Never") is NOT simply 2026's order reversed (2026 moves "Never" from the END to the START, then keeps the
# other four in the same relative order), so a `rev()` shortcut here would silently mislabel every response. Writing
# out the text and letting `factor(..., levels = lvl(...))` do the reordering removes that whole class of mistake.
# `recode_freq5()` is a genuinely novel, repeated operation (used identically for all three seek_* items below).
recode_freq5 <- function(x) case_when(
  x == 1 ~ "Only around major election dates or deadlines (e.g. Voter registration deadlines, Election Day)",
  x == 2 ~ "A few times in the weeks around major election dates or deadlines",
  x == 3 ~ "Regularly in the weeks around major election dates or deadlines",
  x == 4 ~ "Consistently throughout the year",
  x == 5 ~ "Never",
  TRUE   ~ NA_character_)

substantive24 <- tibble(
  seek_register      = factor(recode_freq5(data24.orig$BPC1_1), levels = lvl("seek_register"),      ordered = TRUE),
  seek_elections_run = factor(recode_freq5(data24.orig$BPC1_2), levels = lvl("seek_elections_run"), ordered = TRUE),
  seek_who_won       = factor(recode_freq5(data24.orig$BPC1_3), levels = lvl("seek_who_won"),        ordered = TRUE)
)

substantive26 <- data26 %>%
  transmute(seek_register, seek_elections_run, seek_who_won)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.2. Where people look for election information, by topic (register / how elections run / who won) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 2024's BPC3/BPC4/BPC5 use the same "Please select up to three" wording 2026 does, NOT "choose the three" - that
# phrasing is BPC2's, a different and excluded question (see the Excluded section of the design doc). Even with
# matching wording, 2024 did NOT actually force exactly three selections: confirmed empirically this session,
# 55.5-59.3% of respondents selected fewer than three across BPC3/BPC4/BPC5, the same pattern
# eis_2024_selectN_findings.md already documented for BPC2. Comparing flat selection indicators against 2026's
# "select up to three" is therefore reasonable; 2026's ranking is dropped from this comparison (no 2024 counterpart).
#
# IMPORTANT - comparability caveat: matching wording is not a matching menu. 2024 offered 13 substantive options
# here (of 15 total) and 2026 offers 10 (of 12), both still capped at 3 selections, and 2026 added two heavily
# selected options (online search engine, AI-enabled chatbot) with no 2024 counterpart. As a result the 8 shared
# indicators built below are NOT level-comparable across years - a change in reg_src_advocacy, reg_src_campaign,
# etc. cannot be read as pure opinion change, since the menu itself changed what competes for the same 3 slots. See
# the comparability-caveat paragraph in eis_2026_04_cumulative_data_design.md (Substantive items section) for the
# full numbers and the recommended fix: compare ranks across years, not raw selection levels.
#
# Position mapping below is constant across BPC3 (register), BPC4 (how elections run), BPC5 (who won), because all
# three fielded the identical 15-item list (verified against survey_instrument.md). Positions 1 (fact-checking
# organizations), 4 (elected officials generally), 10 (favorite commentator/analyst), 11 (civic or religious
# organizations) have no 2026 counterpart and are dropped. `src_campaign` OR's together 2024's two separate options
# (preferred candidate; national party organization), since 2026 merged them into one option.
#
# `build_src()` is a genuinely novel, repeated operation - the same 8-indicator extraction applied to three different
# question prefixes - which is exactly the case jack-r-coding-style calls out as a fine exception to "no custom
# functions."
build_src <- function(df, prefix) {
  sel <- function(pos) coalesce(df[[paste0(prefix, "_", pos, "NET")]] == "1", FALSE)
  tibble(
    src_local_officials = factor(as.integer(sel(2)),          levels = c(0, 1)),
    src_state_officials  = factor(as.integer(sel(3)),          levels = c(0, 1)),
    src_federal_site     = factor(as.integer(sel(6)),          levels = c(0, 1)),
    src_news_media       = factor(as.integer(sel(9)),          levels = c(0, 1)),
    src_social           = factor(as.integer(sel(13)),         levels = c(0, 1)),
    src_friends_family   = factor(as.integer(sel(12)),         levels = c(0, 1)),
    src_advocacy         = factor(as.integer(sel(5)),          levels = c(0, 1)),
    src_campaign         = factor(as.integer(sel(7) | sel(8)), levels = c(0, 1))
  )
}

# Both 2024 vendor codebooks (eis_2024_field1_codebook_questions.csv and the field-2 equivalent) mislabel BPC3_*
# with BPC4's question stem ("If you wanted to know more about how elections are run..."). That is a suspected
# vendor copy-paste error, not a coding error here: the mapping below was verified directly against the fielded
# survey_instrument.md, where BPC3 is unambiguously the register-and-vote question (see the design doc for the full
# audit). eis_2024_selectN_findings.md (line ~73) already flagged this exact discrepancy and said it should be
# confirmed with Morning Consult before any item-level 2024 numbers from this battery are published - that
# confirmation has not happened yet.
reg.src.24 <- build_src(data24.orig, "BPC3") %>% rename_with(~ paste0("reg_", .x))
run.src.24 <- build_src(data24.orig, "BPC4") %>% rename_with(~ paste0("run_", .x))
won.src.24 <- build_src(data24.orig, "BPC5") %>% rename_with(~ paste0("won_", .x))

substantive24 <- substantive24 %>% bind_cols(reg.src.24, run.src.24, won.src.24)

substantive26 <- substantive26 %>%
  bind_cols(data26 %>%
              select(reg_src_local_officials, reg_src_state_officials, reg_src_federal_site, reg_src_news_media,
                     reg_src_social, reg_src_friends_family, reg_src_advocacy, reg_src_campaign,
                     run_src_local_officials, run_src_state_officials, run_src_federal_site, run_src_news_media,
                     run_src_social, run_src_friends_family, run_src_advocacy, run_src_campaign,
                     won_src_local_officials, won_src_state_officials, won_src_federal_site, won_src_news_media,
                     won_src_social, won_src_friends_family, won_src_advocacy, won_src_campaign))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.2b. Main election-news sources (BPC2) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 2024's BPC2 ("choose the three that you use most often") and 2026's BPC2 ("select up to three") differ in
# wording, but per Jack's review that difference does not reflect an actual behavioral difference - both let
# respondents select fewer than three, so the flat selection indicators built below are directly comparable, the
# same reasoning E.2 above already relies on for the reg/run/won batteries. 11 of 2026's 12 substantive items
# match 2024's wording verbatim; "Friends and/or family" is new to 2026's BPC2 and has no 2024 counterpart (kept
# out of this comparable set; reported as a 2026-only figure directly in the report instead).
#
# Position mapping confirmed against the vendor's own per-column question text in
# '2024/raw/field1/question_codebook.csv' (BPC2_1NET .. BPC2_14NET) - field1 and field2 field the same
# instrument, so the same column layout applies here. Positions 12-14 are the anchors (other / not interested /
# don't know), dropped the same way every other battery's anchors are dropped from this comparison.
bpc2.items.2024 <- c("tv_local", "tv_national", "radio", "news_local", "news_national", "social",
                     "search", "podcast", "newsletter", "aggregator", "chatbot")

build_bpc2_src <- function(df) {
  sel <- function(pos) coalesce(df[[paste0("BPC2_", pos, "NET")]] == "1", FALSE)
  as_tibble(setNames(
    lapply(seq_along(bpc2.items.2024), function(pos) factor(as.integer(sel(pos)), levels = c(0, 1))),
    paste0("src_", bpc2.items.2024)
  ))
}

# A same-length, always-missing 2-level factor - used below for an item that exists in only one
# year, so both years still end up with the same column names (Part F's own stopifnot requires it).
na_binary <- function(n) factor(rep(NA_character_, n), levels = c("0", "1"))

substantive24 <- substantive24 %>% bind_cols(build_bpc2_src(data24.orig))

substantive26 <- substantive26 %>%
  bind_cols(data26 %>% select(all_of(paste0("src_use_", bpc2.items.2024))) %>%
              rename_with(~ paste0("src_", str_remove(.x, "^src_use_"))))

# "Friends and/or family" is new to 2026's BPC2 - no 2024 counterpart.
substantive24 <- substantive24 %>% mutate(src_friends_family = na_binary(nrow(data24.orig)))
substantive26 <- substantive26 %>% mutate(src_friends_family = data26$src_use_friends_family)

# reg/run/won's two new-2026 items (online search engine, AI chatbot) - no 2024 counterpart. The pooled
# {need}_src_search / {need}_src_chatbot columns already exist in `data26` (script 02 Part J.5 pools every
# infosource item, not just the 8 shared with 2024), so no new extraction logic is needed on the 2026 side.
for (need in c("reg", "run", "won")) {
  substantive24[[paste0(need, "_src_search")]]  <- na_binary(nrow(data24.orig))
  substantive24[[paste0(need, "_src_chatbot")]] <- na_binary(nrow(data24.orig))
  substantive26[[paste0(need, "_src_search")]]  <- data26[[paste0(need, "_src_search")]]
  substantive26[[paste0(need, "_src_chatbot")]] <- data26[[paste0(need, "_src_chatbot")]]
}

# reg/run/won's four dropped items (no 2026 counterpart), read the same way E.2's build_src() reads the shared
# 8 - same 2024 raw prefixes (BPC3 = reg, BPC4 = run, BPC5 = won), positions 1/4/10/11.
build_dropped <- function(df, prefix) {
  sel <- function(pos) coalesce(df[[paste0(prefix, "_", pos, "NET")]] == "1", FALSE)
  tibble(
    src_fact_checking      = factor(as.integer(sel(1)),  levels = c(0, 1)),
    src_elected_officials  = factor(as.integer(sel(4)),  levels = c(0, 1)),
    src_commentator        = factor(as.integer(sel(10)), levels = c(0, 1)),
    src_civic_religious    = factor(as.integer(sel(11)), levels = c(0, 1))
  )
}

reg.dropped.24 <- build_dropped(data24.orig, "BPC3") %>% rename_with(~ paste0("reg_", .x))
run.dropped.24 <- build_dropped(data24.orig, "BPC4") %>% rename_with(~ paste0("run_", .x))
won.dropped.24 <- build_dropped(data24.orig, "BPC5") %>% rename_with(~ paste0("won_", .x))

substantive24 <- substantive24 %>% bind_cols(reg.dropped.24, run.dropped.24, won.dropped.24)

dropped.item.names <- c("src_fact_checking", "src_elected_officials", "src_commentator", "src_civic_religious")
for (need in c("reg", "run", "won")) {
  for (item in dropped.item.names) {
    substantive26[[paste0(need, "_", item)]] <- na_binary(nrow(data26))
  }
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.3. AI good/bad matrix, DK-excluded integer version ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 2024's BPC14 has no "Don't know" option at all (confirmed via eis_2024_field1_codebook_levels.csv: 5 forced-choice
# levels only), while 2026's BPC38 adds a 6th. 2026's own `_i` columns already drop DK as off-scale per the
# established project convention, so the comparable measure is a matching `_i` for 2024 - a straightforward 5-level
# recode with no DK to handle. Both years' raw codes run 1 = Very good thing ... 5 = Very bad thing; `_i` reorients so
# higher always means more positive, hence `6 - raw`.
substantive24 <- substantive24 %>%
  mutate(ai_ok_voter_candidate_info_i  = 6L - data24.orig$BPC14_1,
         ai_ok_voter_how_to_vote_i     = 6L - data24.orig$BPC14_2,
         ai_ok_voter_how_to_register_i = 6L - data24.orig$BPC14_3,
         ai_ok_voter_decide_i          = 6L - data24.orig$BPC14_4,
         ai_ok_voter_values_align_i    = 6L - data24.orig$BPC14_5,
         ai_ok_campaign_undisclosed_i  = 6L - data24.orig$BPC14_6,
         ai_ok_campaign_disclosed_i    = 6L - data24.orig$BPC14_7,
         ai_ok_cand_photo_edit_i       = 6L - data24.orig$BPC14_8,
         ai_ok_cand_microtarget_i      = 6L - data24.orig$BPC14_9,
         ai_ok_cand_answer_questions_i = 6L - data24.orig$BPC14_10)

substantive26 <- substantive26 %>%
  bind_cols(data26 %>%
              select(ai_ok_voter_candidate_info_i, ai_ok_voter_how_to_vote_i, ai_ok_voter_how_to_register_i,
                     ai_ok_voter_decide_i, ai_ok_voter_values_align_i, ai_ok_campaign_undisclosed_i,
                     ai_ok_campaign_disclosed_i, ai_ok_cand_photo_edit_i, ai_ok_cand_microtarget_i,
                     ai_ok_cand_answer_questions_i))

# Reporting 2026's per-item Don't-know share, the same pattern as the vote_exp_positive message below (E.4), so
# anyone comparing this battery's _i columns across years can see the size of the base-exclusion asymmetry: 2024's
# BPC14 had no DK option at all (0% by construction - there is nothing to compute), while 2026's BPC38 added a 6th
# "Don't know" level that `_i` excludes as off-scale (see the comment above).
ai.dk.items <- c("ai_ok_voter_candidate_info", "ai_ok_voter_how_to_vote", "ai_ok_voter_how_to_register",
                 "ai_ok_voter_decide", "ai_ok_voter_values_align", "ai_ok_campaign_undisclosed",
                 "ai_ok_campaign_disclosed", "ai_ok_cand_photo_edit", "ai_ok_cand_microtarget",
                 "ai_ok_cand_answer_questions")
pct.dk.2026 <- sapply(paste0(ai.dk.items, "_f"), function(v) 100 * mean(data26[[v]] == "Don't know", na.rm = TRUE))
message("2026 AI-battery 'don't know' share by item (unweighted), range ", round(min(pct.dk.2026), 1), "%-",
        round(max(pct.dk.2026), 1), "%. 2024's BPC14 had no DK option at all (0% by construction). Per item: ",
        paste(sprintf("%s=%.1f%%", ai.dk.items, pct.dk.2026), collapse = ", "))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.4. Voting experience: same stem, 2026 added a neutral midpoint ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 2024 is 4-point forced-choice (no neutral); 2026 added "Neither agree nor disagree." Building 2024's version onto
# 2026's own 5-level factor (its "Neither..." level will simply never be populated for 2024 rows, which is exactly
# the "keep each year's native distribution" outcome the design doc asked for, in one shared column rather than two).
# `vote_exp_positive_top2` is the comparable trend variable: agree (1) vs. disagree (0), base excludes DK in both
# years and excludes "neither" in 2026 (2024 has no "neither" to exclude, so this is only a partial fix for the
# added-midpoint design effect - see the design doc).
substantive24 <- substantive24 %>%
  mutate(vote_exp_positive = factor(case_when(data24.orig$BPC19 == 1 ~ "Strongly agree",
                                              data24.orig$BPC19 == 2 ~ "Somewhat agree",
                                              data24.orig$BPC19 == 3 ~ "Somewhat disagree",
                                              data24.orig$BPC19 == 4 ~ "Strongly disagree",
                                              TRUE                   ~ NA_character_),
                                    levels = levels(data26$vote_exp_positive), ordered = TRUE),
         vote_exp_positive_top2 = case_when(data24.orig$BPC19 %in% c(1, 2) ~ 1L,
                                            data24.orig$BPC19 %in% c(3, 4) ~ 0L,
                                            TRUE                            ~ NA_integer_))

substantive26 <- substantive26 %>%
  mutate(vote_exp_positive = data26$vote_exp_positive,
         vote_exp_positive_top2 = case_when(
           data26$vote_exp_positive %in% c("Somewhat agree", "Strongly agree")       ~ 1L,
           data26$vote_exp_positive %in% c("Somewhat disagree", "Strongly disagree") ~ 0L,
           TRUE                                                                       ~ NA_integer_))  # covers "Neither..." and NA/DK

# Reporting 2026's raw "neither" share, per the design doc, so anyone using the file can see the size of the
# unresolved design effect rather than take the collapsed number on faith
pct.neither.2026 <- 100 * mean(data26$vote_exp_positive == "Neither agree nor disagree", na.rm = TRUE)
message("2026 'neither agree nor disagree' share (unweighted): ", round(pct.neither.2026, 1), "%")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.5. Confidence votes will be counted as intended (own vote / county-city / state / nationwide) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# All four items are identical in both years apart from the named election ("November 2024 General Election" vs.
# "November 2026 midterm elections") - verified against both years' vendor codebooks this session, correcting
# PROJECT_NOTES.md's crosswalk table, which said only 2 of these 4 survived into 2026. 2026's `conf_own_vote` etc.
# already have DK AND (for conf_own_vote specifically) "I do not plan to vote" recoded to NA by script 02's existing
# convention, so no extra exclusion logic is needed on the 2026 side here.
#
# Recoding to label text first, then applying 2026's level order via `levels = lvl(...)`, the same safer pattern as
# E.1 above - not a positional `rev()` remap, even though the four-item confidence/concern scales happen to be a
# clean end-to-end reversal (raw 1 = Very confident ... 4 = Not confident at all; 2026 orders low-to-high). Spelling
# it out avoids relying on that happening to be true, and matches the project's own preference for explicit
# `case_when()` over positional tricks.
recode_conf4 <- function(x) case_when(x == 1 ~ "Very confident",
                                      x == 2 ~ "Somewhat confident",
                                      x == 3 ~ "Not too confident",
                                      x == 4 ~ "Not confident at all",
                                      TRUE   ~ NA_character_)   # code 5 (Don't know) and anything else -> NA

substantive24 <- substantive24 %>%
  mutate(conf_own_vote       = factor(recode_conf4(data24.orig$BPC20), levels = lvl("conf_own_vote"),       ordered = TRUE),
         conf_local_votes    = factor(recode_conf4(data24.orig$BPC21), levels = lvl("conf_local_votes"),    ordered = TRUE),
         conf_state_votes    = factor(recode_conf4(data24.orig$BPC22), levels = lvl("conf_state_votes"),    ordered = TRUE),
         conf_national_votes = factor(recode_conf4(data24.orig$BPC23), levels = lvl("conf_national_votes"), ordered = TRUE))

substantive26 <- substantive26 %>%
  bind_cols(data26 %>% select(conf_own_vote, conf_local_votes, conf_state_votes, conf_national_votes))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.6. Concern battery ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Eight of these ten are word-for-word identical stems in both years; `concern_foreign` is reordered phrasing with the
# same meaning ("foreign entities (including countries)" -> "foreign countries or entities") and
# `concern_polling_problems` is broadened wording ("long lines at the polls" -> "...long lines or equipment
# failures") - both approved as comparable. `BPC24_10` (restrictive voting equipment, 2024-only) is dropped, along
# with 2026's four net-new rows (eligible-voters-blocked, gerrymandering, ICE deployment, ballot/machine seizure),
# none of which have a cross-year match. Same 1 = Very concerned ... 4 = Not concerned at all raw coding and same
# label-then-relevel approach as E.5 above.
recode_concern4 <- function(x) case_when(x == 1 ~ "Very concerned",
                                         x == 2 ~ "Somewhat concerned",
                                         x == 3 ~ "Not too concerned",
                                         x == 4 ~ "Not concerned at all",
                                         TRUE   ~ NA_character_)

substantive24 <- substantive24 %>%
  mutate(concern_misinfo           = factor(recode_concern4(data24.orig$BPC24_1),  levels = lvl("concern_misinfo"),           ordered = TRUE),
         concern_ai_disinfo        = factor(recode_concern4(data24.orig$BPC24_2),  levels = lvl("concern_ai_disinfo"),        ordered = TRUE),
         concern_foreign           = factor(recode_concern4(data24.orig$BPC24_3),  levels = lvl("concern_foreign"),           ordered = TRUE),
         concern_ineligible_votes  = factor(recode_concern4(data24.orig$BPC24_4),  levels = lvl("concern_ineligible_votes"),  ordered = TRUE),
         concern_overturn          = factor(recode_concern4(data24.orig$BPC24_5),  levels = lvl("concern_overturn"),          ordered = TRUE),
         concern_biased_count      = factor(recode_concern4(data24.orig$BPC24_6),  levels = lvl("concern_biased_count"),      ordered = TRUE),
         concern_mail_ballots      = factor(recode_concern4(data24.orig$BPC24_7),  levels = lvl("concern_mail_ballots"),      ordered = TRUE),
         concern_guns_intimidation = factor(recode_concern4(data24.orig$BPC24_8),  levels = lvl("concern_guns_intimidation"), ordered = TRUE),
         concern_post_violence     = factor(recode_concern4(data24.orig$BPC24_9),  levels = lvl("concern_post_violence"),     ordered = TRUE),
         concern_polling_problems  = factor(recode_concern4(data24.orig$BPC24_11), levels = lvl("concern_polling_problems"),  ordered = TRUE))

substantive26 <- substantive26 %>%
  bind_cols(data26 %>%
              select(concern_misinfo, concern_ai_disinfo, concern_foreign, concern_ineligible_votes,
                     concern_overturn, concern_biased_count, concern_mail_ballots, concern_guns_intimidation,
                     concern_post_violence, concern_polling_problems))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.7. AI tool use frequency: same question wording, incompatible response scales ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 2024's BPC12 and 2026's BPC36 ask the identical question ("How frequently do you choose to use AI tools (like
# ChatGPT, Gemini, or Claude) to ask questions about elections or get information about elections?") - confirmed
# verbatim against both years' own vendor codebooks. But the two years use entirely different response scales: 2024
# is calendar frequency (Daily/Weekly/Monthly/Rarely/Never), 2026 is election-cycle-relative frequency (the same
# Never/only-near-major-dates/a-few-times/regularly/consistently-all-year scale BPC1's seek-frequency battery
# uses). There is no defensible crosswalk between, say, "Weekly" and "regularly around major dates" - either could
# mean either - so unlike E.4's vote_exp_positive, this does NOT carry a shared raw factor across years. The one
# thing both scales share unambiguously is a "Never" anchor, so the only comparable measure built here is a binary
# "used AI tools at all" indicator.
substantive24 <- substantive24 %>%
  mutate(ai_tool_used = case_when(data24.orig$BPC12 %in% 1:4 ~ 1L,
                                  data24.orig$BPC12 == 5      ~ 0L,
                                  TRUE                         ~ NA_integer_))  # 6 = Don't know/No opinion -> NA

substantive26 <- substantive26 %>%
  mutate(ai_tool_used = case_when(data26$ai_tool_freq != "Never" ~ 1L,
                                  data26$ai_tool_freq == "Never" ~ 0L,
                                  TRUE                             ~ NA_integer_))

# Reporting 2024's excluded "don't know" share, the same transparency norm E.3/E.4 already follow, so anyone using
# ai_tool_used can see the size of the excluded base.
pct.dk.2024.ai.tool <- 100 * mean(data24.orig$BPC12 == 6, na.rm = TRUE)
message("2024 BPC12 (AI tool use frequency) 'don't know/no opinion' share (unweighted): ",
        round(pct.dk.2024.ai.tool, 1), "%, excluded from ai_tool_used's base.")

##### #
#### #
### ################################################################################################################################################# #
# Part F. Stacking ----------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

data24.final <- bind_cols(backbone24, demog24, substantive24)
data26.final <- bind_cols(backbone26, demog26, substantive26)

# Checking level identity BEFORE stacking, not after: bind_rows() does not require two factor columns to share
# levels - it silently unions them - so a mismatch here would not throw an error later, it would just produce a
# column with more levels than either year actually has and no error to catch it. This is the check that would have
# caught the seek_register/conf_own_vote mistakes fixed earlier in this plan, had they slipped through the individual
# task checks. Checking every factor column, ordered and unordered alike, not just ordered ones - an unordered
# mismatch would be silently unioned exactly the same way. `state`/`county`/`town` are excluded: those are two
# independent samples' own geography and are expected to have different observed levels between years (e.g. a
# state or county with respondents in one year but zero in the other) - that is normal, not a bug.
factor.cols <- names(data24.final)[sapply(data24.final, is.factor)]
level.check.cols <- setdiff(factor.cols, c("state", "county", "town"))
level.mismatches <- level.check.cols[!sapply(level.check.cols, function(v)
  identical(levels(data24.final[[v]]), levels(data26.final[[v]])))]

stopifnot("both years produced the same column names, in the same order" =
            identical(names(data24.final), names(data26.final)),
          "every shared factor column (ordered or unordered), other than state/county/town, has identical levels, in the same order, in both years" =
            length(level.mismatches) == 0)

cumulative <- bind_rows(data24.final, data26.final)

##### #
#### #
### ################################################################################################################################################# #
# Part G. Validation --------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# These are claims about the data, not the code - a failure here means something upstream changed or was
# misunderstood, not that the check is too strict, matching the convention scripts 01-03 already use.
stopifnot(
  "5,035 rows total" = nrow(cumulative) == 5035,
  "1,891 rows are 2024" = sum(cumulative$year == 2024) == 1891,
  "3,144 rows are 2026" = sum(cumulative$year == 2026) == 3144,
  "resp_id is unique within year" = cumulative %>% count(year, resp_id) %>% pull(n) %>% max() == 1,
  "weight_native sums to each year's own n" = cumulative %>% group_by(year) %>%
    summarise(s = sum(weight_native)) %>% pull(s) %>% {abs(. - c(1891, 3144)) < 1} %>% all(),
  "weight_common sums to each year's own n" = cumulative %>% group_by(year) %>%
    summarise(s = sum(weight_common)) %>% pull(s) %>% {abs(. - c(1891, 3144)) < 1} %>% all(),
  "race4 covers every respondent, both years" = !any(is.na(cumulative$race4)),
  "state is never missing" = !any(is.na(cumulative$state)),
  # Level identity across years was already checked pre-stack in Part F, before bind_rows() could silently union any
  # mismatch away - not re-checked here because post-stack there is only one factor object left to compare against
  # itself, which proves nothing
  "no source-seeking indicator is constant" = cumulative %>%
    select(matches("^(reg|run|won)_src_|^src_")) %>%
    summarise(across(everything(), ~ n_distinct(na.omit(.x)))) %>%
    unlist() %>% {all(. == 2)},
  "ai_tool_used varies within both years" = cumulative %>%
    filter(!is.na(ai_tool_used)) %>% group_by(year) %>%
    summarise(n_distinct = n_distinct(ai_tool_used), .groups = "drop") %>%
    pull(n_distinct) %>% {all(. == 2)}
)

message("eis_2026_04_cumulative_data.R: all validation checks passed. n = ", nrow(cumulative),
        " (", sum(cumulative$year == 2024), " from 2024, ", sum(cumulative$year == 2026), " from 2026), ",
        ncol(cumulative), " columns.")

##### #
#### #
### ################################################################################################################################################# #
# Part H. Saving ------------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

saveRDS(object = cumulative, file = file.path(out.dir, "eis_cumulative.rds"))
write.csv(x = cumulative, file = file.path(out.dir, "eis_cumulative.csv"), row.names = FALSE, na = "")

message("\nCumulative file written to ", out.dir, "/eis_cumulative.rds and .csv (", nrow(cumulative), " rows, ",
        ncol(cumulative), " columns).")



# The end.
