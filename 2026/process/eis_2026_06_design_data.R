


# EIS_2026_design_data



########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####
####  Title: Preparing the BPC Election Information Survey, 2026 field, for weighted analysis
####
####  Author: Jack Friedman
####
####  Overview: A preliminary companion to 'eis_2026_07_plots_overview.R', the script that will visualize every
####            substantive variable in the 2026 field. This file has two jobs: (1) load script 02's cleaned output
####            and set up the survey::svydesign() object every weighted estimate in this project is built on, and
####            (2) demonstrate, as worked, reusable templates, the calculate-then-chart pattern that script 07 will
####            run at scale across roughly 120 variables and batteries. Run it interactively and adapt the templates
####            below to look at anything the topline scripts do not already cover.
####
####  File Description: Reads 'eis_2026_clean.rds' (script 02's output, NOT the raw vendor delivery — see the note in
####                    Part A.2). Builds one survey design object off the delivered `weight` column, with a design
####                    check alongside it. Defines and demonstrates four estimators (a single proportion, every level
####                    of a factor, a mean, and a whole multiselect battery at once), a bonus subgroup-breakdown
####                    estimator, and five matching ggplot templates — all following the exact svyciprop()/svymean()
####                    pattern already used in 'eis_2026_05_trend_comparison_report.Rmd'. Writes nothing to disk;
####                    this script is for interactive use; it leaves `data`, `var.index`, `svy.design`, and every
####                    function below in the environment to poke at.
####
####            WHICH WEIGHT. This file uses `weight`, the delivered Morning Consult weight for the 2026
####            registered-voter sample — NOT the `w2026` common/raked weight built by
####            'eis_2026_03_common_weights.R'. The raked weight exists to solve one specific problem: making a
####            2024-vs-2026 DIFFERENCE free of the two years' different demographic targets. It is not a
####            more-correct weight for describing 2026 on its own, it depends on script 03 (and the 2024 file)
####            having already been run, and reaching for it here would add that dependency for no benefit.
####            Anything describing the 2026 field by itself — this file, and 'eis_2026_07_plots_overview.R' — uses
####            the delivered `weight`. Only a 2024-vs-2026 comparison should reach for the common weight.
####
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# Loading packages
library(tidyverse)
library(survey)   # for svydesign(), svyciprop(), and svymean() — the project's established weighted-estimation tool


##### #
#### #
### ################################################################################################################################################# #
# Part A. Loading the cleaned data ---------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.1. File paths ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Relative to 2026/ — the R project's working directory whenever its .Rproj is open, regardless of which subfolder a
# script itself is saved in (this file lives in 2026/process/, but that is irrelevant to where relative paths
# resolve). Matches scripts 01-04, all of which assume the same thing.
out.dir <- "output"

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.2. Reading in script 02's cleaned output ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Deliberately NOT the raw vendor CSV. The raw delivery's column names are opaque qids (BPC11_4), roughly half its
# ordinal scales run backwards, and its off-scale codes ("Don't know") are ordinary integers rather than NA — all of
# that is exactly what eis_2026_02_clean_data.R exists to fix. Redoing any of it here would duplicate validated logic
# and risk disagreeing with it.
path.clean <- file.path(out.dir, "eis_2026_clean.rds")
path.index <- file.path(out.dir, "eis_2026_variable_index.csv")

if (!file.exists(path.clean) || !file.exists(path.index)) {
  stop("Expected script 02's output in ", out.dir, ". Missing:\n",
       paste0("  ", c(path.clean, path.index)[!file.exists(c(path.clean, path.index))], collapse = "\n"))
}

data <- readRDS(path.clean)

# One row per column of `data`: var_label, var_type, base (which battery, if any), base_condition (the skip logic),
# and missingness. Used below to look up labels and battery membership by name rather than hard-coding them twice.
var.index <- read.csv(path.index, stringsAsFactors = FALSE)

message("Loaded ", nrow(data), " respondents x ", ncol(data), " columns from eis_2026_clean.rds.")




##### #
#### #
### ################################################################################################################################################# #
# Part B. The survey design object ----------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.1. Declaring the design ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# ids = ~1 declares no clustering — this is a weighted simple random sample, not a multi-stage or stratified design,
# the same declaration script 03 and the trend-comparison report both use. `svy.design` covers every respondent with
# no NAs filtered out; Part C's estimators build their OWN design on an NA-filtered subset internally (matching the
# trend report's own helpers), because which rows are "in base" differs item by item. Use `svy.design` directly only
# for a variable that has no missingness of its own.
svy.design <- svydesign(ids = ~1, weights = ~weight, data = data)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.2. A quick design check ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The design effect (1 + the squared coefficient of variation of the weights) and Kish's effective n — the same two
# numbers script 03 Part C.3 reports for the raked weights. Printed here as a sanity check that `weight` still looks
# like the delivered weight script 02 attached, not a claim this script is testing anything new.
cat(sprintf("\nDesign check on `weight`: n = %d, design effect = %.2f, effective n = %.0f\n\n",
            nrow(data),
            1 + (sd(data$weight) / mean(data$weight))^2,
            sum(data$weight)^2 / sum(data$weight^2)))




##### #
#### #
### ################################################################################################################################################# #
# Part C. Calculation templates -------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# Four reusable estimators, in increasing order of how much of a variable they summarize: one proportion, every level
# of one factor, one mean, and every item of a whole battery — plus a bonus fifth for the most common next question,
# "does this differ by subgroup?" Each takes column names as STRINGS and builds its own filtered svydesign()
# internally, exactly like 'eis_2026_05_trend_comparison_report.Rmd's prop_by_year() / factor_props_by_year() /
# mean_by_year() / prop_table_by_item(), minus their year loop. Filtering NA out of the indicator column BEFORE
# building the design, rather than after, is what puts every percentage on the right denominator — for a battery item
# or a branching variable, NA means out of base, not a valid "no" response.

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.1. A single weighted proportion, with a 95% CI ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# `indicator_col` names a 0/1 numeric column. svyciprop()'s logit method is used rather than svymean() + confint(),
# because it cannot cross 0% or 100%, unlike a plain normal-theory interval on a lopsided proportion.
svy_prop <- function(data, indicator_col, weight_col = "weight") {
  sub <- data %>% filter(!is.na(.data[[indicator_col]]))
  des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
  est <- svyciprop(reformulate(indicator_col), des, method = "logit")
  tibble(pct = as.numeric(est) * 100, ci_low = confint(est)[1] * 100, ci_high = confint(est)[2] * 100, n = nrow(sub))
}

# Worked example: what share of registered voters say the country is headed in the right direction?
svy_prop(data %>% mutate(right_direction = as.numeric(country_direction == "Right Direction")), "right_direction")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.2. Weighted proportions for every level of one factor at once ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Uses the covariance-aware SEs svymean() gives a whole factor at once, not the same as calling svy_prop() once per
# level and ignoring how the levels trade off against each other.
factor_props <- function(data, factor_col, weight_col = "weight") {
  sub <- data %>% filter(!is.na(.data[[factor_col]]))
  des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
  est <- svymean(reformulate(factor_col), des)
  tibble(category = sub(paste0("^", factor_col), "", names(est)),
         pct      = as.numeric(est) * 100,
         ci_low   = confint(est)[, 1] * 100,
         ci_high  = confint(est)[, 2] * 100)
}

# Worked example: the full weighted distribution of confidence that state votes are counted as intended
factor_props(data, "conf_state_votes")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.3. A weighted mean, with a 95% CI ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# For a genuine number (age_years) or an ordinal item's _i integer companion — not for a raw ordinal factor, which
# has no mean until it is converted.
svy_mean <- function(data, value_col, weight_col = "weight") {
  sub <- data %>% filter(!is.na(.data[[value_col]]))
  des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
  est <- svymean(reformulate(value_col), des)
  tibble(estimate = as.numeric(est), ci_low = confint(est)[1], ci_high = confint(est)[2], n = nrow(sub))
}

# Worked example: mean age among registered voters
svy_mean(data, "age_years")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.4. Weighted proportions across a whole battery at once ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# `cols` is a vector of 0/1 column names sharing a base condition (e.g. every BPC2 item). Just svy_prop() mapped over
# the battery's columns, stacked long with one row per item.
battery_props <- function(data, cols, weight_col = "weight") {
  map_dfr(cols, function(col) svy_prop(data, col, weight_col) %>% mutate(item = col, .before = 1))
}

# Worked example: the master "where do you get election news" battery (BPC2), every item at once. The multiselect
# columns are factors with levels "0"/"1", not numeric — converting first, the same step
# eis_2026_05_trend_comparison_report.Rmd takes before calling its own version of this estimator.
src_cols <- var.index %>% filter(base == "BPC2", var_type == "multiselect") %>% pull(var_name)
data_bin <- data %>% mutate(across(all_of(src_cols), ~ as.numeric(as.character(.x))))

battery_props(data_bin, src_cols) %>% arrange(desc(pct))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.5. Bonus: a weighted proportion broken out by a subgroup ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Not needed for the topline sweep in script 07, but the single most common next question when looking at any one
# number — "does this differ by party?" — so it earns a template here. `by_col` should already be a clean factor
# (pid3, gender, age4, ...); rows missing it are dropped rather than folded into an "(missing)" category.
svy_prop_by <- function(data, indicator_col, by_col, weight_col = "weight") {
  data %>%
    filter(!is.na(.data[[by_col]])) %>%
    group_by(.data[[by_col]]) %>%
    group_modify(~ svy_prop(.x, indicator_col, weight_col)) %>%
    ungroup()
}

# Worked example: "right direction" by party identification
svy_prop_by(data %>% mutate(right_direction = as.numeric(country_direction == "Right Direction")),
            "right_direction", "pid3")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.6. Bonus: the full distribution of one factor, broken out by subgroup ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The multi-level version of C.5 — every category of `factor_col`, crossed with every level of `by_col`, e.g. the
# full four-point confidence distribution broken out by party ID rather than collapsed to one "% confident" number.
factor_props_by <- function(data, factor_col, by_col, weight_col = "weight") {
  data %>%
    filter(!is.na(.data[[by_col]])) %>%
    group_by(.data[[by_col]]) %>%
    group_modify(~ factor_props(.x, factor_col, weight_col)) %>%
    ungroup()
}

# Worked example: confidence that votes in one's own state will be counted as intended, by party ID
factor_props_by(data, "conf_state_votes", "pid3")




##### #
#### #
### ################################################################################################################################################# #
# Part D. Visualization templates ------------------------------------------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# Five ggplot templates, one per shape of estimate Part C produces above. Same theme_minimal() base and the same
# validated blue as 'eis_2026_05_trend_comparison_report.Rmd' — there is only one series in any of these charts, not
# a year comparison, so YEAR_COLORS itself does not apply, but the single blue it uses for 2026 is reused here as the
# project's neutral default.

theme_set(theme_minimal(base_size = 12))
BAR_COLOR <- "#2a78d6"

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.1. Bar chart: weighted % per category, with a 95% CI ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Takes factor_props()'s own output. Pass `order` (a vector of category names, low to high) to keep an ordinal item's
# scale order; leave it NULL to sort by size instead, which is usually right for a nominal item.
plot_prop_bar <- function(props, x_label = "Weighted %", order = NULL) {
  props <- if (is.null(order)) props %>% mutate(category = fct_reorder(category, pct))
           else                props %>% mutate(category = factor(category, levels = order))
  ggplot(props, aes(x = pct, y = category)) +
    geom_col(fill = BAR_COLOR, width = 0.65) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.9) +
    scale_x_continuous(expand = expansion(mult = c(0, .05))) +
    labs(x = x_label, y = NULL)
}

# Worked example
plot_prop_bar(factor_props(data, "conf_state_votes"),
              order   = levels(data$conf_state_votes),
              x_label = "Weighted % confident votes in own state will be counted as intended")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.2. Single 100%-stacked bar: the full distribution of one ordinal item ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The other way to show factor_props()'s output — one bar, segmented by category, rather than one row per category.
# Reads better than D.1 when the point is the SHAPE of the distribution rather than any one category's exact CI.
# `colors` must be a named vector covering every category, light-to-dark for a true ordinal scale.
plot_prop_stack <- function(props, colors, x_label = "Weighted %") {
  props <- props %>% mutate(category = factor(category, levels = names(colors)))
  ggplot(props, aes(x = pct, y = "", fill = category)) +
    geom_col(width = 0.5) +
    scale_fill_manual(values = colors, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, .02))) +
    labs(x = x_label, y = NULL) +
    theme(legend.position = "top", panel.grid = element_blank(), axis.text.y = element_blank())
}

# Worked example: a light-to-dark ramp for the four confidence levels, low to high
CONF_RAMP <- setNames(c("#b7d3f6", "#6da7ec", "#2a78d6", "#0d366b"), levels(data$conf_state_votes))
plot_prop_stack(factor_props(data, "conf_state_votes"), colors = CONF_RAMP,
                x_label = "Weighted % by confidence votes in own state will be counted as intended")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.3. Horizontal bar chart for a whole battery, sorted by size ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Takes battery_props()'s output. `item_labels` is an optional named vector (var_name -> display text) to relabel
# the raw column names, e.g. the lookup built in the worked example below.
plot_battery_bar <- function(battery, item_labels = NULL, x_label = "Weighted %") {
  if (!is.null(item_labels)) battery <- battery %>% mutate(item = unname(item_labels[item]))
  battery <- battery %>% mutate(item = fct_reorder(item, pct))
  ggplot(battery, aes(x = pct, y = item)) +
    geom_col(fill = BAR_COLOR, width = 0.65) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.9) +
    scale_x_continuous(expand = expansion(mult = c(0, .05))) +
    labs(x = x_label, y = NULL)
}

# Worked example, with each item tag swapped for its display text — pulled from the variable index's own var_label
# ("Main sources ... : Local or regional television" -> "Local or regional television") rather than retyped by hand.
src_labels_lookup <- var.index %>%
  filter(var_name %in% src_cols) %>%
  transmute(var_name, display = str_trim(str_extract(var_label, "(?<=: ).*"))) %>%
  deframe()

plot_battery_bar(battery_props(data_bin, src_cols), item_labels = src_labels_lookup,
                  x_label = "Weighted % naming this a main source for election news and information")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.4. Weighted histogram for a numeric variable ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

plot_weighted_hist <- function(data, value_col, weight_col = "weight", binwidth = NULL, x_label = value_col) {
  ggplot(data, aes(x = .data[[value_col]], weight = .data[[weight_col]])) +
    geom_histogram(fill = BAR_COLOR, binwidth = binwidth, boundary = 0, color = "white", linewidth = 0.2) +
    labs(x = x_label, y = "Weighted count")
}

# Worked example
plot_weighted_hist(data, "age_years", binwidth = 5, x_label = "Age (years)")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.5. Bonus: chart for the subgroup breakdown ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

plot_prop_by <- function(props_by, by_col, x_label = "Weighted %") {
  ggplot(props_by, aes(x = pct, y = fct_reorder(.data[[by_col]], pct))) +
    geom_col(fill = BAR_COLOR, width = 0.65) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.9) +
    scale_x_continuous(expand = expansion(mult = c(0, .05))) +
    labs(x = x_label, y = NULL)
}

# Worked example: "right direction" by party identification
plot_prop_by(svy_prop_by(data %>% mutate(right_direction = as.numeric(country_direction == "Right Direction")),
                          "right_direction", "pid3"),
             by_col = "pid3", x_label = "Weighted % saying the country is headed in the right direction")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.6. Bonus: one 100%-stacked bar per subgroup ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The grouped version of D.2 — one row per level of `by_col` instead of one bar total, each segmented by category.
# Takes factor_props_by()'s output directly.
plot_prop_stack_by <- function(props_by, by_col, colors, x_label = "Weighted %") {
  props_by <- props_by %>% mutate(category = factor(category, levels = names(colors)))
  ggplot(props_by, aes(x = pct, y = .data[[by_col]], fill = category)) +
    geom_col(width = 0.65) +
    scale_fill_manual(values = colors, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, .02))) +
    labs(x = x_label, y = NULL) +
    theme(legend.position = "top", panel.grid = element_blank())
}

# Worked example: confidence that votes in one's own state will be counted as intended, by party ID — the same
# CONF_RAMP colors defined for D.2's single-bar version of this same item, reused here rather than redeclared.
plot_prop_stack_by(factor_props_by(data, "conf_state_votes", "pid3"), by_col = "pid3", colors = CONF_RAMP,
                    x_label = "Weighted % by confidence votes in own state will be counted as intended")




##### #
#### #
### ################################################################################################################################################# #
# Part E. The same estimates, with no custom functions --------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# Part C wrapped every one of these in a small function, on the reasoning that the same handful of steps repeats
# across roughly 120 variables in script 07 and that 'eis_2026_05_trend_comparison_report.Rmd' already set that
# precedent. This part is the same six worked examples with nothing wrapped: E.1-E.3 call `survey::` directly, the
# same package script 03 and the trend-comparison report already use everywhere; E.4 calls `srvyr`, the tidyverse-
# native interface to the same package. Every line here is something you could type at the console — no function
# defined in this part is reused anywhere else in this script.

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.1. Base `survey`: a single proportion ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Compare to C.1's svy_prop(). Three lines instead of one call, because nothing here pre-builds the filtered design
# or unpacks confint() for you — that bookkeeping is the entire body of svy_prop().
direction.data <- data %>%
  mutate(right_direction = as.numeric(country_direction == "Right Direction")) %>%
  filter(!is.na(right_direction))
direction.design <- svydesign(ids = ~1, weights = ~weight, data = direction.data)
direction.est <- svyciprop(~right_direction, direction.design, method = "logit")

direction.est                # the point estimate
confint(direction.est)       # its 95% CI, as a 1x2 matrix

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.2. Base `survey`: every level of a factor at once ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Compare to C.2's factor_props(). svymean() on a factor already returns one coefficient per level — this is the
# exact call factor_props() makes internally, just left as a svystat object instead of reshaped into a tibble.
conf.design <- svydesign(ids = ~1, weights = ~weight, data = data %>% filter(!is.na(conf_state_votes)))
conf.est <- svymean(~conf_state_votes, conf.design)

conf.est                     # one row per level
confint(conf.est)            # one row per level, 2.5% / 97.5%

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.3. Base `survey`: a mean, and a whole battery in one call ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Compare to C.3's svy_mean().
age.design <- svydesign(ids = ~1, weights = ~weight, data = data %>% filter(!is.na(age_years)))
svymean(~age_years, age.design)

# Compare to C.4's battery_props(). svymean() takes a formula with several terms, not just one, and returns one
# coefficient per term — so the whole BPC2 battery is a single call, not a loop, as long as reading the result as a
# named vector (rather than a sorted tibble with an item column) is good enough.
src.design <- svydesign(ids = ~1, weights = ~weight, data = data_bin)
src.est <- svymean(reformulate(src_cols), src.design)

sort(coef(src.est), decreasing = TRUE)    # the estimates, sorted — still no tibble, no loop
confint(src.est)                          # every item's CI, one row per item

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.4. `srvyr`: the tidyverse-native equivalent ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# srvyr wraps a survey design as a `tbl_svy`; every usual dplyr verb after that — filter(), group_by(), summarise()
# — calls survey's own estimators underneath. This is where it earns its keep relative to base `survey`: C.5 and C.6
# above each needed a hand-written group_modify() loop, where here the grouping is just group_by(), like any other
# dplyr pipeline. The one thing to get right: survey_mean()'s CI defaults to the plain (unbounded) normal-theory
# interval, so `proportion = TRUE, prop_method = "logit"` has to be added explicitly to match svyciprop()'s bounded
# interval, which is what every proportion elsewhere in this project uses.
library(srvyr)

svy.tbl <- data %>% as_survey_design(ids = 1, weights = weight)

# Compare to C.1's svy_prop() / E.1 above
svy.tbl %>%
  filter(!is.na(country_direction)) %>%
  summarise(pct = survey_mean(country_direction == "Right Direction",
                               proportion = TRUE, prop_method = "logit", vartype = "ci") * 100)

# Compare to C.5's svy_prop_by(): group_by() replaces the group_modify() loop entirely
svy.tbl %>%
  filter(!is.na(country_direction), !is.na(pid3)) %>%
  group_by(pid3) %>%
  summarise(pct = survey_mean(country_direction == "Right Direction",
                               proportion = TRUE, prop_method = "logit", vartype = "ci") * 100)

# Compare to C.6's factor_props_by(). Unlike base survey's svymean(~factor), srvyr's survey_mean() REFUSES a bare
# factor argument ("Factor not allowed in survey functions, should be used as a grouping variable" — confirmed by
# actually running it before writing this comment). The idiom instead is to put the factor being summarized into
# group_by() too: with group_by(pid3, conf_state_votes), a bare survey_mean() — no argument at all — gives each
# combination's share OF ITS OWN pid3 group, so the four confidence levels still sum to 100% within each party. The
# result is identical to factor_props_by(data, "conf_state_votes", "pid3") above, row for row.
svy.tbl %>%
  filter(!is.na(conf_state_votes), !is.na(pid3)) %>%
  group_by(pid3, conf_state_votes) %>%
  summarise(pct = survey_mean(vartype = "ci") * 100, .groups = "drop")




message("\neis_2026_06_design_data.R ready: `data`, `var.index`, and `svy.design` are in the environment, along with ",
        "the svy_prop() / factor_props() / svy_mean() / battery_props() / svy_prop_by() / factor_props_by() ",
        "estimators and the plot_prop_bar() / plot_prop_stack() / plot_battery_bar() / plot_weighted_hist() / ",
        "plot_prop_by() / plot_prop_stack_by() charts demonstrated above. Swap in any other column name from ",
        "`var.index` to look at something else.\n")




# The end.
