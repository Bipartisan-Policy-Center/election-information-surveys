# EIS 2026 AI, Confidence, and Concern Report Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build one new plain R script (`2026/process/eis_2026_09_ai_confidence_concern_report.R`) that visualizes 2026's weighted AI, voting-experience, confidence, concern, and noncitizen-voting/access-vs-integrity/USPS-policy results (BPC35-BPC48), adding a 2024-vs-2026 comparison wherever one exists.

**Architecture:** A single self-contained `.R` script (no Rmd, no knitting, nothing written to disk), reading two existing pipeline outputs (`eis_2026_clean.rds`, `eis_cumulative.rds`). Every chart is produced by an explicit `print()` call so it displays reliably regardless of how the script is run. Every statistical helper and plot builder is ported from scripts 05/06/07/08 — nothing here is a new estimator.

**Tech Stack:** R 4.5, tidyverse, `survey`. No other packages.

## Global Constraints

- Every 2026-current-state and by-party estimate uses the `weight` column on `data`; every 2024-vs-2026 estimate uses `weight_common` on `cum` — never mixed within one chart.
- **Every chart-producing expression is wrapped in an explicit `print()` call — never a bare top-level expression.** Confirmed empirically before writing this plan: a bare top-level expression only auto-prints under direct `Rscript file.R` execution or truly interactive (line-by-line) use. It does **not** auto-print under plain `source("file.R")` — which is what RStudio's plain "Source" button uses by default — so a bare `ggplot(...)` call would silently produce nothing in that case. `print()` sidesteps this entirely: it runs identically in every execution mode.
- Proportions get a 95% CI via `survey::svyciprop()` (logit method) or `survey::svymean()` — never a plain normal-theory interval on a proportion.
- No new R packages beyond `tidyverse` and `survey`.
- No pipeline file (scripts 01-04) is modified. No file is written to disk by this script — every chart goes to the current graphics device only.
- Every chart's `n` is printed via `cat()` immediately after the chart, matching the project-wide captioning convention.
- No `var.index` lookups — every item here is a single ordinal/nominal question, not a multiselect battery, so display labels are hand-typed per section.

---

## Task 1: Script skeleton, data loading, and every helper function

**Files:**
- Create: `2026/process/eis_2026_09_ai_confidence_concern_report.R`
- Test: run directly with `Rscript`, and with an ad hoc `Rscript -e` snippet

**Interfaces:**
- Produces (for every later task): `data` (2026, from `eis_2026_clean.rds`), `cum` (2024+2026, from `eis_cumulative.rds`, `year` as a factor); colors `BAR_COLOR`, `YEAR_COLORS`, `AI_STATUS_COLORS`, `PID_COLORS`, `RAMP_4PT`; functions `svy_prop(data, indicator_col, weight_col = "weight")`, `factor_props(data, factor_col, weight_col = "weight")`, `battery_props(data, cols, weight_col = "weight")`, `svy_prop_by(data, indicator_col, by_col, weight_col = "weight")`, `prop_by_year(data, indicator_col, weight_col = "weight_common")`, `mean_by_year(data, value_col, weight_col = "weight_common")`, `factor_props_by_year(data, factor_col, weight_col = "weight_common")`, `order_bottom_anchors(items, pct, anchors = c("Don't know", "Not sure"))`, `plot_prop_bar(props, x_label = "Weighted %", order = NULL, anchors = c("Don't know", "Not sure"))`, `plot_prop_stack_by(props_by, by_col, colors, x_label = "Weighted %")`, `plot_battery_bar(battery, item_labels = NULL, x_label = "Weighted %")`, `plot_battery_dodge_party(props_by, x_label = "Weighted %")`, `plot_dumbbell(df, x_label, show_ci = TRUE, sort_desc_change = TRUE)`, `plot_stacked_props(df, colors, x_label = "Weighted %")`.

- [ ] **Step 1: Write the file**

```r



# EIS_2026_ai_confidence_concern_report



########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####
####  Title: A weighted look at the AI, confidence, and concern questions in the BPC Election Information Survey, 2026 field
####
####  Author: Jack Friedman
####
####  Overview: A plain-script companion to 'eis_2026_08_information_sources_report.Rmd', covering everything AFTER
####            the election-information-source questions: AI use and attitudes (BPC35-38), voting experience
####            (BPC39), confidence votes will be counted as intended (BPC40-43), concern about election problems
####            (BPC44, broken out by party ID), and the trailing noncitizen-voting / access-vs-integrity / USPS
####            policy items (BPC45-48). Deliberately a .R script, not an .Rmd — meant to be run directly
####            (`Rscript` or RStudio's "Source") or interactively; every chart is an explicit print() call rather
####            than a bare top-level expression, so it displays reliably either way (bare top-level auto-print is
####            NOT reliable under plain source(), which is what RStudio's "Source" button uses by default).
####
####  File Description: Part A loads eis_2026_clean.rds ('data', 2026 only, weight = `weight`) and
####            eis_cumulative.rds ('cum', 2024+2026, weight = `weight_common`) — never mixed within one chart. Part
####            B defines every estimator and plot builder used below, all ported from scripts 05-08 rather than
####            reinvented. Parts C-G each cover one topic, in survey order.
####
####            WHICH WEIGHT. Same rule as every other script: `weight` describes the 2026 field on its own (every
####            2026-current-state and by-party chart below); `weight_common` is only for a 2024-vs-2026 difference
####            (the four "ported verbatim" trend charts below). See 'eis_2026_06_design_data.R' for the fuller
####            argument.
####
####  Output: none written to disk — every chart prints to the current graphics device. Run directly or
####            interactively.
####
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# Loading packages
library(tidyverse)
library(survey)   # for svydesign(), svyciprop(), and svymean()


##### #
#### #
### ################################################################################################################################################# #
# Part A. Loading the data ------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.1. File paths ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Relative to 2026/ — the R project's working directory. Matches every other script in process/.
out.dir <- "output"

path.clean <- file.path(out.dir, "eis_2026_clean.rds")
path.cum   <- file.path(out.dir, "eis_cumulative.rds")

if (!file.exists(path.clean) || !file.exists(path.cum)) {
  stop("Expected script 02's and script 04's output in ", out.dir, ". Missing:\n",
       paste0("  ", c(path.clean, path.cum)[!file.exists(c(path.clean, path.cum))], collapse = "\n"))
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.2. Reading in the two datasets ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 2026 only — every 2026-current-state and by-party chart below (Parts C.1, E.1, F.1, F.2, F.4, G) uses this on
# `weight`. No var.index needed: every item in this report is a single ordinal/nominal question, not a multiselect
# battery, so display labels are hand-typed per topic below rather than looked up.
data <- readRDS(path.clean)

# 2024 + 2026 stacked, common-weighted — only the four charts explicitly marked "ported verbatim" below (Parts C.2,
# D.1, E.2, F.3) use this, on `weight_common`.
cum <- readRDS(path.cum) %>% mutate(year = factor(year, levels = c(2024, 2026)))

message("Loaded ", nrow(data), " respondents x ", ncol(data), " columns from eis_2026_clean.rds.")
message("Loaded ", nrow(cum), " respondent-years (", sum(cum$year == 2024), " from 2024, ", sum(cum$year == 2026),
        " from 2026) from eis_cumulative.rds.")




##### #
#### #
### ################################################################################################################################################# #
# Part B. Estimators and plot builders -------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

theme_set(theme_minimal(base_size = 12))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.1. Colors ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

BAR_COLOR   <- "#2a78d6"   # the project's neutral single-series default (scripts 05-08)
YEAR_COLORS <- c("2024" = "#2a78d6", "2026" = "#eb6834")                              # ported from the 05 report
AI_STATUS_COLORS <- c("Bad" = "#d03b3b", "Neither" = "#898781", "Good" = "#0ca30c")   # ported from the 05 report
PID_COLORS  <- c(Dem = "#2a78d6", Ind = "#898781", Rep = "#d03b3b")                   # ported from report 08

# One shared light-to-dark blue ramp for a 4-level ordinal item's full distribution (confidence, concern) — the
# same ramp script 06 already uses as CONF_RAMP, applied here with each item's own actual level text via setNames().
RAMP_4PT <- c("#b7d3f6", "#6da7ec", "#2a78d6", "#0d366b")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.2. Estimators on `data` (2026 only) — ported from scripts 06/07 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# `indicator_col` names a 0/1 numeric column. svyciprop()'s logit method is used rather than svymean() + confint(),
# because it cannot cross 0% or 100%, unlike a plain normal-theory interval on a lopsided proportion.
svy_prop <- function(data, indicator_col, weight_col = "weight") {
  sub <- data %>% filter(!is.na(.data[[indicator_col]]))
  des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
  est <- svyciprop(reformulate(indicator_col), des, method = "logit")
  tibble(pct = as.numeric(est) * 100, ci_low = confint(est)[1] * 100, ci_high = confint(est)[2] * 100, n = nrow(sub))
}

# Uses the covariance-aware SEs svymean() gives a whole factor at once.
factor_props <- function(data, factor_col, weight_col = "weight") {
  sub <- data %>% filter(!is.na(.data[[factor_col]]))
  des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
  est <- svymean(reformulate(factor_col), des)
  tibble(category = sub(paste0("^", factor_col), "", names(est)),
         pct      = as.numeric(est) * 100,
         ci_low   = confint(est)[, 1] * 100,
         ci_high  = confint(est)[, 2] * 100,
         n        = nrow(sub))
}

# `cols` is a vector of 0/1 column names. Just svy_prop() mapped over them, stacked long with one row per item.
battery_props <- function(data, cols, weight_col = "weight") {
  map_dfr(cols, function(col) svy_prop(data, col, weight_col) %>% mutate(item = col, .before = 1))
}

# `by_col` should already be a clean factor (pid3, ...); rows missing it are dropped rather than folded into an
# "(missing)" category. Ported from script 06 Part C.5.
svy_prop_by <- function(data, indicator_col, by_col, weight_col = "weight") {
  data %>%
    filter(!is.na(.data[[by_col]])) %>%
    group_by(.data[[by_col]]) %>%
    group_modify(~ svy_prop(.x, indicator_col, weight_col)) %>%
    ungroup()
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.3. Estimators on `cum` (2024 vs. 2026) — ported unchanged from the 05 report ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

prop_by_year <- function(data, indicator_col, weight_col = "weight_common") {
  map_dfr(levels(data$year), function(yr) {
    sub <- data %>% filter(year == yr, !is.na(.data[[indicator_col]]))
    des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
    est <- svyciprop(reformulate(indicator_col), des, method = "logit")
    tibble(year = yr, pct = as.numeric(est) * 100,
           ci_low = confint(est)[1] * 100, ci_high = confint(est)[2] * 100, n = nrow(sub))
  })
}

mean_by_year <- function(data, value_col, weight_col = "weight_common") {
  map_dfr(levels(data$year), function(yr) {
    sub <- data %>% filter(year == yr, !is.na(.data[[value_col]]))
    des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
    est <- svymean(reformulate(value_col), des)
    tibble(year = yr, estimate = as.numeric(est),
           ci_low = confint(est)[1], ci_high = confint(est)[2], n = nrow(sub))
  })
}

factor_props_by_year <- function(data, factor_col, weight_col = "weight_common") {
  map_dfr(levels(data$year), function(yr) {
    sub <- data %>% filter(year == yr, !is.na(.data[[factor_col]]))
    des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
    est <- svymean(reformulate(factor_col), des)
    tibble(year = yr,
           category = sub(paste0("^", factor_col), "", names(est)),
           pct = as.numeric(est) * 100,
           ci_low = confint(est)[, 1] * 100,
           ci_high = confint(est)[, 2] * 100)
  })
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.4. Plot builders on `data` (2026 only / by party) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Orders `items` by `pct` ascending (a horizontal bar chart draws factor level 1 at the bottom, so ascending pct
# puts the biggest bar at the top), except any name in `anchors`: a non-answer isn't a substantive position, so it
# always sorts below every real item regardless of its own %. Ported from report 08's most recent revision.
order_bottom_anchors <- function(items, pct, anchors = c("Don't know", "Not sure")) {
  is_anchor <- items %in% anchors
  c(items[is_anchor][order(pct[is_anchor])], items[!is_anchor][order(pct[!is_anchor])])
}

# `order`: NULL sorts by size, pinning any name in `anchors` to the bottom regardless of its own %; a vector of
# levels low-to-high keeps an ordinal item's own scale order instead (anchors is ignored in that branch). Ported
# from script 06 Part D.1 (`plot_prop_bar`), generalized to default to report 08's anchor-pinning behavior
# (`plot_categorical()`/`order_bottom_anchors()`) rather than a plain fct_reorder() — the two functions were doing
# the same job with different defaults, so this keeps one version instead of carrying both.
plot_prop_bar <- function(props, x_label = "Weighted %", order = NULL, anchors = c("Don't know", "Not sure")) {
  props <- if (is.null(order)) props %>% mutate(category = factor(category, levels = order_bottom_anchors(category, pct, anchors)))
           else                props %>% mutate(category = factor(category, levels = order))
  ggplot(props, aes(x = pct, y = category)) +
    geom_col(fill = BAR_COLOR, width = 0.65) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.9) +
    scale_x_continuous(expand = expansion(mult = c(0, .06))) +
    labs(x = x_label, y = NULL)
}

# One 100%-stacked bar per item — the full distribution of several ordinal items at once, not collapsed to a single
# cutoff. Same construction the 05 report already uses for its own seek-frequency chart (there faceted by `year`;
# here by `item` instead). `colors` must be a named vector covering every level of `category`, in display order.
plot_prop_stack_by <- function(props_by, by_col, colors, x_label = "Weighted %") {
  props_by <- props_by %>% mutate(category = factor(category, levels = names(colors)))
  ggplot(props_by, aes(x = pct, y = .data[[by_col]], fill = category)) +
    geom_col(width = 0.65) +
    scale_fill_manual(values = colors, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, .02))) +
    labs(x = x_label, y = NULL) +
    theme(legend.position = "top", panel.grid = element_blank())
}

# Horizontal bar chart for a whole set of independent 0/1 items, sorted by size. Takes battery_props()'s output.
# `item_labels` is an optional named vector (raw column name -> display text). Ported from script 06/07.
plot_battery_bar <- function(battery, item_labels = NULL, x_label = "Weighted %") {
  if (!is.null(item_labels)) battery <- battery %>% mutate(item = unname(item_labels[item]))
  battery <- battery %>% mutate(item = fct_reorder(item, pct))
  ggplot(battery, aes(x = pct, y = item)) +
    geom_col(fill = BAR_COLOR, width = 0.65) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.9) +
    scale_x_continuous(expand = expansion(mult = c(0, .06))) +
    labs(x = x_label, y = NULL)
}

# `props_by` has one row per (item, pid3): item, pid3, pct, ci_low, ci_high, n. Dodged, not stacked — each item's
# "concerned" share is independent of the others, so the three parties' bars for one item do not sum to anything
# meaningful. No CI whiskers — three dodged bars per item already crowd the chart. Ported unchanged from report 08.
plot_battery_dodge_party <- function(props_by, x_label = "Weighted %") {
  item_means <- props_by %>% group_by(item) %>% summarise(m = mean(pct), .groups = "drop")
  ord <- order_bottom_anchors(item_means$item, item_means$m)
  props_by <- props_by %>% mutate(item = factor(item, levels = ord))
  ggplot(props_by, aes(x = pct, y = item, fill = pid3)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    scale_fill_manual(values = PID_COLORS, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, .06))) +
    labs(x = x_label, y = NULL) +
    theme(legend.position = "top")
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.5. Plot builders on `cum` (2024 vs. 2026) — ported unchanged from the 05 report ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# A dot per year, joined by a line, with a 95% CI whisker.
plot_dumbbell <- function(df, x_label, show_ci = TRUE, sort_desc_change = TRUE) {
  wide <- df %>% select(item, year, pct) %>% pivot_wider(names_from = year, values_from = pct, names_prefix = "y")
  ord <- if (sort_desc_change) {
    wide %>% mutate(chg = abs(y2026 - y2024)) %>% arrange(chg) %>% pull(item)
  } else {
    wide %>% arrange(y2026) %>% pull(item)
  }
  df <- df %>% mutate(item = factor(item, levels = ord))

  p <- ggplot(df, aes(x = pct, y = item)) +
    geom_line(aes(group = item), color = "grey75", linewidth = 0.6)
  if (show_ci) {
    p <- p + geom_linerange(aes(xmin = ci_low, xmax = ci_high, color = year), linewidth = 2, alpha = 0.35)
  }
  yr_levels <- names(YEAR_COLORS)
  p <- p +
    geom_point(data = df %>% filter(year == yr_levels[1]), aes(color = year), size = 4.2, shape = 1, stroke = 1.4) +
    geom_point(data = df %>% filter(year == yr_levels[2]), aes(color = year), size = 3, shape = 16)
  p + scale_color_manual(values = YEAR_COLORS, name = NULL) +
    labs(x = x_label, y = NULL) +
    theme(legend.position = "top", panel.grid.minor = element_blank())
}

# One stacked bar per year, one segment per response category.
plot_stacked_props <- function(df, colors, x_label = "Weighted %") {
  df <- df %>% mutate(category = factor(category, levels = names(colors)))
  ggplot(df, aes(x = pct, y = year, fill = category)) +
    geom_col(width = 0.65) +
    facet_wrap(~item, ncol = 1, strip.position = "top") +
    scale_fill_manual(values = colors, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, .02))) +
    labs(x = x_label, y = NULL) +
    theme(legend.position = "top", panel.grid = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"))
}

message("\neis_2026_09_ai_confidence_concern_report.R: setup complete. `data` and `cum` are loaded; every estimator ",
        "and plot builder from Part B is in the environment. Continue running Parts C-G below.")
```

- [ ] **Step 2: Run it and check the load messages**

```bash
cd 2026 && Rscript process/eis_2026_09_ai_confidence_concern_report.R
```

Expected (exact — confirmed by running this file's Part A/B while writing this plan):

```
Loaded 3144 respondents x 609 columns from eis_2026_clean.rds.
Loaded 5035 respondent-years (1891 from 2024, 3144 from 2026) from eis_cumulative.rds.

eis_2026_09_ai_confidence_concern_report.R: setup complete. `data` and `cum` are loaded; every estimator and plot builder from Part B is in the environment. Continue running Parts C-G below.
```

(Package-attach messages from `library(tidyverse)`/`library(survey)` will also print above these lines — ignore them, that's normal.)

- [ ] **Step 3: Spot-check two helpers against known-good values**

```bash
cd 2026 && Rscript -e '
source("process/eis_2026_09_ai_confidence_concern_report.R")
cat("\n--- factor_props(data, \"conf_own_vote\") ---\n")
print(factor_props(data, "conf_own_vote"))
cat("\n--- svy_prop_by() on concern_misinfo_top2 by pid3 ---\n")
data2 <- data %>% mutate(concern_misinfo_top2 = as.numeric(as.integer(concern_misinfo) >= 3))
print(svy_prop_by(data2, "concern_misinfo_top2", "pid3"))
'
```

Expected (values should match to within floating-point noise):

```
--- factor_props(data, "conf_own_vote") ---
# A tibble: 4 × 5
  category               pct ci_low ci_high     n
  <chr>                <dbl>  <dbl>   <dbl> <int>
1 Not confident at all  4.37   3.50    5.25  2829
2 Not too confident    12.0   10.5    13.6   2829
3 Somewhat confident   36.2   34.0    38.3   2829
4 Very confident       47.4   45.2    49.7   2829

--- svy_prop_by() on concern_misinfo_top2 by pid3 ---
# A tibble: 3 × 5
  pid3    pct ci_low ci_high     n
  <fct> <dbl>  <dbl>   <dbl> <int>
1 Dem    74.3   71.0    77.4  1108
2 Ind    72.2   68.0    76.0   737
3 Rep    71.3   67.9    74.5  1051
```

If these don't match, stop and re-check Part B before continuing — every later task depends on these functions being correct.

- [ ] **Step 4: Commit**

```bash
git add 2026/process/eis_2026_09_ai_confidence_concern_report.R
git commit -m "Add skeleton, data loading, and helper functions for the AI/confidence/concern report

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

## Task 2: Part C — AI (BPC35-38)

**Files:**
- Modify: `2026/process/eis_2026_09_ai_confidence_concern_report.R` (append after Part B)

**Interfaces:**
- Consumes: `data`, `cum`, `factor_props()`, `mean_by_year()`, `factor_props_by_year()`, `plot_prop_bar()`, `plot_dumbbell()`, `plot_stacked_props()`, `AI_STATUS_COLORS` (all from Task 1)
- Produces: nothing later tasks depend on — Part C is self-contained.

- [ ] **Step 1: Append Part C**

```r



##### #
#### #
### ################################################################################################################################################# #
# Part C. AI (BPC35-38) ------------------------------------------------------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.1. Three single-item AI questions (BPC35-37) — new, 2026 only, no 2024 counterpart ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

ai_single_labels <- c(ai_prevalence  = "How much election info seen is AI-generated",
                       ai_tool_freq   = "How often uses AI tools such as chatbots",
                       ai_detect_conf = "Confidence detecting AI-generated election content")

for (col in names(ai_single_labels)) {
  props <- factor_props(data, col)
  print(plot_prop_bar(props, order = levels(data[[col]]), x_label = ai_single_labels[[col]]))
  cat("n =", format(props$n[1], big.mark = ","), "\n")
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.2. The AI good/bad matrix (BPC38, 10 items) — ported verbatim, 2024 vs. 2026 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Ported from eis_2026_05_trend_comparison_report.Rmd's "Attitudes Toward AI Use in Elections" section. Already
# shows 2026 (as one dot / one year's stacked bar) alongside 2024, so no separate 2026-only chart is built for
# these 10 items — that would just duplicate what's already on the page.
ai_labels <- c(
  ai_ok_voter_candidate_info   = "Voters use AI chatbots for candidate/issue info",
  ai_ok_voter_how_to_vote      = "Voters use AI chatbots to learn how to cast a ballot",
  ai_ok_voter_how_to_register  = "Voters use AI chatbots to learn how to register",
  ai_ok_voter_decide           = "Voters use AI chatbots to decide who to vote for",
  ai_ok_voter_values_align     = "Voters use AI chatbots to compare candidates' positions to their values",
  ai_ok_campaign_undisclosed   = "Campaigns use undisclosed AI-generated content",
  ai_ok_campaign_disclosed     = "Campaigns use disclosed AI-generated content",
  ai_ok_cand_photo_edit        = "Candidates use AI to edit photos/videos",
  ai_ok_cand_microtarget       = "Candidates use AI to microtarget ads",
  ai_ok_cand_answer_questions  = "Candidates use AI chatbots to answer voter questions")

# The 1-5 reoriented integer score arrives as a factor with levels "1".."5" in `cum`; converting to numeric and
# collapsing to bad/neither/good, exactly matching the 05 report's own helpers-2 chunk.
cum <- cum %>% mutate(across(all_of(paste0(names(ai_labels), "_i")), ~ as.numeric(as.character(.x))))
for (col in paste0(names(ai_labels), "_i")) {
  cum[[paste0(col, "_cat3")]] <- factor(
    case_when(cum[[col]] <= 2 ~ "Bad", cum[[col]] == 3 ~ "Neither", cum[[col]] >= 4 ~ "Good", TRUE ~ NA_character_),
    levels = c("Bad", "Neither", "Good"))
}

ai_mean_trend <- map_dfr(paste0(names(ai_labels), "_i"), function(col) mean_by_year(cum, col) %>% mutate(item = col, .before = 1)) %>%
  mutate(item = ai_labels[str_remove(item, "_i$")], pct = estimate)
print(plot_dumbbell(ai_mean_trend, "Weighted mean (1 = bad, 5 = good)"))

ai_cat3_trend <- map_dfr(paste0(names(ai_labels), "_i_cat3"), function(col) factor_props_by_year(cum, col) %>% mutate(item = ai_labels[[str_remove(col, "_i_cat3$")]], .before = 1))
print(plot_stacked_props(ai_cat3_trend, colors = AI_STATUS_COLORS))
```

- [ ] **Step 2: Run it and check output**

```bash
cd 2026 && Rscript -e "pdf(NULL); source('process/eis_2026_09_ai_confidence_concern_report.R'); dev.off()"
```

(`pdf(NULL)` opens a device that discards its output — this is only to confirm every chart *constructs* without a ggplot error when there is no display attached; it does not affect how the real file behaves when you run it yourself and see the charts.)

Expected: no errors. Between the two "setup complete" / next messages, five `n = ...` lines should print, matching (exactly, confirmed while writing this plan):

```
n = 2,551
n = 3,144
n = 2,771
```

(Only three `n =` lines print in Part C — one per BPC35-37 item; C.2's two charts have no `cat("n = ...")` line of their own, matching how the 05 report's own AI section reports `n` only via the dumbbell/stacked chart's own visual encoding, not a separate caption.)

- [ ] **Step 3: Commit**

```bash
git add 2026/process/eis_2026_09_ai_confidence_concern_report.R
git commit -m "Add Part C: AI use and attitudes (BPC35-38)

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

## Task 3: Part D and Part E — Voting experience and confidence (BPC39-43)

**Files:**
- Modify: `2026/process/eis_2026_09_ai_confidence_concern_report.R` (append after Part C)

**Interfaces:**
- Consumes: `data`, `cum`, `factor_props()`, `prop_by_year()`, `plot_prop_bar()` (unused here, listed for completeness — not actually called in this task), `plot_prop_stack_by()`, `plot_dumbbell()`, `RAMP_4PT` (all from Task 1)
- Produces: nothing later tasks depend on.

- [ ] **Step 1: Append Part D and Part E**

```r



##### #
#### #
### ################################################################################################################################################# #
# Part D. Voting experience (BPC39) ------------------------------------------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# Ported verbatim from the 05 report's "Voting Experience" section — a single trend dumbbell, 2024 vs. 2026.
# `vote_exp_positive_top2` is already pre-built in eis_cumulative.rds by script 04 (Part E.4): it deliberately
# excludes 2026 respondents who answered "Neither agree nor disagree" (2026 added a neutral midpoint that 2024's
# item never had) rather than counting them as "disagree" — so it is used directly here, never recomputed. (An
# earlier draft of this task recomputed it from the raw ordinal column, which silently dropped that exclusion and
# undercounted 2026 agreement by 13 points — caught in task review, corrected before implementation.)
vote_exp_trend <- prop_by_year(cum, "vote_exp_positive_top2") %>% mutate(item = "Own voting experience was positive")
print(plot_dumbbell(vote_exp_trend, "Weighted % agreeing"))




##### #
#### #
### ################################################################################################################################################# #
# Part E. Confidence (BPC40-43) ------------------------------------------------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.1. Full 2026 distribution, all 4 items at once — new ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# New relative to the 05 report, which shows only the collapsed top-2 trend for confidence (E.2 below). No
# party-ID breakdown here — confirmed: concern only.
conf_labels <- c(conf_own_vote = "Your own vote", conf_local_votes = "Votes in your community",
                  conf_state_votes = "Votes in your state", conf_national_votes = "Votes nationwide")

conf_dist <- map_dfr(names(conf_labels), function(col) factor_props(data, col) %>% mutate(item = conf_labels[[col]], .before = 1))
print(plot_prop_stack_by(conf_dist, by_col = "item", colors = setNames(RAMP_4PT, levels(data$conf_own_vote))))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.2. Top-2 trend, 2024 vs. 2026 — ported verbatim ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Ported from the 05 report's "Confidence Votes Will Be Counted as Intended" section. "Confident" combines
# "somewhat confident" and "very confident" — the top2 columns are built here on `cum` because they are not
# pre-saved in eis_cumulative.rds; the 05 report builds this same set itself at runtime.
conf_top2_labels <- c(conf_own_vote_top2 = "Your own vote", conf_local_votes_top2 = "Votes in your community",
                       conf_state_votes_top2 = "Votes in your state", conf_national_votes_top2 = "Votes nationwide")
cum <- cum %>%
  mutate(
    conf_own_vote_top2       = as.numeric(as.integer(conf_own_vote) >= 3),
    conf_local_votes_top2    = as.numeric(as.integer(conf_local_votes) >= 3),
    conf_state_votes_top2    = as.numeric(as.integer(conf_state_votes) >= 3),
    conf_national_votes_top2 = as.numeric(as.integer(conf_national_votes) >= 3)
  )
conf_trend <- map_dfr(names(conf_top2_labels), function(col) prop_by_year(cum, col) %>% mutate(item = conf_top2_labels[[col]], .before = 1))
print(plot_dumbbell(conf_trend, "Weighted % confident"))
```

- [ ] **Step 2: Run it and check output**

```bash
cd 2026 && Rscript -e "pdf(NULL); source('process/eis_2026_09_ai_confidence_concern_report.R'); dev.off()"
```

Expected: no errors — three more charts render (D.1, E.1, E.2), on top of Task 2's five. No new `n = ...` lines are expected in Parts D/E (matching how the 05 report itself captions these sections — via the dumbbell/stacked chart's own point/CI encoding, not a separate `cat()` line).

- [ ] **Step 3: Verify the trend numbers directly**

```bash
cd 2026 && Rscript -e '
source("process/eis_2026_09_ai_confidence_concern_report.R")
cat("\n--- vote_exp_trend ---\n"); print(vote_exp_trend)
cat("\n--- conf_trend ---\n"); print(conf_trend)
' 2>&1 | grep -A6 -- "---"
```

Expected (exact — confirmed while writing this plan):

```
--- vote_exp_trend ---
# A tibble: 2 × 6
  year    pct ci_low ci_high     n item
1 2024   91.9   90.1    93.5  1710 Own voting experience was positive
2 2026   95.7   94.7    96.5  2539 Own voting experience was positive

--- conf_trend ---
# A tibble: 8 × 6
1 Your own vote           2024   83.7   81.2    85.9  1755
2 Your own vote           2026   83.6   81.8    85.2  2829
3 Votes in your community 2024   85.8   83.5    87.7  1758
4 Votes in your community 2026   85.7   84.1    87.2  2909
5 Votes in your state     2024   82.6   80.1    84.9  1761
6 Votes in your state     2026   82.3   80.5    83.9  2902
7 Votes nationwide        2024   75.8   73.0    78.4  1749
8 Votes nationwide        2026   74.8   72.8    76.7  2896
```

- [ ] **Step 4: Commit**

```bash
git add 2026/process/eis_2026_09_ai_confidence_concern_report.R
git commit -m "Add Part D (voting experience) and Part E (confidence)

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

## Task 4: Part F — Concern (BPC44, 14 items, with party-ID breakdown)

**Files:**
- Modify: `2026/process/eis_2026_09_ai_confidence_concern_report.R` (append after Part E)

**Interfaces:**
- Consumes: `data`, `cum`, `factor_props()`, `svy_prop_by()`, `battery_props()`, `plot_prop_stack_by()`, `plot_battery_dodge_party()`, `plot_dumbbell()`, `plot_battery_bar()`, `RAMP_4PT`, `PID_COLORS` (all from Task 1)
- Produces: `data$concern_*_top2` (14 new columns, added by mutating `data` in place) — not consumed by any later task, but note for Task 5: Task 5 does not read or depend on these.

**Important ordering note:** within this task, F.2's step (which adds the `_top2` columns to `data`) **must** run before F.4's step, because F.4 reuses those same columns. The step order below already reflects this — do not reorder.

- [ ] **Step 1: Append Part F**

```r



##### #
#### #
### ################################################################################################################################################# #
# Part F. Concern (BPC44, 14 items) --------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.0. Setup: labels for all 14 items, and which 10 are comparable to 2024 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

concern_labels_all <- c(
  concern_misinfo            = "Inaccurate or misleading election information",
  concern_ai_disinfo         = "AI used to spread disinformation",
  concern_foreign             = "Foreign interference",
  concern_ineligible_votes   = "Ineligible votes being counted",
  concern_overturn            = "Attempts to overturn a fair election's results",
  concern_biased_count        = "Biased or inaccurate ballot counting",
  concern_mail_ballots         = "Illegal or improper mail ballot/drop box use",
  concern_guns_intimidation    = "Guns, violence, or intimidation at voting locations",
  concern_post_violence        = "Violence or unrest after election day",
  concern_polling_problems     = "Long lines or equipment problems at polls",
  concern_ice_deployment      = "ICE/federal law enforcement in your community",
  concern_ballot_seizure       = "Federal/state seizure of ballots or voting machines",
  concern_eligible_blocked     = "Eligible voters blocked from voting",
  concern_gerrymander          = "Unfair district lines distorting outcomes")

# The 4 items new to 2026 — confirmed absent from eis_cumulative.rds and never asked in the 2024 field's own
# question codebook (2024/raw/field1/question_codebook.csv) — have no possible trend comparison.
concern_new_2026   <- c("concern_ice_deployment", "concern_ballot_seizure", "concern_eligible_blocked", "concern_gerrymander")
concern_comparable <- setdiff(names(concern_labels_all), concern_new_2026)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.1. Full 2026 distribution, all 14 items at once — new ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

concern_dist <- map_dfr(names(concern_labels_all), function(col) factor_props(data, col) %>% mutate(item = concern_labels_all[[col]], .before = 1))
print(plot_prop_stack_by(concern_dist, by_col = "item", colors = setNames(RAMP_4PT, levels(data$concern_misinfo))))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.2. By-party top-2 breakdown, all 14 items at once — new ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# "Concerned" collapses "somewhat" and "very concerned" into one 0/1 indicator per item, matching the top-2
# convention the 05 report already uses for confidence/concern. Built on `data` (all 14 items) — the 05 report
# only builds this for the 10 comparable items (on `cum`, in F.3 below), so this extends that same convention to
# the 4 items it doesn't cover, rather than introducing a second convention.
data <- data %>%
  mutate(across(all_of(names(concern_labels_all)), ~ as.numeric(as.integer(.x) >= 3), .names = "{.col}_top2"))

concern_by_party <- map_dfr(names(concern_labels_all), function(col) {
  svy_prop_by(data, paste0(col, "_top2"), "pid3") %>% mutate(item = concern_labels_all[[col]], .before = 1)
})
print(plot_battery_dodge_party(concern_by_party, x_label = "Weighted % concerned"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.3. Top-2 trend for the 10 comparable items, 2024 vs. 2026 — ported verbatim ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Ported from the 05 report's "Concern About Election Problems" section. Built on `cum`, for the 10 items that
# exist there — the same top2-on-`cum` step that section's own helpers-2 chunk already does.
concern_trend_labels <- concern_labels_all[concern_comparable]
cum <- cum %>%
  mutate(across(all_of(concern_comparable), ~ as.numeric(as.integer(.x) >= 3), .names = "{.col}_top2"))

concern_trend <- map_dfr(names(concern_trend_labels), function(col) prop_by_year(cum, paste0(col, "_top2")) %>% mutate(item = concern_trend_labels[[col]], .before = 1))
print(plot_dumbbell(concern_trend, "Weighted % concerned"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.4. The 4 items new to 2026, 2026 only — new ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Reuses the `_top2` columns F.2 already added to `data` — explicitly captioned as having no 2024 counterpart.
concern_new_labels <- concern_labels_all[concern_new_2026]
concern_new_props <- battery_props(data, paste0(concern_new_2026, "_top2")) %>%
  mutate(item = unname(concern_new_labels[str_remove(item, "_top2$")]))
print(plot_battery_bar(concern_new_props, x_label = "Weighted % concerned (2026 only — no 2024 counterpart)"))
cat("n =", format(concern_new_props$n[1], big.mark = ","), "\n")
```

- [ ] **Step 2: Run it and check output**

```bash
cd 2026 && Rscript -e "pdf(NULL); source('process/eis_2026_09_ai_confidence_concern_report.R'); dev.off()"
```

Expected: no errors — four more charts render (F.1-F.4), on top of Task 3's eight. One new `n = ...` line prints (F.4's), with value `n = 2,904` (confirmed while writing this plan — that's the smallest of the four new items' bases; if you print each one individually you'll see 2,904 / 2,856 / 2,901 / 2,866 for ICE / ballot seizure / eligible-blocked / gerrymander respectively, but the script only captions the first one, matching every other multi-item battery caption in this project, which reports `props$n[1]` as a representative base size).

- [ ] **Step 3: Verify row counts and a few real values**

```bash
cd 2026 && Rscript -e '
source("process/eis_2026_09_ai_confidence_concern_report.R")
cat("\nconcern_dist rows (expect 56 = 14 items x 4 levels):", nrow(concern_dist), "\n")
cat("concern_by_party rows (expect 42 = 14 items x 3 parties):", nrow(concern_by_party), "\n")
cat("concern_trend rows (expect 20 = 10 items x 2 years):", nrow(concern_trend), "\n")
cat("\n--- concern_by_party, first 3 rows (Inaccurate/misleading info, by party) ---\n")
print(head(concern_by_party, 3))
cat("\n--- concern_new_props ---\n")
print(concern_new_props)
'
```

Expected (exact — confirmed while writing this plan):

```
concern_dist rows (expect 56 = 14 items x 4 levels): 56
concern_by_party rows (expect 42 = 14 items x 3 parties): 42
concern_trend rows (expect 20 = 10 items x 2 years): 20

--- concern_by_party, first 3 rows (Inaccurate/misleading info, by party) ---
# A tibble: 3 × 6
  item                                           pid3    pct ci_low ci_high     n
1 Inaccurate or misleading election information  Dem    74.3   71.0    77.4  1108
2 Inaccurate or misleading election information  Ind    72.2   68.0    76.0   737
3 Inaccurate or misleading election information  Rep    71.3   67.9    74.5  1051

--- concern_new_props ---
# A tibble: 4 × 5
  item                                                  pct ci_low ci_high     n
1 ICE/federal law enforcement in your community        56.1   53.9    58.3  2904
2 Federal/state seizure of ballots or voting machines  57.3   55.1    59.6  2856
3 Eligible voters blocked from voting                  61.9   59.7    64.0  2901
4 Unfair district lines distorting outcomes            67.9   65.7    70.0  2866
```

- [ ] **Step 4: Commit**

```bash
git add 2026/process/eis_2026_09_ai_confidence_concern_report.R
git commit -m "Add Part F: concern about election problems, with a party-ID breakdown

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

## Task 5: Part G — Noncitizen voting, access-vs-integrity, and USPS policy (BPC45-48)

**Files:**
- Modify: `2026/process/eis_2026_09_ai_confidence_concern_report.R` (append after Part F — this is the final part)

**Interfaces:**
- Consumes: `data`, `factor_props()`, `plot_prop_bar()` (all from Task 1)
- Produces: nothing — this is the last part of the file.

- [ ] **Step 1: Append Part G and the closing line**

```r



##### #
#### #
### ################################################################################################################################################# #
# Part G. Noncitizen voting, access-vs-integrity, and USPS policy (BPC45-48) ------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# Four 2026-only bar charts — none have a 2024 counterpart (confirmed absent from eis_cumulative.rds and from the
# 2024 field's own question codebook). For noncitizen_alters and access_vs_integrity, the catch-all ("Don't know" /
# "Don't know / No opinion") is pinned to the bottom rather than sorted by its own %, via plot_prop_bar()'s default
# (or, for access_vs_integrity, an explicit `anchors` override matching its own exact wording).

## G.1. How often illegal noncitizen voting occurs ----
props <- factor_props(data, "noncitizen_freq")
print(plot_prop_bar(props, order = levels(data$noncitizen_freq), x_label = "How often illegal noncitizen voting occurs"))
cat("n =", format(props$n[1], big.mark = ","), "\n")

## G.2. Whether illegal noncitizen voting changes election outcomes ----
props <- factor_props(data, "noncitizen_alters")
print(plot_prop_bar(props, x_label = "Illegal noncitizen voting changes election outcomes"))
cat("n =", format(props$n[1], big.mark = ","), "\n")

## G.3. Priority: easier for eligible voters, or harder for ineligible voters ----
props <- factor_props(data, "access_vs_integrity")
print(plot_prop_bar(props, anchors = c("Don't know / No opinion"), x_label = "Higher voting-law priority"))
cat("n =", format(props$n[1], big.mark = ","), "\n")

## G.4. Support for the Postal Service's August mail-ballot policy change ----
props <- factor_props(data, "usps_policy_support")
print(plot_prop_bar(props, order = levels(data$usps_policy_support), x_label = "Support for USPS mail-ballot policy change"))
cat("n =", format(props$n[1], big.mark = ","), "\n")




# The end.
```

- [ ] **Step 2: Run the complete script and check output**

```bash
cd 2026 && Rscript -e "pdf(NULL); source('process/eis_2026_09_ai_confidence_concern_report.R'); dev.off()"
```

Expected: no errors — the full script now runs top to bottom, all 16 charts render. Four new `n = ...` lines print, matching (exact, confirmed while writing this plan):

```
n = 2,888
n = 2,648
n = 3,144
n = 2,819
```

- [ ] **Step 3: Verify the four items' real distributions**

```bash
cd 2026 && Rscript -e '
source("process/eis_2026_09_ai_confidence_concern_report.R")
for (v in c("noncitizen_freq", "noncitizen_alters", "access_vs_integrity", "usps_policy_support")) {
  cat("\n---", v, "---\n"); print(factor_props(data, v))
}
'
```

Expected (exact — confirmed while writing this plan):

```
--- noncitizen_freq ---
# A tibble: 5 × 5
  category          pct ci_low ci_high     n
1 Never            8.23   7.01    9.45  2888
2 Rarely          30.8   28.7    32.8   2888
3 Occasionally    23.8   21.9    25.7   2888
4 Frequently      20.8   19.0    22.6   2888
5 Very frequently 16.4   14.7    18.0   2888

--- noncitizen_alters ---
# A tibble: 3 × 5
  category     pct ci_low ci_high     n
1 Yes         41.3   39.0    43.6  2648
2 No          45.1   42.7    47.4  2648
3 Don't know  13.6   12.1    15.2  2648

--- access_vs_integrity ---
# A tibble: 3 × 5
  category                                              pct ci_low ci_high     n
1 Passing laws that make it easier for eligible voters 45.6  43.5    47.8  3144
2 Passing laws that make it harder for ineligible ...   43.2  41.1    45.3  3144
3 Don't know / No opinion                               11.2   9.87   12.5  3144

--- usps_policy_support ---
# A tibble: 5 × 5
  category                     pct ci_low ci_high     n
1 Strongly oppose             26.8   24.7    28.8  2819
2 Somewhat oppose             12.3   10.8    13.8  2819
3 Neither support nor oppose  18.2   16.4    19.9  2819
4 Somewhat support            19.1   17.3    20.8  2819
5 Strongly support            23.7   21.8    25.6  2819
```

- [ ] **Step 4: Run once more, without the null device, to see the charts for real**

```bash
cd 2026 && Rscript process/eis_2026_09_ai_confidence_concern_report.R
```

If run in a session with an active display (e.g., from within RStudio, or a terminal with a display attached), 16 chart windows/plots will appear in sequence. If you're checking this from a headless shell, this step will error trying to open a display — that's expected and not a bug; Step 2's `pdf(NULL)` check is the correct way to verify correctness without a display.

- [ ] **Step 5: Commit**

```bash
git add 2026/process/eis_2026_09_ai_confidence_concern_report.R
git commit -m "Add Part G: noncitizen voting, access-vs-integrity, and USPS policy

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```
