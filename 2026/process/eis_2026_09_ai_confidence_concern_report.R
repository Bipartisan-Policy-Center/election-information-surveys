


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
####  Output: none written to disk by default — every chart prints to the current graphics device. Each chart's
####            underlying data is also kept in a distinctly named data frame, with a commented-out write.csv()
####            line immediately after it (uncomment to export that one chart's data to
####            output/eis_2026_09_<name>.csv). Run directly or interactively.
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
           ci_high = confint(est)[, 2] * 100,
           n = nrow(sub))
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

ai_prevalence_props <- factor_props(data, "ai_prevalence")
print(plot_prop_bar(ai_prevalence_props, order = levels(data$ai_prevalence), x_label = paste("Weighted % —", ai_single_labels[["ai_prevalence"]])))
cat("n =", format(ai_prevalence_props$n[1], big.mark = ","), "\n")
# write.csv(x = ai_prevalence_props, file = file.path(out.dir, "eis_2026_09_ai_prevalence.csv"), row.names = FALSE, na = "")

ai_tool_freq_props <- factor_props(data, "ai_tool_freq")
print(plot_prop_bar(ai_tool_freq_props, order = levels(data$ai_tool_freq), x_label = paste("Weighted % —", ai_single_labels[["ai_tool_freq"]])))
cat("n =", format(ai_tool_freq_props$n[1], big.mark = ","), "\n")
# write.csv(x = ai_tool_freq_props, file = file.path(out.dir, "eis_2026_09_ai_tool_freq.csv"), row.names = FALSE, na = "")

ai_detect_conf_props <- factor_props(data, "ai_detect_conf")
print(plot_prop_bar(ai_detect_conf_props, order = levels(data$ai_detect_conf), x_label = paste("Weighted % —", ai_single_labels[["ai_detect_conf"]])))
cat("n =", format(ai_detect_conf_props$n[1], big.mark = ","), "\n")
# write.csv(x = ai_detect_conf_props, file = file.path(out.dir, "eis_2026_09_ai_detect_conf.csv"), row.names = FALSE, na = "")

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
# write.csv(x = ai_mean_trend, file = file.path(out.dir, "eis_2026_09_ai_mean_trend.csv"), row.names = FALSE, na = "")

ai_cat3_trend <- map_dfr(paste0(names(ai_labels), "_i_cat3"), function(col) factor_props_by_year(cum, col) %>% mutate(item = ai_labels[[str_remove(col, "_i_cat3$")]], .before = 1))
print(plot_stacked_props(ai_cat3_trend, colors = AI_STATUS_COLORS))
# write.csv(x = ai_cat3_trend, file = file.path(out.dir, "eis_2026_09_ai_cat3_trend.csv"), row.names = FALSE, na = "")



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
# write.csv(x = vote_exp_trend, file = file.path(out.dir, "eis_2026_09_vote_exp_trend.csv"), row.names = FALSE, na = "")




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
# write.csv(x = conf_dist, file = file.path(out.dir, "eis_2026_09_conf_dist.csv"), row.names = FALSE, na = "")

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
# write.csv(x = conf_trend, file = file.path(out.dir, "eis_2026_09_conf_trend.csv"), row.names = FALSE, na = "")



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
  concern_misinfo             = "Inaccurate or misleading election information",
  concern_ai_disinfo          = "AI used to spread disinformation",
  concern_foreign             = "Foreign interference",
  concern_ineligible_votes    = "Ineligible votes being counted",
  concern_overturn            = "Attempts to overturn a fair election's results",
  concern_biased_count        = "Biased or inaccurate ballot counting",
  concern_mail_ballots        = "Illegal or improper mail ballot/drop box use",
  concern_guns_intimidation   = "Guns, violence, or intimidation at voting locations",
  concern_post_violence       = "Violence or unrest after election day",
  concern_polling_problems    = "Long lines or equipment problems at polls",
  concern_ice_deployment      = "ICE/federal law enforcement in your community",
  concern_ballot_seizure      = "Federal/state seizure of ballots or voting machines",
  concern_eligible_blocked    = "Eligible voters blocked from voting",
  concern_gerrymander         = "Unfair district lines distorting outcomes")

# The 4 items new to 2026 — confirmed absent from eis_cumulative.rds and never asked in the 2024 field's own
# question codebook (2024/raw/field1/question_codebook.csv) — have no possible trend comparison.
concern_new_2026   <- c("concern_ice_deployment", "concern_ballot_seizure", "concern_eligible_blocked", "concern_gerrymander")
concern_comparable <- setdiff(names(concern_labels_all), concern_new_2026)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.1. Full 2026 distribution, all 14 items at once — new ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

concern_dist <- map_dfr(names(concern_labels_all), function(col) factor_props(data, col) %>% mutate(item = concern_labels_all[[col]], .before = 1))
print(plot_prop_stack_by(concern_dist, by_col = "item", colors = setNames(RAMP_4PT, levels(data$concern_misinfo))))
# write.csv(x = concern_dist, file = file.path(out.dir, "eis_2026_09_concern_dist.csv"), row.names = FALSE, na = "")

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
# write.csv(x = concern_by_party, file = file.path(out.dir, "eis_2026_09_concern_by_party.csv"), row.names = FALSE, na = "")

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
# write.csv(x = concern_trend, file = file.path(out.dir, "eis_2026_09_concern_trend.csv"), row.names = FALSE, na = "")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.4. The 4 items new to 2026, 2026 only — new ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Reuses the `_top2` columns F.2 already added to `data` — explicitly captioned as having no 2024 counterpart.
concern_new_labels <- concern_labels_all[concern_new_2026]
concern_new_props <- battery_props(data, paste0(concern_new_2026, "_top2")) %>%
  mutate(item = unname(concern_new_labels[str_remove(item, "_top2$")]))
print(plot_battery_bar(concern_new_props, x_label = "Weighted % concerned (2026 only — no 2024 counterpart)"))
cat("n =", format(concern_new_props$n[1], big.mark = ","), "\n")
# write.csv(x = concern_new_props, file = file.path(out.dir, "eis_2026_09_concern_new_props.csv"), row.names = FALSE, na = "")




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
noncitizen_freq_props <- factor_props(data, "noncitizen_freq")
print(plot_prop_bar(noncitizen_freq_props, order = levels(data$noncitizen_freq), x_label = "Weighted % — How often illegal noncitizen voting occurs"))
cat("n =", format(noncitizen_freq_props$n[1], big.mark = ","), "\n")
# write.csv(x = noncitizen_freq_props, file = file.path(out.dir, "eis_2026_09_noncitizen_freq.csv"), row.names = FALSE, na = "")

## G.2. Whether illegal noncitizen voting changes election outcomes ----
noncitizen_alters_props <- factor_props(data, "noncitizen_alters")
print(plot_prop_bar(noncitizen_alters_props, x_label = "Weighted % — Illegal noncitizen voting changes election outcomes"))
cat("n =", format(noncitizen_alters_props$n[1], big.mark = ","), "\n")
# write.csv(x = noncitizen_alters_props, file = file.path(out.dir, "eis_2026_09_noncitizen_alters.csv"), row.names = FALSE, na = "")

## G.3. Priority: easier for eligible voters, or harder for ineligible voters ----
access_vs_integrity_props <- factor_props(data, "access_vs_integrity")
print(plot_prop_bar(access_vs_integrity_props, anchors = c("Don't know / No opinion"), x_label = "Weighted % — Higher voting-law priority"))
cat("n =", format(access_vs_integrity_props$n[1], big.mark = ","), "\n")
# write.csv(x = access_vs_integrity_props, file = file.path(out.dir, "eis_2026_09_access_vs_integrity.csv"), row.names = FALSE, na = "")

## G.4. Support for the Postal Service's August mail-ballot policy change ----
usps_policy_support_props <- factor_props(data, "usps_policy_support")
print(plot_prop_bar(usps_policy_support_props, order = levels(data$usps_policy_support), x_label = "Weighted % — Support for USPS mail-ballot policy change"))
cat("n =", format(usps_policy_support_props$n[1], big.mark = ","), "\n")
# write.csv(x = usps_policy_support_props, file = file.path(out.dir, "eis_2026_09_usps_policy_support.csv"), row.names = FALSE, na = "")




# The end.
