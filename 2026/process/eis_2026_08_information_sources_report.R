

# EIS_2026_information_sources_report



########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####
####  Title: R-script version of the BPC Election Information Survey, 2026 field - Information Sources Report
####
####  Overview: A plain-R-script companion to 'eis_2026_08_information_sources_report.Rmd', covering the same
####            content: how often voters seek election information, where they get election news, where they
####            look for three specific kinds of election information (register/vote, how elections are run,
####            who won), and how both compare to 2024. The Rmd renders every chart inline as an embedded HTML
####            image; this script instead (1) assigns the weighted numbers behind EVERY chart to its own
####            named data frame (every such object's name ends in "_df"), so the numbers behind any chart can
####            be inspected, exported, or reused directly without opening the HTML report, and (2) saves
####            every chart as its own PNG file rather than embedding it. The naming convention for both is
####            documented in Part B below. This file does not replace the Rmd - the Rmd is still the source
####            for the self-contained HTML report; this is a companion for working with the underlying numbers
####            as plain R objects.
####
####  Outputs: output/eis_2026_08_charts/*.png       one PNG per chart (61 total)
####           output/eis_2026_08_plot_data.rds      one named list, one element per "_df" object below
####
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# Loading packages
library(tidyverse)
library(survey)


##### #
#### #
### ################################################################################################################################################# #
# Part A. Setup ------------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.1. File paths ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Relative to 2026/ - the R project's working directory whenever its .Rproj is open, matching every
# other script in process/.
out.dir   <- "output"
chart.dir <- file.path(out.dir, "eis_2026_08_charts")
dir.create(chart.dir, showWarnings = FALSE, recursive = TRUE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.2. Colors and theme ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

theme_set(theme_minimal(base_size = 12))

# BAR_COLOR/RANK_COLOR/PID_COLORS are for Parts 1-2 (2026 only); YEAR_COLORS is for Part 3 (the
# 2024-2026 trend); both are copied verbatim from eis_2026_05_trend_comparison_report.Rmd /
# eis_2026_08_information_sources_report.Rmd so a reader sees the same colors in every report.
BAR_COLOR   <- "#2a78d6"
RANK_COLOR  <- "#eb6834"
PID_COLORS  <- c(Dem = "#2a78d6", Ind = "#898781", Rep = "#d03b3b")
YEAR_COLORS <- c("2024" = "#2a78d6", "2026" = "#eb6834")
SEEK_RAMP   <- c("#b7d3f6", "#6da7ec", "#2a78d6", "#1c5cab", "#0d366b")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.3. Saving one chart consistently ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Every chart in this script is saved through this one function, so every PNG shares the same dpi
# and background regardless of which part of the script built it.
save_chart <- function(plot, name, width = 7, height = 4.5, dpi = 96) {
  ggsave(file.path(chart.dir, paste0(name, ".png")), plot, width = width, height = height, dpi = dpi, bg = "white")
}




##### #
#### #
### ################################################################################################################################################# #
# Part B. Naming convention for the saved data frames ------------------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# Every object created below whose name ends in "_df" is the exact data behind one chart, and Part
# Z at the end of this script collects all of them into one list, `plot_data`, and writes it to
# output/eis_2026_08_plot_data.rds. The names follow one of these patterns:
#
#   seek_freq_df                                the BPC1 seek-frequency chart
#   part1_main_df                                one long data frame (a "metric" column separates
#                                                the "Selected (top 3)" and "Ranked #1" series)
#                                                behind Part 1's main combined chart
#   part1_{tv|radio|outlet|social|bot}_df       Part 1's five source-specific drill-downs
#   {need}_{arm}_overall_df                     Part 2's top-level "selected + ranked #1" combined
#                                                chart, need is reg/run/won and arm is exclude/
#                                                include/never - same "metric" column as part1_main_df
#   {need}_{arm}_{selected|ranked}_party_df     Part 2's top-level source-by-party charts (these stay
#                                                as two separate objects - each is its own dodged
#                                                chart, not a combined one, so there is nothing to
#                                                merge them into)
#   {need}_mode_{overall|party}_df              Part 2's contact-mode drill-down
#   {need}_{social|bot}_{overall|party}_df      Part 2's social/chatbot drill-downs
#   {bpc2|reg|run|won}_trend_df                 Part 3's main year-over-year chart per battery
#   {reg|run|won}_dropped_df                    Part 3's 2024-only "dropped after 2024" chart
#
# So, for example, the data behind "How Elections Are Run > Include everyone > By party ID -
# selected" is `run_include_selected_party_df`.




##### #
#### #
### ################################################################################################################################################# #
# Part C. Loading the cleaned data ------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

data         <- readRDS(file.path(out.dir, "eis_2026_clean.rds"))
var.index    <- read.csv(file.path(out.dir, "eis_2026_variable_index.csv"), stringsAsFactors = FALSE)
sources.long <- readRDS(file.path(out.dir, "eis_2026_sources_long.rds")) %>%
  left_join(data %>% select(resp_id, pid3), by = "resp_id") %>%
  mutate(display = str_trim(str_extract(item_label, "(?<=: ).*")))
cum          <- readRDS(file.path(out.dir, "eis_cumulative.rds")) %>%
  mutate(year = factor(year, levels = c(2024, 2026)))

message("Part C: loaded ", nrow(data), " respondents (2026), ", nrow(sources.long), " long-table rows, ",
        nrow(cum), " cumulative rows (", sum(cum$year == 2024), " 2024 / ", sum(cum$year == 2026), " 2026).")




##### #
#### #
### ################################################################################################################################################# #
# Part D. Part 3 setup: new/dropped items and the pooled contact-mode columns ----------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.1. Converting the new/dropped 0/1 factor columns to numeric ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Every {need}_src_* / src_* column built in eis_2026_04_cumulative_data.R's E.2b arrives as a
# 2-level factor ("0"/"1"), the same convention eis_2026_05_trend_comparison_report.Rmd's own
# helpers-2 chunk already converts before use.
new_src_cols <- c(
  paste0("src_", c("tv_local","tv_national","radio","news_local","news_national","social",
                    "search","podcast","newsletter","aggregator","chatbot","friends_family")),
  paste0(rep(c("reg_", "run_", "won_"), each = 6),
         rep(c("src_search","src_chatbot","src_fact_checking","src_elected_officials",
               "src_commentator","src_civic_religious"), 3))
)
cum <- cum %>% mutate(across(all_of(new_src_cols), ~ as.numeric(as.character(.x))))

NEW_LABELS     <- c(search = "Online search engine", chatbot = "AI-enabled chatbot")
DROPPED_LABELS <- c(fact_checking = "Fact-checking organizations", elected_officials = "Elected officials generally",
                    commentator = "Favorite commentator/analyst", civic_religious = "Civic/religious organizations")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.2. The pooled contact-mode columns ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Contact mode is a single nominal item, not a multiselect battery, so it is the one variable in
# this report that is not already poolable by filtering eis_2026_sources_long.rds - it never went
# into that long table to begin with. The hypothetical arm's wording is "I would visit/call/visit"
# vs. the actual arm's "I visit/call/visit" (confirmed directly against the data while building this
# report); normalizing that prefix away before coalescing keeps the pooled factor at 5 levels
# instead of silently ballooning to 8.
normalize_mode <- function(x) str_replace(as.character(x), "^I would ", "I ")

data <- data %>%
  mutate(
    reg_official_mode_pooled = factor(coalesce(normalize_mode(reg_act_official_mode), normalize_mode(reg_hyp_official_mode)),
                                       levels = levels(reg_act_official_mode)),
    run_official_mode_pooled = factor(coalesce(normalize_mode(run_act_official_mode), normalize_mode(run_hyp_official_mode)),
                                       levels = levels(run_act_official_mode)),
    won_official_mode_pooled = factor(coalesce(normalize_mode(won_act_official_mode), normalize_mode(won_hyp_official_mode)),
                                       levels = levels(won_act_official_mode))
  )




##### #
#### #
### ################################################################################################################################################# #
# Part E. Estimators --------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.1. Estimators for a single wide column or battery of wide columns ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Ported unchanged from eis_2026_06_design_data.R / eis_2026_07_plots_overview.R. Used throughout
# Part 1 and for the Part 2 drill-down that has no arm to pool (contact mode).

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

svy_prop <- function(data, indicator_col, weight_col = "weight") {
  sub <- data %>% filter(!is.na(.data[[indicator_col]]))
  des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
  est <- svyciprop(reformulate(indicator_col), des, method = "logit")
  tibble(pct = as.numeric(est) * 100, ci_low = confint(est)[1] * 100, ci_high = confint(est)[2] * 100, n = nrow(sub))
}

battery_props <- function(data, cols, weight_col = "weight") {
  data.bin <- data %>% mutate(across(all_of(cols), ~ as.numeric(as.character(.x))))
  map_dfr(cols, function(col) svy_prop(data.bin, col, weight_col) %>% mutate(item = col, .before = 1))
}

factor_props_by <- function(data, factor_col, by_col, weight_col = "weight") {
  data %>%
    filter(!is.na(.data[[by_col]])) %>%
    group_by(.data[[by_col]]) %>%
    group_modify(~ factor_props(.x, factor_col, weight_col)) %>%
    ungroup()
}

item_display_labels <- function(cols) {
  var.index %>% filter(var_name %in% cols) %>%
    transmute(var_name, display = str_trim(str_extract(var_label, "(?<=: ).*"))) %>%
    deframe()
}

rank_display_labels <- function(rank_1st_col) {
  parent.qid <- var.index %>% filter(var_name == rank_1st_col) %>% pull(derived_from) %>% str_extract("BPC[0-9]+")
  var.index %>% filter(base == parent.qid, var_type == "multiselect") %>%
    transmute(item_tag, display = str_trim(str_extract(var_label, "(?<=: ).*"))) %>%
    deframe()
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.2. Estimators built on the long select/rank table ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Used throughout Part 2, where "which arm(s) count as in scope" changes per chart (exclude never-
# seekers / include everyone / never-seekers only) but is always expressible as a filter on the long
# table's `battery` column, with no new pooled wide column required. "Include everyone" filters to
# BOTH arms' battery values at once; since exactly one arm is `in_base` per respondent, that filter
# alone already is the pooled population - no coalesce() needed.

prep_battery_long <- function(battery_values) {
  sources.long %>% filter(battery %in% battery_values, in_base)
}

# One weighted proportion for one already-filtered subset - the same svyciprop() logit pattern as
# svy_prop() above, reused directly rather than via srvyr/svyby.
prop_one_group <- function(sub, indicator_col, weight_col = "weight") {
  des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
  est <- svyciprop(reformulate(indicator_col), des, method = "logit")
  tibble(pct = as.numeric(est) * 100, ci_low = confint(est)[1] * 100, ci_high = confint(est)[2] * 100, n = nrow(sub))
}

# `indicator_col` names a logical column the caller has already added to `long_data` (e.g.
# `selected == "1"`, or `coalesce(rank == 1, FALSE)`). Set by_party = FALSE to collapse to one
# weighted % per item overall; TRUE also splits by pid3.
#
# An explicit loop over item_tag (x pid3), calling svyciprop() fresh per subset, rather than
# group_by()/summarise(survey_mean()) or svyby() - the same recipe eis_2026_06_design_data.R's
# svy_prop_by()/factor_props_by() already use for "estimate X within each level of a grouping
# variable" elsewhere in this project. Verified while building the Rmd version of this report to
# reproduce the grouped estimators' numbers exactly (0 difference) against the established wide-
# column pattern; grouped alternatives (srvyr, svyby, group_modify) intermittently corrupted an
# unrelated later call after several svydesign()/svyciprop() calls had already run in the same loop.
battery_props_by_party <- function(long_data, indicator_col, weight_col = "weight", by_party = TRUE) {
  if (by_party) long_data <- long_data %>% filter(!is.na(pid3))
  tags <- sort(unique(as.character(long_data$item_tag)))
  results <- list()
  k <- 1
  if (by_party) {
    for (tg in tags) for (p in levels(long_data$pid3)) {
      sub <- long_data %>% filter(item_tag == tg, pid3 == p)
      if (nrow(sub) == 0) next
      results[[k]] <- prop_one_group(sub, indicator_col, weight_col) %>%
        mutate(item_tag = tg, item = sub$display[1], pid3 = p, .before = 1)
      k <- k + 1
    }
  } else {
    for (tg in tags) {
      sub <- long_data %>% filter(item_tag == tg)
      if (nrow(sub) == 0) next
      results[[k]] <- prop_one_group(sub, indicator_col, weight_col) %>%
        mutate(item_tag = tg, item = sub$display[1], .before = 1)
      k <- k + 1
    }
  }
  bind_rows(results)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.3. Estimators for the 2024-2026 trend ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Ported unchanged from eis_2026_05_trend_comparison_report.Rmd, for Part 3.

prop_by_year <- function(data, indicator_col, weight_col = "weight_common") {
  map_dfr(levels(data$year), function(yr) {
    sub <- data %>% filter(year == yr, !is.na(.data[[indicator_col]]))
    des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
    est <- svyciprop(reformulate(indicator_col), des, method = "logit")
    tibble(year = yr, pct = as.numeric(est) * 100,
           ci_low = confint(est)[1] * 100, ci_high = confint(est)[2] * 100, n = nrow(sub))
  })
}

prop_table_by_item <- function(data, cols, weight_col = "weight_common") {
  map_dfr(cols, function(col) prop_by_year(data, col, weight_col) %>% mutate(item = col, .before = 1))
}

# Computes a 2026-only weighted % for each column in `cols` (named by its display label), shaped
# like prop_table_by_item()'s output (item, year, pct, ci_low, ci_high, n) so the rows can be bound
# directly onto a battery's common-item trend data and plotted in the SAME dumbbell chart, rather
# than a separate one-off bar chart. plot_dumbbell() below already handles an item with a point in
# only one year cleanly: pivot_wider() fills the missing 2024 cell with NA, geom_line() draws
# nothing for a single-point group, and each year's geom_point() call filters to rows that exist for
# that year, so no 2024 marker is drawn for an item that never has one.
new_item_trend <- function(cols) {
  map_dfr(names(cols), function(col) {
    svy_prop(cum %>% filter(year == 2026), col, "weight_common") %>%
      mutate(item = cols[[col]], year = "2026", .before = 1)
  })
}




##### #
#### #
### ################################################################################################################################################# #
# Part F. Chart builders ----------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.1. Sorting helper: "Don't know"/"Not sure" always sort last ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Orders `items` by `pct` ascending (ggplot draws factor level 1 at the BOTTOM of a horizontal
# chart, so ascending pct puts the biggest bar at the top), except "Don't know"/"Not sure": a non-
# answer isn't a substantive position, so it always sorts below every real item regardless of its
# own %.
order_bottom_anchors <- function(items, pct, anchors = c("Don't know", "Not sure")) {
  is_anchor <- items %in% anchors
  c(items[is_anchor][order(pct[is_anchor])], items[!is_anchor][order(pct[!is_anchor])])
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.2. Single-series charts ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Bar chart for one factor's weighted distribution (nominal items - e.g. contact mode). Ported from
# eis_2026_07_plots_overview.R.
plot_categorical <- function(props, x_label = "Weighted %", order = NULL) {
  props <- if (is.null(order)) props %>% mutate(category = factor(category, levels = order_bottom_anchors(category, pct)))
           else                props %>% mutate(category = factor(category, levels = order))
  ggplot(props, aes(x = pct, y = category)) +
    geom_col(fill = BAR_COLOR, width = 0.65) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.9) +
    scale_x_continuous(expand = expansion(mult = c(0, .06))) +
    labs(x = x_label, y = NULL)
}

# Horizontal bar chart for a whole battery, sorted by size. Ported from eis_2026_07_plots_overview.R
# (`item_labels` optional - the long-table helpers already put display text in `item`).
plot_battery_bar <- function(battery, item_labels = NULL, x_label = "Weighted %") {
  if (!is.null(item_labels)) battery <- battery %>% mutate(item = unname(item_labels[item]))
  battery <- battery %>% mutate(item = factor(item, levels = order_bottom_anchors(item, pct)))
  ggplot(battery, aes(x = pct, y = item)) +
    geom_col(fill = BAR_COLOR, width = 0.65) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.9) +
    scale_x_continuous(expand = expansion(mult = c(0, .06))) +
    labs(x = x_label, y = NULL)
}

# A battery's weighted % selected (bar) with a ranked-#1 % overlay (point) on the same axis and
# category order - both are proportions of the same base, so they are directly comparable. `df` is
# one long data frame (item, metric, pct, ci_low, ci_high, n) with a "metric" column distinguishing
# the "Selected (top 3)" rows from the "Ranked #1" rows for the same items - see Part B. If `df` has
# no "Ranked #1" rows at all, geom_point() below is simply handed an empty data frame and draws
# nothing, so this same function also covers a selected-only chart with no rank data.
plot_combo_bar_point <- function(df, x_label = "Weighted %") {
  sel <- df %>% filter(metric == "Selected (top 3)")
  df  <- df %>% mutate(item = factor(item, levels = order_bottom_anchors(sel$item, sel$pct)))
  ggplot(filter(df, metric == "Selected (top 3)"), aes(y = item)) +
    geom_col(aes(x = pct, fill = metric), width = 0.65) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.9) +
    geom_point(data = filter(df, metric == "Ranked #1"), aes(x = pct, color = metric), size = 2.6, shape = 18) +
    scale_fill_manual(values = c("Selected (top 3)" = BAR_COLOR), name = NULL) +
    scale_color_manual(values = c("Ranked #1" = RANK_COLOR), name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, .06))) +
    labs(x = x_label, y = NULL) +
    theme(legend.position = "top")
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.3. Party-breakdown charts ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# `props_by` is battery_props_by_party()'s by-party output (item, pid3, pct, ci_low, ci_high).
# Dodged, not stacked - each item's selection is an independent 0/1, not a mutually-exclusive
# category, so the three parties' bars for one item do not sum to anything meaningful. No CI
# whiskers here (unlike the single-series charts above) - three dodged bars per item already crowd
# the chart, and a whisker per bar reads as clutter rather than signal at that density.
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

# Single 100%-stacked bar per subgroup - for a genuinely mutually-exclusive nominal item (contact
# mode) broken out by party. Ported from eis_2026_06_design_data.R Part D.6.
plot_prop_stack_by <- function(props_by, by_col, colors, x_label = "Weighted %") {
  props_by <- props_by %>% mutate(category = factor(category, levels = names(colors)))
  ggplot(props_by, aes(x = pct, y = .data[[by_col]], fill = category)) +
    geom_col(width = 0.65) +
    scale_fill_manual(values = colors, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, .02))) +
    labs(x = x_label, y = NULL) +
    theme(legend.position = "top", panel.grid = element_blank())
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.4. Trend chart ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

message("Part F: chart builders ready.")




##### #
#### #
### ################################################################################################################################################# #
# Part G. How Often Voters Seek Election Information (BPC1) ------------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# BPC1 in the questionnaire - renamed to seek_register / seek_elections_run / seek_who_won. Shown as
# the full weighted distribution across all five frequency categories, not collapsed to a single
# cutoff, since these categories are ordered (more/less often) but not evenly spaced. This is the
# item that defines "never-seekers" for Part 2 below: within each of the three information needs, a
# respondent who answers "Never" here is the one routed to that need's hypothetical arm.

seek_labels <- c(seek_register = "How to register and vote",
                  seek_elections_run = "How elections are run",
                  seek_who_won = "Who won an election")

# Short display labels for the legend only - copied verbatim from
# eis_2026_05_trend_comparison_report.Rmd so the same category reads the same way in both reports.
seek_freq_short <- c("Never" = "Never",
  "Only around major election dates or deadlines (e.g. Voter registration deadlines, Election Day)" = "Only near major dates",
  "A few times in the weeks around major election dates or deadlines" = "A few times near major dates",
  "Regularly in the weeks around major election dates or deadlines" = "Regularly near major dates",
  "Consistently throughout the year" = "Consistently all year")

seek_freq_df <- map_dfr(names(seek_labels), function(col) factor_props(data, col) %>% mutate(item = seek_labels[[col]], .before = 1)) %>%
  mutate(category = seek_freq_short[category])

p_seek <- plot_prop_stack_by(seek_freq_df, by_col = "item", colors = setNames(SEEK_RAMP, seek_freq_short)) +
  guides(fill = guide_legend(nrow = 2))
save_chart(p_seek, "seek_freq", width = 8, height = 3)

message("Part G: seek_freq_df built (n = ", format(seek_freq_df$n[1], big.mark = ","), ").")




##### #
#### #
### ################################################################################################################################################# #
# Part H. Where Voters Get Election News (BPC2 and its follow-ups) ------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## H.1. Main sources ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

src_cols <- var.index %>% filter(base == "BPC2", var_type == "multiselect") %>% pull(var_name)

# One long data frame instead of two: a `metric` column distinguishes the "% selected" rows from
# the "% ranked #1" rows for the same 15 items, so this single object holds everything the combined
# chart below needs (see Part B, and plot_combo_bar_point() in Part F.2).
part1_main_df <- bind_rows(
  data %>%
    mutate(across(all_of(src_cols), ~ as.numeric(as.character(.x)))) %>%
    battery_props(src_cols) %>%
    mutate(item = unname(item_display_labels(src_cols)[item]), metric = "Selected (top 3)", .before = 1),
  factor_props(data, "src_rank_1st") %>%
    mutate(item = unname(rank_display_labels("src_rank_1st")[category]), metric = "Ranked #1", .before = 1) %>%
    select(item, metric, pct, ci_low, ci_high, n)
)

p_part1_main <- plot_combo_bar_point(part1_main_df, x_label = "Weighted %")
save_chart(p_part1_main, "part1_main", height = 4.5)

message("Part H.1: part1_main_df built (n = ",
        format(part1_main_df$n[part1_main_df$metric == "Selected (top 3)"][1], big.mark = ","), ").")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## H.2. Drill-downs ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
# One block per source-specific follow-up battery, each scoped to the respondents who named that
# category at the main question (BPC2) - the base condition printed in each message below.

# National television networks
cols <- var.index %>% filter(base == "BPC3", var_type == "multiselect") %>% pull(var_name)
part1_tv_df <- data %>% mutate(across(all_of(cols), ~ as.numeric(as.character(.x)))) %>% battery_props(cols)
p <- plot_battery_bar(part1_tv_df, item_labels = item_display_labels(cols), x_label = "Weighted %")
save_chart(p, "part1_tv", height = 3)
message("Part H.2: part1_tv_df built (n = ", format(part1_tv_df$n[1], big.mark = ","),
        " - Base: ", var.index %>% filter(var_name == cols[1]) %>% pull(base_condition), ").")

# Radio programming
cols <- var.index %>% filter(base == "BPC4", var_type == "multiselect") %>% pull(var_name)
part1_radio_df <- data %>% mutate(across(all_of(cols), ~ as.numeric(as.character(.x)))) %>% battery_props(cols)
p <- plot_battery_bar(part1_radio_df, item_labels = item_display_labels(cols), x_label = "Weighted %")
save_chart(p, "part1_radio", height = 2.5)
message("Part H.2: part1_radio_df built (n = ", format(part1_radio_df$n[1], big.mark = ","),
        " - Base: ", var.index %>% filter(var_name == cols[1]) %>% pull(base_condition), ").")

# Print and online news outlets
cols <- var.index %>% filter(base == "BPC5", var_type == "multiselect") %>% pull(var_name)
part1_outlet_df <- data %>% mutate(across(all_of(cols), ~ as.numeric(as.character(.x)))) %>% battery_props(cols)
p <- plot_battery_bar(part1_outlet_df, item_labels = item_display_labels(cols), x_label = "Weighted %")
save_chart(p, "part1_outlet", height = 6)
message("Part H.2: part1_outlet_df built (n = ", format(part1_outlet_df$n[1], big.mark = ","),
        " - Base: ", var.index %>% filter(var_name == cols[1]) %>% pull(base_condition), ").")

# Social media platforms
cols <- var.index %>% filter(base == "BPC6", var_type == "multiselect") %>% pull(var_name)
part1_social_df <- data %>% mutate(across(all_of(cols), ~ as.numeric(as.character(.x)))) %>% battery_props(cols)
p <- plot_battery_bar(part1_social_df, item_labels = item_display_labels(cols), x_label = "Weighted %")
save_chart(p, "part1_social", height = 4)
message("Part H.2: part1_social_df built (n = ", format(part1_social_df$n[1], big.mark = ","),
        " - Base: ", var.index %>% filter(var_name == cols[1]) %>% pull(base_condition), ").")

# AI chatbots
cols <- var.index %>% filter(base == "BPC10", var_type == "multiselect") %>% pull(var_name)
part1_bot_df <- data %>% mutate(across(all_of(cols), ~ as.numeric(as.character(.x)))) %>% battery_props(cols)
p <- plot_battery_bar(part1_bot_df, item_labels = item_display_labels(cols), x_label = "Weighted %")
save_chart(p, "part1_bot", height = 3)
message("Part H.2: part1_bot_df built (n = ", format(part1_bot_df$n[1], big.mark = ","),
        " - Base: ", var.index %>% filter(var_name == cols[1]) %>% pull(base_condition), ").")

rm(cols, p)




##### #
#### #
### ################################################################################################################################################# #
# Part I. Where Voters Look for Specific Kinds of Information (register/vote, how elections are run, who won) ----------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# Deliberately unrolled - no loops - so every chart below is its own standalone block: the lines
# for any one chart can be selected and copied into another script without a loop variable (need /
# arm_key / dd) needing to make sense of it. The only thing shared within a block is the `_long`
# object feeding one arm's or drill-down's 2-3 charts, since they are different estimates of the
# same filtered subset - copying one of those charts means copying its `_long` line too.
#
# save_chart() calls are commented out through all of Part I - uncomment the ones for charts you
# decide to keep. Each chart is also displayed (a bare `p` after building it) so running a block
# shows the plot immediately, the same way it would print in RStudio's Plots pane.


##### #
#### #
### ################################################################################################################################################# #
# Part I.1. Registering and Voting ------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

message("Part I.1: Registering and Voting -----")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.1.1. Which sources: exclude never-seekers ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

reg_exclude_long <- prep_battery_long("reg_act")

reg_exclude_overall_df <- bind_rows(
  battery_props_by_party(reg_exclude_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE) %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(reg_exclude_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = FALSE) %>%
    mutate(metric = "Ranked #1", .before = 1)
)
p <- plot_combo_bar_point(reg_exclude_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_exclude_selected_overall", width = 10, height = 4.5)

reg_exclude_selected_party_df <- battery_props_by_party(reg_exclude_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(reg_exclude_selected_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_exclude_selected_party", width = 10, height = 4.5)

reg_exclude_ranked_party_df <- battery_props_by_party(reg_exclude_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = TRUE)
p <- plot_battery_dodge_party(reg_exclude_ranked_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_exclude_ranked_party", width = 10, height = 4.5)

message("  Exclude never-seekers: n = ", format(reg_exclude_overall_df$n[reg_exclude_overall_df$metric == "Selected (top 3)"][1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.1.2. Which sources: include everyone ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

reg_include_long <- prep_battery_long(c("reg_act", "reg_hyp"))

reg_include_overall_df <- bind_rows(
  battery_props_by_party(reg_include_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE) %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(reg_include_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = FALSE) %>%
    mutate(metric = "Ranked #1", .before = 1)
)
p <- plot_combo_bar_point(reg_include_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_include_selected_overall", width = 10, height = 4.5)

reg_include_selected_party_df <- battery_props_by_party(reg_include_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(reg_include_selected_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_include_selected_party", width = 10, height = 4.5)

reg_include_ranked_party_df <- battery_props_by_party(reg_include_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = TRUE)
p <- plot_battery_dodge_party(reg_include_ranked_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_include_ranked_party", width = 10, height = 4.5)

message("  Include everyone: n = ", format(reg_include_overall_df$n[reg_include_overall_df$metric == "Selected (top 3)"][1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.1.3. Which sources: never-seekers only ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

reg_never_long <- prep_battery_long("reg_hyp")

reg_never_overall_df <- bind_rows(
  battery_props_by_party(reg_never_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE) %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(reg_never_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = FALSE) %>%
    mutate(metric = "Ranked #1", .before = 1)
)
p <- plot_combo_bar_point(reg_never_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_never_selected_overall", width = 10, height = 4.5)

reg_never_selected_party_df <- battery_props_by_party(reg_never_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(reg_never_selected_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_never_selected_party", width = 10, height = 4.5)

reg_never_ranked_party_df <- battery_props_by_party(reg_never_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = TRUE)
p <- plot_battery_dodge_party(reg_never_ranked_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_never_ranked_party", width = 10, height = 4.5)

message("  Never-seekers only: n = ", format(reg_never_overall_df$n[reg_never_overall_df$metric == "Selected (top 3)"][1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.1.4. Drill-down: contact mode (local/state election officials) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

reg_mode_overall_df <- factor_props(data, "reg_official_mode_pooled")
p <- plot_categorical(reg_mode_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_mode_overall", width = 10, height = 4.5)

reg_mode_party_df <- factor_props_by(data, "reg_official_mode_pooled", "pid3")
reg_mode_colors <- setNames(colorRampPalette(c("#b7d3f6", "#0d366b"))(length(levels(data$reg_official_mode_pooled))),
                            levels(data$reg_official_mode_pooled))
p <- plot_prop_stack_by(reg_mode_party_df, by_col = "pid3", colors = reg_mode_colors)
p
# save_chart(p, "part2_reg_mode_party", width = 10, height = 4.5)

message("  Contact mode: n = ", format(reg_mode_overall_df$n[1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.1.5. Drill-down: social media platform ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

reg_social_long <- prep_battery_long(c("reg_act_social", "reg_hyp_social"))

reg_social_overall_df <- battery_props_by_party(reg_social_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE)
p <- plot_battery_bar(reg_social_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_social_overall", width = 10, height = 4.5)

reg_social_party_df <- battery_props_by_party(reg_social_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(reg_social_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_social_party", width = 10, height = 4.5)

message("  Social media platform: n = ", format(reg_social_overall_df$n[1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.1.6. Drill-down: AI chatbot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

reg_bot_long <- prep_battery_long(c("reg_act_bot", "reg_hyp_bot"))

reg_bot_overall_df <- battery_props_by_party(reg_bot_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE)
p <- plot_battery_bar(reg_bot_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_bot_overall", width = 10, height = 4.5)

reg_bot_party_df <- battery_props_by_party(reg_bot_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(reg_bot_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_reg_bot_party", width = 10, height = 4.5)

message("  AI chatbot: n = ", format(reg_bot_overall_df$n[1], big.mark = ","))




##### #
#### #
### ################################################################################################################################################# #
# Part I.2. How Elections Are Run -------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

message("Part I.2: How Elections Are Run -----")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.2.1. Which sources: exclude never-seekers ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

run_exclude_long <- prep_battery_long("run_act")

run_exclude_overall_df <- bind_rows(
  battery_props_by_party(run_exclude_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE) %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(run_exclude_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = FALSE) %>%
    mutate(metric = "Ranked #1", .before = 1)
)
p <- plot_combo_bar_point(run_exclude_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_exclude_selected_overall", width = 10, height = 4.5)

run_exclude_selected_party_df <- battery_props_by_party(run_exclude_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(run_exclude_selected_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_exclude_selected_party", width = 10, height = 4.5)

run_exclude_ranked_party_df <- battery_props_by_party(run_exclude_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = TRUE)
p <- plot_battery_dodge_party(run_exclude_ranked_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_exclude_ranked_party", width = 10, height = 4.5)

message("  Exclude never-seekers: n = ", format(run_exclude_overall_df$n[run_exclude_overall_df$metric == "Selected (top 3)"][1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.2.2. Which sources: include everyone ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

run_include_long <- prep_battery_long(c("run_act", "run_hyp"))

run_include_overall_df <- bind_rows(
  battery_props_by_party(run_include_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE) %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(run_include_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = FALSE) %>%
    mutate(metric = "Ranked #1", .before = 1)
)
p <- plot_combo_bar_point(run_include_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_include_selected_overall", width = 10, height = 4.5)

run_include_selected_party_df <- battery_props_by_party(run_include_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(run_include_selected_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_include_selected_party", width = 10, height = 4.5)

run_include_ranked_party_df <- battery_props_by_party(run_include_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = TRUE)
p <- plot_battery_dodge_party(run_include_ranked_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_include_ranked_party", width = 10, height = 4.5)

message("  Include everyone: n = ", format(run_include_overall_df$n[run_include_overall_df$metric == "Selected (top 3)"][1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.2.3. Which sources: never-seekers only ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

run_never_long <- prep_battery_long("run_hyp")

run_never_overall_df <- bind_rows(
  battery_props_by_party(run_never_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE) %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(run_never_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = FALSE) %>%
    mutate(metric = "Ranked #1", .before = 1)
)
p <- plot_combo_bar_point(run_never_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_never_selected_overall", width = 10, height = 4.5)

run_never_selected_party_df <- battery_props_by_party(run_never_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(run_never_selected_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_never_selected_party", width = 10, height = 4.5)

run_never_ranked_party_df <- battery_props_by_party(run_never_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = TRUE)
p <- plot_battery_dodge_party(run_never_ranked_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_never_ranked_party", width = 10, height = 4.5)

message("  Never-seekers only: n = ", format(run_never_overall_df$n[run_never_overall_df$metric == "Selected (top 3)"][1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.2.4. Drill-down: contact mode (local/state election officials) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

run_mode_overall_df <- factor_props(data, "run_official_mode_pooled")
p <- plot_categorical(run_mode_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_mode_overall", width = 10, height = 4.5)

run_mode_party_df <- factor_props_by(data, "run_official_mode_pooled", "pid3")
run_mode_colors <- setNames(colorRampPalette(c("#b7d3f6", "#0d366b"))(length(levels(data$run_official_mode_pooled))),
                            levels(data$run_official_mode_pooled))
p <- plot_prop_stack_by(run_mode_party_df, by_col = "pid3", colors = run_mode_colors)
p
# save_chart(p, "part2_run_mode_party", width = 10, height = 4.5)

message("  Contact mode: n = ", format(run_mode_overall_df$n[1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.2.5. Drill-down: social media platform ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

run_social_long <- prep_battery_long(c("run_act_social", "run_hyp_social"))

run_social_overall_df <- battery_props_by_party(run_social_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE)
p <- plot_battery_bar(run_social_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_social_overall", width = 10, height = 4.5)

run_social_party_df <- battery_props_by_party(run_social_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(run_social_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_social_party", width = 10, height = 4.5)

message("  Social media platform: n = ", format(run_social_overall_df$n[1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.2.6. Drill-down: AI chatbot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

run_bot_long <- prep_battery_long(c("run_act_bot", "run_hyp_bot"))

run_bot_overall_df <- battery_props_by_party(run_bot_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE)
p <- plot_battery_bar(run_bot_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_bot_overall", width = 10, height = 4.5)

run_bot_party_df <- battery_props_by_party(run_bot_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(run_bot_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_run_bot_party", width = 10, height = 4.5)

message("  AI chatbot: n = ", format(run_bot_overall_df$n[1], big.mark = ","))




##### #
#### #
### ################################################################################################################################################# #
# Part I.3. Who Won an Election ---------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

message("Part I.3: Who Won an Election -----")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.3.1. Which sources: exclude never-seekers ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

won_exclude_long <- prep_battery_long("won_act")

won_exclude_overall_df <- bind_rows(
  battery_props_by_party(won_exclude_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE) %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(won_exclude_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = FALSE) %>%
    mutate(metric = "Ranked #1", .before = 1)
)
p <- plot_combo_bar_point(won_exclude_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_exclude_selected_overall", width = 10, height = 4.5)

won_exclude_selected_party_df <- battery_props_by_party(won_exclude_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(won_exclude_selected_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_exclude_selected_party", width = 10, height = 4.5)

won_exclude_ranked_party_df <- battery_props_by_party(won_exclude_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = TRUE)
p <- plot_battery_dodge_party(won_exclude_ranked_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_exclude_ranked_party", width = 10, height = 4.5)

message("  Exclude never-seekers: n = ", format(won_exclude_overall_df$n[won_exclude_overall_df$metric == "Selected (top 3)"][1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.3.2. Which sources: include everyone ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

won_include_long <- prep_battery_long(c("won_act", "won_hyp"))

won_include_overall_df <- bind_rows(
  battery_props_by_party(won_include_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE) %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(won_include_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = FALSE) %>%
    mutate(metric = "Ranked #1", .before = 1)
)
p <- plot_combo_bar_point(won_include_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_include_selected_overall", width = 10, height = 4.5)

won_include_selected_party_df <- battery_props_by_party(won_include_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(won_include_selected_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_include_selected_party", width = 10, height = 4.5)

won_include_ranked_party_df <- battery_props_by_party(won_include_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = TRUE)
p <- plot_battery_dodge_party(won_include_ranked_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_include_ranked_party", width = 10, height = 4.5)

message("  Include everyone: n = ", format(won_include_overall_df$n[won_include_overall_df$metric == "Selected (top 3)"][1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.3.3. Which sources: never-seekers only ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

won_never_long <- prep_battery_long("won_hyp")

won_never_overall_df <- bind_rows(
  battery_props_by_party(won_never_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE) %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(won_never_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = FALSE) %>%
    mutate(metric = "Ranked #1", .before = 1)
)
p <- plot_combo_bar_point(won_never_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_never_selected_overall", width = 10, height = 4.5)

won_never_selected_party_df <- battery_props_by_party(won_never_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(won_never_selected_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_never_selected_party", width = 10, height = 4.5)

won_never_ranked_party_df <- battery_props_by_party(won_never_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first", by_party = TRUE)
p <- plot_battery_dodge_party(won_never_ranked_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_never_ranked_party", width = 10, height = 4.5)

message("  Never-seekers only: n = ", format(won_never_overall_df$n[won_never_overall_df$metric == "Selected (top 3)"][1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.3.4. Drill-down: contact mode (local/state election officials) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

won_mode_overall_df <- factor_props(data, "won_official_mode_pooled")
p <- plot_categorical(won_mode_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_mode_overall", width = 10, height = 4.5)

won_mode_party_df <- factor_props_by(data, "won_official_mode_pooled", "pid3")
won_mode_colors <- setNames(colorRampPalette(c("#b7d3f6", "#0d366b"))(length(levels(data$won_official_mode_pooled))),
                            levels(data$won_official_mode_pooled))
p <- plot_prop_stack_by(won_mode_party_df, by_col = "pid3", colors = won_mode_colors)
p
# save_chart(p, "part2_won_mode_party", width = 10, height = 4.5)

message("  Contact mode: n = ", format(won_mode_overall_df$n[1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.3.5. Drill-down: social media platform ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

won_social_long <- prep_battery_long(c("won_act_social", "won_hyp_social"))

won_social_overall_df <- battery_props_by_party(won_social_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE)
p <- plot_battery_bar(won_social_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_social_overall", width = 10, height = 4.5)

won_social_party_df <- battery_props_by_party(won_social_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(won_social_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_social_party", width = 10, height = 4.5)

message("  Social media platform: n = ", format(won_social_overall_df$n[1], big.mark = ","))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.3.6. Drill-down: AI chatbot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

won_bot_long <- prep_battery_long(c("won_act_bot", "won_hyp_bot"))

won_bot_overall_df <- battery_props_by_party(won_bot_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = FALSE)
p <- plot_battery_bar(won_bot_overall_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_bot_overall", width = 10, height = 4.5)

won_bot_party_df <- battery_props_by_party(won_bot_long %>% mutate(is_selected = selected == "1"), "is_selected", by_party = TRUE)
p <- plot_battery_dodge_party(won_bot_party_df, x_label = "Weighted %")
p
# save_chart(p, "part2_won_bot_party", width = 10, height = 4.5)

message("  AI chatbot: n = ", format(won_bot_overall_df$n[1], big.mark = ","))




##### #
#### #
### ################################################################################################################################################# #
# Part J. 2026 vs. 2024 Trend ------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# Scoped to the election-information-source questions covered in Parts G-I. For every other survey
# topic (right direction, AI attitudes, confidence, concern, etc.), see
# eis_2026_05_trend_comparison_report.Rmd. All estimates below use weight_common.
#
# For reg/run/won, the 2026 side of this comparison is the POOLED (both-arms-combined) source
# columns, not the actual-only or hypothetical-only arm - see Part D.2's note and the equivalent
# columns already used by eis_2026_05_trend_comparison_report.Rmd. 2024 asked these questions with
# no actual/hypothetical split at all, so pooling 2026's two arms back into one number is what lines
# the two years' populations up correctly.

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## J.1. Main election-news sources (BPC2) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
# "Friends and/or family" is new to 2026 and appears in the chart as a 2026-only point (no ring, no
# connecting line). There is no item in the 2024 survey that was dropped from this battery.

bpc2_items <- c(tv_local = "Local/regional TV", tv_national = "National TV", radio = "Radio",
                news_local = "Local/state print-online", news_national = "National print-online",
                social = "Social media", search = "Search engine", podcast = "Podcasts",
                newsletter = "Newsletters/blogs/forums", aggregator = "News aggregator apps",
                chatbot = "AI chatbot")
bpc2_cols <- paste0("src_", names(bpc2_items))
bpc2_trend_df <- bind_rows(
  prop_table_by_item(cum, bpc2_cols) %>% mutate(item = bpc2_items[str_remove(item, "^src_")]),
  new_item_trend(c(src_friends_family = "Friends and/or family"))
)
p <- plot_dumbbell(bpc2_trend_df, "Weighted % selected (among top 3)")
p
# save_chart(p, "part3_bpc2_trend", height = 4.5)
message("Part J.1: bpc2_trend_df built.")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## J.2. Registering and voting, how elections are run, who won ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
# "Online search engine" and "AI-enabled chatbot" are new to 2026 for all three of these batteries
# and appear in each chart as 2026-only points. Each battery also has four items dropped after 2024
# (fact-checking organizations, elected officials generally, favorite commentator/analyst,
# civic/religious organizations), shown in their own bar chart since there is no 2026 point to plot
# them against.

trend_items <- c(local_officials = "Local/county officials", state_officials = "State officials",
                  federal_site = "Federal agency/website", news_media = "News media",
                  social = "Social media influencer", friends_family = "Friends/family",
                  advocacy = "Election-integrity/advocacy orgs", campaign = "Candidate, campaign, or party")

# Unrolled deliberately, matching Part I - three explicit blocks below, no loop, save_chart() calls
# commented out, and each chart displayed (a bare `p`) as soon as it is built.

# --- Registering and voting ---
reg_trend_df <- bind_rows(
  prop_table_by_item(cum, paste0("reg_src_", names(trend_items))) %>%
    mutate(item = trend_items[str_remove(item, "^reg_src_")]),
  new_item_trend(setNames(NEW_LABELS, paste0("reg_src_", names(NEW_LABELS))))
)
p <- plot_dumbbell(reg_trend_df, "Weighted % selected (among top 3)")
p
# save_chart(p, "part3_reg_trend", height = 3.5)

reg_dropped_df <- map_dfr(paste0("reg_src_", names(DROPPED_LABELS)), function(col) svy_prop(cum %>% filter(year == 2024), col, "weight_common") %>%
                             mutate(item = DROPPED_LABELS[[str_remove(col, "^reg_src_")]], .before = 1))
p <- plot_battery_bar(reg_dropped_df, x_label = "Weighted % selected, 2024 only")
p
# save_chart(p, "part3_reg_dropped", height = 2.5)

message("Part J.2: reg_trend_df / reg_dropped_df built.")

# --- How elections are run ---
run_trend_df <- bind_rows(
  prop_table_by_item(cum, paste0("run_src_", names(trend_items))) %>%
    mutate(item = trend_items[str_remove(item, "^run_src_")]),
  new_item_trend(setNames(NEW_LABELS, paste0("run_src_", names(NEW_LABELS))))
)
p <- plot_dumbbell(run_trend_df, "Weighted % selected (among top 3)")
p
# save_chart(p, "part3_run_trend", height = 3.5)

run_dropped_df <- map_dfr(paste0("run_src_", names(DROPPED_LABELS)), function(col) svy_prop(cum %>% filter(year == 2024), col, "weight_common") %>%
                             mutate(item = DROPPED_LABELS[[str_remove(col, "^run_src_")]], .before = 1))
p <- plot_battery_bar(run_dropped_df, x_label = "Weighted % selected, 2024 only")
p
# save_chart(p, "part3_run_dropped", height = 2.5)

message("Part J.2: run_trend_df / run_dropped_df built.")

# --- Who won ---
won_trend_df <- bind_rows(
  prop_table_by_item(cum, paste0("won_src_", names(trend_items))) %>%
    mutate(item = trend_items[str_remove(item, "^won_src_")]),
  new_item_trend(setNames(NEW_LABELS, paste0("won_src_", names(NEW_LABELS))))
)
p <- plot_dumbbell(won_trend_df, "Weighted % selected (among top 3)")
p
# save_chart(p, "part3_won_trend", height = 3.5)

won_dropped_df <- map_dfr(paste0("won_src_", names(DROPPED_LABELS)), function(col) svy_prop(cum %>% filter(year == 2024), col, "weight_common") %>%
                             mutate(item = DROPPED_LABELS[[str_remove(col, "^won_src_")]], .before = 1))
p <- plot_battery_bar(won_dropped_df, x_label = "Weighted % selected, 2024 only")
p
# save_chart(p, "part3_won_dropped", height = 2.5)

message("Part J.2: won_trend_df / won_dropped_df built.")




##### #
#### #
### ################################################################################################################################################# #
# Part K. Collecting every plot's data into one list -------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# Every object created above whose name ends in "_df" - see Part B for the naming convention -
# gathered into one list so the whole set can be saved, exported, or inspected together, in
# addition to each one already being its own top-level object.
plot_data <- mget(ls(pattern = "_df$"))

saveRDS(plot_data, file.path(out.dir, "eis_2026_08_plot_data.rds"))

message("\nPart K: wrote ", length(plot_data), " data frames to ", out.dir, "/eis_2026_08_plot_data.rds\n",
        "  (one per chart; load with readRDS() and access by name, e.g. plot_data$run_include_selected_party_df)")




# The end.
