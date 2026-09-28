


# EIS_2026_datawrapper_source_plots



########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####
####  Title: R-script prep for Datawrapper migration - Information Sources plots
####
####  Overview: Rebuilds a subset of the charts from 'eis_2026_08_information_sources_report.R' as
####            standalone ggplot blocks - no plot-building helper functions - so each chart's data
####            frame (every object below ending in "_df") can be exported to CSV and rebuilt in
####            Datawrapper. No chart is saved to disk here; each block just builds its data frame,
####            builds the plot with its own ggplot code, and prints it.
####
####  Scope: (1) "Which sources" - selected % (bar, with a gray capped CI) + ranked-#1 % (diamond)
####             - for each of the 3 information needs (register/vote, how elections are run, who
####             won), excluding never-seekers and never-seekers-only (6 charts total; the "everyone
####             included" arm is intentionally omitted for this set).
####         (2) Drill-downs (contact mode, social media platform, AI chatbot), each combined across
####             all 3 information needs as facets, for all 3 population arms (exclude never-seekers
####             / everyone included / never-seekers only) - 9 charts total.
####
####  Visual conventions (per Jack's direction, distinct from the original report):
####         - Every long axis/legend label gets a line break (str_wrap) instead of shrinking the
####           plot area.
####         - Every CI is a gray, semi-transparent horizontal bar with a vertical cap at each end
####           (geom_errorbar), not a plain line.
####         - No minor gridlines anywhere (set once, globally, via theme_set()).
####         - Every plot has a title naming the question it visualizes.
####
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# Loading packages
library(tidyverse)
library(survey)


##### #
#### #
### ################################################################################################################################################# #
# Part A. Setup -------------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

out.dir   <- "output"
plots.dir <- file.path(out.dir, "plots")
dir.create(plots.dir, showWarnings = FALSE, recursive = TRUE)

# No minor gridlines anywhere, set once here rather than repeated in every block below.
theme_set(theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank()))

BAR_COLOR   <- "#2a78d6"
RANK_COLOR  <- "#eb6834"
NEED_LEVELS <- c("Registering and Voting", "How Elections Are Run", "Who Won an Election")
NEED_COLORS <- c("Registering and Voting" = "#2a78d6", "How Elections Are Run" = "#eb6834", "Who Won an Election" = "#3fa34d")
NEED_SLUGS  <- c("Registering and Voting" = "reg", "How Elections Are Run" = "run", "Who Won an Election" = "won")

# For Parts G-K (AI/confidence/concern/noncitizen batch), ported from eis_2026_09_ai_confidence_concern_report.R.
# "2022" is only ever present in Part I.1's own cum_conf (the confidence trend, the one series 2022
# has data for) - harmless to define here for every other YEAR_COLORS-using chart, which never has
# a "2022" level in its own data for this color to match against.
YEAR_COLORS      <- c("2022" = "#6a3d9a", "2024" = "#2a78d6", "2026" = "#eb6834")
PID_COLORS       <- c(Dem = "#2a78d6", Ind = "#898781", Rep = "#d03b3b")
AI_STATUS_COLORS <- c("Bad" = "#d03b3b", "Neither" = "#898781", "Good" = "#0ca30c")
RAMP_4PT         <- c("#b7d3f6", "#6da7ec", "#2a78d6", "#0d366b")

wrap40 <- function(x) str_wrap(x, width = 40)
wrap36 <- function(x) str_wrap(x, width = 36)
wrap30 <- function(x) str_wrap(x, width = 30)

# Titles/subtitles don't auto-wrap to the plot's device width in ggplot2 - long ones get clipped
# instead - so every title/subtitle below is wrapped explicitly at a fixed character width rather
# than relying on however wide the eventual export happens to be.
wrap_title    <- function(x) str_wrap(x, width = 65)
wrap_subtitle <- function(x) str_wrap(x, width = 58)




##### #
#### #
### ################################################################################################################################################# #
# Part B. Loading the cleaned data -------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

data      <- readRDS(file.path(out.dir, "eis_2026_clean.rds"))
var.index <- read.csv(file.path(out.dir, "eis_2026_variable_index.csv"), stringsAsFactors = FALSE)

sources.long <- readRDS(file.path(out.dir, "eis_2026_sources_long.rds"))

# tools::toTitleCase() is a slow, per-string R-level algorithm (not a vectorized stringi/ICU
# routine like the str_*() calls above it), so running it on sources.long's 984k rows directly
# takes well over a minute. Only 313 distinct item_label values actually exist underneath (every
# respondent x battery just repeats one of them), so this computes the transform once per distinct
# label and joins the result back - same output, about 600x faster.
display_lookup <- sources.long %>%
  distinct(item_label) %>%
  mutate(display = str_extract(item_label, "(?<=: ).*"),
         display = str_trim(str_remove_all(display, "\\s*\\([^)]*\\)")),
         display = tools::toTitleCase(display))

sources.long <- sources.long %>% left_join(display_lookup, by = "item_label")

# Question-stem title for one battery, e.g. "reg_act_src_local_officials" ->
# "Sources used to find information about how to register and vote" - the text before the item's
# own colon in that column's var_label, so chart titles quote the actual survey question rather
# than a paraphrase.
qtitle <- function(one_var_name) {
  var.index %>% filter(var_name == one_var_name) %>% pull(var_label) %>% str_remove(":.*$") %>% str_trim()
}

message("Part B: loaded ", nrow(data), " respondents, ", nrow(sources.long), " long-table rows.")




##### #
#### #
### ################################################################################################################################################# #
# Part C. Estimators (ported unchanged from eis_2026_08_information_sources_report.R) ----------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# For one wide factor column (used below only for contact mode, the one drill-down not already in
# sources.long).
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

# Cross-tab of factor_col by every level of by_col (e.g. age4, pid3) - the subgroup-breakdown
# analogue of factor_props() above. Ported from eis_2026_06_design_data.R's C.6; not otherwise
# used in this script until F.1.4 below.
factor_props_by <- function(data, factor_col, by_col, weight_col = "weight") {
  data %>%
    filter(!is.na(.data[[by_col]])) %>%
    group_by(.data[[by_col]]) %>%
    group_modify(~ factor_props(.x, factor_col, weight_col)) %>%
    ungroup()
}

# Orders `items` by `pct` ascending (ggplot draws factor level 1 at the bottom of a horizontal
# chart), except "Don't Know"/"Not Sure", which always sort last regardless of their own %. Title
# Case here to match Part B's tools::toTitleCase() recoding of `display` - these are compared
# against the already-recoded item text, not the raw survey wording.
order_bottom_anchors <- function(items, pct, anchors = c("Don't Know", "Not Sure")) {
  is_anchor <- items %in% anchors
  c(items[is_anchor][order(pct[is_anchor])], items[!is_anchor][order(pct[!is_anchor])])
}

# One weighted proportion for one already-filtered subset of the long select/rank table.
prop_one_group <- function(sub, indicator_col, weight_col = "weight") {
  des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
  est <- svyciprop(reformulate(indicator_col), des, method = "logit")
  tibble(pct = as.numeric(est) * 100, ci_low = confint(est)[1] * 100, ci_high = confint(est)[2] * 100, n = nrow(sub))
}

# `indicator_col` names a logical column the caller has already added to `long_data` (e.g.
# `selected == "1"`, or `coalesce(rank == 1, FALSE)`). One weighted % per item_tag, overall (no
# party breakdown - not needed by anything in this script).
battery_props_by_party <- function(long_data, indicator_col, weight_col = "weight") {
  tags <- sort(unique(as.character(long_data$item_tag)))
  results <- list()
  for (i in seq_along(tags)) {
    sub <- long_data %>% filter(item_tag == tags[i])
    if (nrow(sub) == 0) next
    results[[i]] <- prop_one_group(sub, indicator_col, weight_col) %>%
      mutate(item_tag = tags[i], item = sub$display[1], .before = 1)
  }
  bind_rows(results)
}

# Filters the long select/rank table to one or more `battery` values, keeping only in-base rows -
# "include everyone" is simply both arms' battery values at once (exactly one arm is in_base per
# respondent, so no coalesce() is needed to pool them).
prep_battery_long <- function(battery_values) {
  sources.long %>% filter(battery %in% battery_values, in_base)
}

message("Part C: estimators ready.")




##### #
#### #
### ################################################################################################################################################# #
# Part D. Pooled contact-mode columns (needed only for the "everyone included" arm) --------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# Contact mode is a single nominal item, not a multiselect battery, so it never went into
# sources.long. The exclude/never-only arms below read the raw act/hyp columns directly (both
# already exist in `data`); only the "everyone included" arm needs this pooled version, which
# normalizes the hypothetical arm's "I would visit/call/visit" wording to the actual arm's "I
# visit/call/visit" before coalescing, so the pooled factor stays at 5 levels instead of 8.
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

message("Part D: pooled contact-mode columns ready.")




##### #
#### #
### ################################################################################################################################################# #
# Part E. Which sources (exclude never-seekers / never-seekers only) ------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# Deliberately unrolled - no loops - so each chart below is a standalone, copy-paste-able block.
# Bars show weighted % selected (top 3), with a gray capped CI; the orange diamond shows weighted %
# ranked #1 among the same base - both metrics come from the same filtered slice of sources.long,
# so they are directly comparable on one axis.


### ####################################################################################### ###
## E.1. Registering and Voting ----
### ####################################################################################### ###

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.1.1. Exclude never-seekers ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

reg_exclude_long <- prep_battery_long("reg_act")

reg_exclude_which_df <- bind_rows(
  battery_props_by_party(reg_exclude_long %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(reg_exclude_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first") %>%
    mutate(metric = "Ranked #1", .before = 1)
)

reg_exclude_sel <- reg_exclude_which_df %>% filter(metric == "Selected (top 3)")
reg_exclude_which_df <- reg_exclude_which_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(reg_exclude_sel$item, reg_exclude_sel$pct)))

p <- ggplot(filter(reg_exclude_which_df, metric == "Selected (top 3)"), aes(y = item)) +
  geom_col(aes(x = pct, fill = metric), width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2,
                linewidth = 1, color = "grey75", alpha = 0.6) +
  geom_point(data = filter(reg_exclude_which_df, metric == "Ranked #1"), aes(x = pct, color = metric), size = 2.8, shape = 18) +
  scale_fill_manual(values = c("Selected (top 3)" = BAR_COLOR), name = NULL) +
  scale_color_manual(values = c("Ranked #1" = RANK_COLOR), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap36) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title(qtitle("reg_act_src_local_officials")),
       subtitle = wrap_subtitle(paste0("Among voters who seek this information; n = ", format(reg_exclude_sel$n[1], big.mark = ",")))) +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank())
p

reg_exclude_which_wide_df <- reg_exclude_which_df %>%
  filter(metric == "Selected (top 3)") %>%
  select(item_tag, item, n, selected_pct = pct, ci_low, ci_high) %>%
  left_join(reg_exclude_which_df %>% filter(metric == "Ranked #1") %>% select(item_tag, ranked_pct = pct), by = "item_tag")
write.csv(reg_exclude_which_wide_df, file.path(plots.dir, "reg_exclude_which_wide.csv"), row.names = FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.1.2. Never-seekers only ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

reg_never_long <- prep_battery_long("reg_hyp")

reg_never_which_df <- bind_rows(
  battery_props_by_party(reg_never_long %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(reg_never_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first") %>%
    mutate(metric = "Ranked #1", .before = 1)
)

reg_never_sel <- reg_never_which_df %>% filter(metric == "Selected (top 3)")
reg_never_which_df <- reg_never_which_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(reg_never_sel$item, reg_never_sel$pct)))

p <- ggplot(filter(reg_never_which_df, metric == "Selected (top 3)"), aes(y = item)) +
  geom_col(aes(x = pct, fill = metric), width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2,
                linewidth = 1, color = "grey75", alpha = 0.6) +
  geom_point(data = filter(reg_never_which_df, metric == "Ranked #1"), aes(x = pct, color = metric), size = 2.8, shape = 18) +
  scale_fill_manual(values = c("Selected (top 3)" = BAR_COLOR), name = NULL) +
  scale_color_manual(values = c("Ranked #1" = RANK_COLOR), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap36) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title(qtitle("reg_hyp_src_local_officials")),
       subtitle = wrap_subtitle(paste0("Among voters who never seek this information; n = ", format(reg_never_sel$n[1], big.mark = ",")))) +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank())
p

reg_never_which_wide_df <- reg_never_which_df %>%
  filter(metric == "Selected (top 3)") %>%
  select(item_tag, item, n, selected_pct = pct, ci_low, ci_high) %>%
  left_join(reg_never_which_df %>% filter(metric == "Ranked #1") %>% select(item_tag, ranked_pct = pct), by = "item_tag")
write.csv(reg_never_which_wide_df, file.path(plots.dir, "reg_never_which_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## E.2. How Elections Are Run ----
### ####################################################################################### ###

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.2.1. Exclude never-seekers ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

run_exclude_long <- prep_battery_long("run_act")

run_exclude_which_df <- bind_rows(
  battery_props_by_party(run_exclude_long %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(run_exclude_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first") %>%
    mutate(metric = "Ranked #1", .before = 1)
)

run_exclude_sel <- run_exclude_which_df %>% filter(metric == "Selected (top 3)")
run_exclude_which_df <- run_exclude_which_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(run_exclude_sel$item, run_exclude_sel$pct)))

p <- ggplot(filter(run_exclude_which_df, metric == "Selected (top 3)"), aes(y = item)) +
  geom_col(aes(x = pct, fill = metric), width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.35,
                linewidth = 1, color = "grey35", alpha = 0.6) +
  geom_point(data = filter(run_exclude_which_df, metric == "Ranked #1"), aes(x = pct, color = metric), size = 2.8, shape = 18) +
  scale_fill_manual(values = c("Selected (top 3)" = BAR_COLOR), name = NULL) +
  scale_color_manual(values = c("Ranked #1" = RANK_COLOR), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap36) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title(qtitle("run_act_src_local_officials")),
       subtitle = wrap_subtitle(paste0("Among voters who seek this information; n = ", format(run_exclude_sel$n[1], big.mark = ",")))) +
  theme(legend.position = "top")
p

run_exclude_which_wide_df <- run_exclude_which_df %>%
  filter(metric == "Selected (top 3)") %>%
  select(item_tag, item, n, selected_pct = pct, ci_low, ci_high) %>%
  left_join(run_exclude_which_df %>% filter(metric == "Ranked #1") %>% select(item_tag, ranked_pct = pct), by = "item_tag")
write.csv(run_exclude_which_wide_df, file.path(plots.dir, "run_exclude_which_wide.csv"), row.names = FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.2.2. Never-seekers only ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

run_never_long <- prep_battery_long("run_hyp")

run_never_which_df <- bind_rows(
  battery_props_by_party(run_never_long %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(run_never_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first") %>%
    mutate(metric = "Ranked #1", .before = 1)
)

run_never_sel <- run_never_which_df %>% filter(metric == "Selected (top 3)")
run_never_which_df <- run_never_which_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(run_never_sel$item, run_never_sel$pct)))

p <- ggplot(filter(run_never_which_df, metric == "Selected (top 3)"), aes(y = item)) +
  geom_col(aes(x = pct, fill = metric), width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.35,
                linewidth = 1, color = "grey35", alpha = 0.6) +
  geom_point(data = filter(run_never_which_df, metric == "Ranked #1"), aes(x = pct, color = metric), size = 2.8, shape = 18) +
  scale_fill_manual(values = c("Selected (top 3)" = BAR_COLOR), name = NULL) +
  scale_color_manual(values = c("Ranked #1" = RANK_COLOR), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap36) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title(qtitle("run_hyp_src_local_officials")),
       subtitle = wrap_subtitle(paste0("Among voters who never seek this information; n = ", format(run_never_sel$n[1], big.mark = ",")))) +
  theme(legend.position = "top")
p

run_never_which_wide_df <- run_never_which_df %>%
  filter(metric == "Selected (top 3)") %>%
  select(item_tag, item, n, selected_pct = pct, ci_low, ci_high) %>%
  left_join(run_never_which_df %>% filter(metric == "Ranked #1") %>% select(item_tag, ranked_pct = pct), by = "item_tag")
write.csv(run_never_which_wide_df, file.path(plots.dir, "run_never_which_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## E.3. Who Won an Election ----
### ####################################################################################### ###

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.3.1. Exclude never-seekers ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

won_exclude_long <- prep_battery_long("won_act")

won_exclude_which_df <- bind_rows(
  battery_props_by_party(won_exclude_long %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(won_exclude_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first") %>%
    mutate(metric = "Ranked #1", .before = 1)
)

won_exclude_sel <- won_exclude_which_df %>% filter(metric == "Selected (top 3)")
won_exclude_which_df <- won_exclude_which_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(won_exclude_sel$item, won_exclude_sel$pct)))

p <- ggplot(filter(won_exclude_which_df, metric == "Selected (top 3)"), aes(y = item)) +
  geom_col(aes(x = pct, fill = metric), width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.35,
                linewidth = 1, color = "grey35", alpha = 0.6) +
  geom_point(data = filter(won_exclude_which_df, metric == "Ranked #1"), aes(x = pct, color = metric), size = 2.8, shape = 18) +
  scale_fill_manual(values = c("Selected (top 3)" = BAR_COLOR), name = NULL) +
  scale_color_manual(values = c("Ranked #1" = RANK_COLOR), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap36) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title(qtitle("won_act_src_local_officials")),
       subtitle = wrap_subtitle(paste0("Among voters who seek this information; n = ", format(won_exclude_sel$n[1], big.mark = ",")))) +
  theme(legend.position = "top")
p

won_exclude_which_wide_df <- won_exclude_which_df %>%
  filter(metric == "Selected (top 3)") %>%
  select(item_tag, item, n, selected_pct = pct, ci_low, ci_high) %>%
  left_join(won_exclude_which_df %>% filter(metric == "Ranked #1") %>% select(item_tag, ranked_pct = pct), by = "item_tag")
write.csv(won_exclude_which_wide_df, file.path(plots.dir, "won_exclude_which_wide.csv"), row.names = FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.3.2. Never-seekers only ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

won_never_long <- prep_battery_long("won_hyp")

won_never_which_df <- bind_rows(
  battery_props_by_party(won_never_long %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(won_never_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first") %>%
    mutate(metric = "Ranked #1", .before = 1)
)

won_never_sel <- won_never_which_df %>% filter(metric == "Selected (top 3)")
won_never_which_df <- won_never_which_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(won_never_sel$item, won_never_sel$pct)))

p <- ggplot(filter(won_never_which_df, metric == "Selected (top 3)"), aes(y = item)) +
  geom_col(aes(x = pct, fill = metric), width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.35,
                linewidth = 1, color = "grey35", alpha = 0.6) +
  geom_point(data = filter(won_never_which_df, metric == "Ranked #1"), aes(x = pct, color = metric), size = 2.8, shape = 18) +
  scale_fill_manual(values = c("Selected (top 3)" = BAR_COLOR), name = NULL) +
  scale_color_manual(values = c("Ranked #1" = RANK_COLOR), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap36) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title(qtitle("won_hyp_src_local_officials")),
       subtitle = wrap_subtitle(paste0("Among voters who never seek this information; n = ", format(won_never_sel$n[1], big.mark = ",")))) +
  theme(legend.position = "top")
p

won_never_which_wide_df <- won_never_which_df %>%
  filter(metric == "Selected (top 3)") %>%
  select(item_tag, item, n, selected_pct = pct, ci_low, ci_high) %>%
  left_join(won_never_which_df %>% filter(metric == "Ranked #1") %>% select(item_tag, ranked_pct = pct), by = "item_tag")
write.csv(won_never_which_wide_df, file.path(plots.dir, "won_never_which_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## E.4. Combined across all three information needs ----
### ####################################################################################### ###

# Same combo (bar + gray capped CI + ranked-#1 diamond) as E.1-E.3 above, but faceted across the
# 3 information needs instead of one chart per need - combines E.1.1/E.2.1/E.3.1 into one plot,
# and E.1.2/E.2.2/E.3.2 into another. `fill`/`color` still encode `metric` (Selected vs. Ranked
# #1), exactly as in the single-need charts above - `need` only drives the facet, not a color, so
# the existing two-item legend stays meaningful instead of being replaced by a need-based one.

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.4.1. Exclude never-seekers ---- ARTICLEPLOT -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Self-contained - built straight from prep_battery_long()/battery_props_by_party() (Part C), not
# from E.1.1/E.2.1/E.3.1's reg_exclude_which_df/run_exclude_which_df/won_exclude_which_df, so this
# block (and E.4.2 below) can run on its own without E.1-E.3 having run first.

which_exclude_reg_long <- prep_battery_long("reg_act")
which_exclude_reg_df <- bind_rows(
  battery_props_by_party(which_exclude_reg_long %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(which_exclude_reg_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first") %>%
    mutate(metric = "Ranked #1", .before = 1)
) %>% mutate(need = "Registering and Voting", .before = 1)

which_exclude_run_long <- prep_battery_long("run_act")
which_exclude_run_df <- bind_rows(
  battery_props_by_party(which_exclude_run_long %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(which_exclude_run_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first") %>%
    mutate(metric = "Ranked #1", .before = 1)
) %>% mutate(need = "How Elections Are Run", .before = 1)

which_exclude_won_long <- prep_battery_long("won_act")
which_exclude_won_df <- bind_rows(
  battery_props_by_party(which_exclude_won_long %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(which_exclude_won_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first") %>%
    mutate(metric = "Ranked #1", .before = 1)
) %>% mutate(need = "Who Won an Election", .before = 1)

which_exclude_combined_df <- bind_rows(which_exclude_reg_df, which_exclude_run_df, which_exclude_won_df)

which_exclude_means <- which_exclude_combined_df %>% filter(metric == "Selected (top 3)") %>%
  group_by(item) %>% summarise(m = mean(pct), .groups = "drop")

# n differs by need (each is its own base), so it goes in the facet strip rather than a single
# subtitle line that could only ever state one of the three - a two-line label (via an explicit
# "\n") avoids the risk of a "Need (n = X)" one-liner overflowing the panel width.
which_exclude_n_by_need <- which_exclude_combined_df %>% distinct(need, n) %>% deframe()
which_exclude_combined_df <- which_exclude_combined_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(which_exclude_means$item, which_exclude_means$m)),
         need = factor(need, levels = NEED_LEVELS),
         need_label = factor(paste0(need, "\n(n = ", format(which_exclude_n_by_need[as.character(need)], big.mark = ","), ")"),
                              levels = paste0(NEED_LEVELS, "\n(n = ", format(which_exclude_n_by_need[NEED_LEVELS], big.mark = ","), ")"))) %>%
  filter(item_tag != "other")

p <- ggplot(filter(which_exclude_combined_df, metric == "Selected (top 3)"), aes(y = item)) +
  geom_col(aes(x = pct, fill = metric), width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey75", alpha = 0.6) +
  geom_point(data = filter(which_exclude_combined_df, metric == "Ranked #1"), aes(x = pct, color = metric), size = 2.4, shape = 18) +
  scale_fill_manual(values = c("Selected (top 3)" = BAR_COLOR), name = NULL) +
  scale_color_manual(values = c("Ranked #1" = RANK_COLOR), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .125))) +
  scale_y_discrete(labels = wrap30) +
  facet_wrap(~ need_label, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("Which Sources Voters Use to Find Election Information"),
       subtitle = "Among respondents who report seeking this information") +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p

# Unlike the mode/social/bot wide exports, this chart is faceted by need - Datawrapper has no
# facet equivalent, so recreating it there means one Datawrapper chart per need. Split into 3
# CSVs, one per need (same shape the single-need which-sources charts already use: item_tag, item,
# n, selected_pct, ci_low, ci_high, ranked_pct), each keeping the "Other, please specify" exclusion
# already applied to which_exclude_combined_df (11 items, not the 12 in reg/run/won_exclude_which_wide.csv).
which_exclude_combined_wide_df <- which_exclude_combined_df %>%
  filter(metric == "Selected (top 3)") %>%
  select(need, item_tag, item, n, selected_pct = pct, ci_low, ci_high) %>%
  left_join(which_exclude_combined_df %>% filter(metric == "Ranked #1") %>% select(need, item_tag, ranked_pct = pct),
            by = c("need", "item_tag"))
for (nd in NEED_LEVELS) {
  write.csv(which_exclude_combined_wide_df %>% filter(need == nd) %>% select(-need),
            file.path(plots.dir, paste0("which_exclude_", NEED_SLUGS[[nd]], "_wide.csv")), row.names = FALSE)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.4.2. Never-seekers only ---- ARTICLEPLOT ------
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Self-contained, same reasoning as E.4.1 - built from prep_battery_long()/battery_props_by_party()
# directly rather than reusing E.1.2/E.2.2/E.3.2's reg_never_which_df/run_never_which_df/won_never_which_df.

which_never_reg_long <- prep_battery_long("reg_hyp")
which_never_reg_df <- bind_rows(
  battery_props_by_party(which_never_reg_long %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(which_never_reg_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first") %>%
    mutate(metric = "Ranked #1", .before = 1)
) %>% mutate(need = "Registering and Voting", .before = 1)

which_never_run_long <- prep_battery_long("run_hyp")
which_never_run_df <- bind_rows(
  battery_props_by_party(which_never_run_long %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(which_never_run_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first") %>%
    mutate(metric = "Ranked #1", .before = 1)
) %>% mutate(need = "How Elections Are Run", .before = 1)

which_never_won_long <- prep_battery_long("won_hyp")
which_never_won_df <- bind_rows(
  battery_props_by_party(which_never_won_long %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(metric = "Selected (top 3)", .before = 1),
  battery_props_by_party(which_never_won_long %>% mutate(is_first = coalesce(rank == 1, FALSE)), "is_first") %>%
    mutate(metric = "Ranked #1", .before = 1)
) %>% mutate(need = "Who Won an Election", .before = 1)

which_never_combined_df <- bind_rows(which_never_reg_df, which_never_run_df, which_never_won_df)

which_never_means <- which_never_combined_df %>% filter(metric == "Selected (top 3)") %>%
  group_by(item) %>% summarise(m = mean(pct), .groups = "drop")

which_never_n_by_need <- which_never_combined_df %>% distinct(need, n) %>% deframe()
which_never_combined_df <- which_never_combined_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(which_never_means$item, which_never_means$m)),
         need = factor(need, levels = NEED_LEVELS),
         need_label = factor(paste0(need, "\n(n = ", format(which_never_n_by_need[as.character(need)], big.mark = ","), ")"),
                              levels = paste0(NEED_LEVELS, "\n(n = ", format(which_never_n_by_need[NEED_LEVELS], big.mark = ","), ")"))) %>%
  filter(item_tag != "other")

p <- ggplot(filter(which_never_combined_df, metric == "Selected (top 3)"), aes(y = item)) +
  geom_col(aes(x = pct, fill = metric), width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey75", alpha = 0.6) +
  geom_point(data = filter(which_never_combined_df, metric == "Ranked #1"), aes(x = pct, color = metric), size = 2.4, shape = 18) +
  scale_fill_manual(values = c("Selected (top 3)" = BAR_COLOR), name = NULL) +
  scale_color_manual(values = c("Ranked #1" = RANK_COLOR), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .12))) +
  scale_y_discrete(labels = wrap30) +
  facet_wrap(~ need_label, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("Which Sources Voters Would Use to Find Election Information"),
       subtitle = wrap_subtitle("Among respondents who never seek this information, by information need")) +
  theme(legend.position = "top", strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p

# Split into 3 CSVs, one per need, same reasoning as E.4.1's export above.
which_never_combined_wide_df <- which_never_combined_df %>%
  filter(metric == "Selected (top 3)") %>%
  select(need, item_tag, item, n, selected_pct = pct, ci_low, ci_high) %>%
  left_join(which_never_combined_df %>% filter(metric == "Ranked #1") %>% select(need, item_tag, ranked_pct = pct),
            by = c("need", "item_tag"))
for (nd in NEED_LEVELS) {
  write.csv(which_never_combined_wide_df %>% filter(need == nd) %>% select(-need),
            file.path(plots.dir, paste0("which_never_", NEED_SLUGS[[nd]], "_wide.csv")), row.names = FALSE)
}




##### #
#### #
### ################################################################################################################################################# #
# Part F. Drill-downs, combined across all 3 information needs ------------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# Each drill-down category (contact mode, social media platform, AI chatbot) gets ONE chart per
# population arm, faceted across the 3 information needs rather than one chart per need - e.g. one
# "social media platforms" chart with a panel each for register/vote, elections-run, and who-won.
# Deliberately unrolled, same as Part E.


### ####################################################################################### ###
## F.1. Contact mode (local/state election officials) ----
### ####################################################################################### ###

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.1.1. Exclude never-seekers ---- ARTICLEPLOT ------
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

mode_exclude_combined_df <- bind_rows(
  factor_props(data, "reg_act_official_mode") %>% mutate(need = "Registering and Voting", .before = 1),
  factor_props(data, "run_act_official_mode") %>% mutate(need = "How Elections Are Run", .before = 1),
  factor_props(data, "won_act_official_mode") %>% mutate(need = "Who Won an Election", .before = 1)
) %>%
  filter(!category %in% c("Other, please specify", "Don't know")) %>%
  mutate(need = fct_relevel(need, "Registering and Voting", "How Elections Are Run", "Who Won an Election"),
         category = case_when(!grepl("website", category) ~ gsub(" \\(town, city, county\\)", "", category),
                              TRUE ~ category),
         category = as.factor(category))

mode_exclude_levels <- setdiff(levels(mode_exclude_combined_df$category), c("Other, please specify", "Don't know"))
mode_exclude_colors <- setNames(colorRampPalette(c("#b7d3f6", "#0d366b"))(length(mode_exclude_levels)), mode_exclude_levels)
mode_exclude_combined_df <- mode_exclude_combined_df %>%
  mutate(category = fct_relevel(category,
                                "I visit my local (town, city, county) or state election office's website",
                                "I call my local or state election office on the phone",
                                "I visit my local or state election office in person"))

# Same "(n = X)" treatment as E.4.1/F.1.2 - n differs by need (each is its own base), so a two-line
# "\n(n = X)" label is built per need rather than a single subtitle that could only state one.
mode_exclude_n_by_need <- mode_exclude_combined_df %>% distinct(need, n) %>% deframe()
mode_exclude_combined_df <- mode_exclude_combined_df %>%
  mutate(need_label = factor(paste0(need, "\n(n = ", trimws(format(mode_exclude_n_by_need[as.character(need)], big.mark = ",")), ")"),
                              levels = paste0(NEED_LEVELS, "\n(n = ", trimws(format(mode_exclude_n_by_need[NEED_LEVELS], big.mark = ",")), ")")))

p <- ggplot(mode_exclude_combined_df, aes(y = need_label, x = pct, fill = category)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), position = position_dodge(width = 0.9),
                orientation = "y", width = 0.2, color = "grey35", alpha = 0.6) +
  geom_text(aes(x = ci_high, label = paste0(round(pct), "%")), position = position_dodge(width = 0.9),
            hjust = -0.15, size = 3.1, color = "grey30") +
  scale_fill_manual(values = mode_exclude_colors, name = NULL, labels = wrap40) +
  scale_x_continuous(expand = expansion(mult = c(0, .125))) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("How Voters Contact Local or State Election Officials"),
       subtitle = "Among respondents who seek this information, by information need") +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank())
p

mode_exclude_combined_wide_df <- mode_exclude_combined_df %>%
  mutate(category_key = case_when(grepl("website", category) ~ "website",
                                   grepl("phone", category) ~ "phone",
                                   TRUE ~ "in_person")) %>%
  pivot_wider(id_cols = need, names_from = category_key, values_from = pct, names_glue = "{category_key}_pct") %>%
  select(need, website_pct, phone_pct, in_person_pct)
write.csv(mode_exclude_combined_wide_df, file.path(plots.dir, "mode_exclude_combined_wide.csv"), row.names = FALSE)



# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.1.2. Everyone included ---- 
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

mode_include_combined_df <- bind_rows(
  factor_props(data, "reg_official_mode_pooled") %>% mutate(need = "Registering and Voting", .before = 1),
  factor_props(data, "run_official_mode_pooled") %>% mutate(need = "How Elections Are Run", .before = 1),
  factor_props(data, "won_official_mode_pooled") %>% mutate(need = "Who Won an Election", .before = 1)
) %>%
  filter(!category %in% c("Other, please specify", "Don't know")) %>%
  mutate(need = fct_relevel(need, "Registering and Voting", "How Elections Are Run", "Who Won an Election"),
         category = case_when(!grepl("website", category) ~ gsub(" \\(town, city, county\\)", "", category),
                              TRUE ~ category),
         category = as.factor(category))

mode_include_levels <- setdiff(levels(mode_include_combined_df$category), c("Other, please specify", "Don't know"))
mode_include_colors <- setNames(colorRampPalette(c("#b7d3f6", "#0d366b"))(length(mode_include_levels)), mode_include_levels)
mode_include_combined_df <- mode_include_combined_df %>%
  mutate(category = fct_relevel(category,
                                "I visit my local (town, city, county) or state election office's website",
                                "I call my local or state election office on the phone",
                                "I visit my local or state election office in person"))

# Same "(n = X)" treatment as E.4.1's facet strips, just on an axis tick label here instead of a
# facet strip - n differs by need (each is its own base), so a two-line "\n(n = X)" label is built
# per need rather than a single subtitle that could only ever state one of the three.
mode_include_n_by_need <- mode_include_combined_df %>% distinct(need, n) %>% deframe()
mode_include_combined_df <- mode_include_combined_df %>%
  mutate(need_label = factor(paste0(need, "\n(n = ", trimws(format(mode_include_n_by_need[as.character(need)], big.mark = ",")), ")"),
                              levels = paste0(NEED_LEVELS, "\n(n = ", trimws(format(mode_include_n_by_need[NEED_LEVELS], big.mark = ",")), ")")))

p <- ggplot(mode_include_combined_df, aes(y = need_label, x = pct, fill = category)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), position = position_dodge(width = 0.9),
                orientation = "y", width = 0.2, color = "grey65", alpha = 0.6) +
  geom_text(aes(x = ci_high, label = paste0(round(pct), "%")), position = position_dodge(width = 0.9),
            hjust = -0.15, size = 3.1, color = "grey30") +
  scale_fill_manual(values = mode_include_colors, name = NULL, labels = wrap40) +
  scale_x_continuous(expand = expansion(mult = c(0, .125))) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("How Voters Contact Local or State Election Officials"),
       subtitle = "Among all respondents (actual- and hypothetical-arm pooled), by information need") +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank())
p

mode_include_combined_wide_df <- mode_include_combined_df %>%
  mutate(category_key = case_when(grepl("website", category) ~ "website",
                                   grepl("phone", category) ~ "phone",
                                   TRUE ~ "in_person")) %>%
  pivot_wider(id_cols = need, names_from = category_key, values_from = pct, names_glue = "{category_key}_pct") %>%
  select(need, website_pct, phone_pct, in_person_pct)
write.csv(mode_include_combined_wide_df, file.path(plots.dir, "mode_include_combined_wide.csv"), row.names = FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.1.3. Never-seekers only ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

mode_never_combined_df <- bind_rows(
  factor_props(data, "reg_hyp_official_mode") %>% mutate(need = "Registering and Voting", .before = 1),
  factor_props(data, "run_hyp_official_mode") %>% mutate(need = "How Elections Are Run", .before = 1),
  factor_props(data, "won_hyp_official_mode") %>% mutate(need = "Who Won an Election", .before = 1)
) %>%
  filter(!category %in% c("Other, please specify", "Don't know")) %>%
  mutate(need = fct_relevel(need, "Registering and Voting", "How Elections Are Run", "Who Won an Election"),
         category = case_when(!grepl("website", category) ~ gsub(" \\(town, city, county\\)", "", category),
                              TRUE ~ category),
         category = as.factor(category))

mode_never_levels <- setdiff(levels(mode_never_combined_df$category), c("Other, please specify", "Don't know"))
mode_never_colors <- setNames(colorRampPalette(c("#b7d3f6", "#0d366b"))(length(mode_never_levels)), mode_never_levels)
mode_never_combined_df <- mode_never_combined_df %>%
  mutate(category = fct_relevel(category,
                                "I would visit my local (town, city, county) or state election office's website",
                                "I would call my local or state election office on the phone",
                                "I would visit my local or state election office in person"))

p <- ggplot(mode_never_combined_df, aes(x = need, y = pct, fill = category)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = 0.9),
                width = 0.2, color = "grey35", alpha = 0.6) +
  scale_fill_manual(values = mode_never_colors, name = NULL, labels = wrap40) +
  scale_y_continuous(expand = expansion(mult = c(0, .02))) +
  labs(y = "Weighted %", x = NULL,
       title = wrap_title("How Voters Contact Local or State Election Officials"),
       subtitle = "Among respondents who never seek this information, by information need") +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank())
p

mode_never_combined_wide_df <- mode_never_combined_df %>%
  mutate(category_key = case_when(grepl("website", category) ~ "website",
                                   grepl("phone", category) ~ "phone",
                                   TRUE ~ "in_person")) %>%
  pivot_wider(id_cols = need, names_from = category_key, values_from = pct, names_glue = "{category_key}_pct") %>%
  select(need, website_pct, phone_pct, in_person_pct)
write.csv(mode_never_combined_wide_df, file.path(plots.dir, "mode_never_combined_wide.csv"), row.names = FALSE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.1.4. Everyone included, by age group ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Same population arm and variables as F.1.2, cross-tabbed by age4 instead of collapsed across
# it. Reuses mode_include_colors (computed in F.1.2, just above) so website/phone/in-person keep
# the same color in both charts.
mode_include_age_df <- bind_rows(
  factor_props_by(data, "reg_official_mode_pooled", "age4") %>% mutate(need = "Registering and Voting", .before = 1),
  factor_props_by(data, "run_official_mode_pooled", "age4") %>% mutate(need = "How Elections Are Run", .before = 1),
  factor_props_by(data, "won_official_mode_pooled", "age4") %>% mutate(need = "Who Won an Election", .before = 1)
) %>%
  filter(!category %in% c("Other, please specify", "Don't know")) %>%
  mutate(need = fct_relevel(need, "Registering and Voting", "How Elections Are Run", "Who Won an Election"),
         category = case_when(!grepl("website", category) ~ gsub(" \\(town, city, county\\)", "", category),
                              TRUE ~ category),
         category = as.factor(category))

mode_include_age_df <- mode_include_age_df %>%
  mutate(category = fct_relevel(category,
                                "I visit my local (town, city, county) or state election office's website",
                                "I call my local or state election office on the phone",
                                "I visit my local or state election office in person"))

p <- ggplot(mode_include_age_df, aes(x = age4, y = pct, fill = category)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = 0.9),
                width = 0.2, color = "grey65", alpha = 0.6) +
  facet_wrap(~ need, nrow = 1) +
  scale_fill_manual(values = mode_include_colors, name = NULL, labels = wrap40) +
  scale_y_continuous(expand = expansion(mult = c(0, .02))) +
  labs(y = "Weighted %", x = NULL,
       title = wrap_title("How Voters Contact Local or State Election Officials"),
       subtitle = "Among all respondents (actual- and hypothetical-arm pooled), by age group and information need") +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank())
p

mode_include_age_wide_df <- mode_include_age_df %>%
  mutate(category_key = case_when(grepl("website", category) ~ "website",
                                   grepl("phone", category) ~ "phone",
                                   TRUE ~ "in_person")) %>%
  pivot_wider(id_cols = c(need, age4), names_from = category_key, values_from = pct, names_glue = "{category_key}_pct") %>%
  select(need, age4, website_pct, phone_pct, in_person_pct)
write.csv(mode_include_age_wide_df, file.path(plots.dir, "mode_include_by_age_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## F.2. Social media platform ----
### ####################################################################################### ###

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.2.1. Exclude never-seekers ----
# - - - - - - - - - - - -  - - - - - - - - - - - - - - -

social_exclude_combined_df <- bind_rows(
  battery_props_by_party(prep_battery_long("reg_act_social") %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "Registering and Voting", .before = 1),
  battery_props_by_party(prep_battery_long("run_act_social") %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "How Elections Are Run", .before = 1),
  battery_props_by_party(prep_battery_long("won_act_social") %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "Who Won an Election", .before = 1)
)

social_exclude_means <- social_exclude_combined_df %>% group_by(item) %>% summarise(m = mean(pct), .groups = "drop")
social_exclude_combined_df <- social_exclude_combined_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(social_exclude_means$item, social_exclude_means$m)),
         need = factor(need, levels = NEED_LEVELS))

p <- ggplot(social_exclude_combined_df, aes(x = pct, y = item, fill = need)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey75", alpha = 0.6) +
  scale_fill_manual(values = NEED_COLORS, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap30) +
  facet_wrap(~ need, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("Social Media Platforms Used for Election Information"),
       subtitle = wrap_subtitle("Among respondents who seek this information, by information need")) +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p

social_exclude_combined_wide_df <- social_exclude_combined_df %>%
  pivot_wider(id_cols = c(item_tag, item), names_from = need, values_from = pct)
write.csv(social_exclude_combined_wide_df, file.path(plots.dir, "social_exclude_combined_wide.csv"), row.names = FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.2.2. Everyone included ---- ARTICLEPLOT ------
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

social_include_combined_df <- bind_rows(
  battery_props_by_party(prep_battery_long(c("reg_act_social", "reg_hyp_social")) %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "Registering and Voting", .before = 1),
  battery_props_by_party(prep_battery_long(c("run_act_social", "run_hyp_social")) %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "How Elections Are Run", .before = 1),
  battery_props_by_party(prep_battery_long(c("won_act_social", "won_hyp_social")) %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "Who Won an Election", .before = 1)
)

social_include_means <- social_include_combined_df %>% group_by(item) %>% summarise(m = mean(pct), .groups = "drop")
social_include_combined_df <- social_include_combined_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(social_include_means$item, social_include_means$m)),
         need = factor(need, levels = NEED_LEVELS)) %>% 
  filter(!item_tag %in% c("not_sure", "other"))

p <- ggplot(social_include_combined_df, aes(x = pct, y = item, fill = need)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey75", alpha = 0.6) +
  scale_fill_manual(values = NEED_COLORS, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap30) +
  facet_wrap(~ need, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("Social Media Platforms Used for Election Information"),
       subtitle = "Among all respondents (actual- and hypothetical-arm pooled), by information type") +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p

social_include_combined_wide_df <- social_include_combined_df %>%
  pivot_wider(id_cols = c(item_tag, item), names_from = need, values_from = pct)
write.csv(social_include_combined_wide_df, file.path(plots.dir, "social_include_combined_wide.csv"), row.names = FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.2.3. Never-seekers only ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

social_never_combined_df <- bind_rows(
  battery_props_by_party(prep_battery_long("reg_hyp_social") %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "Registering and Voting", .before = 1),
  battery_props_by_party(prep_battery_long("run_hyp_social") %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "How Elections Are Run", .before = 1),
  battery_props_by_party(prep_battery_long("won_hyp_social") %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "Who Won an Election", .before = 1)
)

social_never_means <- social_never_combined_df %>% group_by(item) %>% summarise(m = mean(pct), .groups = "drop")
social_never_combined_df <- social_never_combined_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(social_never_means$item, social_never_means$m)),
         need = factor(need, levels = NEED_LEVELS))

p <- ggplot(social_never_combined_df, aes(x = pct, y = item, fill = need)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey75", alpha = 0.6) +
  scale_fill_manual(values = NEED_COLORS, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap30) +
  facet_wrap(~ need, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("Social Media Platforms Used for Election Information"),
       subtitle = wrap_subtitle("Among respondents who never seek this information, by information need")) +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p

social_never_combined_wide_df <- social_never_combined_df %>%
  pivot_wider(id_cols = c(item_tag, item), names_from = need, values_from = pct)
write.csv(social_never_combined_wide_df, file.path(plots.dir, "social_never_combined_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## F.3. AI chatbot ----
### ####################################################################################### ###

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.3.1. Exclude never-seekers ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

bot_exclude_combined_df <- bind_rows(
  battery_props_by_party(prep_battery_long("reg_act_bot") %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "Registering and Voting", .before = 1),
  battery_props_by_party(prep_battery_long("run_act_bot") %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "How Elections Are Run", .before = 1),
  battery_props_by_party(prep_battery_long("won_act_bot") %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "Who Won an Election", .before = 1)
)

bot_exclude_means <- bot_exclude_combined_df %>% group_by(item) %>% summarise(m = mean(pct), .groups = "drop")
bot_exclude_combined_df <- bot_exclude_combined_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(bot_exclude_means$item, bot_exclude_means$m)),
         need = factor(need, levels = NEED_LEVELS))

p <- ggplot(bot_exclude_combined_df, aes(x = pct, y = item, fill = need)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.3, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_fill_manual(values = NEED_COLORS, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap30) +
  facet_wrap(~ need, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("AI Chatbots Used for Election Information"),
       subtitle = wrap_subtitle("Among respondents who seek this information, by information need")) +
  theme(strip.text = element_text(face = "bold"))
p

bot_exclude_combined_wide_df <- bot_exclude_combined_df %>%
  pivot_wider(id_cols = c(item_tag, item), names_from = need, values_from = pct)
write.csv(bot_exclude_combined_wide_df, file.path(plots.dir, "bot_exclude_combined_wide.csv"), row.names = FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.3.2. Everyone included ---- ARTICLEPLOT ------
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

bot_include_combined_df <- bind_rows(
  battery_props_by_party(prep_battery_long(c("reg_act_bot", "reg_hyp_bot")) %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "Registering and Voting", .before = 1),
  battery_props_by_party(prep_battery_long(c("run_act_bot", "run_hyp_bot")) %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "How Elections Are Run", .before = 1),
  battery_props_by_party(prep_battery_long(c("won_act_bot", "won_hyp_bot")) %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "Who Won an Election", .before = 1)
)

bot_include_means <- bot_include_combined_df %>% group_by(item) %>% summarise(m = mean(pct), .groups = "drop")
bot_include_combined_df <- bot_include_combined_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(bot_include_means$item, bot_include_means$m)),
         need = factor(need, levels = NEED_LEVELS)) %>% 
  filter(!item_tag %in% c("not_sure", "other"))

p <- ggplot(bot_include_combined_df, aes(x = pct, y = item, fill = need)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.3, linewidth = 0.8, color = "grey65", alpha = 0.6) +
  scale_fill_manual(values = NEED_COLORS, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap30) +
  facet_wrap(~ need, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("AI Chatbots Used for Election Information"),
       subtitle = "Among all respondents (actual- and hypothetical-arm pooled), by information type") +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p

bot_include_combined_wide_df <- bot_include_combined_df %>%
  pivot_wider(id_cols = c(item_tag, item), names_from = need, values_from = pct)
write.csv(bot_include_combined_wide_df, file.path(plots.dir, "bot_include_combined_wide.csv"), row.names = FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.3.3. Never-seekers only ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

bot_never_combined_df <- bind_rows(
  battery_props_by_party(prep_battery_long("reg_hyp_bot") %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "Registering and Voting", .before = 1),
  battery_props_by_party(prep_battery_long("run_hyp_bot") %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "How Elections Are Run", .before = 1),
  battery_props_by_party(prep_battery_long("won_hyp_bot") %>% mutate(is_selected = selected == "1"), "is_selected") %>%
    mutate(need = "Who Won an Election", .before = 1)
)

bot_never_means <- bot_never_combined_df %>% group_by(item) %>% summarise(m = mean(pct), .groups = "drop")
bot_never_combined_df <- bot_never_combined_df %>%
  mutate(item = factor(item, levels = order_bottom_anchors(bot_never_means$item, bot_never_means$m)),
         need = factor(need, levels = NEED_LEVELS))

p <- ggplot(bot_never_combined_df, aes(x = pct, y = item, fill = need)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.3, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_fill_manual(values = NEED_COLORS, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap30) +
  facet_wrap(~ need, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("AI Chatbots Used for Election Information"),
       subtitle = wrap_subtitle("Among respondents who never seek this information, by information need")) +
  theme(strip.text = element_text(face = "bold"))
p

bot_never_combined_wide_df <- bot_never_combined_df %>%
  pivot_wider(id_cols = c(item_tag, item), names_from = need, values_from = pct)
write.csv(bot_never_combined_wide_df, file.path(plots.dir, "bot_never_combined_wide.csv"), row.names = FALSE)




##### #
#### #
### ################################################################################################################################################# #
# Part G. Additional setup for AI / confidence / concern / noncitizen (ported from eis_2026_09_ai_confidence_concern_report.R) ------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

cum <- readRDS(file.path(out.dir, "eis_cumulative.rds")) %>% mutate(year = factor(year, levels = c(2024, 2026)))

# One weighted proportion for one already-filtered subset, grouped by an arbitrary factor column
# (e.g. pid3) - the by-party analogue of prop_one_group() above, reused rather than duplicated.
svy_prop_by <- function(data, indicator_col, by_col, weight_col = "weight") {
  data %>%
    filter(!is.na(.data[[indicator_col]]), !is.na(.data[[by_col]])) %>%
    group_by(.data[[by_col]]) %>%
    group_modify(~ prop_one_group(.x, indicator_col, weight_col)) %>%
    ungroup()
}

# The three `cum` (2024-vs-2026) estimators below are ported from eis_2026_09_ai_confidence_concern_report.R
# Part B.3, each reusing an estimator already defined above rather than re-deriving the same svydesign()/
# confint() logic a second time.
prop_by_year <- function(data, indicator_col, weight_col = "weight_common") {
  map_dfr(levels(data$year), function(yr) {
    prop_one_group(data %>% filter(year == yr, !is.na(.data[[indicator_col]])), indicator_col, weight_col) %>%
      mutate(year = yr, .before = 1)
  })
}

mean_by_year <- function(data, value_col, weight_col = "weight_common") {
  map_dfr(levels(data$year), function(yr) {
    sub <- data %>% filter(year == yr, !is.na(.data[[value_col]]))
    des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
    est <- svymean(reformulate(value_col), des)
    tibble(year = yr, pct = as.numeric(est), ci_low = confint(est)[1], ci_high = confint(est)[2], n = nrow(sub))
  })
}

factor_props_by_year <- function(data, factor_col, weight_col = "weight_common") {
  map_dfr(levels(data$year), function(yr) factor_props(data %>% filter(year == yr), factor_col, weight_col) %>% mutate(year = yr, .before = 1))
}

message("Part G: `cum` loaded (", nrow(cum), " respondent-years), trend/by-party estimators ready.")




##### #
#### #
### ################################################################################################################################################# #
# Part H. AI (BPC35-38) --------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

### ####################################################################################### ###
## H.1. Three single-item AI questions - full 2026 distribution ----
### ####################################################################################### ###

# Each is a Likert-style item - ordered by its own scale (own_scale), not sorted by size the way
# a battery's items would be, matching how eis_2026_09_ai_confidence_concern_report.R shows them.

ai_prevalence_df <- factor_props(data, "ai_prevalence") %>%
  mutate(category = factor(category, levels = levels(data$ai_prevalence)))

p <- ggplot(ai_prevalence_df, aes(x = pct, y = category)) +
  geom_col(fill = BAR_COLOR, width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap36) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("How Much Election Information Voters See Is AI-Generated"),
       subtitle = wrap_subtitle(paste0("n = ", format(ai_prevalence_df$n[1], big.mark = ",")))) +
  theme(panel.grid.major.y = element_blank())
p

#write.csv(ai_prevalence_df %>% select(category, pct, ci_low, ci_high, n), file.path(plots.dir, "ai_prevalence_wide.csv"), row.names = FALSE)

ai_tool_freq_df <- factor_props(data, "ai_tool_freq") %>%
  mutate(category = factor(category, levels = levels(data$ai_tool_freq)))

p <- ggplot(ai_tool_freq_df, aes(x = pct, y = category)) +
  geom_col(fill = BAR_COLOR, width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap40) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("How Often Voters Use AI Tools Such as Chatbots"),
       subtitle = wrap_subtitle(paste0("n = ", format(ai_tool_freq_df$n[1], big.mark = ",")))) +
  theme(panel.grid.major.y = element_blank())
p

#write.csv(ai_tool_freq_df %>% select(category, pct, ci_low, ci_high, n), file.path(plots.dir, "ai_tool_freq_wide.csv"), row.names = FALSE)

ai_detect_conf_df <- factor_props(data, "ai_detect_conf") %>%
  mutate(category = factor(category, levels = levels(data$ai_detect_conf)))

p <- ggplot(ai_detect_conf_df, aes(x = pct, y = category)) +
  geom_col(fill = BAR_COLOR, width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap36) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("Confidence Detecting AI-Generated Election Content"),
       subtitle = wrap_subtitle(paste0("n = ", format(ai_detect_conf_df$n[1], big.mark = ",")))) +
  theme(panel.grid.major.y = element_blank())
p

write.csv(ai_detect_conf_df %>% select(category, pct, ci_low, ci_high, n), file.path(plots.dir, "ai_detect_conf_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## H.2. The AI good/bad matrix (BPC38, 10 items), 2024 vs. 2026 ----
### ####################################################################################### ###

ai_labels <- c(
  ai_ok_voter_candidate_info   = "Voters Use AI Chatbots for Candidate/Issue Info",
  ai_ok_voter_how_to_vote      = "Voters Use AI Chatbots to Learn How to Cast a Ballot",
  ai_ok_voter_how_to_register  = "Voters Use AI Chatbots to Learn How to Register",
  ai_ok_voter_decide           = "Voters Use AI Chatbots to Decide Who to Vote For",
  ai_ok_voter_values_align     = "Voters Use AI Chatbots to Compare Candidates to Their Values",
  ai_ok_campaign_undisclosed   = "Campaigns Use Undisclosed AI-Generated Content",
  ai_ok_campaign_disclosed     = "Campaigns Use Disclosed AI-Generated Content",
  ai_ok_cand_photo_edit        = "Candidates Use AI to Edit Photos/Videos",
  ai_ok_cand_microtarget       = "Candidates Use AI to Microtarget Ads",
  ai_ok_cand_answer_questions  = "Candidates Use AI Chatbots to Answer Voter Questions")

# `decide` and `values_align` are a split-ballot pair - confirmed directly against the data every
# respondent answers at most one of the two (never both), so this is a deliberate survey-design
# choice, not item nonresponse. Nothing special is needed for the trend/matrix charts below (each
# item is estimated on its own non-missing base, same as any other item); it matters again in H.3.
cum <- cum %>% mutate(across(all_of(paste0(names(ai_labels), "_i")), ~ as.numeric(as.character(.x))))
for (col in paste0(names(ai_labels), "_i")) {
  cum[[paste0(col, "_cat3")]] <- factor(
    case_when(cum[[col]] <= 2 ~ "Bad", cum[[col]] == 3 ~ "Neither", cum[[col]] >= 4 ~ "Good", TRUE ~ NA_character_),
    levels = c("Bad", "Neither", "Good"))
}

ai_mean_trend_df <- map_dfr(paste0(names(ai_labels), "_i"), function(col) mean_by_year(cum, col) %>% mutate(item = ai_labels[[str_remove(col, "_i$")]], .before = 1))

ai_mean_order <- ai_mean_trend_df %>% select(item, year, pct) %>% pivot_wider(names_from = year, values_from = pct, names_prefix = "y") %>%
  mutate(chg = abs(y2026 - y2024)) %>% arrange(chg) %>% pull(item)
ai_mean_trend_df <- ai_mean_trend_df %>% mutate(item = factor(item, levels = ai_mean_order))

p <- ggplot(ai_mean_trend_df, aes(x = pct, y = item)) +
  geom_line(aes(group = item), color = "grey75", linewidth = 0.6) +
  geom_linerange(aes(xmin = ci_low, xmax = ci_high, color = year), linewidth = 2, alpha = 0.35) +
  geom_point(data = filter(ai_mean_trend_df, year == "2024"), aes(color = year), size = 4.2, shape = 1, stroke = 1.4) +
  geom_point(data = filter(ai_mean_trend_df, year == "2026"), aes(color = year), size = 3, shape = 16) +
  scale_color_manual(values = YEAR_COLORS, name = NULL) +
  scale_y_discrete(labels = wrap40) +
  labs(x = "Weighted Mean (1 = Bad, 5 = Good)", y = NULL,
       title = wrap_title("Attitudes Toward AI Use in Elections, 2024 vs. 2026"),
       subtitle = wrap_subtitle("Mean rating per item; higher means more comfortable with that AI use")) +
  theme(legend.position = "top")
p

# write.csv(ai_mean_trend_df %>% pivot_wider(id_cols = item, names_from = year, values_from = c(pct, ci_low, ci_high, n)),
#           file.path(plots.dir, "ai_mean_trend_wide.csv"), row.names = FALSE)

ai_cat3_trend_df <- map_dfr(paste0(names(ai_labels), "_i_cat3"), function(col)
  factor_props_by_year(cum, col) %>% mutate(item = ai_labels[[str_remove(col, "_i_cat3$")]], .before = 1))

p <- ggplot(ai_cat3_trend_df %>% mutate(category = factor(category, levels = c("Bad", "Neither", "Good"))), aes(x = pct, y = year, fill = category)) +
  geom_col(width = 0.65) +
  facet_wrap(~item, ncol = 1, strip.position = "top") +
  scale_fill_manual(values = AI_STATUS_COLORS, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .02))) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("Attitudes Toward AI Use in Elections, 2024 vs. 2026"),
       #subtitle = "Share rating each use bad / neither / good, by item"
       ) +
  theme(legend.position = "top", panel.grid = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold", size = 8))
p

# This chart is faceted by item (10 panels), so - same reasoning as the which_exclude/never split
# above - it becomes 10 CSVs, one per item, each a plain 2-row (2024/2026) stacked bar shape.
ai_cat3_trend_wide_df <- ai_cat3_trend_df %>% pivot_wider(id_cols = c(item, year), names_from = category, values_from = pct)
for (col in paste0(names(ai_labels), "_i_cat3")) {
  item_label <- ai_labels[[str_remove(col, "_i_cat3$")]]
  item_slug  <- str_remove(str_remove(col, "_i_cat3$"), "^ai_ok_")
  write.csv(ai_cat3_trend_wide_df %>% filter(item == item_label) %>% select(-item),
            file.path(plots.dir, paste0("ai_cat3_trend_", item_slug, "_wide.csv")), row.names = FALSE)
}


### ####################################################################################### ###
## H.3. The AI good/bad INDEX, and how it varies with age ----
### ####################################################################################### ###

# One respondent-level index: reverse each of the 10 items (6 - raw score, so higher = WORSE,
# matching Jack's direction - opposite of the raw "_i" columns, which run 1 = bad, 5 = good) and
# take the row mean across whichever of the 10 a respondent actually answered (na.rm = TRUE). This
# is deliberate given the decide/values_align split-ballot pair noted in H.2: na.rm = TRUE means
# each respondent's index reflects their own 8, 9, or 10 answered items rather than being penalized
# for a survey design choice outside their control. 2026 only (`data`), not the 2024-2026 `cum`.
ai_index_df <- data %>%
  mutate(across(all_of(paste0(names(ai_labels), "_i")), ~ 6 - as.numeric(as.character(.x)), .names = "{.col}_rev")) %>%
  mutate(ai_bad_index = rowMeans(across(all_of(paste0(names(ai_labels), "_i_rev"))), na.rm = TRUE)) %>%
  filter(!is.nan(ai_bad_index)) %>%
  select(resp_id, age_years, ai_bad_index)

# Unweighted scatter + loess smoother, per Jack's direction - every other estimate in this script
# is a design-weighted svymean()/svyciprop(), so this is a deliberate, noted exception: points are
# each respondent equally, and the smoother does not account for survey weights or the sample design.
p <- ggplot(ai_index_df, aes(x = age_years, y = ai_bad_index)) +
  geom_point(color = BAR_COLOR, alpha = 0.15, size = 1.5) +
  geom_smooth(method = "loess", color = RANK_COLOR, fill = RANK_COLOR, alpha = 0.2) +
  scale_x_continuous(breaks = seq(20, 90, by = 10)) +
  labs(x = "Age (years)", y = "AI good/bad index (higher = more negative toward AI)",
       title = wrap_title("How Attitudes Toward AI Use in Elections Vary by Age"),
       subtitle = wrap_subtitle(paste0("Unweighted; n = ", format(nrow(ai_index_df), big.mark = ","),
                                        ". Index = mean of the 10 reversed 1-5 items above."))) +
  theme(panel.grid.minor = element_blank())
p

write.csv(ai_index_df, file.path(plots.dir, "ai_index_wide.csv"), row.names = FALSE)




##### #
#### #
### ################################################################################################################################################# #
# Part I. Confidence (BPC40-43) ------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

### ####################################################################################### ###
## I.1. Top-2 trend, 2022 vs. 2024 vs. 2026 ----
### ####################################################################################### ###

# 2022 fielded the identical 4-item battery, prospectively, on the same 4-point + DK scale (see
# eis_2026_11_cumulative_2022_2023_design.md) - the only content anywhere in this script 2022
# actually has. Loaded as a separate `cum_conf` rather than widening the shared `cum` (used by
# every other chart in Parts H-K), for two reasons: `cum$year` was already coerced to NA for 2022
# rows back at Part G's `factor(year, levels = c(2024, 2026))`, so that coercion can't be undone on
# the same object; and every other chart in this script (AI, concern, noncitizen/USPS) genuinely
# has no 2022 data, so widening `cum` itself would risk silently adding an empty 2022 category to
# charts that have nothing to show for it.
cum_conf <- readRDS(file.path(out.dir, "eis_cumulative.rds")) %>%
  mutate(year = factor(year, levels = c(2022, 2024, 2026)))

conf_top2_labels <- c(conf_own_vote_top2 = "Your Own Vote", conf_local_votes_top2 = "Votes in Your County/City",
                       conf_state_votes_top2 = "Votes in Your State", conf_national_votes_top2 = "Votes Nationwide")
cum_conf <- cum_conf %>%
  mutate(
    conf_own_vote_top2       = as.numeric(as.integer(conf_own_vote) >= 3),
    conf_local_votes_top2    = as.numeric(as.integer(conf_local_votes) >= 3),
    conf_state_votes_top2    = as.numeric(as.integer(conf_state_votes) >= 3),
    conf_national_votes_top2 = as.numeric(as.integer(conf_national_votes) >= 3)
  )

# prop_by_year()'s default weight_col ("weight_common") now covers 2022 too -
# eis_2026_03_common_weights.R was extended to re-rake 2022 to the same common demographic target as
# 2024/2026, so this chart can use the same default every other trend chart in this script uses, on
# the same consistent weighting basis across all three years.
conf_trend_df <- map_dfr(names(conf_top2_labels), function(col)
  prop_by_year(cum_conf, col) %>% mutate(item = conf_top2_labels[[col]], .before = 1))

conf_trend_order <- conf_trend_df %>% select(item, year, pct) %>% pivot_wider(names_from = year, values_from = pct, names_prefix = "y") %>%
  mutate(chg = pmax(y2022, y2024, y2026, na.rm = TRUE) - pmin(y2022, y2024, y2026, na.rm = TRUE)) %>% arrange(chg) %>% pull(item)
conf_trend_df <- conf_trend_df %>% mutate(item = factor(item, levels = conf_trend_order))

# Previously: 3 separate geom_point() layers, each pre-filtered to one year, with shape/size set
# as fixed layer arguments rather than mapped - color was the only aesthetic ggplot had to build a
# legend key from, and each layer's shape was invisible to the other years' keys, which produced
# an unlabeled/mismatched entry. shape (and size, to keep the hollow-circle year visually as heavy
# as the filled ones) are now mapped to year too, same as color, so all three fold into one legend
# with the right glyph per key - and mapping them means every layer carries all 3 years at once
# instead of 3 redundant single-year layers.
# position_dodge() on the linerange/point layers (re-enabled below) keeps the 3 years from sitting
# on top of each other when their pct values land close together (e.g. "Votes in Your County/City")
# - it offsets each year to its own sub-row within the item's row, the same way it would space out
# separate fill/color groups on a discrete axis elsewhere in this script. The thin connecting line
# stays undodged, straight through the row's center, as a faint spine rather than trying to track
# 3 moving targets.
p <- ggplot(conf_trend_df, aes(x = pct, y = item, color = year, shape = year)) +
  geom_line(aes(group = item), color = "grey75", linewidth = 0.6) +
  geom_linerange(aes(xmin = ci_low, xmax = ci_high), position = position_dodge(width = 0.5), linewidth = 2, alpha = 0.35) +
  geom_point(aes(size = year), position = position_dodge(width = 0.5), stroke = 1.4) +
  scale_color_manual(values = YEAR_COLORS, name = NULL) +
  scale_shape_manual(values = c("2022" = 15, "2024" = 1, "2026" = 16), name = NULL) +
  scale_size_manual(values = c("2022" = 3.4, "2024" = 4.2, "2026" = 3), name = NULL, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.02, .08))) +
  labs(x = "Weighted % Confident", y = NULL,
       title = wrap_title("Confidence Votes Will Be Counted as Intended, 2022 vs. 2024 vs. 2026"),
       subtitle = wrap_subtitle("\"Confident\" combines somewhat + very confident")) +
  theme(legend.position = "top")
p

write.csv(conf_trend_df %>% pivot_wider(id_cols = item, names_from = year, values_from = c(pct, ci_low, ci_high, n)),
          file.path(plots.dir, "conf_trend_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## I.2. Top-2 by party ID - 2026 only ----
### ####################################################################################### ###

conf_labels <- c(conf_own_vote = "Your Own Vote", conf_local_votes = "Votes in Your County/City",
                  conf_state_votes = "Votes in Your State", conf_national_votes = "Votes Nationwide")
data <- data %>% mutate(across(all_of(names(conf_labels)), ~ as.numeric(as.integer(.x) >= 3), .names = "{.col}_top2"))

conf_by_party_df <- map_dfr(names(conf_labels), function(col) svy_prop_by(data, paste0(col, "_top2"), "pid3") %>% mutate(item = conf_labels[[col]], .before = 1))

conf_party_order <- conf_by_party_df %>% group_by(item) %>% summarise(m = mean(pct), .groups = "drop") %>% arrange(m) %>% pull(item)
conf_by_party_df <- conf_by_party_df %>% mutate(item = factor(item, levels = conf_party_order), pid3 = factor(pid3, levels = c("Dem", "Ind", "Rep")))

p <- ggplot(conf_by_party_df %>% filter(pid3 != "Ind"), 
            aes(x = pct, y = item, fill = pid3)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = PID_COLORS, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .06))) +
  labs(x = "Weighted % Confident", y = NULL,
       title = wrap_title("Confidence Votes Will Be Counted as Intended, by Party ID"),
       subtitle = "2026 only; \"confident\" combines somewhat + very confident") +
  theme(legend.position = "top")
p

write.csv(conf_by_party_df %>% pivot_wider(id_cols = item, names_from = pid3, values_from = pct), file.path(plots.dir, "conf_by_party_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## I.3. Top-2 by party ID, 2022 vs. 2024 vs. 2026 ----
### ####################################################################################### ###

# Crosses I.1's 3-year trend with I.2's party split: same cum_conf/_top2 columns as I.1, filtered
# to Dem/Rep (Ind dropped per Jack's direction) before calling prop_by_year() per party, then
# combined - the same filter-then-bind_rows pattern as the noncitizen/mode by-party charts above,
# just with 2 party subsets instead of 3. One facet per item; year on x so each party's 3 points
# (2022/2024/2026) trace as its own line.
conf_trend_party_df <- map_dfr(names(conf_top2_labels), function(col) {
  bind_rows(
    prop_by_year(cum_conf %>% filter(pid3 == "Dem"), col) %>% mutate(pid3 = "Dem", .before = 1),
    prop_by_year(cum_conf %>% filter(pid3 == "Rep"), col) %>% mutate(pid3 = "Rep", .before = 1)
  ) %>% mutate(item = conf_top2_labels[[col]], .before = 1)
})

conf_trend_party_df <- conf_trend_party_df %>%
  mutate(item = factor(item, levels = conf_trend_order),
         pid3 = factor(pid3, levels = c("Dem", "Rep")))

p <- ggplot(conf_trend_party_df, aes(x = year, y = pct, color = pid3, group = pid3)) +
  geom_line(linewidth = 0.8) +
  #geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, alpha = 0.5) +
  geom_point(size = 2.6) +
  facet_wrap(~ item) +
  scale_color_manual(values = PID_COLORS, name = NULL) +
  scale_y_continuous(limits = c(50, 100), 
                     expand = expansion(mult = c(0.04, .08))) +
  labs(x = NULL, y = "Weighted % Confident",
       title = "Democrats and Republicans Now Equally Confident that Votes will be Counted as Intended \nin Upcoming Elections",
       subtitle = "Showing percentage of those \"very confident\" and \"somewhat confident\"") +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank())
p

write.csv(conf_trend_party_df %>% pivot_wider(id_cols = c(item, year), names_from = pid3, values_from = pct),
          file.path(plots.dir, "conf_trend_by_party_wide.csv"), row.names = FALSE)


x <- conf_trend_party_df %>% 
  select(-c(ci_low, ci_high, n)) %>% 
  pivot_wider(names_from = pid3, 
              values_from = pct) %>% 
  mutate(diff = Dem-Rep)

##### #
#### #
### ################################################################################################################################################# #
# Part J. Concern (BPC44, 14 items) ----------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

concern_labels_all <- c(
  concern_misinfo             = "Inaccurate or Misleading Election Information",
  concern_ai_disinfo          = "AI Used to Spread Disinformation",
  concern_foreign             = "Foreign Interference",
  concern_ineligible_votes    = "Ineligible Votes Being Counted",
  concern_overturn            = "Attempts to Overturn a Fair Election's Results",
  concern_biased_count        = "Biased or Inaccurate Ballot Counting",
  concern_mail_ballots        = "Illegal or Improper Mail Ballot/Drop Box Use",
  concern_guns_intimidation   = "Guns, Violence, or Intimidation at Voting Locations",
  concern_post_violence       = "Violence or Unrest After Election Day",
  concern_polling_problems    = "Long Lines or Equipment Problems at Polls",
  concern_ice_deployment      = "ICE/Federal Law Enforcement in Your Community",
  concern_ballot_seizure      = "Federal/State Seizure of Ballots or Voting Machines",
  concern_eligible_blocked    = "Eligible Voters Blocked From Voting",
  concern_gerrymander         = "Unfair District Lines Distorting Outcomes")

# The 4 items new to 2026 - absent from eis_cumulative.rds and the 2024 field's own question
# codebook - have no possible trend comparison, so they appear only as 2026-only points below.
concern_new_2026   <- c("concern_ice_deployment", "concern_ballot_seizure", "concern_eligible_blocked", "concern_gerrymander")
concern_comparable <- setdiff(names(concern_labels_all), concern_new_2026)

data <- data %>% mutate(across(all_of(names(concern_labels_all)), ~ as.numeric(as.integer(.x) >= 3), .names = "{.col}_top2"))

### ####################################################################################### ###
## J.1. Top-2 trend for the 10 comparable items, plus the 4 new-2026 items as single points ----
### ####################################################################################### ###

cum <- cum %>% mutate(across(all_of(concern_comparable), ~ as.numeric(as.integer(.x) >= 3), .names = "{.col}_top2"))
concern_trend_df <- map_dfr(concern_comparable, function(col) prop_by_year(cum, paste0(col, "_top2")) %>% mutate(item = concern_labels_all[[col]], .before = 1))

# 2026-only, built on `data` (its own weight) rather than `cum` - there is no 2024 value to compare
# against. Same shape as concern_trend_df (item, year, pct, ci_low, ci_high, n), so it binds
# straight onto it; each of these 4 items then has exactly one point instead of two.
concern_new_df <- map_dfr(concern_new_2026, function(col)
  prop_one_group(data %>% filter(!is.na(.data[[paste0(col, "_top2")]])), paste0(col, "_top2"), "weight") %>%
    mutate(year = "2026", item = concern_labels_all[[col]], .before = 1))

concern_combined_df <- bind_rows(concern_trend_df, concern_new_df)

concern_order <- concern_combined_df %>% filter(year == "2026") %>% arrange(pct) %>% pull(item)
concern_combined_df <- concern_combined_df %>% mutate(item = factor(item, levels = concern_order))

p <- ggplot(concern_combined_df, aes(x = pct, y = item)) +
  geom_line(aes(group = item), color = "grey75", linewidth = 0.6) +
  #geom_linerange(aes(xmin = ci_low, xmax = ci_high, color = year), linewidth = 2, alpha = 0.35) +
  geom_point(data = filter(concern_combined_df, year == "2024"), aes(color = year), size = 4.2, shape = 1, stroke = 1.4) +
  geom_point(data = filter(concern_combined_df, year == "2026"), aes(color = year), size = 3, shape = 16) +
  scale_color_manual(values = YEAR_COLORS, name = NULL) +
  scale_y_discrete(labels = wrap40) +
  scale_x_continuous(breaks = seq(0, 100, 10),
                     limits = c(40, 80)) +
  labs(x = "Weighted % Concerned", y = NULL,
       title = wrap_title("Concern About Election Problems, 2024 vs. 2026"),
       subtitle = wrap_subtitle("\"Concerned\" combines somewhat + very concerned. 4 items new to 2026 shown as single 2026-only points.")) +
  theme(legend.position = "top")
p

write.csv(concern_combined_df %>% pivot_wider(id_cols = item, names_from = year, values_from = c(pct, ci_low, ci_high, n)),
          file.path(plots.dir, "concern_combined_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## J.2. By-party top-2 breakdown, all 14 items - 2026 only ----
### ####################################################################################### ###

concern_by_party_df <- map_dfr(names(concern_labels_all), function(col) svy_prop_by(data, paste0(col, "_top2"), "pid3") %>% mutate(item = concern_labels_all[[col]], .before = 1))

concern_party_order <- concern_by_party_df %>% group_by(item) %>% summarise(m = mean(pct), .groups = "drop") %>% arrange(m) %>% pull(item)
concern_by_party_df <- concern_by_party_df %>% 
  filter(pid3 != "Ind") %>% 
  mutate(item = factor(item, levels = concern_party_order), 
         pid3 = factor(pid3, levels = c("Dem", "Rep")))

# Wide, one row per item, just to place the gap label at the midpoint between Dem's and Rep's
# points - the long concern_by_party_df above still drives the line/point layers directly.
concern_party_diff_df <- concern_by_party_df %>%
  select(item, pid3, pct) %>%
  pivot_wider(names_from = pid3, values_from = pct) %>%
  mutate(mid_pct = (Dem + Rep) / 2, diff = abs(Dem - Rep))

p <- ggplot(concern_by_party_df, aes(y = item)) +
  geom_line(aes(x = pct, group = item), color = "grey75", linewidth = 1) +
  geom_point(aes(x = pct, color = pid3), size = 3.4) +
  geom_text(data = concern_party_diff_df, aes(x = mid_pct, label = round(diff)),
            vjust = -0.8, size = 3.1, color = "grey30") +
  scale_color_manual(values = PID_COLORS, name = NULL) +
  scale_x_continuous(limits = c(0, 100),
                     expand = expansion(mult = c(0.02, .08))) +
  scale_y_discrete(labels = wrap40) +
  labs(x = "Weighted % Concerned", y = NULL,
       title = "Concerns about the midterm election, by party ID",
       subtitle = "") +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank())
p

write.csv(concern_party_diff_df, file.path(plots.dir, "concern_by_party_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## J.3. Top-2, all 14 items - 2026 only ----
### ####################################################################################### ###

# J.1's year comparison dropped, leaving one bar per item - the same 2026 values J.1 already
# computed (concern_combined_df's year == "2026" rows cover all 14 items: the 10 comparable ones
# alongside their 2024 point, the 4 new-to-2026 ones on their own already), so no re-estimation is
# needed. concern_order is already 2026-pct-ascending, so the item factor carries over as-is.
concern_2026_df <- concern_combined_df %>% filter(year == "2026")

p <- ggplot(concern_2026_df, aes(x = pct, y = item)) +
  #geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  geom_point(color = BAR_COLOR, size = 3.2) +
  scale_x_continuous(limits = c(0, 100),
                     expand = expansion(mult = c(0.02, .08))) +
  scale_y_discrete(labels = wrap40) +
  labs(x = "Weighted % Concerned", y = NULL,
       title = wrap_title("Concern About Election Problems"),
       subtitle = "2026 only; all 14 items, including the 4 new to 2026")
p

write.csv(concern_2026_df %>% select(item, pct, ci_low, ci_high, n), file.path(plots.dir, "concern_2026_wide.csv"), row.names = FALSE)




##### #
#### #
### ################################################################################################################################################# #
# Part K. Noncitizen voting, access-vs-integrity, and USPS policy (BPC45-48) ----------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# Four 2026-only items, none with a 2024 counterpart. noncitizen_freq and usps_policy_support are
# ordinal, so each is ordered by its own scale rather than sorted by size; noncitizen_alters and
# access_vs_integrity are nominal with a real "don't know" catch-all, so each uses
# order_bottom_anchors() with its own exact anchor wording (this data's raw "Don't know" text,
# never passed through Part B's tools::toTitleCase() recoding, unlike sources.long's `display`).

### ####################################################################################### ###
## K.1. How often illegal noncitizen voting occurs ----
### ####################################################################################### ###

noncitizen_freq_df <- factor_props(data, "noncitizen_freq") %>%
  mutate(category = factor(category, levels = levels(data$noncitizen_freq)))

p <- ggplot(noncitizen_freq_df, aes(x = pct, y = category)) +
  geom_col(fill = BAR_COLOR, width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("How Often Illegal Noncitizen Voting Occurs"),
       subtitle = wrap_subtitle(paste0("n = ", format(noncitizen_freq_df$n[1], big.mark = ",")))) +
  theme(panel.grid.major.y = element_blank())
p

write.csv(noncitizen_freq_df %>% select(category, pct, ci_low, ci_high, n), file.path(plots.dir, "noncitizen_freq_wide.csv"), row.names = FALSE)

noncitizen_freq_party_df <- bind_rows(
  factor_props(data %>% filter(pid3 == "Dem"), "noncitizen_freq") %>% mutate(pid3 = "Dem", .before = 1),
  factor_props(data %>% filter(pid3 == "Ind"), "noncitizen_freq") %>% mutate(pid3 = "Ind", .before = 1),
  factor_props(data %>% filter(pid3 == "Rep"), "noncitizen_freq") %>% mutate(pid3 = "Rep", .before = 1)
) %>% mutate(category = factor(category, levels = levels(data$noncitizen_freq)), pid3 = factor(pid3, levels = c("Dem", "Ind", "Rep")))

p <- ggplot(noncitizen_freq_party_df, aes(x = pct, y = category, fill = pid3)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", position = position_dodge(width = 0.75),
                width = 0.2, color = "grey35", alpha = 0.6) +
  scale_fill_manual(values = PID_COLORS, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .06))) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("How Often Illegal Noncitizen Voting Occurs, by Party ID"),
       subtitle = "2026 only") +
  theme(legend.position = "top")
p

write.csv(noncitizen_freq_party_df %>% pivot_wider(id_cols = category, names_from = pid3, values_from = pct),
          file.path(plots.dir, "noncitizen_freq_party_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## K.2. Whether illegal noncitizen voting changes election outcomes ----
### ####################################################################################### ###

noncitizen_alters_df <- factor_props(data, "noncitizen_alters") %>%
  mutate(category = factor(category, levels = order_bottom_anchors(category, pct, anchors = "Don't know")))

p <- ggplot(noncitizen_alters_df, aes(x = pct, y = category)) +
  geom_col(fill = BAR_COLOR, width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("Illegal Noncitizen Voting Changes Election Outcomes"),
       subtitle = wrap_subtitle(paste0("n = ", format(noncitizen_alters_df$n[1], big.mark = ",")))) +
  theme(panel.grid.major.y = element_blank())
p

write.csv(noncitizen_alters_df %>% select(category, pct, ci_low, ci_high, n), file.path(plots.dir, "noncitizen_alters_wide.csv"), row.names = FALSE)

noncitizen_alters_party_order <- order_bottom_anchors(levels(data$noncitizen_alters), rep(0, length(levels(data$noncitizen_alters))), anchors = "Don't know")
noncitizen_alters_party_df <- bind_rows(
  factor_props(data %>% filter(pid3 == "Dem"), "noncitizen_alters") %>% mutate(pid3 = "Dem", .before = 1),
  factor_props(data %>% filter(pid3 == "Ind"), "noncitizen_alters") %>% mutate(pid3 = "Ind", .before = 1),
  factor_props(data %>% filter(pid3 == "Rep"), "noncitizen_alters") %>% mutate(pid3 = "Rep", .before = 1)
) %>% mutate(category = factor(category, levels = noncitizen_alters_party_order), pid3 = factor(pid3, levels = c("Dem", "Ind", "Rep")))

p <- ggplot(noncitizen_alters_party_df, aes(x = pct, y = category, fill = pid3)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", position = position_dodge(width = 0.75),
                width = 0.2, color = "grey35", alpha = 0.6) +
  scale_fill_manual(values = PID_COLORS, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .06))) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("Illegal Noncitizen Voting Changes Election Outcomes, by Party ID"),
       subtitle = "2026 only") +
  theme(legend.position = "top")
p

write.csv(noncitizen_alters_party_df %>% pivot_wider(id_cols = category, names_from = pid3, values_from = pct),
          file.path(plots.dir, "noncitizen_alters_party_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## K.3. Priority: easier for eligible voters, or harder for ineligible voters ----
### ####################################################################################### ###

access_vs_integrity_df <- factor_props(data, "access_vs_integrity") %>%
  mutate(category = factor(category, levels = order_bottom_anchors(category, pct, anchors = "Don't know / No opinion")))

p <- ggplot(access_vs_integrity_df, aes(x = pct, y = category)) +
  geom_col(fill = BAR_COLOR, width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = wrap40) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("Higher Voting-Law Priority: Access or Integrity"),
       subtitle = wrap_subtitle(paste0("n = ", format(access_vs_integrity_df$n[1], big.mark = ",")))) +
  theme(panel.grid.major.y = element_blank())
p

write.csv(access_vs_integrity_df %>% select(category, pct, ci_low, ci_high, n), file.path(plots.dir, "access_vs_integrity_wide.csv"), row.names = FALSE)


### ####################################################################################### ###
## K.4. Support for the Postal Service's August mail-ballot policy change, collapsed to 3 categories ----
### ####################################################################################### ###

usps_collapse_key <- c("Strongly oppose" = "Oppose", "Somewhat oppose" = "Oppose",
                        "Neither support nor oppose" = "Neither Support nor Oppose",
                        "Somewhat support" = "Support", "Strongly support" = "Support")
data <- data %>% mutate(usps_policy_support_3 = factor(usps_collapse_key[as.character(usps_policy_support)],
                                                        levels = c("Oppose", "Neither Support nor Oppose", "Support")))

usps_policy_support_df <- factor_props(data, "usps_policy_support_3") %>%
  mutate(category = factor(category, levels = c("Oppose", "Neither Support nor Oppose", "Support")))

p <- ggplot(usps_policy_support_df, aes(x = pct, y = category)) +
  geom_col(fill = BAR_COLOR, width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  labs(x = "Weighted %", y = NULL,
       title = wrap_title("Support for the Postal Service's August Mail-Ballot Policy Change"),
       subtitle = wrap_subtitle(paste0("Collapsed from the original 5-point scale; n = ", format(usps_policy_support_df$n[1], big.mark = ",")))) +
  theme(panel.grid.major.y = element_blank())
p

write.csv(usps_policy_support_df %>% select(category, pct, ci_low, ci_high, n), file.path(plots.dir, "usps_policy_support_wide.csv"), row.names = FALSE)




##### #
#### #
### ################################################################################################################################################# #
# Part L. Miscellaneous calculations ------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

### ####################################################################################### ###
## L.1. 2nd- and 3rd-choice sources, among those who ranked one source first ----
### ####################################################################################### ###

# No custom functions here on purpose (unlike everywhere else in this script) - every subsetting
# step for every need x first-choice combination is written out on its own, so each one can be
# read/checked in place rather than traced into a function defined elsewhere. "Everyone included"
# (act+hyp battery values, both filtered to in_base rows) for each need's main sources battery.
# For each of 3 needs x 2 first-choice items: filter to that need's long rows, pull the resp_ids
# who ranked the first-choice item #1, then re-filter that same long table down to just those
# resp_ids' rank-2 (then rank-3) rows and hand-build the weighted % - the same svydesign()/
# svymean()/confint() calls factor_props() uses elsewhere, just written out instead of called.
# n shrinks on its own between the 2nd- and 3rd-choice tables (not everyone who gave a #1 choice
# gave a #2 or #3), and n_first (the #1 group's own size) is carried along for reference.

# --- Registering and Voting ---------------------------------------------------------------------

reg_long <- sources.long %>% filter(battery %in% c("reg_act", "reg_hyp"), in_base)

# Online Search Engine first
reg_search_first_ids <- reg_long %>% filter(rank == 1, item_tag == "search") %>% pull(resp_id)

reg_search_2nd    <- reg_long %>% filter(resp_id %in% reg_search_first_ids, rank == 2) %>% mutate(display = as.factor(display))
reg_search_2nd_des <- svydesign(ids = ~1, weights = ~weight, data = reg_search_2nd)
reg_search_2nd_est <- svymean(~display, reg_search_2nd_des)
reg_search_2nd_df <- tibble(
  need = "Registering and Voting", first_choice = "Online Search Engine", followup = "2nd choice",
  category = sub("^display", "", names(reg_search_2nd_est)),
  pct      = as.numeric(reg_search_2nd_est) * 100,
  ci_low   = confint(reg_search_2nd_est)[, 1] * 100,
  ci_high  = confint(reg_search_2nd_est)[, 2] * 100,
  n        = nrow(reg_search_2nd),
  n_first  = length(reg_search_first_ids)
)

reg_search_3rd    <- reg_long %>% filter(resp_id %in% reg_search_first_ids, rank == 3) %>% mutate(display = as.factor(display))
reg_search_3rd_des <- svydesign(ids = ~1, weights = ~weight, data = reg_search_3rd)
reg_search_3rd_est <- svymean(~display, reg_search_3rd_des)
reg_search_3rd_df <- tibble(
  need = "Registering and Voting", first_choice = "Online Search Engine", followup = "3rd choice",
  category = sub("^display", "", names(reg_search_3rd_est)),
  pct      = as.numeric(reg_search_3rd_est) * 100,
  ci_low   = confint(reg_search_3rd_est)[, 1] * 100,
  ci_high  = confint(reg_search_3rd_est)[, 2] * 100,
  n        = nrow(reg_search_3rd),
  n_first  = length(reg_search_first_ids)
)

# Local Election Officials first
reg_local_first_ids <- reg_long %>% filter(rank == 1, item_tag == "local_officials") %>% pull(resp_id)

reg_local_2nd    <- reg_long %>% filter(resp_id %in% reg_local_first_ids, rank == 2) %>% mutate(display = as.factor(display))
reg_local_2nd_des <- svydesign(ids = ~1, weights = ~weight, data = reg_local_2nd)
reg_local_2nd_est <- svymean(~display, reg_local_2nd_des)
reg_local_2nd_df <- tibble(
  need = "Registering and Voting", first_choice = "Local Election Officials", followup = "2nd choice",
  category = sub("^display", "", names(reg_local_2nd_est)),
  pct      = as.numeric(reg_local_2nd_est) * 100,
  ci_low   = confint(reg_local_2nd_est)[, 1] * 100,
  ci_high  = confint(reg_local_2nd_est)[, 2] * 100,
  n        = nrow(reg_local_2nd),
  n_first  = length(reg_local_first_ids)
)

reg_local_3rd    <- reg_long %>% filter(resp_id %in% reg_local_first_ids, rank == 3) %>% mutate(display = as.factor(display))
reg_local_3rd_des <- svydesign(ids = ~1, weights = ~weight, data = reg_local_3rd)
reg_local_3rd_est <- svymean(~display, reg_local_3rd_des)
reg_local_3rd_df <- tibble(
  need = "Registering and Voting", first_choice = "Local Election Officials", followup = "3rd choice",
  category = sub("^display", "", names(reg_local_3rd_est)),
  pct      = as.numeric(reg_local_3rd_est) * 100,
  ci_low   = confint(reg_local_3rd_est)[, 1] * 100,
  ci_high  = confint(reg_local_3rd_est)[, 2] * 100,
  n        = nrow(reg_local_3rd),
  n_first  = length(reg_local_first_ids)
)

# --- How Elections Are Run ----------------------------------------------------------------------

run_long <- sources.long %>% filter(battery %in% c("run_act", "run_hyp"), in_base)

# Online Search Engine first
run_search_first_ids <- run_long %>% filter(rank == 1, item_tag == "search") %>% pull(resp_id)

run_search_2nd    <- run_long %>% filter(resp_id %in% run_search_first_ids, rank == 2) %>% mutate(display = as.factor(display))
run_search_2nd_des <- svydesign(ids = ~1, weights = ~weight, data = run_search_2nd)
run_search_2nd_est <- svymean(~display, run_search_2nd_des)
run_search_2nd_df <- tibble(
  need = "How Elections Are Run", first_choice = "Online Search Engine", followup = "2nd choice",
  category = sub("^display", "", names(run_search_2nd_est)),
  pct      = as.numeric(run_search_2nd_est) * 100,
  ci_low   = confint(run_search_2nd_est)[, 1] * 100,
  ci_high  = confint(run_search_2nd_est)[, 2] * 100,
  n        = nrow(run_search_2nd),
  n_first  = length(run_search_first_ids)
)

run_search_3rd    <- run_long %>% filter(resp_id %in% run_search_first_ids, rank == 3) %>% mutate(display = as.factor(display))
run_search_3rd_des <- svydesign(ids = ~1, weights = ~weight, data = run_search_3rd)
run_search_3rd_est <- svymean(~display, run_search_3rd_des)
run_search_3rd_df <- tibble(
  need = "How Elections Are Run", first_choice = "Online Search Engine", followup = "3rd choice",
  category = sub("^display", "", names(run_search_3rd_est)),
  pct      = as.numeric(run_search_3rd_est) * 100,
  ci_low   = confint(run_search_3rd_est)[, 1] * 100,
  ci_high  = confint(run_search_3rd_est)[, 2] * 100,
  n        = nrow(run_search_3rd),
  n_first  = length(run_search_first_ids)
)

# Local Election Officials first
run_local_first_ids <- run_long %>% filter(rank == 1, item_tag == "local_officials") %>% pull(resp_id)

run_local_2nd    <- run_long %>% filter(resp_id %in% run_local_first_ids, rank == 2) %>% mutate(display = as.factor(display))
run_local_2nd_des <- svydesign(ids = ~1, weights = ~weight, data = run_local_2nd)
run_local_2nd_est <- svymean(~display, run_local_2nd_des)
run_local_2nd_df <- tibble(
  need = "How Elections Are Run", first_choice = "Local Election Officials", followup = "2nd choice",
  category = sub("^display", "", names(run_local_2nd_est)),
  pct      = as.numeric(run_local_2nd_est) * 100,
  ci_low   = confint(run_local_2nd_est)[, 1] * 100,
  ci_high  = confint(run_local_2nd_est)[, 2] * 100,
  n        = nrow(run_local_2nd),
  n_first  = length(run_local_first_ids)
)

run_local_3rd    <- run_long %>% filter(resp_id %in% run_local_first_ids, rank == 3) %>% mutate(display = as.factor(display))
run_local_3rd_des <- svydesign(ids = ~1, weights = ~weight, data = run_local_3rd)
run_local_3rd_est <- svymean(~display, run_local_3rd_des)
run_local_3rd_df <- tibble(
  need = "How Elections Are Run", first_choice = "Local Election Officials", followup = "3rd choice",
  category = sub("^display", "", names(run_local_3rd_est)),
  pct      = as.numeric(run_local_3rd_est) * 100,
  ci_low   = confint(run_local_3rd_est)[, 1] * 100,
  ci_high  = confint(run_local_3rd_est)[, 2] * 100,
  n        = nrow(run_local_3rd),
  n_first  = length(run_local_first_ids)
)

# --- Who Won an Election -------------------------------------------------------------------------

won_long <- sources.long %>% filter(battery %in% c("won_act", "won_hyp"), in_base)

# Online Search Engine first
won_search_first_ids <- won_long %>% filter(rank == 1, item_tag == "search") %>% pull(resp_id)

won_search_2nd    <- won_long %>% filter(resp_id %in% won_search_first_ids, rank == 2) %>% mutate(display = as.factor(display))
won_search_2nd_des <- svydesign(ids = ~1, weights = ~weight, data = won_search_2nd)
won_search_2nd_est <- svymean(~display, won_search_2nd_des)
won_search_2nd_df <- tibble(
  need = "Who Won an Election", first_choice = "Online Search Engine", followup = "2nd choice",
  category = sub("^display", "", names(won_search_2nd_est)),
  pct      = as.numeric(won_search_2nd_est) * 100,
  ci_low   = confint(won_search_2nd_est)[, 1] * 100,
  ci_high  = confint(won_search_2nd_est)[, 2] * 100,
  n        = nrow(won_search_2nd),
  n_first  = length(won_search_first_ids)
)

won_search_3rd    <- won_long %>% filter(resp_id %in% won_search_first_ids, rank == 3) %>% mutate(display = as.factor(display))
won_search_3rd_des <- svydesign(ids = ~1, weights = ~weight, data = won_search_3rd)
won_search_3rd_est <- svymean(~display, won_search_3rd_des)
won_search_3rd_df <- tibble(
  need = "Who Won an Election", first_choice = "Online Search Engine", followup = "3rd choice",
  category = sub("^display", "", names(won_search_3rd_est)),
  pct      = as.numeric(won_search_3rd_est) * 100,
  ci_low   = confint(won_search_3rd_est)[, 1] * 100,
  ci_high  = confint(won_search_3rd_est)[, 2] * 100,
  n        = nrow(won_search_3rd),
  n_first  = length(won_search_first_ids)
)

# Local Election Officials first
won_local_first_ids <- won_long %>% filter(rank == 1, item_tag == "local_officials") %>% pull(resp_id)

won_local_2nd    <- won_long %>% filter(resp_id %in% won_local_first_ids, rank == 2) %>% mutate(display = as.factor(display))
won_local_2nd_des <- svydesign(ids = ~1, weights = ~weight, data = won_local_2nd)
won_local_2nd_est <- svymean(~display, won_local_2nd_des)
won_local_2nd_df <- tibble(
  need = "Who Won an Election", first_choice = "Local Election Officials", followup = "2nd choice",
  category = sub("^display", "", names(won_local_2nd_est)),
  pct      = as.numeric(won_local_2nd_est) * 100,
  ci_low   = confint(won_local_2nd_est)[, 1] * 100,
  ci_high  = confint(won_local_2nd_est)[, 2] * 100,
  n        = nrow(won_local_2nd),
  n_first  = length(won_local_first_ids)
)

won_local_3rd    <- won_long %>% filter(resp_id %in% won_local_first_ids, rank == 3) %>% mutate(display = as.factor(display))
won_local_3rd_des <- svydesign(ids = ~1, weights = ~weight, data = won_local_3rd)
won_local_3rd_est <- svymean(~display, won_local_3rd_des)
won_local_3rd_df <- tibble(
  need = "Who Won an Election", first_choice = "Local Election Officials", followup = "3rd choice",
  category = sub("^display", "", names(won_local_3rd_est)),
  pct      = as.numeric(won_local_3rd_est) * 100,
  ci_low   = confint(won_local_3rd_est)[, 1] * 100,
  ci_high  = confint(won_local_3rd_est)[, 2] * 100,
  n        = nrow(won_local_3rd),
  n_first  = length(won_local_first_ids)
)

# --- Combine the 12 small tables just for viewing/export -----------------------------------------

secondthird_df <- bind_rows(
  reg_search_2nd_df, reg_search_3rd_df, reg_local_2nd_df, reg_local_3rd_df,
  run_search_2nd_df, run_search_3rd_df, run_local_2nd_df, run_local_3rd_df,
  won_search_2nd_df, won_search_3rd_df, won_local_2nd_df, won_local_3rd_df
) %>%
  mutate(need     = factor(need, levels = c("Registering and Voting", "How Elections Are Run", "Who Won an Election")),
         followup = factor(followup, levels = c("2nd choice", "3rd choice"))) %>%
  arrange(need, first_choice, followup, desc(pct))

print(secondthird_df %>% mutate(pct = round(pct, 1)) %>% select(need, first_choice, followup, category, pct, n, n_first),
      n = 300, width = 200)

write.csv(secondthird_df %>% select(need, first_choice, followup, category, pct, ci_low, ci_high, n, n_first),
          file.path(plots.dir, "secondthird_choices_wide.csv"), row.names = FALSE)




# The end.
