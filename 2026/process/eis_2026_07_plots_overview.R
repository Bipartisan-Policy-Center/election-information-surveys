


# EIS_2026_plots_overview



########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####
####  Title: A weighted overview of every substantive variable in the BPC Election Information Survey, 2026 field
####
####  Author: Jack Friedman
####
####  Overview: The topical scripts (03-04) each look at a handful of hand-picked items. This file instead sweeps
####            script 02's ENTIRE cleaned output — all 609 columns — and produces one weighted chart or table per
####            substantive question, at scale, using the same estimation pattern worked out by hand in
####            'eis_2026_06_design_data.R' (compare Part C there to Part C here: same functions, same math). Run it
####            after script 02; nothing here depends on scripts 03 or 04.
####
####  File Description: Part B is the part worth reading first. `var_type` in the variable index is only populated
####            for the 409 DELIVERED columns — every derived column (the 166 built in script 02 Part J, plus the 36
####            collapsed-rank slot columns) has it blank, so a chart plan cannot be built from var_type alone. Part B
####            classifies every column into exactly one of five chart kinds — ordinal, nominal, battery (several 0/1
####            columns shown together), numeric, or table (a nominal item with more categories than a bar chart can
####            hold) — using the variable index where it has an answer and the column's own live class/cardinality
####            where it does not. Parts C-D estimate and draw each one; Part E assembles a single, self-contained
####            HTML page (every image inlined as a base64 data URI via knitr::image_uri(), the same mechanism
####            'eis_2026_05_trend_comparison_report.Rmd' already uses to stay pandoc-free) so the whole overview is
####            one file to open, not a folder of PNGs plus an index.
####
####            WHICH WEIGHT. `weight`, the delivered Morning Consult weight — see the note in
####            'eis_2026_06_design_data.R' for why this, and not the raked common weight from script 03, is right
####            for describing the 2026 field on its own.
####
####  Output: output/eis_2026_variable_atlas.html
####
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# Loading packages
library(tidyverse)
library(survey)   # for svydesign(), svyciprop(), and svymean()
library(knitr)    # for image_uri(), which inlines each chart as a base64 data URI


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
# script itself is saved in (this file lives in 2026/process/, but that is irrelevant to where relative paths resolve).
out.dir <- "output"

path.clean <- file.path(out.dir, "eis_2026_clean.rds")
path.index <- file.path(out.dir, "eis_2026_variable_index.csv")

if (!file.exists(path.clean) || !file.exists(path.index)) {
  stop("Expected script 02's output in ", out.dir, ". Missing:\n",
       paste0("  ", c(path.clean, path.index)[!file.exists(c(path.clean, path.index))], collapse = "\n"))
}

data <- readRDS(path.clean)
var.index <- read.csv(path.index, stringsAsFactors = FALSE) %>% as_tibble()

message("Loaded ", nrow(data), " respondents x ", ncol(data), " columns from eis_2026_clean.rds.")




##### #
#### #
### ################################################################################################################################################# #
# Part B. Building the chart plan ------------------------------------------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# One row per chart or table this script will produce: `kind` (ordinal / nominal / rank / battery / numeric / table),
# `key` (the var_name, or the shared label stem for a battery), `cols` (a list-column of the one or more var_names
# behind it), `title`, and `base_condition` (the questionnaire's own skip logic, for the caption). Every rule below is
# driven by the variable index or the data's own live class — nothing is a hand-typed list of column names, so the
# plan does not go stale when script 02's specification changes.

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.1. Constants ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# A nominal item with more categories than this becomes a table instead of a bar chart. 15 sits cleanly between the
# largest ordinary nominal item (religion, 12 categories) and the one genuine outlier (state, 51).
TABLE_THRESHOLD <- 15

# Internal geocoding QA, not a survey question — the respondent never saw a "town of residence" item. Excluded by
# name because nothing about their var_type or origin distinguishes them from a real derived variable.
GEO_EXCLUDE <- c("state_zip", "state_zip_name", "county", "town", "zip_match", "flag_state_zip_mismatch")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.2. Ordinal-like columns: declared, or a derived ordered factor with its own _i companion ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# `var_type == "ordinal"` catches the 51 DELIVERED ordinal items. It misses pid5 and pid7 — script 02 builds these as
# ordered factors with their own _i integer companions, exactly like a delivered ordinal item, but as a derived
# column its var_type is blank. Finding them by their own shape (an ordered factor with a matching _i column)
# rather than hand-listing "pid5, pid7" is what keeps this rule from going stale if a third one is added later.
ordinal.declared <- var.index %>% filter(var_type == "ordinal") %>% pull(var_name)

ordinal.derived <- var.index %>%
  filter(origin == "derived", !var_name %in% GEO_EXCLUDE) %>%
  pull(var_name) %>%
  keep(~ is.ordered(data[[.x]]) && paste0(.x, "_i") %in% names(data))

ordinal.stems <- c(ordinal.declared, ordinal.derived)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.3. Columns excluded outright ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# n_unique <= 1: a constant (e.g. sample_rv, always "1" by construction) has no distribution to show.
# id / text / weight: resp_id and zip are administrative; weight gets its own bonus histogram in Part F, not the
# per-variable loop. GEO_EXCLUDE: see B.1.
skip.vars <- var.index %>%
  filter(n_unique <= 1 | var_type %in% c("id", "text", "weight") | var_name %in% GEO_EXCLUDE) %>%
  pull(var_name)

# Every ordinal stem's _i (integer) and _f (Don't-know-included) companion is folded into that item's OWN chart
# (the _i mean annotation) rather than charted on its own.
companion.vars <- c(paste0(ordinal.stems, "_i"), paste0(ordinal.stems, "_f")) %>% keep(~ .x %in% names(data))

# Every rank battery's four slot columns (_1st/_2nd/_3rd/_n) — only _1st is charted (B.5); the rest are claimed here
# so they do not fall through to individual classification in B.8.
rank.slot.vars <- var.index %>% filter(origin == "collapsed rank") %>% pull(var_name)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.4. Multiselect batteries: grouped by the codebook's own `base` ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

plan.multiselect <- var.index %>%
  filter(var_type == "multiselect") %>%
  group_by(base) %>%
  summarise(cols  = list(var_name),
            title = str_trim(str_extract(first(var_label), "^[^:]+(?=:)")),
            .groups = "drop") %>%
  transmute(kind = "battery", key = base, cols, title)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.5. Rank batteries: the _1st slot column, read as a nominal item ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# "Who did each respondent rank first?" is exactly a nominal question — one factor, one level per rankable item —
# so it needs no estimator of its own; Part C's ordinary single-factor estimator applies unchanged. _2nd/_3rd/_n
# exist in the data (claimed in B.3) but are not charted here, to keep one battery to one chart.
plan.rank <- var.index %>%
  filter(origin == "collapsed rank", str_detect(var_name, "_1st$")) %>%
  transmute(kind = "rank", key = var_name, cols = as.list(var_name),
            title = str_trim(str_remove(var_label, ":\\s*item ranked 1st$")))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.6. Ordinal items ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

plan.ordinal <- var.index %>%
  filter(var_name %in% ordinal.stems) %>%
  transmute(kind = "ordinal", key = var_name, cols = as.list(var_name), title = var_label)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.7. Declared nominal / binary / flag items, split from tables by cardinality ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

plan.declared.nom <- var.index %>%
  filter(var_type %in% c("nominal", "binary", "flag")) %>%
  transmute(kind  = if_else(n_unique > TABLE_THRESHOLD, "table", "nominal"),
            key   = var_name, cols = as.list(var_name), title = var_label)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.8. Declared numeric (age_years) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

plan.declared.numeric <- var.index %>%
  filter(var_type == "numeric") %>%
  transmute(kind = "numeric", key = var_name, cols = as.list(var_name), title = var_label)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.9. Everything left over: classified from the live data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# What remains after B.2-B.8 is entirely "derived"-origin columns the variable index does not type at all: the
# straightlining/contradiction/routing flags, the pid3_lean and race4 recodes, the n_*_selected counts, and the
# "both arms pooled" reg_src_*/run_src_*/won_src_* batteries. None of it is hand-listed below — every rule reads the
# column's own class or its label text.
claimed <- c(unlist(plan.ordinal$cols), paste0(unlist(plan.ordinal$cols), "_i"), paste0(unlist(plan.ordinal$cols), "_f"),
             unlist(plan.multiselect$cols), rank.slot.vars, plan.declared.nom$key, plan.declared.numeric$key, skip.vars)

leftover <- var.index %>% filter(!var_name %in% claimed)

# Leftover 0/1 factors that share an identical var_label stem (2+ of them) are a battery the codebook does not mark
# as one — e.g. reg_src_local_officials / reg_src_state_officials / ... , the pooled versions of the
# reg_act_src/reg_hyp_src split, which are derived columns with no `base` of their own. Grouping by label stem
# rather than a hand-maintained name-prefix list, because the label is already guaranteed identical across an item
# family (script 02 Part C.3 builds it that way) and a prefix list would go stale the moment a new pooled battery is
# added.
leftover.binary <- leftover %>% filter(map_lgl(var_name, ~ n_distinct(data[[.x]], na.rm = TRUE) == 2))
leftover.stems  <- leftover.binary %>%
  mutate(stem = str_trim(str_extract(var_label, "^[^:]+(?=:)"))) %>%
  filter(!is.na(stem))
stems.repeated  <- leftover.stems %>% count(stem) %>% filter(n >= 2) %>% pull(stem)

plan.leftover.battery <- leftover.stems %>%
  filter(stem %in% stems.repeated) %>%
  group_by(stem) %>%
  summarise(cols = list(var_name), .groups = "drop") %>%
  transmute(kind = "battery", key = stem, cols, title = stem)

# Whatever a 0/1 column did NOT get folded into a battery above, and everything that was never 0/1 to begin with,
# is a genuine one-off — classified individually from its own class and cardinality.
leftover.remaining <- leftover %>% filter(!var_name %in% unlist(plan.leftover.battery$cols))

classify_one <- function(vn) {
  x <- data[[vn]]
  if (is.numeric(x))                                            return("numeric")
  if (is.factor(x) && n_distinct(x, na.rm = TRUE) > TABLE_THRESHOLD) return("table")
  if (is.factor(x))                                              return("nominal")
  "unclassified"   # a safety net — nothing in this data should ever hit this branch
}

leftover.remaining <- leftover.remaining %>% mutate(resolved_kind = map_chr(var_name, classify_one))

if (any(leftover.remaining$resolved_kind == "unclassified")) {
  stop("Part B could not classify: ",
       paste(leftover.remaining$var_name[leftover.remaining$resolved_kind == "unclassified"], collapse = ", "))
}

plan.leftover.single <- leftover.remaining %>%
  transmute(kind = resolved_kind, key = var_name, cols = as.list(var_name), title = var_label)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.10. Stacking every group into one chart plan ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

chart.plan <- bind_rows(plan.ordinal, plan.multiselect, plan.rank, plan.declared.nom, plan.declared.numeric,
                        plan.leftover.battery, plan.leftover.single) %>%
  mutate(base_condition = map_chr(cols, ~ {
           bc <- var.index$base_condition[match(.x[[1]], var.index$var_name)]
           if (length(bc) == 0 || is.na(bc) || bc == "") "All respondents" else bc
         }),
         mean_col = if_else(kind == "ordinal" & paste0(key, "_i") %in% names(data), paste0(key, "_i"), NA_character_)) %>%
  arrange(kind, key)

message("\nPart B: chart plan built.")
print(count(chart.plan, kind, name = "n_outputs"))

# Every column in `data` is accounted for exactly once: skipped, a companion, a rank slot, or claimed by one row of
# the plan. If this fails, some column fell through every rule in Part B silently instead of being classified.
accounted.for <- c(skip.vars, companion.vars, rank.slot.vars, unlist(chart.plan$cols))
missed <- setdiff(names(data), accounted.for)
stopifnot("every column in `data` is accounted for by Part B" = length(missed) == 0)




##### #
#### #
### ################################################################################################################################################# #
# Part C. Estimators --------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# The same three estimators worked out by hand in 'eis_2026_06_design_data.R' Part C, reproduced here rather than
# sourced from that file — that file is a scratch companion for interactive use, not a dependency, the same way
# script 02 stands alone rather than sourcing script 01.

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

svy_mean <- function(data, value_col, weight_col = "weight") {
  sub <- data %>% filter(!is.na(.data[[value_col]]))
  des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
  est <- svymean(reformulate(value_col), des)
  tibble(estimate = as.numeric(est), ci_low = confint(est)[1], ci_high = confint(est)[2], n = nrow(sub))
}

svy_prop <- function(data, indicator_col, weight_col = "weight") {
  sub <- data %>% filter(!is.na(.data[[indicator_col]]))
  des <- svydesign(ids = ~1, weights = reformulate(weight_col), data = sub)
  est <- svyciprop(reformulate(indicator_col), des, method = "logit")
  tibble(pct = as.numeric(est) * 100, ci_low = confint(est)[1] * 100, ci_high = confint(est)[2] * 100, n = nrow(sub))
}

# `cols` is a vector of 0/1 factor column names sharing a base condition. Converts each to numeric first (the
# multiselect columns are factors with levels "0"/"1", not numeric), then svy_prop() mapped over them, stacked long.
battery_props <- function(data, cols, weight_col = "weight") {
  data.bin <- data %>% mutate(across(all_of(cols), ~ as.numeric(as.character(.x))))
  map_dfr(cols, function(col) svy_prop(data.bin, col, weight_col) %>% mutate(item = col, .before = 1))
}




##### #
#### #
### ################################################################################################################################################# #
# Part D. Chart and table builders -------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

theme_set(theme_minimal(base_size = 12))
BAR_COLOR <- "#2a78d6"

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.1. One factor's weighted distribution: ordinal, nominal, and rank kinds ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# `order`: NULL sorts by size (nominal, rank); a vector of levels low-to-high keeps an ordinal item's scale order.
# No title/subtitle baked into the plot — those are set as real (ctrl-F-searchable) HTML text in Part E instead, so
# the image itself is nothing but the data.
plot_categorical <- function(props, x_label = "Weighted %", order = NULL) {
  props <- if (is.null(order)) props %>% mutate(category = fct_reorder(category, pct))
           else                props %>% mutate(category = factor(category, levels = order))
  ggplot(props, aes(x = pct, y = category)) +
    geom_col(fill = BAR_COLOR, width = 0.65) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.9) +
    scale_x_continuous(expand = expansion(mult = c(0, .06))) +
    labs(x = x_label, y = NULL)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.2. Several 0/1 columns shown together: the battery kind ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

plot_battery <- function(props, x_label = "Weighted %") {
  props <- props %>% mutate(item = fct_reorder(item, pct))
  ggplot(props, aes(x = pct, y = item)) +
    geom_col(fill = BAR_COLOR, width = 0.65) +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.9) +
    scale_x_continuous(expand = expansion(mult = c(0, .06))) +
    labs(x = x_label, y = NULL)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.3. A weighted histogram: the numeric kind ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

plot_numeric <- function(data, value_col, x_label, weight_col = "weight") {
  sub <- data %>% filter(!is.na(.data[[value_col]]))
  ggplot(sub, aes(x = .data[[value_col]], weight = .data[[weight_col]])) +
    geom_histogram(fill = BAR_COLOR, bins = 20, color = "white", linewidth = 0.2) +
    labs(x = x_label, y = "Weighted count")
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.4. An HTML table: the table kind (currently just `state`, 51 categories) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

html_escape <- function(x) {
  x %>% str_replace_all(fixed("&"), "&amp;") %>% str_replace_all(fixed("<"), "&lt;") %>% str_replace_all(fixed(">"), "&gt;")
}

render_table_html <- function(props) {
  rows <- props %>%
    transmute(category = html_escape(category), pct = sprintf("%.1f", pct), ci = sprintf("%.1f-%.1f", ci_low, ci_high)) %>%
    pmap_chr(~ sprintf("<tr><td>%s</td><td>%s</td><td>%s</td></tr>", ..1, ..2, ..3))
  paste0("<table class='vartable'><thead><tr><th>Category</th><th>Weighted %</th><th>95% CI</th></tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table>")
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.5. Relabeling helpers: column names and factor levels are var_names/tags, not display text ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# For a battery's items: var_name -> the text after the LAST colon in its var_label ("Main sources ... : Local or
# regional television" -> "Local or regional television"). Works for both delivered multiselect batteries and the
# leftover pooled-battery items found in Part B.9 — both are built to the same "stem: item" label convention.
item_display_labels <- function(cols) {
  var.index %>% filter(var_name %in% cols) %>%
    transmute(var_name, display = str_trim(str_extract(var_label, "(?<=: ).*"))) %>%
    deframe()
}

# For a rank battery's _1st column: its factor levels are short item TAGS ("tv_local"), not display text, and the
# same tag can mean different things in different vocabularies (item_tag "search" is "Search through Google or
# other search engines" under the BPC2 source battery but "Online search engine" under the BPC11/19/27
# information-source batteries) — so the lookup has to be scoped to the ONE multiselect battery this rank battery
# followed, not built globally. `derived_from` names that parent battery directly ("the BPC2a per-item rank
# columns..."); stripping the trailing "a" recovers the multiselect qid whose items share this rank battery's tags.
rank_display_labels <- function(rank_1st_col) {
  parent.qid <- var.index %>% filter(var_name == rank_1st_col) %>% pull(derived_from) %>% str_extract("BPC[0-9]+")
  var.index %>% filter(base == parent.qid, var_type == "multiselect") %>%
    transmute(item_tag, display = str_trim(str_extract(var_label, "(?<=: ).*"))) %>%
    deframe()
}




##### #
#### #
### ################################################################################################################################################# #
# Part E. Building every chart and table -------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# Rasterizes one ggplot to a temp PNG and inlines it as a base64 data URI via knitr::image_uri() — the same
# mechanism 'eis_2026_05_trend_comparison_report.Rmd' uses to stay a single self-contained file with no pandoc step.
embed_plot <- function(p, width = 7.5, height = 4, dpi = 110) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  ggsave(tmp, p, width = width, height = height, dpi = dpi, bg = "white")
  knitr::image_uri(tmp)
}

# More rows needs more height, capped so the largest battery (outlet, ~29 items) does not produce an unreasonably
# tall image.
bar_height <- function(n_rows) pmin(10, pmax(2.2, 1.0 + 0.28 * n_rows))

# One <div> per row of chart.plan: a heading, an optional caption (base condition, n, and — for ordinal and numeric
# items — the weighted mean), and either an embedded chart image or an HTML table.
build_entry <- function(row) {
  kind <- row$kind; key <- row$key; cols <- row$cols[[1]]; title <- html_escape(row$title)
  base.cap <- if (row$base_condition == "All respondents") NULL else paste0("Base: ", row$base_condition)

  wrap <- function(body_html, extra_caps = NULL) {
    caps <- c(base.cap, extra_caps)
    cap_html <- if (length(caps) == 0) "" else paste0("<p class='cap'>", html_escape(paste(caps, collapse = " — ")), "</p>")
    paste0("<div class='entry' id='", row$anchor, "'><h3>", title, "</h3>", cap_html, body_html, "</div>")
  }

  if (kind %in% c("ordinal", "nominal", "rank", "table")) {
    props <- factor_props(data, key)
    if (kind == "rank") props <- props %>% mutate(category = unname(rank_display_labels(key)[category]))

    n.cap <- paste0("n = ", format(props$n[1], big.mark = ","))
    if (kind == "table") return(wrap(render_table_html(props %>% arrange(desc(pct))), n.cap))

    ord <- if (kind == "ordinal") levels(data[[key]]) else NULL
    mean.cap <- NULL
    if (kind == "ordinal" && !is.na(row$mean_col)) {
      m <- svy_mean(data, row$mean_col)
      mean.cap <- sprintf("Weighted mean %.2f (95%% CI %.2f-%.2f) on a 1-%d scale", m$estimate, m$ci_low, m$ci_high, length(ord))
    }
    x.lab <- if (kind == "rank") "Weighted % ranking this their first choice" else "Weighted %"
    img <- embed_plot(plot_categorical(props, x_label = x.lab, order = ord), height = bar_height(nrow(props)))
    return(wrap(paste0("<img src='", img, "'>"), c(n.cap, mean.cap)))
  }

  if (kind == "battery") {
    props <- battery_props(data, cols) %>% mutate(item = unname(item_display_labels(cols)[item]))
    img <- embed_plot(plot_battery(props), height = bar_height(nrow(props)))
    return(wrap(paste0("<img src='", img, "'>"), paste0("n = ", format(props$n[1], big.mark = ","))))
  }

  if (kind == "numeric") {
    m <- svy_mean(data, key)
    img <- embed_plot(plot_numeric(data, key, x_label = title), height = 3.2)
    mean.cap <- sprintf("Weighted mean %.1f (95%% CI %.1f-%.1f)", m$estimate, m$ci_low, m$ci_high)
    return(wrap(paste0("<img src='", img, "'>"), c(paste0("n = ", format(m$n, big.mark = ",")), mean.cap)))
  }

  stop("Unhandled kind: ", kind)
}

message("\nPart E: building ", nrow(chart.plan), " charts and tables — this takes a few minutes.")
chart.plan$anchor <- paste0("v-", make.names(chart.plan$key), "-", seq_len(nrow(chart.plan)))
chart.plan$html   <- map_chr(seq_len(nrow(chart.plan)), ~ build_entry(chart.plan[.x, ]))




##### #
#### #
### ################################################################################################################################################# #
# Part F. Assembling the single-page gallery ---------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

kind.order <- c("ordinal", "nominal", "battery", "rank", "numeric", "table")
kind.section.titles <- c(ordinal = "Ordinal items", nominal = "Nominal, binary, and flag items",
                          battery = "Batteries (select-all-that-apply, and pooled indicators)",
                          rank = "Rank batteries (first-choice share)", numeric = "Numeric items",
                          table = "High-cardinality items (shown as a table)")

toc_html <- map_chr(kind.order, function(k) {
  rows <- chart.plan %>% filter(kind == k)
  if (nrow(rows) == 0) return("")
  links <- paste0("<li><a href='#", rows$anchor, "'>", html_escape(rows$title), "</a></li>", collapse = "\n")
  paste0("<li class='tocgroup'><strong>", kind.section.titles[[k]], " (", nrow(rows), ")</strong><ul>", links, "</ul></li>")
}) %>% paste(collapse = "\n")

sections_html <- map_chr(kind.order, function(k) {
  rows <- chart.plan %>% filter(kind == k)
  if (nrow(rows) == 0) return("")
  paste0("<section><h2 id='sec-", k, "'>", kind.section.titles[[k]], " (", nrow(rows), ")</h2>",
         paste(rows$html, collapse = "\n"), "</section>")
}) %>% paste(collapse = "\n")

page.css <- "
body { font-family: -apple-system, Helvetica, Arial, sans-serif; max-width: 900px; margin: 0 auto; padding: 24px; color: #1a1a1a; }
h1 { font-size: 1.6em; margin-bottom: 0.1em; }
p.subtitle { color: #555; margin-top: 0; }
p.meta { color: #555; font-size: 0.95em; margin-bottom: 2em; }
nav.toc { background: #f4f6f9; border: 1px solid #dde3ea; border-radius: 8px; padding: 16px 20px; margin-bottom: 2.5em; column-count: 2; column-gap: 32px; }
nav.toc > ul { list-style: none; padding-left: 0; margin: 0; }
nav.toc li.tocgroup { break-inside: avoid; margin-bottom: 14px; }
nav.toc ul ul { margin: 4px 0 0 0; padding-left: 18px; font-size: 0.92em; }
section { margin-bottom: 2.5em; }
section h2 { border-bottom: 2px solid #2a78d6; padding-bottom: 4px; }
.entry { margin: 1.6em 0; padding-bottom: 1.2em; border-bottom: 1px solid #eee; }
.entry h3 { margin-bottom: 2px; font-size: 1.05em; }
.entry .cap { color: #666; font-size: 0.88em; margin: 2px 0 8px 0; }
.entry img { max-width: 100%; height: auto; }
table.vartable { border-collapse: collapse; margin-top: 8px; }
table.vartable th, table.vartable td { border: 1px solid #ddd; padding: 5px 10px; text-align: left; font-size: 0.92em; }
table.vartable th { background: #f4f6f9; }
"

page.html <- paste0(
  "<!DOCTYPE html><html><head><meta charset='utf-8'><title>EIS 2026 Variable Atlas</title><style>", page.css, "</style></head><body>",
  "<h1>EIS 2026 Variable Atlas</h1>",
  "<p class='subtitle'>Every substantive variable in the BPC Election Information Survey, 2026 field, weighted.</p>",
  "<p class='meta'>", format(nrow(data), big.mark = ","), " registered voters, weighted by the delivered Morning ",
  "Consult weight (see 'eis_2026_06_design_data.R' for why this weight rather than a raked one). Bars show a ",
  "weighted percentage with a 95% confidence interval (<code>survey::svyciprop</code> / <code>svymean</code>). ",
  "A branching item's caption states its base condition and the respondent count within that base. Generated by ",
  "eis_2026_07_plots_overview.R on ", format(Sys.Date(), "%B %d, %Y"), ".</p>",
  "<nav class='toc'><ul>", toc_html, "</ul></nav>",
  sections_html,
  "</body></html>")

path.out <- file.path(out.dir, "eis_2026_variable_atlas.html")
writeLines(page.html, path.out)




##### #
#### #
### ################################################################################################################################################# #
# Part G. Summary -------------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

counts <- count(chart.plan, kind, name = "n")
message("\neis_2026_07_plots_overview.R: wrote ", nrow(chart.plan), " entries to ", path.out, "\n  ",
        paste(sprintf("%s: %d", counts$kind, counts$n), collapse = "\n  "))




# The end.
