# 04_analysis.R
#
# Replicates the figures and standalone statistics reported in "What Voters Think Ahead of the 2026
# Midterms: Election Information, Confidence, and Concerns," plus a set of supplementary figures
# (the appendix at the end of this script) that are not in the article.
#
# Main figures:
#   - Figure 1: "Which Sources Voters Use to Find Election Information" (share selecting each source
#               among their top 3; excludes people who report never seeking this information)
#   - Figure 2: "How Voters Contact Local or State Election Officials"
#   - Figure 3: "Confidence Votes Will Be Counted as Intended, by Party ID and Overall, 2022-2026"
#   - Figure 4: "Concerns about the midterm election, by party ID"
#
# Standalone statistics (cited in the text but not directly readable off Figures 1-4):
#   1. Total 2026 sample size (n = 3,144)
#   2. Local election officials as an information source for "registering and voting" and for
#      "how elections are run," 2024 vs. 2026
#   3. The Democratic-Republican confidence gap by item for 2022, 2024, and 2026, and its statistical
#      significance (weighted difference-of-proportions test)
#   4. Concern about AI-based election disinformation, 2024 vs. 2026
#
# Appendix (supplementary figures):
#   A. Figure 1, with 95% confidence intervals
#   B. Figure 1, showing the share ranking each source #1 instead of the share selecting it among
#      their top 3 (with confidence intervals)
#   C. Figure 1 for never-seekers (respondents who say they never look for that type of information),
#      in both the "selected" and "ranked #1" versions (with confidence intervals)
#   D. Figure 2, with confidence intervals
#   E. Figure 2 for never-seekers (with confidence intervals)
#   F. Figure 3, with confidence intervals
#   G. Figure 4, with confidence intervals
#   H. Concern about each of the 14 midterm-election issues, 2024 vs. 2026, among all respondents
#      (not split by party), with confidence intervals
#   I. The full four-level response distribution for each of the 14 concern items, by party ID,
#      shown as a diverging stacked bar chart
#
# Every figure also writes the data frame it is built from to output/plots/ as a .csv file.
#
# Before running this script, run the other scripts in this folder in the following order:
#   1. 01_clean_data.R       -> output/eis_2026_clean.rds
#                               output/eis_2026_sources_long.rds
#   2. 02_common_weights.R   -> output/eis_common_weights.rds
#   3. 03_cumulative_data.R  -> output/eis_cumulative.rds (2022, 2024, and 2026 stacked)
#
# All file paths are relative to R's working directory, which should be the folder that contains
# these scripts and the "data" folder. Set it however you prefer (for example, with setwd() or by
# opening the folder as an RStudio project). Intermediate files and figure data are written to an
# "output" folder, which the scripts create if it does not exist.
#
# Required packages: tidyverse, survey, zipcodeR (used by 03_cumulative_data.R and 01_clean_data.R), and readxl.

library(tidyverse)
library(survey)

theme_set(theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank()))

out.dir <- "output"
plots.dir <- file.path(out.dir, "plots")
dir.create(plots.dir, showWarnings = FALSE, recursive = TRUE)

BAR_COLOR     <- "#2a78d6"
RANK_COLOR    <- "#eb6834"
PID_COLORS    <- c(Dem = "#2a78d6", Ind = "#898781", Rep = "#d03b3b")
SERIES_COLORS <- c(Dem = "#2a78d6", Rep = "#d03b3b", "All respondents" = "grey70")
SERIES_LINETYPES <- c(Dem = "solid", Rep = "solid", "All respondents" = "dotted")
YEAR_COLORS   <- c("2024" = "#2a78d6", "2026" = "#eb6834")


##### #
#### #
### ################################################################### #
# Load data ----
### ################################################################### #
#### #
##### #

data <- readRDS(file.path(out.dir, "eis_2026_clean.rds"))
sources.long <- readRDS(file.path(out.dir, "eis_2026_sources_long.rds"))

# Display labels for the information sources (e.g., "Local Election Officials"), used for axis labels
# on Figure 1 and its appendix variants.
display_lookup <- sources.long %>%
  distinct(item_label) %>%
  mutate(display = str_extract(item_label, "(?<=: ).*"),
         display = str_trim(str_remove_all(display, "\\s*\\([^)]*\\)")),
         display = tools::toTitleCase(display))

# Lookup table from item_tag to display label, joined onto each source-item estimate below. It is
# limited to the "which sources" questions (registering and voting, how elections are run, and who won;
# asked of both seekers and never-seekers). A few item_tag values (e.g., "search" and "social") are
# also used, with different labels, by a separate election-news-sources question, so joining against
# all of sources.long would match some item_tags to more than one label.
item_display_lookup <- sources.long %>%
  left_join(display_lookup, by = "item_label") %>%
  filter(battery %in% c("reg_act", "run_act", "won_act", "reg_hyp", "run_hyp", "won_hyp")) %>%
  distinct(item_tag, display)

# eis_cumulative.rds holds one row per respondent per year for 2022, 2024, and 2026 pooled together.
# weight_native is each year's own survey weight; weight_common is the re-raked weight used for
# cross-year comparisons (see 02_common_weights.R).
cum <- readRDS(file.path(out.dir, "eis_cumulative.rds")) %>%
  mutate(year = factor(year, levels = c("2022", "2024", "2026")))


##### #
#### #
### ################################################################### #
# Figure 1: "Which Sources Voters Use to Find Election Information" ----
### ################################################################### #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 1.1. Never-seekers-excluded population, by need ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Exclude never-seekers: keep only respondents who report actually seeking each type of information.
# (The "_act" questions were asked of seekers; the "_hyp" versions were asked of never-seekers and are
# used in the appendix.)
reg_exclude_long <- sources.long %>% filter(battery %in% "reg_act", in_base)
run_exclude_long <- sources.long %>% filter(battery %in% "run_act", in_base)
won_exclude_long <- sources.long %>% filter(battery %in% "won_act", in_base)

reg_exclude_n <- n_distinct(reg_exclude_long$resp_id)
run_exclude_n <- n_distinct(run_exclude_long$resp_id)
won_exclude_n <- n_distinct(won_exclude_long$resp_id)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 1.2. Weighted % selecting each source, by need ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# One svyby() call per need: sources.long has one row per respondent per item, so a single survey
# design covers every item at once, and svyby() calls svyciprop() once per item_tag group.
reg_selected_design <- svydesign(ids = ~1, weights = ~weight, data = reg_exclude_long %>% mutate(is_selected = selected == "1"))
reg_selected_by_item <- svyby(~is_selected, ~item_tag, reg_selected_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(need = "Registering and Voting", item_tag = as.character(item_tag),
            pct = is_selected * 100, ci_low = ci_l * 100, ci_high = ci_u * 100, n = reg_exclude_n)

run_selected_design <- svydesign(ids = ~1, weights = ~weight, data = run_exclude_long %>% mutate(is_selected = selected == "1"))
run_selected_by_item <- svyby(~is_selected, ~item_tag, run_selected_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(need = "How Elections Are Run", item_tag = as.character(item_tag),
            pct = is_selected * 100, ci_low = ci_l * 100, ci_high = ci_u * 100, n = run_exclude_n)

won_selected_design <- svydesign(ids = ~1, weights = ~weight, data = won_exclude_long %>% mutate(is_selected = selected == "1"))
won_selected_by_item <- svyby(~is_selected, ~item_tag, won_selected_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(need = "Who Won an Election", item_tag = as.character(item_tag),
            pct = is_selected * 100, ci_low = ci_l * 100, ci_high = ci_u * 100, n = won_exclude_n)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 1.3. Combine, order, and build facet labels ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

which_exclude_selected_raw <- bind_rows(reg_selected_by_item, run_selected_by_item, won_selected_by_item) %>%
  left_join(item_display_lookup, by = "item_tag") %>%
  filter(!item_tag %in% c("other", "dont_know"))

# Item order: ascending by mean pct across the 3 needs (renders highest at the top of the chart)
which_exclude_item_means <- which_exclude_selected_raw %>% group_by(display) %>% summarise(m = mean(pct), .groups = "drop") %>% arrange(m)
which_exclude_selected_ordered <- which_exclude_selected_raw %>% mutate(display = factor(display, levels = which_exclude_item_means$display))

# Facet strip "(n = X)" labels, one per need
which_exclude_n_by_need <- which_exclude_selected_ordered %>% distinct(need, n) %>% deframe()
which_exclude_selected_df <- which_exclude_selected_ordered %>%
  mutate(need = factor(need, levels = c("Registering and Voting", "How Elections Are Run", "Who Won an Election")),
         need_label = factor(paste0(need, "\n(n = ", format(which_exclude_n_by_need[as.character(need)], big.mark = ","), ")"),
                              levels = paste0(c("Registering and Voting", "How Elections Are Run", "Who Won an Election"),
                                              "\n(n = ", format(which_exclude_n_by_need[c("Registering and Voting", "How Elections Are Run", "Who Won an Election")], big.mark = ","), ")")))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 1.4. Export and plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(which_exclude_selected_df, file.path(plots.dir, "fig1_which_sources_selected.csv"), row.names = FALSE)

p_which_sources <- ggplot(which_exclude_selected_df, aes(y = display, x = pct)) +
  geom_col(fill = BAR_COLOR, width = 0.65) +
  scale_x_continuous(expand = expansion(mult = c(0, .08))) +
  scale_y_discrete(labels = scales::label_wrap(30)) +
  facet_wrap(~ need_label, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = str_wrap("Which Sources Voters Use to Find Election Information", width = 65),
       subtitle = "Among respondents who report seeking this information") +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p_which_sources



##### #
#### #
### ################################################################### #
# Figure 2: "How Voters Contact Local or State Election Officials" ----
### ################################################################### #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 2.1. Weighted % choosing each contact method, by need ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

mode_reg_data <- data %>% filter(!is.na(reg_act_official_mode))
mode_reg_design <- svydesign(ids = ~1, weights = ~weight, data = mode_reg_data)
mode_reg_est <- svymean(~reg_act_official_mode, mode_reg_design)
mode_reg_df <- tibble(category = sub("^reg_act_official_mode", "", names(mode_reg_est)),
                       pct = as.numeric(mode_reg_est) * 100,
                       ci_low = confint(mode_reg_est)[, 1] * 100,
                       ci_high = confint(mode_reg_est)[, 2] * 100,
                       n = nrow(mode_reg_data)) %>%
  mutate(need = "Registering and Voting", .before = 1)

mode_run_data <- data %>% filter(!is.na(run_act_official_mode))
mode_run_design <- svydesign(ids = ~1, weights = ~weight, data = mode_run_data)
mode_run_est <- svymean(~run_act_official_mode, mode_run_design)
mode_run_df <- tibble(category = sub("^run_act_official_mode", "", names(mode_run_est)),
                       pct = as.numeric(mode_run_est) * 100,
                       ci_low = confint(mode_run_est)[, 1] * 100,
                       ci_high = confint(mode_run_est)[, 2] * 100,
                       n = nrow(mode_run_data)) %>%
  mutate(need = "How Elections Are Run", .before = 1)

mode_won_data <- data %>% filter(!is.na(won_act_official_mode))
mode_won_design <- svydesign(ids = ~1, weights = ~weight, data = mode_won_data)
mode_won_est <- svymean(~won_act_official_mode, mode_won_design)
mode_won_df <- tibble(category = sub("^won_act_official_mode", "", names(mode_won_est)),
                       pct = as.numeric(mode_won_est) * 100,
                       ci_low = confint(mode_won_est)[, 1] * 100,
                       ci_high = confint(mode_won_est)[, 2] * 100,
                       n = nrow(mode_won_data)) %>%
  mutate(need = "Who Won an Election", .before = 1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 2.2. Combine, relevel, and build facet labels ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

mode_exclude_raw <- bind_rows(mode_reg_df, mode_run_df, mode_won_df) %>%
  filter(!category %in% c("Other, please specify", "Don't know")) %>%
  mutate(need = fct_relevel(need, "Registering and Voting", "How Elections Are Run", "Who Won an Election"),
         category = case_when(!grepl("website", category) ~ gsub(" \\(town, city, county\\)", "", category),
                              TRUE ~ category),
         category = as.factor(category))

# Reorder categories so that, top to bottom on the y-axis, they read website / phone / in person.
# ggplot draws the first level of a discrete y-axis at the bottom, so "in person" is listed first
# and "website" last.
mode_exclude_levels <- setdiff(levels(mode_exclude_raw$category), c("Other, please specify", "Don't know"))
mode_exclude_colors <- setNames(colorRampPalette(c("#b7d3f6", "#0d366b"))(length(mode_exclude_levels)), mode_exclude_levels)
mode_exclude_releveled <- mode_exclude_raw %>%
  mutate(category = fct_relevel(category,
                                "I visit my local or state election office in person",
                                "I call my local or state election office on the phone",
                                "I visit my local (town, city, county) or state election office's website"))

mode_exclude_n_by_need <- mode_exclude_releveled %>% distinct(need, n) %>% deframe()
mode_exclude_combined_df <- mode_exclude_releveled %>%
  mutate(need_label = factor(paste0(need, "\n(n = ", trimws(format(mode_exclude_n_by_need[as.character(need)], big.mark = ",")), ")"),
                              levels = paste0(c("Registering and Voting", "How Elections Are Run", "Who Won an Election"),
                                              "\n(n = ", trimws(format(mode_exclude_n_by_need[c("Registering and Voting", "How Elections Are Run", "Who Won an Election")], big.mark = ",")), ")")))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 2.3. Export and plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(mode_exclude_combined_df, file.path(plots.dir, "fig2_contact_mode.csv"), row.names = FALSE)

p_contact_mode <- ggplot(mode_exclude_combined_df, aes(y = category, x = pct, fill = category)) +
  geom_col(width = 0.65) +
  geom_text(aes(x = pct, label = paste0(round(pct), "%")), hjust = -0.15, size = 3.1, color = "grey30") +
  scale_fill_manual(values = mode_exclude_colors, name = NULL, labels = scales::label_wrap(40)) +
  scale_x_continuous(expand = expansion(mult = c(0, .125))) +
  scale_y_discrete(labels = scales::label_wrap(30)) +
  facet_wrap(~ need_label, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = str_wrap("How Voters Contact Local or State Election Officials", width = 65),
       subtitle = "Among respondents who seek this information, by information need") +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p_contact_mode



##### #
#### #
### ################################################################### #
# Figure 3: "Confidence Votes Will Be Counted as Intended, by Party ID and Overall, 2022-2026" ----
# Combines an all-respondents trend across years with a by-party trend across years, both built below.
### ################################################################### #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 3.1. "Confident" indicators (very or somewhat confident) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Each confidence item is collapsed from four levels to a 0/1 indicator: 1 if the respondent is
# "very confident" or "somewhat confident" (levels 3 and 4), 0 otherwise. The indicators are added to
# a copy of `cum` so that `cum` itself stays unmodified for the later sections.
conf_items <- c("conf_own_vote", "conf_local_votes", "conf_state_votes", "conf_national_votes")
cum_conf <- cum %>% mutate(across(all_of(conf_items), ~ as.numeric(as.integer(.x) >= 3), .names = "{.col}_confident"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 3.2. All-respondents and by-party trends, by item ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# For each item, one svyby() call estimates the all-respondents trend (grouped by year) and another
# estimates the by-party trend (grouped by year and party ID, Democrats and Republicans only).

### Figure 3.2.1. Your Own Vote ----
conf_own_vote_all_data <- cum_conf %>% filter(!is.na(conf_own_vote_confident))
conf_own_vote_all_design <- svydesign(ids = ~1, weights = ~weight_common, data = conf_own_vote_all_data)
conf_own_vote_all_by_year <- svyby(~conf_own_vote_confident, ~year, conf_own_vote_all_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(item = "Your Own Vote", year = as.character(year), series = "All respondents",
            pct = conf_own_vote_confident * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

conf_own_vote_party_data <- cum_conf %>% filter(!is.na(conf_own_vote_confident), pid3 %in% c("Dem", "Rep"))
conf_own_vote_party_design <- svydesign(ids = ~1, weights = ~weight_common, data = conf_own_vote_party_data)
conf_own_vote_by_party <- svyby(~conf_own_vote_confident, ~year + pid3, conf_own_vote_party_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(item = "Your Own Vote", year = as.character(year), series = as.character(pid3),
            pct = conf_own_vote_confident * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

### Figure 3.2.2. Votes in Your County/City ----
conf_local_votes_all_data <- cum_conf %>% filter(!is.na(conf_local_votes_confident))
conf_local_votes_all_design <- svydesign(ids = ~1, weights = ~weight_common, data = conf_local_votes_all_data)
conf_local_votes_all_by_year <- svyby(~conf_local_votes_confident, ~year, conf_local_votes_all_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(item = "Votes in Your County/City", year = as.character(year), series = "All respondents",
            pct = conf_local_votes_confident * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

conf_local_votes_party_data <- cum_conf %>% filter(!is.na(conf_local_votes_confident), pid3 %in% c("Dem", "Rep"))
conf_local_votes_party_design <- svydesign(ids = ~1, weights = ~weight_common, data = conf_local_votes_party_data)
conf_local_votes_by_party <- svyby(~conf_local_votes_confident, ~year + pid3, conf_local_votes_party_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(item = "Votes in Your County/City", year = as.character(year), series = as.character(pid3),
            pct = conf_local_votes_confident * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

### Figure 3.2.3. Votes in Your State ----
conf_state_votes_all_data <- cum_conf %>% filter(!is.na(conf_state_votes_confident))
conf_state_votes_all_design <- svydesign(ids = ~1, weights = ~weight_common, data = conf_state_votes_all_data)
conf_state_votes_all_by_year <- svyby(~conf_state_votes_confident, ~year, conf_state_votes_all_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(item = "Votes in Your State", year = as.character(year), series = "All respondents",
            pct = conf_state_votes_confident * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

conf_state_votes_party_data <- cum_conf %>% filter(!is.na(conf_state_votes_confident), pid3 %in% c("Dem", "Rep"))
conf_state_votes_party_design <- svydesign(ids = ~1, weights = ~weight_common, data = conf_state_votes_party_data)
conf_state_votes_by_party <- svyby(~conf_state_votes_confident, ~year + pid3, conf_state_votes_party_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(item = "Votes in Your State", year = as.character(year), series = as.character(pid3),
            pct = conf_state_votes_confident * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

### Figure 3.2.4. Votes Nationwide ----
conf_national_votes_all_data <- cum_conf %>% filter(!is.na(conf_national_votes_confident))
conf_national_votes_all_design <- svydesign(ids = ~1, weights = ~weight_common, data = conf_national_votes_all_data)
conf_national_votes_all_by_year <- svyby(~conf_national_votes_confident, ~year, conf_national_votes_all_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(item = "Votes Nationwide", year = as.character(year), series = "All respondents",
            pct = conf_national_votes_confident * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

conf_national_votes_party_data <- cum_conf %>% filter(!is.na(conf_national_votes_confident), pid3 %in% c("Dem", "Rep"))
conf_national_votes_party_design <- svydesign(ids = ~1, weights = ~weight_common, data = conf_national_votes_party_data)
conf_national_votes_by_party <- svyby(~conf_national_votes_confident, ~year + pid3, conf_national_votes_party_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(item = "Votes Nationwide", year = as.character(year), series = as.character(pid3),
            pct = conf_national_votes_confident * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 3.3. Combine and order for plotting ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

conf_trend_all_raw <- bind_rows(
  conf_own_vote_all_by_year, conf_own_vote_by_party,
  conf_local_votes_all_by_year, conf_local_votes_by_party,
  conf_state_votes_all_by_year, conf_state_votes_by_party,
  conf_national_votes_all_by_year, conf_national_votes_by_party
) %>%
  mutate(year = factor(year, levels = c("2022", "2024", "2026")),
         series = factor(series, levels = c("Dem", "Rep", "All respondents")))

# Facet order: ascending by each item's range across the 3 years (All respondents series)
conf_item_range <- conf_trend_all_raw %>%
  filter(series == "All respondents") %>%
  group_by(item) %>%
  summarise(rng = max(pct) - min(pct), .groups = "drop") %>%
  arrange(rng)
conf_trend_all_df <- conf_trend_all_raw %>% mutate(item = factor(item, levels = conf_item_range$item))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 3.4. Export and plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(conf_trend_all_df, file.path(plots.dir, "fig3_confidence_trend.csv"), row.names = FALSE)

p_confidence <- ggplot(conf_trend_all_df, aes(x = year, y = pct, color = series, group = series)) +
  geom_line(aes(linetype = series), linewidth = 0.8) +
  geom_point(size = 2.6) +
  facet_wrap(~ item) +
  scale_color_manual(values = SERIES_COLORS, name = NULL) +
  scale_linetype_manual(values = SERIES_LINETYPES, name = NULL) +
  scale_y_continuous(limits = c(50, 100), expand = expansion(mult = c(0.04, .08))) +
  labs(x = NULL, y = "Weighted % Confident",
       title = str_wrap("Confidence Votes Will Be Counted as Intended, by Party ID and Overall, 2022-2026", width = 65),
       subtitle = str_wrap("Showing percentage of those \"very confident\" and \"somewhat confident\"; Independents included only in \"All respondents\"", width = 100)) +
  theme(legend.position = "top", panel.grid.major.x = element_blank())
p_confidence



##### #
#### #
### ################################################################### #
# Figure 4: "Concerns about the midterm election, by party ID" ----
### ################################################################### #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 4.1. "Concerned" indicators (very or somewhat concerned) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Each concern item is collapsed from four levels to a 0/1 indicator: 1 if the respondent is "very
# concerned" or "somewhat concerned" (levels 3 and 4), 0 if "not too concerned" or "not concerned at
# all." The indicators are added to a copy of `data`, one per item.
concern_items <- c("concern_misinfo", "concern_ai_disinfo", "concern_foreign", "concern_ineligible_votes",
                   "concern_overturn", "concern_biased_count", "concern_mail_ballots", "concern_guns_intimidation",
                   "concern_post_violence", "concern_polling_problems", "concern_ice_deployment",
                   "concern_ballot_seizure", "concern_eligible_blocked", "concern_gerrymander")
data_concern <- data %>% mutate(across(all_of(concern_items), ~ as.numeric(as.integer(.x) >= 3), .names = "{.col}_concerned"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 4.2. Weighted % concerned, by party, by item ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# One svyby() call per item, grouped by party (Democrats and Republicans only).
concern_data_party <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"))
concern_design_party <- svydesign(ids = ~1, weights = ~weight, data = concern_data_party)

concern_misinfo_by_party <- svyby(~concern_misinfo_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Inaccurate or Misleading Election Information", pid3 = as.character(pid3),
                             pct = concern_misinfo_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_ai_disinfo_by_party <- svyby(~concern_ai_disinfo_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "AI Used to Spread Disinformation", pid3 = as.character(pid3),
                             pct = concern_ai_disinfo_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_foreign_by_party <- svyby(~concern_foreign_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Foreign Interference", pid3 = as.character(pid3),
                             pct = concern_foreign_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_ineligible_votes_by_party <- svyby(~concern_ineligible_votes_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Ineligible Votes Being Counted", pid3 = as.character(pid3),
                             pct = concern_ineligible_votes_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_overturn_by_party <- svyby(~concern_overturn_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Attempts to Overturn a Fair Election's Results", pid3 = as.character(pid3),
                             pct = concern_overturn_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_biased_count_by_party <- svyby(~concern_biased_count_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Biased or Inaccurate Ballot Counting", pid3 = as.character(pid3),
                             pct = concern_biased_count_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_mail_ballots_by_party <- svyby(~concern_mail_ballots_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Illegal or Improper Mail Ballot/Drop Box Use", pid3 = as.character(pid3),
                             pct = concern_mail_ballots_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_guns_intimidation_by_party <- svyby(~concern_guns_intimidation_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Guns, Violence, or Intimidation at Voting Locations", pid3 = as.character(pid3),
                             pct = concern_guns_intimidation_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_post_violence_by_party <- svyby(~concern_post_violence_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Violence or Unrest After Election Day", pid3 = as.character(pid3),
                             pct = concern_post_violence_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_polling_problems_by_party <- svyby(~concern_polling_problems_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Long Lines or Equipment Problems at Polls", pid3 = as.character(pid3),
                             pct = concern_polling_problems_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_ice_deployment_by_party <- svyby(~concern_ice_deployment_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "ICE/Federal Law Enforcement in Your Community", pid3 = as.character(pid3),
                             pct = concern_ice_deployment_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_ballot_seizure_by_party <- svyby(~concern_ballot_seizure_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Federal/State Seizure of Ballots or Voting Machines", pid3 = as.character(pid3),
                             pct = concern_ballot_seizure_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_eligible_blocked_by_party <- svyby(~concern_eligible_blocked_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Eligible Voters Blocked From Voting", pid3 = as.character(pid3),
                             pct = concern_eligible_blocked_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)
concern_gerrymander_by_party <- svyby(~concern_gerrymander_concerned, ~pid3, concern_design_party, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Unfair District Lines Distorting Outcomes", pid3 = as.character(pid3),
                             pct = concern_gerrymander_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 4.3. Combine, order, and mark which party is more concerned ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

concern_by_party_raw <- bind_rows(
  concern_misinfo_by_party, concern_ai_disinfo_by_party, concern_foreign_by_party, concern_ineligible_votes_by_party,
  concern_overturn_by_party, concern_biased_count_by_party, concern_mail_ballots_by_party, concern_guns_intimidation_by_party,
  concern_post_violence_by_party, concern_polling_problems_by_party, concern_ice_deployment_by_party, concern_ballot_seizure_by_party,
  concern_eligible_blocked_by_party, concern_gerrymander_by_party
)

# Item order: ascending by mean pct across Dem/Rep
concern_item_means <- concern_by_party_raw %>% group_by(item) %>% summarise(m = mean(pct), .groups = "drop") %>% arrange(m)
concern_by_party_df <- concern_by_party_raw %>%
  mutate(item = factor(item, levels = concern_item_means$item),
         pid3 = factor(pid3, levels = c("Dem", "Rep")))

# One row per item: the midpoint between the two parties, the absolute Democratic-Republican gap
# (used as the label on each line), and which party is more concerned (used to color the line).
concern_party_diff_df <- concern_by_party_df %>%
  select(item, pid3, pct) %>%
  pivot_wider(names_from = pid3, values_from = pct) %>%
  mutate(mid_pct = (Dem + Rep) / 2, diff = abs(Dem - Rep),
         higher_party = if_else(Dem > Rep, "Dem", "Rep"))

# Join higher_party onto the long data, which the plot needs to color the connecting lines.
concern_by_party_plot_df <- concern_by_party_df %>%
  left_join(concern_party_diff_df %>% select(item, higher_party), by = "item")

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Figure 4.4. Export and plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(concern_by_party_plot_df, file.path(plots.dir, "fig4_concern_by_party.csv"), row.names = FALSE)

p_concern <- ggplot(concern_by_party_plot_df, aes(y = item)) +
  geom_line(aes(x = pct, group = item, color = higher_party), linewidth = 1) +
  geom_point(aes(x = pct, color = pid3), size = 3.4) +
  geom_text(data = concern_party_diff_df, aes(x = mid_pct, label = round(diff)), vjust = -0.8, size = 3.1, color = "grey30") +
  scale_color_manual(values = PID_COLORS, name = NULL) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0.02, .08))) +
  scale_y_discrete(labels = scales::label_wrap(40)) +
  labs(x = "Weighted % Concerned", y = NULL,
       title = "Concerns about the midterm election, by party ID",
       subtitle = "") +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
p_concern



##### #
#### #
### ################################################################### #
# Standalone statistics (cited in the text but not directly readable off Figures 1-4 above) ----
### ################################################################### #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## 1. Total 2026 sample size ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

n_2026_respondents <- nrow(data)
n_2026_respondents  # 3,144 registered voters

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## 2. Local election officials as an information source, 2024 vs. 2026 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# In 2024, the "select up to 3 sources" question was asked of every respondent regardless of how often
# they seek election information (no respondent has a missing value on reg_src_local_officials or
# run_src_local_officials in 2024). In 2026, it was asked only of self-identified seekers, the same
# population used in Figure 1. The 2024 estimate therefore covers all respondents, and the 2026
# estimate is taken directly from the Figure 1 results (which_exclude_selected_df).

### 2.1. Registering and voting ----
local_reg_2024_data <- cum %>% filter(year == "2024", !is.na(reg_src_local_officials)) %>%
  mutate(ind = reg_src_local_officials == "1")
local_reg_2024_design <- svydesign(ids = ~1, weights = ~weight_native, data = local_reg_2024_data)
local_reg_2024_est <- svyciprop(~ind, local_reg_2024_design, method = "logit")
local_reg_pct_2024 <- as.numeric(local_reg_2024_est) * 100

local_reg_pct_2026 <- which_exclude_selected_df %>%
  filter(need == "Registering and Voting", item_tag == "local_officials") %>% pull(pct)  # already computed for Figure 1
local_reg_pp_change <- local_reg_pct_2026 - local_reg_pct_2024
local_reg_pct_2024; local_reg_pct_2026; local_reg_pp_change  # ~27%, ~31%, +~4 points

### 2.2. How elections are run ----
local_run_2024_data <- cum %>% filter(year == "2024", !is.na(run_src_local_officials)) %>%
  mutate(ind = run_src_local_officials == "1")
local_run_2024_design <- svydesign(ids = ~1, weights = ~weight_native, data = local_run_2024_data)
local_run_2024_est <- svyciprop(~ind, local_run_2024_design, method = "logit")
local_run_pct_2024 <- as.numeric(local_run_2024_est) * 100

local_run_pct_2026 <- which_exclude_selected_df %>%
  filter(need == "How Elections Are Run", item_tag == "local_officials") %>% pull(pct)  # already computed for Figure 1
local_run_pct_2024; local_run_pct_2026  # ~16%, ~26%

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## 3. Partisan confidence gap: Democrats vs. Republicans, by item and year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Weighted difference-of-proportions test (design-based two-sample t-test). The Democratic and
# Republican percentages are taken from Figure 3's conf_trend_all_df rather than re-estimated; only the
# p-value is new, because a two-sample t-test needs both parties in one survey design (svyby() estimates
# one group at a time).

### 3.1. Your Own Vote ----
difftest_conf_own_vote_2022_data <- cum_conf %>% filter(year == "2022", pid3 %in% c("Dem", "Rep"), !is.na(conf_own_vote_confident))
difftest_conf_own_vote_2022_design <- svydesign(ids = ~1, weights = ~weight_common, data = difftest_conf_own_vote_2022_data)
difftest_conf_own_vote_2022_test <- svyttest(conf_own_vote_confident ~ pid3, difftest_conf_own_vote_2022_design)
difftest_conf_own_vote_2022_row <- tibble(
  item = "Your Own Vote", year = "2022",
  pct_dem = conf_trend_all_df %>% filter(item == "Your Own Vote", year == "2022", series == "Dem") %>% pull(pct),
  pct_rep = conf_trend_all_df %>% filter(item == "Your Own Vote", year == "2022", series == "Rep") %>% pull(pct),
  diff = pct_dem - pct_rep, p_value = difftest_conf_own_vote_2022_test$p.value, n = nrow(difftest_conf_own_vote_2022_data))

difftest_conf_own_vote_2024_data <- cum_conf %>% filter(year == "2024", pid3 %in% c("Dem", "Rep"), !is.na(conf_own_vote_confident))
difftest_conf_own_vote_2024_design <- svydesign(ids = ~1, weights = ~weight_common, data = difftest_conf_own_vote_2024_data)
difftest_conf_own_vote_2024_test <- svyttest(conf_own_vote_confident ~ pid3, difftest_conf_own_vote_2024_design)
difftest_conf_own_vote_2024_row <- tibble(
  item = "Your Own Vote", year = "2024",
  pct_dem = conf_trend_all_df %>% filter(item == "Your Own Vote", year == "2024", series == "Dem") %>% pull(pct),
  pct_rep = conf_trend_all_df %>% filter(item == "Your Own Vote", year == "2024", series == "Rep") %>% pull(pct),
  diff = pct_dem - pct_rep, p_value = difftest_conf_own_vote_2024_test$p.value, n = nrow(difftest_conf_own_vote_2024_data))

difftest_conf_own_vote_2026_data <- cum_conf %>% filter(year == "2026", pid3 %in% c("Dem", "Rep"), !is.na(conf_own_vote_confident))
difftest_conf_own_vote_2026_design <- svydesign(ids = ~1, weights = ~weight_common, data = difftest_conf_own_vote_2026_data)
difftest_conf_own_vote_2026_test <- svyttest(conf_own_vote_confident ~ pid3, difftest_conf_own_vote_2026_design)
difftest_conf_own_vote_2026_row <- tibble(
  item = "Your Own Vote", year = "2026",
  pct_dem = conf_trend_all_df %>% filter(item == "Your Own Vote", year == "2026", series == "Dem") %>% pull(pct),
  pct_rep = conf_trend_all_df %>% filter(item == "Your Own Vote", year == "2026", series == "Rep") %>% pull(pct),
  diff = pct_dem - pct_rep, p_value = difftest_conf_own_vote_2026_test$p.value, n = nrow(difftest_conf_own_vote_2026_data))

### 3.2. Votes in Your County/City ----
difftest_conf_local_votes_2022_data <- cum_conf %>% filter(year == "2022", pid3 %in% c("Dem", "Rep"), !is.na(conf_local_votes_confident))
difftest_conf_local_votes_2022_design <- svydesign(ids = ~1, weights = ~weight_common, data = difftest_conf_local_votes_2022_data)
difftest_conf_local_votes_2022_test <- svyttest(conf_local_votes_confident ~ pid3, difftest_conf_local_votes_2022_design)
difftest_conf_local_votes_2022_row <- tibble(
  item = "Votes in Your County/City", year = "2022",
  pct_dem = conf_trend_all_df %>% filter(item == "Votes in Your County/City", year == "2022", series == "Dem") %>% pull(pct),
  pct_rep = conf_trend_all_df %>% filter(item == "Votes in Your County/City", year == "2022", series == "Rep") %>% pull(pct),
  diff = pct_dem - pct_rep, p_value = difftest_conf_local_votes_2022_test$p.value, n = nrow(difftest_conf_local_votes_2022_data))

difftest_conf_local_votes_2024_data <- cum_conf %>% filter(year == "2024", pid3 %in% c("Dem", "Rep"), !is.na(conf_local_votes_confident))
difftest_conf_local_votes_2024_design <- svydesign(ids = ~1, weights = ~weight_common, data = difftest_conf_local_votes_2024_data)
difftest_conf_local_votes_2024_test <- svyttest(conf_local_votes_confident ~ pid3, difftest_conf_local_votes_2024_design)
difftest_conf_local_votes_2024_row <- tibble(
  item = "Votes in Your County/City", year = "2024",
  pct_dem = conf_trend_all_df %>% filter(item == "Votes in Your County/City", year == "2024", series == "Dem") %>% pull(pct),
  pct_rep = conf_trend_all_df %>% filter(item == "Votes in Your County/City", year == "2024", series == "Rep") %>% pull(pct),
  diff = pct_dem - pct_rep, p_value = difftest_conf_local_votes_2024_test$p.value, n = nrow(difftest_conf_local_votes_2024_data))

difftest_conf_local_votes_2026_data <- cum_conf %>% filter(year == "2026", pid3 %in% c("Dem", "Rep"), !is.na(conf_local_votes_confident))
difftest_conf_local_votes_2026_design <- svydesign(ids = ~1, weights = ~weight_common, data = difftest_conf_local_votes_2026_data)
difftest_conf_local_votes_2026_test <- svyttest(conf_local_votes_confident ~ pid3, difftest_conf_local_votes_2026_design)
difftest_conf_local_votes_2026_row <- tibble(
  item = "Votes in Your County/City", year = "2026",
  pct_dem = conf_trend_all_df %>% filter(item == "Votes in Your County/City", year == "2026", series == "Dem") %>% pull(pct),
  pct_rep = conf_trend_all_df %>% filter(item == "Votes in Your County/City", year == "2026", series == "Rep") %>% pull(pct),
  diff = pct_dem - pct_rep, p_value = difftest_conf_local_votes_2026_test$p.value, n = nrow(difftest_conf_local_votes_2026_data))

### 3.3. Votes in Your State ----
difftest_conf_state_votes_2022_data <- cum_conf %>% filter(year == "2022", pid3 %in% c("Dem", "Rep"), !is.na(conf_state_votes_confident))
difftest_conf_state_votes_2022_design <- svydesign(ids = ~1, weights = ~weight_common, data = difftest_conf_state_votes_2022_data)
difftest_conf_state_votes_2022_test <- svyttest(conf_state_votes_confident ~ pid3, difftest_conf_state_votes_2022_design)
difftest_conf_state_votes_2022_row <- tibble(
  item = "Votes in Your State", year = "2022",
  pct_dem = conf_trend_all_df %>% filter(item == "Votes in Your State", year == "2022", series == "Dem") %>% pull(pct),
  pct_rep = conf_trend_all_df %>% filter(item == "Votes in Your State", year == "2022", series == "Rep") %>% pull(pct),
  diff = pct_dem - pct_rep, p_value = difftest_conf_state_votes_2022_test$p.value, n = nrow(difftest_conf_state_votes_2022_data))

difftest_conf_state_votes_2024_data <- cum_conf %>% filter(year == "2024", pid3 %in% c("Dem", "Rep"), !is.na(conf_state_votes_confident))
difftest_conf_state_votes_2024_design <- svydesign(ids = ~1, weights = ~weight_common, data = difftest_conf_state_votes_2024_data)
difftest_conf_state_votes_2024_test <- svyttest(conf_state_votes_confident ~ pid3, difftest_conf_state_votes_2024_design)
difftest_conf_state_votes_2024_row <- tibble(
  item = "Votes in Your State", year = "2024",
  pct_dem = conf_trend_all_df %>% filter(item == "Votes in Your State", year == "2024", series == "Dem") %>% pull(pct),
  pct_rep = conf_trend_all_df %>% filter(item == "Votes in Your State", year == "2024", series == "Rep") %>% pull(pct),
  diff = pct_dem - pct_rep, p_value = difftest_conf_state_votes_2024_test$p.value, n = nrow(difftest_conf_state_votes_2024_data))

difftest_conf_state_votes_2026_data <- cum_conf %>% filter(year == "2026", pid3 %in% c("Dem", "Rep"), !is.na(conf_state_votes_confident))
difftest_conf_state_votes_2026_design <- svydesign(ids = ~1, weights = ~weight_common, data = difftest_conf_state_votes_2026_data)
difftest_conf_state_votes_2026_test <- svyttest(conf_state_votes_confident ~ pid3, difftest_conf_state_votes_2026_design)
difftest_conf_state_votes_2026_row <- tibble(
  item = "Votes in Your State", year = "2026",
  pct_dem = conf_trend_all_df %>% filter(item == "Votes in Your State", year == "2026", series == "Dem") %>% pull(pct),
  pct_rep = conf_trend_all_df %>% filter(item == "Votes in Your State", year == "2026", series == "Rep") %>% pull(pct),
  diff = pct_dem - pct_rep, p_value = difftest_conf_state_votes_2026_test$p.value, n = nrow(difftest_conf_state_votes_2026_data))

### 3.4. Votes Nationwide ----
difftest_conf_national_votes_2022_data <- cum_conf %>% filter(year == "2022", pid3 %in% c("Dem", "Rep"), !is.na(conf_national_votes_confident))
difftest_conf_national_votes_2022_design <- svydesign(ids = ~1, weights = ~weight_common, data = difftest_conf_national_votes_2022_data)
difftest_conf_national_votes_2022_test <- svyttest(conf_national_votes_confident ~ pid3, difftest_conf_national_votes_2022_design)
difftest_conf_national_votes_2022_row <- tibble(
  item = "Votes Nationwide", year = "2022",
  pct_dem = conf_trend_all_df %>% filter(item == "Votes Nationwide", year == "2022", series == "Dem") %>% pull(pct),
  pct_rep = conf_trend_all_df %>% filter(item == "Votes Nationwide", year == "2022", series == "Rep") %>% pull(pct),
  diff = pct_dem - pct_rep, p_value = difftest_conf_national_votes_2022_test$p.value, n = nrow(difftest_conf_national_votes_2022_data))

difftest_conf_national_votes_2024_data <- cum_conf %>% filter(year == "2024", pid3 %in% c("Dem", "Rep"), !is.na(conf_national_votes_confident))
difftest_conf_national_votes_2024_design <- svydesign(ids = ~1, weights = ~weight_common, data = difftest_conf_national_votes_2024_data)
difftest_conf_national_votes_2024_test <- svyttest(conf_national_votes_confident ~ pid3, difftest_conf_national_votes_2024_design)
difftest_conf_national_votes_2024_row <- tibble(
  item = "Votes Nationwide", year = "2024",
  pct_dem = conf_trend_all_df %>% filter(item == "Votes Nationwide", year == "2024", series == "Dem") %>% pull(pct),
  pct_rep = conf_trend_all_df %>% filter(item == "Votes Nationwide", year == "2024", series == "Rep") %>% pull(pct),
  diff = pct_dem - pct_rep, p_value = difftest_conf_national_votes_2024_test$p.value, n = nrow(difftest_conf_national_votes_2024_data))

difftest_conf_national_votes_2026_data <- cum_conf %>% filter(year == "2026", pid3 %in% c("Dem", "Rep"), !is.na(conf_national_votes_confident))
difftest_conf_national_votes_2026_design <- svydesign(ids = ~1, weights = ~weight_common, data = difftest_conf_national_votes_2026_data)
difftest_conf_national_votes_2026_test <- svyttest(conf_national_votes_confident ~ pid3, difftest_conf_national_votes_2026_design)
difftest_conf_national_votes_2026_row <- tibble(
  item = "Votes Nationwide", year = "2026",
  pct_dem = conf_trend_all_df %>% filter(item == "Votes Nationwide", year == "2026", series == "Dem") %>% pull(pct),
  pct_rep = conf_trend_all_df %>% filter(item == "Votes Nationwide", year == "2026", series == "Rep") %>% pull(pct),
  diff = pct_dem - pct_rep, p_value = difftest_conf_national_votes_2026_test$p.value, n = nrow(difftest_conf_national_votes_2026_data))

conf_diff_test_df <- bind_rows(
  difftest_conf_own_vote_2022_row, difftest_conf_own_vote_2024_row, difftest_conf_own_vote_2026_row,
  difftest_conf_local_votes_2022_row, difftest_conf_local_votes_2024_row, difftest_conf_local_votes_2026_row,
  difftest_conf_state_votes_2022_row, difftest_conf_state_votes_2024_row, difftest_conf_state_votes_2026_row,
  difftest_conf_national_votes_2022_row, difftest_conf_national_votes_2024_row, difftest_conf_national_votes_2026_row
)

# Cited in the article text: the Democratic-Republican gap on the nationwide vote-counting item narrowed
# from 2022 to 2024.
conf_diff_test_df %>% filter(item == "Votes Nationwide", year %in% c("2022", "2024"))  # ~35 points -> ~30 points

# Cited in the article's footnotes: which 2026 gaps are statistically significant.
conf_diff_test_df %>% filter(year == "2026")  # own vote and county/city: p > .05; state and nationwide: p < .05 (but far smaller than in 2022 and 2024)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## 4. Concern about AI-based election disinformation, 2024 vs. 2026 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Both years are estimated from the pooled file (`cum`) so that the same weight (weight_common) is used
# for each; `data_concern` covers 2026 only.
ai_disinfo_2024_data <- cum %>% filter(year == "2024", !is.na(concern_ai_disinfo)) %>%
  mutate(concern_ai_disinfo_concerned = as.numeric(as.integer(concern_ai_disinfo) >= 3))
ai_disinfo_2024_design <- svydesign(ids = ~1, weights = ~weight_common, data = ai_disinfo_2024_data)
ai_disinfo_2024_est <- svyciprop(~concern_ai_disinfo_concerned, ai_disinfo_2024_design, method = "logit")
ai_disinfo_pct_2024 <- as.numeric(ai_disinfo_2024_est) * 100

ai_disinfo_2026_data <- cum %>% filter(year == "2026", !is.na(concern_ai_disinfo)) %>%
  mutate(concern_ai_disinfo_concerned = as.numeric(as.integer(concern_ai_disinfo) >= 3))
ai_disinfo_2026_design <- svydesign(ids = ~1, weights = ~weight_common, data = ai_disinfo_2026_data)
ai_disinfo_2026_est <- svyciprop(~concern_ai_disinfo_concerned, ai_disinfo_2026_design, method = "logit")
ai_disinfo_pct_2026 <- as.numeric(ai_disinfo_2026_est) * 100

ai_disinfo_pct_2024; ai_disinfo_pct_2026  # both ~77%




##### #
#### #
### ################################################################################################# #
# Appendix. Supplementary figures ------------------------------------------------------------------- #
### ################################################################################################# #
#### #
##### #

# Figures 1-4 are reproduced here with 95% confidence intervals, followed by additional versions of
# Figures 1, 2, and 4 and two new concern figures. None of these appear in the article.

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Appendix A. Figure 1 again, with confidence intervals ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(which_exclude_selected_df, file.path(plots.dir, "appendix_fig1_which_sources_selected_ci.csv"), row.names = FALSE)

p_which_sources_ci <- ggplot(which_exclude_selected_df, aes(y = display, x = pct)) +
  geom_col(fill = BAR_COLOR, width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_x_continuous(expand = expansion(mult = c(0, .1))) +
  scale_y_discrete(labels = scales::label_wrap(30)) +
  facet_wrap(~ need_label, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = str_wrap("Which Sources Voters Use to Find Election Information", width = 65),
       subtitle = "Among respondents who report seeking this information; with 95% confidence intervals") +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p_which_sources_ci


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Appendix B. Figure 1 again, showing the share ranking each source #1 (with confidence intervals) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

reg_ranked_design <- svydesign(ids = ~1, weights = ~weight, data = reg_exclude_long %>% mutate(is_first = coalesce(rank == 1, FALSE)))
reg_ranked_by_item <- svyby(~is_first, ~item_tag, reg_ranked_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(need = "Registering and Voting", item_tag = as.character(item_tag),
            pct = is_first * 100, ci_low = ci_l * 100, ci_high = ci_u * 100, n = reg_exclude_n)

run_ranked_design <- svydesign(ids = ~1, weights = ~weight, data = run_exclude_long %>% mutate(is_first = coalesce(rank == 1, FALSE)))
run_ranked_by_item <- svyby(~is_first, ~item_tag, run_ranked_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(need = "How Elections Are Run", item_tag = as.character(item_tag),
            pct = is_first * 100, ci_low = ci_l * 100, ci_high = ci_u * 100, n = run_exclude_n)

won_ranked_design <- svydesign(ids = ~1, weights = ~weight, data = won_exclude_long %>% mutate(is_first = coalesce(rank == 1, FALSE)))
won_ranked_by_item <- svyby(~is_first, ~item_tag, won_ranked_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(need = "Who Won an Election", item_tag = as.character(item_tag),
            pct = is_first * 100, ci_low = ci_l * 100, ci_high = ci_u * 100, n = won_exclude_n)

which_exclude_ranked_raw <- bind_rows(reg_ranked_by_item, run_ranked_by_item, won_ranked_by_item) %>%
  left_join(item_display_lookup, by = "item_tag") %>%
  filter(!item_tag %in% c("other", "dont_know"))

# Item order: ascending by mean ranked-#1 pct across the 3 needs (renders highest at the top)
which_exclude_ranked_means <- which_exclude_ranked_raw %>% group_by(display) %>% summarise(m = mean(pct), .groups = "drop") %>% arrange(m)
which_exclude_ranked_ordered <- which_exclude_ranked_raw %>% mutate(display = factor(display, levels = which_exclude_ranked_means$display))

which_exclude_ranked_df <- which_exclude_ranked_ordered %>%
  mutate(need = factor(need, levels = c("Registering and Voting", "How Elections Are Run", "Who Won an Election")),
         need_label = factor(paste0(need, "\n(n = ", format(which_exclude_n_by_need[as.character(need)], big.mark = ","), ")"),
                              levels = paste0(c("Registering and Voting", "How Elections Are Run", "Who Won an Election"),
                                              "\n(n = ", format(which_exclude_n_by_need[c("Registering and Voting", "How Elections Are Run", "Who Won an Election")], big.mark = ","), ")")))

write.csv(which_exclude_ranked_df, file.path(plots.dir, "appendix_fig1_which_sources_ranked.csv"), row.names = FALSE)

p_which_sources_ranked <- ggplot(which_exclude_ranked_df, aes(y = display, x = pct)) +
  geom_col(fill = RANK_COLOR, width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_x_continuous(expand = expansion(mult = c(0, .1))) +
  scale_y_discrete(labels = scales::label_wrap(30)) +
  facet_wrap(~ need_label, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = str_wrap("Which Source Voters Rank #1 for Finding Election Information", width = 65),
       subtitle = "Among respondents who report seeking this information; with 95% confidence intervals") +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p_which_sources_ranked



# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Appendix C. Figure 1 again, for never-seekers (with confidence intervals) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Respondents who say they never look for this type of information, instead of people who actually
# seek it out - both the "selected" and "ranked #1" framings.

reg_never_long <- sources.long %>% filter(battery %in% "reg_hyp", in_base)
run_never_long <- sources.long %>% filter(battery %in% "run_hyp", in_base)
won_never_long <- sources.long %>% filter(battery %in% "won_hyp", in_base)

reg_never_n <- n_distinct(reg_never_long$resp_id)
run_never_n <- n_distinct(run_never_long$resp_id)
won_never_n <- n_distinct(won_never_long$resp_id)

### Appendix C.1. "Selected" framing ----
reg_never_selected_design <- svydesign(ids = ~1, weights = ~weight, data = reg_never_long %>% mutate(is_selected = selected == "1"))
reg_never_selected_by_item <- svyby(~is_selected, ~item_tag, reg_never_selected_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(need = "Registering and Voting", item_tag = as.character(item_tag),
            pct = is_selected * 100, ci_low = ci_l * 100, ci_high = ci_u * 100, n = reg_never_n)

run_never_selected_design <- svydesign(ids = ~1, weights = ~weight, data = run_never_long %>% mutate(is_selected = selected == "1"))
run_never_selected_by_item <- svyby(~is_selected, ~item_tag, run_never_selected_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(need = "How Elections Are Run", item_tag = as.character(item_tag),
            pct = is_selected * 100, ci_low = ci_l * 100, ci_high = ci_u * 100, n = run_never_n)

won_never_selected_design <- svydesign(ids = ~1, weights = ~weight, data = won_never_long %>% mutate(is_selected = selected == "1"))
won_never_selected_by_item <- svyby(~is_selected, ~item_tag, won_never_selected_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(need = "Who Won an Election", item_tag = as.character(item_tag),
            pct = is_selected * 100, ci_low = ci_l * 100, ci_high = ci_u * 100, n = won_never_n)

which_never_selected_raw <- bind_rows(reg_never_selected_by_item, run_never_selected_by_item, won_never_selected_by_item) %>%
  left_join(item_display_lookup, by = "item_tag") %>%
  filter(!item_tag %in% c("other", "dont_know"))

which_never_selected_means <- which_never_selected_raw %>% group_by(display) %>% summarise(m = mean(pct), .groups = "drop") %>% arrange(m)
which_never_selected_ordered <- which_never_selected_raw %>% mutate(display = factor(display, levels = which_never_selected_means$display))

which_never_n_by_need <- which_never_selected_ordered %>% distinct(need, n) %>% deframe()
which_never_selected_df <- which_never_selected_ordered %>%
  mutate(need = factor(need, levels = c("Registering and Voting", "How Elections Are Run", "Who Won an Election")),
         need_label = factor(paste0(need, "\n(n = ", format(which_never_n_by_need[as.character(need)], big.mark = ","), ")"),
                              levels = paste0(c("Registering and Voting", "How Elections Are Run", "Who Won an Election"),
                                              "\n(n = ", format(which_never_n_by_need[c("Registering and Voting", "How Elections Are Run", "Who Won an Election")], big.mark = ","), ")")))

write.csv(which_never_selected_df, file.path(plots.dir, "appendix_fig1_which_sources_never_seekers.csv"), row.names = FALSE)

p_which_sources_never <- ggplot(which_never_selected_df, aes(y = display, x = pct)) +
  geom_col(fill = BAR_COLOR, width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_x_continuous(expand = expansion(mult = c(0, .1))) +
  scale_y_discrete(labels = scales::label_wrap(30)) +
  facet_wrap(~ need_label, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = str_wrap("Which Sources Voters Would Use to Find Election Information", width = 65),
       subtitle = str_wrap("Among respondents who never seek this information; with 95% confidence intervals", width = 58)) +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p_which_sources_never


### Appendix C.2. "Ranked #1" framing ----
reg_never_ranked_design <- svydesign(ids = ~1, weights = ~weight, data = reg_never_long %>% mutate(is_first = coalesce(rank == 1, FALSE)))
reg_never_ranked_by_item <- svyby(~is_first, ~item_tag, reg_never_ranked_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(need = "Registering and Voting", item_tag = as.character(item_tag),
            pct = is_first * 100, ci_low = ci_l * 100, ci_high = ci_u * 100, n = reg_never_n)

run_never_ranked_design <- svydesign(ids = ~1, weights = ~weight, data = run_never_long %>% mutate(is_first = coalesce(rank == 1, FALSE)))
run_never_ranked_by_item <- svyby(~is_first, ~item_tag, run_never_ranked_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(need = "How Elections Are Run", item_tag = as.character(item_tag),
            pct = is_first * 100, ci_low = ci_l * 100, ci_high = ci_u * 100, n = run_never_n)

won_never_ranked_design <- svydesign(ids = ~1, weights = ~weight, data = won_never_long %>% mutate(is_first = coalesce(rank == 1, FALSE)))
won_never_ranked_by_item <- svyby(~is_first, ~item_tag, won_never_ranked_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>%
  transmute(need = "Who Won an Election", item_tag = as.character(item_tag),
            pct = is_first * 100, ci_low = ci_l * 100, ci_high = ci_u * 100, n = won_never_n)

which_never_ranked_raw <- bind_rows(reg_never_ranked_by_item, run_never_ranked_by_item, won_never_ranked_by_item) %>%
  left_join(item_display_lookup, by = "item_tag") %>%
  filter(!item_tag %in% c("other", "dont_know"))

which_never_ranked_means <- which_never_ranked_raw %>% group_by(display) %>% summarise(m = mean(pct), .groups = "drop") %>% arrange(m)
which_never_ranked_df <- which_never_ranked_raw %>%
  mutate(display = factor(display, levels = which_never_ranked_means$display),
         need = factor(need, levels = c("Registering and Voting", "How Elections Are Run", "Who Won an Election")),
         need_label = factor(paste0(need, "\n(n = ", format(which_never_n_by_need[as.character(need)], big.mark = ","), ")"),
                              levels = paste0(c("Registering and Voting", "How Elections Are Run", "Who Won an Election"),
                                              "\n(n = ", format(which_never_n_by_need[c("Registering and Voting", "How Elections Are Run", "Who Won an Election")], big.mark = ","), ")")))

write.csv(which_never_ranked_df, file.path(plots.dir, "appendix_fig1_which_sources_never_seekers_ranked.csv"), row.names = FALSE)

p_which_sources_never_ranked <- ggplot(which_never_ranked_df, aes(y = display, x = pct)) +
  geom_col(fill = RANK_COLOR, width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_x_continuous(expand = expansion(mult = c(0, .1))) +
  scale_y_discrete(labels = scales::label_wrap(30)) +
  facet_wrap(~ need_label, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = str_wrap("Which Source Voters Would Rank #1 for Finding Election Information", width = 65),
       subtitle = str_wrap("Among respondents who never seek this information; with 95% confidence intervals", width = 58)) +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p_which_sources_never_ranked



# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Appendix D. Figure 2 again, with confidence intervals ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(mode_exclude_combined_df, file.path(plots.dir, "appendix_fig2_contact_mode_ci.csv"), row.names = FALSE)

p_contact_mode_ci <- ggplot(mode_exclude_combined_df, aes(y = category, x = pct, fill = category)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_fill_manual(values = mode_exclude_colors, name = NULL, labels = scales::label_wrap(40)) +
  scale_x_continuous(expand = expansion(mult = c(0, .125))) +
  scale_y_discrete(labels = scales::label_wrap(30)) +
  facet_wrap(~ need_label, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = str_wrap("How Voters Contact Local or State Election Officials", width = 65),
       subtitle = "Among respondents who seek this information, by information need; with 95% confidence intervals") +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p_contact_mode_ci


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Appendix E. Figure 2 again, for never-seekers (with confidence intervals) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

mode_never_reg_data <- data %>% filter(!is.na(reg_hyp_official_mode))
mode_never_reg_design <- svydesign(ids = ~1, weights = ~weight, data = mode_never_reg_data)
mode_never_reg_est <- svymean(~reg_hyp_official_mode, mode_never_reg_design)
mode_never_reg_df <- tibble(category = sub("^reg_hyp_official_mode", "", names(mode_never_reg_est)),
                            pct = as.numeric(mode_never_reg_est) * 100,
                            ci_low = confint(mode_never_reg_est)[, 1] * 100,
                            ci_high = confint(mode_never_reg_est)[, 2] * 100,
                            n = nrow(mode_never_reg_data)) %>%
  mutate(need = "Registering and Voting", .before = 1)

mode_never_run_data <- data %>% filter(!is.na(run_hyp_official_mode))
mode_never_run_design <- svydesign(ids = ~1, weights = ~weight, data = mode_never_run_data)
mode_never_run_est <- svymean(~run_hyp_official_mode, mode_never_run_design)
mode_never_run_df <- tibble(category = sub("^run_hyp_official_mode", "", names(mode_never_run_est)),
                            pct = as.numeric(mode_never_run_est) * 100,
                            ci_low = confint(mode_never_run_est)[, 1] * 100,
                            ci_high = confint(mode_never_run_est)[, 2] * 100,
                            n = nrow(mode_never_run_data)) %>%
  mutate(need = "How Elections Are Run", .before = 1)

mode_never_won_data <- data %>% filter(!is.na(won_hyp_official_mode))
mode_never_won_design <- svydesign(ids = ~1, weights = ~weight, data = mode_never_won_data)
mode_never_won_est <- svymean(~won_hyp_official_mode, mode_never_won_design)
mode_never_won_df <- tibble(category = sub("^won_hyp_official_mode", "", names(mode_never_won_est)),
                            pct = as.numeric(mode_never_won_est) * 100,
                            ci_low = confint(mode_never_won_est)[, 1] * 100,
                            ci_high = confint(mode_never_won_est)[, 2] * 100,
                            n = nrow(mode_never_won_data)) %>%
  mutate(need = "Who Won an Election", .before = 1)

mode_never_raw <- bind_rows(mode_never_reg_df, mode_never_run_df, mode_never_won_df) %>%
  filter(!category %in% c("Other, please specify", "Don't know")) %>%
  mutate(need = fct_relevel(need, "Registering and Voting", "How Elections Are Run", "Who Won an Election"),
         category = case_when(!grepl("website", category) ~ gsub(" \\(town, city, county\\)", "", category),
                              TRUE ~ category),
         category = as.factor(category))

# Same top-to-bottom order as Figure 2 (website / phone / in person) - "in person" listed first below
# because ggplot draws a discrete y-axis's first level at the bottom.
mode_never_levels <- setdiff(levels(mode_never_raw$category), c("Other, please specify", "Don't know"))
mode_never_colors <- setNames(colorRampPalette(c("#b7d3f6", "#0d366b"))(length(mode_never_levels)), mode_never_levels)
mode_never_releveled <- mode_never_raw %>%
  mutate(category = fct_relevel(category,
                                "I would visit my local or state election office in person",
                                "I would call my local or state election office on the phone",
                                "I would visit my local (town, city, county) or state election office's website"))

mode_never_n_by_need <- mode_never_releveled %>% distinct(need, n) %>% deframe()
mode_never_combined_df <- mode_never_releveled %>%
  mutate(need_label = factor(paste0(need, "\n(n = ", trimws(format(mode_never_n_by_need[as.character(need)], big.mark = ",")), ")"),
                              levels = paste0(c("Registering and Voting", "How Elections Are Run", "Who Won an Election"),
                                              "\n(n = ", trimws(format(mode_never_n_by_need[c("Registering and Voting", "How Elections Are Run", "Who Won an Election")], big.mark = ",")), ")")))

write.csv(mode_never_combined_df, file.path(plots.dir, "appendix_fig2_contact_mode_never_seekers.csv"), row.names = FALSE)

p_contact_mode_never <- ggplot(mode_never_combined_df, aes(y = category, x = pct, fill = category)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.2, linewidth = 0.8, color = "grey35", alpha = 0.6) +
  scale_fill_manual(values = mode_never_colors, name = NULL, labels = scales::label_wrap(40)) +
  scale_x_continuous(expand = expansion(mult = c(0, .125))) +
  scale_y_discrete(labels = scales::label_wrap(30)) +
  facet_wrap(~ need_label, ncol = 3) +
  labs(x = "Weighted %", y = NULL,
       title = str_wrap("How Voters Would Contact Local or State Election Officials", width = 65),
       subtitle = str_wrap("Among respondents who never seek this information, by information need; with 95% confidence intervals", width = 58)) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
p_contact_mode_never



# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Appendix F. Figure 3 again, with confidence intervals ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(conf_trend_all_df, file.path(plots.dir, "appendix_fig3_confidence_trend_ci.csv"), row.names = FALSE)

p_confidence_ci <- ggplot(conf_trend_all_df, aes(x = year, y = pct, color = series, group = series)) +
  geom_line(aes(linetype = series), linewidth = 0.8, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = 0.3), width = 0.15, alpha = 0.6) +
  geom_point(size = 2.6, position = position_dodge(width = 0.3)) +
  facet_wrap(~ item) +
  scale_color_manual(values = SERIES_COLORS, name = NULL) +
  scale_linetype_manual(values = SERIES_LINETYPES, name = NULL) +
  scale_y_continuous(limits = c(40, 100), expand = expansion(mult = c(0.04, .08))) +
  labs(x = NULL, y = "Weighted % Confident",
       title = str_wrap("Confidence Votes Will Be Counted as Intended, by Party ID and Overall, 2022-2026", width = 65),
       subtitle = str_wrap("Showing percentage of those \"very confident\" and \"somewhat confident\"; Independents included only in \"All respondents\"; with 95% confidence intervals", width = 100)) +
  theme(legend.position = "top", panel.grid.major.x = element_blank())
p_confidence_ci


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Appendix G. Figure 4 again, with confidence intervals ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(concern_by_party_plot_df, file.path(plots.dir, "appendix_fig4_concern_by_party_ci.csv"), row.names = FALSE)

p_concern_ci <- ggplot(concern_by_party_plot_df, aes(y = item)) +
  geom_line(aes(x = pct, group = item, color = higher_party), linewidth = 1) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high, color = pid3), position = position_dodge(width = 0.5), width = 0.3, alpha = 0.6) +
  geom_point(aes(x = pct, color = pid3), position = position_dodge(width = 0.5), size = 3.4) +
  scale_color_manual(values = PID_COLORS, name = NULL) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0.02, .08))) +
  scale_y_discrete(labels = scales::label_wrap(40)) +
  labs(x = "Weighted % Concerned", y = NULL,
       title = "Concerns about the midterm election, by party ID",
       subtitle = "With 95% confidence intervals") +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
p_concern_ci



# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Appendix H. Concern about each midterm-election issue, 2024 vs. 2026, all respondents ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Add "concerned" indicators (very or somewhat concerned) for the 10 concern items asked in both 2024
# and 2026, on a copy of `cum`.
concern_items_comparable <- c("concern_misinfo", "concern_ai_disinfo", "concern_foreign", "concern_ineligible_votes",
                              "concern_overturn", "concern_biased_count", "concern_mail_ballots",
                              "concern_guns_intimidation", "concern_post_violence", "concern_polling_problems")
cum_concern <- cum %>% mutate(across(all_of(concern_items_comparable), ~ as.numeric(as.integer(.x) >= 3), .names = "{.col}_concerned"))

# One svyby() call per item, grouped by year (2024 and 2026 only; the 2022 survey did not include the
# concern questions, so every 2022 row is missing on these columns).
concern_misinfo_data <- cum_concern %>% filter(year %in% c("2024", "2026"), !is.na(concern_misinfo_concerned))
concern_misinfo_design <- svydesign(ids = ~1, weights = ~weight_common, data = concern_misinfo_data)
concern_misinfo_trend <- svyby(~concern_misinfo_concerned, ~year, concern_misinfo_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Inaccurate or Misleading Election Information", year = as.character(year),
                             pct = concern_misinfo_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

concern_ai_disinfo_data <- cum_concern %>% filter(year %in% c("2024", "2026"), !is.na(concern_ai_disinfo_concerned))
concern_ai_disinfo_design <- svydesign(ids = ~1, weights = ~weight_common, data = concern_ai_disinfo_data)
concern_ai_disinfo_trend <- svyby(~concern_ai_disinfo_concerned, ~year, concern_ai_disinfo_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "AI Used to Spread Disinformation", year = as.character(year),
                             pct = concern_ai_disinfo_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

concern_foreign_data <- cum_concern %>% filter(year %in% c("2024", "2026"), !is.na(concern_foreign_concerned))
concern_foreign_design <- svydesign(ids = ~1, weights = ~weight_common, data = concern_foreign_data)
concern_foreign_trend <- svyby(~concern_foreign_concerned, ~year, concern_foreign_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Foreign Interference", year = as.character(year),
                             pct = concern_foreign_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

concern_ineligible_votes_data <- cum_concern %>% filter(year %in% c("2024", "2026"), !is.na(concern_ineligible_votes_concerned))
concern_ineligible_votes_design <- svydesign(ids = ~1, weights = ~weight_common, data = concern_ineligible_votes_data)
concern_ineligible_votes_trend <- svyby(~concern_ineligible_votes_concerned, ~year, concern_ineligible_votes_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Ineligible Votes Being Counted", year = as.character(year),
                             pct = concern_ineligible_votes_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

concern_overturn_data <- cum_concern %>% filter(year %in% c("2024", "2026"), !is.na(concern_overturn_concerned))
concern_overturn_design <- svydesign(ids = ~1, weights = ~weight_common, data = concern_overturn_data)
concern_overturn_trend <- svyby(~concern_overturn_concerned, ~year, concern_overturn_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Attempts to Overturn a Fair Election's Results", year = as.character(year),
                             pct = concern_overturn_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

concern_biased_count_data <- cum_concern %>% filter(year %in% c("2024", "2026"), !is.na(concern_biased_count_concerned))
concern_biased_count_design <- svydesign(ids = ~1, weights = ~weight_common, data = concern_biased_count_data)
concern_biased_count_trend <- svyby(~concern_biased_count_concerned, ~year, concern_biased_count_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Biased or Inaccurate Ballot Counting", year = as.character(year),
                             pct = concern_biased_count_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

concern_mail_ballots_data <- cum_concern %>% filter(year %in% c("2024", "2026"), !is.na(concern_mail_ballots_concerned))
concern_mail_ballots_design <- svydesign(ids = ~1, weights = ~weight_common, data = concern_mail_ballots_data)
concern_mail_ballots_trend <- svyby(~concern_mail_ballots_concerned, ~year, concern_mail_ballots_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Illegal or Improper Mail Ballot/Drop Box Use", year = as.character(year),
                             pct = concern_mail_ballots_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

concern_guns_intimidation_data <- cum_concern %>% filter(year %in% c("2024", "2026"), !is.na(concern_guns_intimidation_concerned))
concern_guns_intimidation_design <- svydesign(ids = ~1, weights = ~weight_common, data = concern_guns_intimidation_data)
concern_guns_intimidation_trend <- svyby(~concern_guns_intimidation_concerned, ~year, concern_guns_intimidation_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Guns, Violence, or Intimidation at Voting Locations", year = as.character(year),
                             pct = concern_guns_intimidation_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

concern_post_violence_data <- cum_concern %>% filter(year %in% c("2024", "2026"), !is.na(concern_post_violence_concerned))
concern_post_violence_design <- svydesign(ids = ~1, weights = ~weight_common, data = concern_post_violence_data)
concern_post_violence_trend <- svyby(~concern_post_violence_concerned, ~year, concern_post_violence_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Violence or Unrest After Election Day", year = as.character(year),
                             pct = concern_post_violence_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

concern_polling_problems_data <- cum_concern %>% filter(year %in% c("2024", "2026"), !is.na(concern_polling_problems_concerned))
concern_polling_problems_design <- svydesign(ids = ~1, weights = ~weight_common, data = concern_polling_problems_data)
concern_polling_problems_trend <- svyby(~concern_polling_problems_concerned, ~year, concern_polling_problems_design, svyciprop, method = "logit", vartype = "ci") %>%
  as_tibble() %>% transmute(item = "Long Lines or Equipment Problems at Polls", year = as.character(year),
                             pct = concern_polling_problems_concerned * 100, ci_low = ci_l * 100, ci_high = ci_u * 100)

# The 4 items new in 2026 have no 2024 counterpart, so each gets a single 2026 estimate. These use the
# 2026 weight (`weight`) rather than weight_common because no cross-year comparison is involved, and
# they reuse the indicator columns built on `data_concern` for Figure 4.
concern_ice_deployment_2026_data <- data_concern %>% filter(!is.na(concern_ice_deployment_concerned))
concern_ice_deployment_2026_design <- svydesign(ids = ~1, weights = ~weight, data = concern_ice_deployment_2026_data)
concern_ice_deployment_2026_est <- svyciprop(~concern_ice_deployment_concerned, concern_ice_deployment_2026_design, method = "logit")
concern_ice_deployment_2026_row <- tibble(item = "ICE/Federal Law Enforcement in Your Community", year = "2026",
                 pct = as.numeric(concern_ice_deployment_2026_est) * 100,
                 ci_low = confint(concern_ice_deployment_2026_est)[1] * 100, ci_high = confint(concern_ice_deployment_2026_est)[2] * 100)

concern_ballot_seizure_2026_data <- data_concern %>% filter(!is.na(concern_ballot_seizure_concerned))
concern_ballot_seizure_2026_design <- svydesign(ids = ~1, weights = ~weight, data = concern_ballot_seizure_2026_data)
concern_ballot_seizure_2026_est <- svyciprop(~concern_ballot_seizure_concerned, concern_ballot_seizure_2026_design, method = "logit")
concern_ballot_seizure_2026_row <- tibble(item = "Federal/State Seizure of Ballots or Voting Machines", year = "2026",
                 pct = as.numeric(concern_ballot_seizure_2026_est) * 100,
                 ci_low = confint(concern_ballot_seizure_2026_est)[1] * 100, ci_high = confint(concern_ballot_seizure_2026_est)[2] * 100)

concern_eligible_blocked_2026_data <- data_concern %>% filter(!is.na(concern_eligible_blocked_concerned))
concern_eligible_blocked_2026_design <- svydesign(ids = ~1, weights = ~weight, data = concern_eligible_blocked_2026_data)
concern_eligible_blocked_2026_est <- svyciprop(~concern_eligible_blocked_concerned, concern_eligible_blocked_2026_design, method = "logit")
concern_eligible_blocked_2026_row <- tibble(item = "Eligible Voters Blocked From Voting", year = "2026",
                 pct = as.numeric(concern_eligible_blocked_2026_est) * 100,
                 ci_low = confint(concern_eligible_blocked_2026_est)[1] * 100, ci_high = confint(concern_eligible_blocked_2026_est)[2] * 100)

concern_gerrymander_2026_data <- data_concern %>% filter(!is.na(concern_gerrymander_concerned))
concern_gerrymander_2026_design <- svydesign(ids = ~1, weights = ~weight, data = concern_gerrymander_2026_data)
concern_gerrymander_2026_est <- svyciprop(~concern_gerrymander_concerned, concern_gerrymander_2026_design, method = "logit")
concern_gerrymander_2026_row <- tibble(item = "Unfair District Lines Distorting Outcomes", year = "2026",
                 pct = as.numeric(concern_gerrymander_2026_est) * 100,
                 ci_low = confint(concern_gerrymander_2026_est)[1] * 100, ci_high = confint(concern_gerrymander_2026_est)[2] * 100)

concern_trend_raw <- bind_rows(
  concern_misinfo_trend, concern_ai_disinfo_trend, concern_foreign_trend, concern_ineligible_votes_trend,
  concern_overturn_trend, concern_biased_count_trend, concern_mail_ballots_trend, concern_guns_intimidation_trend,
  concern_post_violence_trend, concern_polling_problems_trend,
  concern_ice_deployment_2026_row, concern_ballot_seizure_2026_row, concern_eligible_blocked_2026_row, concern_gerrymander_2026_row
)

# Item order: ascending by each item's 2026 value
concern_trend_order <- concern_trend_raw %>% filter(year == "2026") %>% arrange(pct) %>% pull(item)
concern_trend_df <- concern_trend_raw %>%
  mutate(item = factor(item, levels = concern_trend_order),
         year = factor(year, levels = c("2024", "2026")))

write.csv(concern_trend_df, file.path(plots.dir, "appendix_concern_trend_2024_2026.csv"), row.names = FALSE)

p_concern_trend <- ggplot(concern_trend_df, aes(x = pct, y = item)) +
  geom_line(aes(group = item), color = "grey75", linewidth = 0.6) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high, color = year), width = 0.2, alpha = 0.5) +
  geom_point(data = filter(concern_trend_df, year == "2024"), aes(color = year), size = 4.2, shape = 1, stroke = 1.4) +
  geom_point(data = filter(concern_trend_df, year == "2026"), aes(color = year), size = 3, shape = 16) +
  scale_color_manual(values = YEAR_COLORS, name = NULL) +
  scale_y_discrete(labels = scales::label_wrap(40)) +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(30, 85)) +
  labs(x = "Weighted % Concerned", y = NULL,
       title = str_wrap("Concern About Election Problems, 2024 vs. 2026", width = 65),
       subtitle = str_wrap("\"Concerned\" combines somewhat + very concerned. 4 items new to 2026 shown as single 2026-only points. With 95% confidence intervals.", width = 90)) +
  theme(legend.position = "top")
p_concern_trend



# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Appendix I. Full response distribution for each concern item, by party ID ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Figure 4 collapses each item's four response levels into a single "concerned" percentage (very or
# somewhat concerned). This figure instead shows the full distribution across all four levels as a
# diverging stacked bar: the two "not concerned" levels extend left of zero and the two "concerned"
# levels extend right, with the milder level of each pair closest to zero. Parties are shown in
# separate panels because side-by-side stacks within one bar are hard to read.
#
# One svyby() call per item, grouped by party, using svymean() to return one proportion per response
# level. svyby() returns each combination of statistic (the estimate, "ci_l", and "ci_u") and response
# level as its own wide column (e.g., "concern_misinfoVery concerned"); the pivot_longer(), mutate(),
# and pivot_wider() steps reshape that into one row per party and response level, with pct, ci_low,
# and ci_high columns.
CONCERN_COLORS <- c("Not concerned at all" = "#b7d3f6", "Not too concerned" = "#2a78d6",
                    "Somewhat concerned" = "#f4a582", "Very concerned" = "#d03b3b")

concern_misinfo_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_misinfo))
concern_misinfo_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_misinfo_dist_data)
concern_misinfo_dist_by_party <- svyby(~concern_misinfo, ~pid3, concern_misinfo_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_misinfo")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_misinfo_dist_row <- tibble(item = "Inaccurate or Misleading Election Information", pid3 = as.character(concern_misinfo_dist_by_party$pid3), category = concern_misinfo_dist_by_party$category,
                pct = concern_misinfo_dist_by_party$pct * 100, ci_low = concern_misinfo_dist_by_party$ci_low * 100, ci_high = concern_misinfo_dist_by_party$ci_high * 100)

concern_ai_disinfo_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_ai_disinfo))
concern_ai_disinfo_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_ai_disinfo_dist_data)
concern_ai_disinfo_dist_by_party <- svyby(~concern_ai_disinfo, ~pid3, concern_ai_disinfo_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_ai_disinfo")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_ai_disinfo_dist_row <- tibble(item = "AI Used to Spread Disinformation", pid3 = as.character(concern_ai_disinfo_dist_by_party$pid3), category = concern_ai_disinfo_dist_by_party$category,
                pct = concern_ai_disinfo_dist_by_party$pct * 100, ci_low = concern_ai_disinfo_dist_by_party$ci_low * 100, ci_high = concern_ai_disinfo_dist_by_party$ci_high * 100)

concern_foreign_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_foreign))
concern_foreign_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_foreign_dist_data)
concern_foreign_dist_by_party <- svyby(~concern_foreign, ~pid3, concern_foreign_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_foreign")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_foreign_dist_row <- tibble(item = "Foreign Interference", pid3 = as.character(concern_foreign_dist_by_party$pid3), category = concern_foreign_dist_by_party$category,
                pct = concern_foreign_dist_by_party$pct * 100, ci_low = concern_foreign_dist_by_party$ci_low * 100, ci_high = concern_foreign_dist_by_party$ci_high * 100)

concern_ineligible_votes_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_ineligible_votes))
concern_ineligible_votes_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_ineligible_votes_dist_data)
concern_ineligible_votes_dist_by_party <- svyby(~concern_ineligible_votes, ~pid3, concern_ineligible_votes_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_ineligible_votes")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_ineligible_votes_dist_row <- tibble(item = "Ineligible Votes Being Counted", pid3 = as.character(concern_ineligible_votes_dist_by_party$pid3), category = concern_ineligible_votes_dist_by_party$category,
                pct = concern_ineligible_votes_dist_by_party$pct * 100, ci_low = concern_ineligible_votes_dist_by_party$ci_low * 100, ci_high = concern_ineligible_votes_dist_by_party$ci_high * 100)

concern_overturn_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_overturn))
concern_overturn_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_overturn_dist_data)
concern_overturn_dist_by_party <- svyby(~concern_overturn, ~pid3, concern_overturn_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_overturn")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_overturn_dist_row <- tibble(item = "Attempts to Overturn a Fair Election's Results", pid3 = as.character(concern_overturn_dist_by_party$pid3), category = concern_overturn_dist_by_party$category,
                pct = concern_overturn_dist_by_party$pct * 100, ci_low = concern_overturn_dist_by_party$ci_low * 100, ci_high = concern_overturn_dist_by_party$ci_high * 100)

concern_biased_count_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_biased_count))
concern_biased_count_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_biased_count_dist_data)
concern_biased_count_dist_by_party <- svyby(~concern_biased_count, ~pid3, concern_biased_count_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_biased_count")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_biased_count_dist_row <- tibble(item = "Biased or Inaccurate Ballot Counting", pid3 = as.character(concern_biased_count_dist_by_party$pid3), category = concern_biased_count_dist_by_party$category,
                pct = concern_biased_count_dist_by_party$pct * 100, ci_low = concern_biased_count_dist_by_party$ci_low * 100, ci_high = concern_biased_count_dist_by_party$ci_high * 100)

concern_mail_ballots_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_mail_ballots))
concern_mail_ballots_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_mail_ballots_dist_data)
concern_mail_ballots_dist_by_party <- svyby(~concern_mail_ballots, ~pid3, concern_mail_ballots_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_mail_ballots")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_mail_ballots_dist_row <- tibble(item = "Illegal or Improper Mail Ballot/Drop Box Use", pid3 = as.character(concern_mail_ballots_dist_by_party$pid3), category = concern_mail_ballots_dist_by_party$category,
                pct = concern_mail_ballots_dist_by_party$pct * 100, ci_low = concern_mail_ballots_dist_by_party$ci_low * 100, ci_high = concern_mail_ballots_dist_by_party$ci_high * 100)

concern_guns_intimidation_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_guns_intimidation))
concern_guns_intimidation_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_guns_intimidation_dist_data)
concern_guns_intimidation_dist_by_party <- svyby(~concern_guns_intimidation, ~pid3, concern_guns_intimidation_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_guns_intimidation")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_guns_intimidation_dist_row <- tibble(item = "Guns, Violence, or Intimidation at Voting Locations", pid3 = as.character(concern_guns_intimidation_dist_by_party$pid3), category = concern_guns_intimidation_dist_by_party$category,
                pct = concern_guns_intimidation_dist_by_party$pct * 100, ci_low = concern_guns_intimidation_dist_by_party$ci_low * 100, ci_high = concern_guns_intimidation_dist_by_party$ci_high * 100)

concern_post_violence_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_post_violence))
concern_post_violence_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_post_violence_dist_data)
concern_post_violence_dist_by_party <- svyby(~concern_post_violence, ~pid3, concern_post_violence_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_post_violence")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_post_violence_dist_row <- tibble(item = "Violence or Unrest After Election Day", pid3 = as.character(concern_post_violence_dist_by_party$pid3), category = concern_post_violence_dist_by_party$category,
                pct = concern_post_violence_dist_by_party$pct * 100, ci_low = concern_post_violence_dist_by_party$ci_low * 100, ci_high = concern_post_violence_dist_by_party$ci_high * 100)

concern_polling_problems_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_polling_problems))
concern_polling_problems_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_polling_problems_dist_data)
concern_polling_problems_dist_by_party <- svyby(~concern_polling_problems, ~pid3, concern_polling_problems_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_polling_problems")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_polling_problems_dist_row <- tibble(item = "Long Lines or Equipment Problems at Polls", pid3 = as.character(concern_polling_problems_dist_by_party$pid3), category = concern_polling_problems_dist_by_party$category,
                pct = concern_polling_problems_dist_by_party$pct * 100, ci_low = concern_polling_problems_dist_by_party$ci_low * 100, ci_high = concern_polling_problems_dist_by_party$ci_high * 100)

concern_ice_deployment_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_ice_deployment))
concern_ice_deployment_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_ice_deployment_dist_data)
concern_ice_deployment_dist_by_party <- svyby(~concern_ice_deployment, ~pid3, concern_ice_deployment_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_ice_deployment")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_ice_deployment_dist_row <- tibble(item = "ICE/Federal Law Enforcement in Your Community", pid3 = as.character(concern_ice_deployment_dist_by_party$pid3), category = concern_ice_deployment_dist_by_party$category,
                pct = concern_ice_deployment_dist_by_party$pct * 100, ci_low = concern_ice_deployment_dist_by_party$ci_low * 100, ci_high = concern_ice_deployment_dist_by_party$ci_high * 100)

concern_ballot_seizure_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_ballot_seizure))
concern_ballot_seizure_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_ballot_seizure_dist_data)
concern_ballot_seizure_dist_by_party <- svyby(~concern_ballot_seizure, ~pid3, concern_ballot_seizure_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_ballot_seizure")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_ballot_seizure_dist_row <- tibble(item = "Federal/State Seizure of Ballots or Voting Machines", pid3 = as.character(concern_ballot_seizure_dist_by_party$pid3), category = concern_ballot_seizure_dist_by_party$category,
                pct = concern_ballot_seizure_dist_by_party$pct * 100, ci_low = concern_ballot_seizure_dist_by_party$ci_low * 100, ci_high = concern_ballot_seizure_dist_by_party$ci_high * 100)

concern_eligible_blocked_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_eligible_blocked))
concern_eligible_blocked_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_eligible_blocked_dist_data)
concern_eligible_blocked_dist_by_party <- svyby(~concern_eligible_blocked, ~pid3, concern_eligible_blocked_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_eligible_blocked")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_eligible_blocked_dist_row <- tibble(item = "Eligible Voters Blocked From Voting", pid3 = as.character(concern_eligible_blocked_dist_by_party$pid3), category = concern_eligible_blocked_dist_by_party$category,
                pct = concern_eligible_blocked_dist_by_party$pct * 100, ci_low = concern_eligible_blocked_dist_by_party$ci_low * 100, ci_high = concern_eligible_blocked_dist_by_party$ci_high * 100)

concern_gerrymander_dist_data <- data_concern %>% filter(pid3 %in% c("Dem", "Rep"), !is.na(concern_gerrymander))
concern_gerrymander_dist_design <- svydesign(ids = ~1, weights = ~weight, data = concern_gerrymander_dist_data)
concern_gerrymander_dist_by_party <- svyby(~concern_gerrymander, ~pid3, concern_gerrymander_dist_design, svymean, vartype = "ci") %>%
  as_tibble() %>%
  pivot_longer(-pid3, names_to = "colname", values_to = "value") %>%
  mutate(stat = case_when(str_starts(colname, "ci_l.") ~ "ci_low",
                         str_starts(colname, "ci_u.") ~ "ci_high",
                         TRUE ~ "pct"),
         category = str_remove(colname, "^(ci_l\\.|ci_u\\.)?concern_gerrymander")) %>%
  select(pid3, category, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
concern_gerrymander_dist_row <- tibble(item = "Unfair District Lines Distorting Outcomes", pid3 = as.character(concern_gerrymander_dist_by_party$pid3), category = concern_gerrymander_dist_by_party$category,
                pct = concern_gerrymander_dist_by_party$pct * 100, ci_low = concern_gerrymander_dist_by_party$ci_low * 100, ci_high = concern_gerrymander_dist_by_party$ci_high * 100)

concern_dist_raw <- bind_rows(
  concern_misinfo_dist_row, concern_ai_disinfo_dist_row, concern_foreign_dist_row,
  concern_ineligible_votes_dist_row, concern_overturn_dist_row, concern_biased_count_dist_row,
  concern_mail_ballots_dist_row, concern_guns_intimidation_dist_row, concern_post_violence_dist_row,
  concern_polling_problems_dist_row, concern_ice_deployment_dist_row, concern_ballot_seizure_dist_row,
  concern_eligible_blocked_dist_row, concern_gerrymander_dist_row
)

# Reuse Figure 4's item order (concern_item_means, based on the mean percentage concerned across
# parties) so the two figures list items in the same sequence.
concern_dist_df <- concern_dist_raw %>%
  mutate(item = factor(item, levels = concern_item_means$item),
         category = factor(category, levels = c("Not concerned at all", "Not too concerned", "Somewhat concerned", "Very concerned")),
         pid3 = factor(pid3, levels = c("Dem", "Rep")),
         signed_pct = if_else(category %in% c("Not too concerned", "Not concerned at all"), -pct, pct))

# Split into the "not concerned" and "concerned" halves so that each side's stacking order (milder
# level nearest zero, more extreme level outermost) can be set independently; position_stack() applies
# one order across the whole range, not a different one on each side of zero.
concern_dist_neg <- concern_dist_df %>% filter(category %in% c("Not concerned at all", "Not too concerned"))
concern_dist_pos <- concern_dist_df %>% filter(category %in% c("Somewhat concerned", "Very concerned"))

write.csv(concern_dist_df, file.path(plots.dir, "appendix_concern_distribution_by_party.csv"), row.names = FALSE)

p_concern_dist <- ggplot(concern_dist_df, aes(y = item, fill = category)) +
  geom_col(data = concern_dist_neg, aes(x = signed_pct), position = "stack", width = 0.7) +
  geom_col(data = concern_dist_pos, aes(x = signed_pct), position = position_stack(reverse = TRUE), width = 0.7) +
  geom_vline(xintercept = 0, color = "grey40", linewidth = 0.4) +
  facet_wrap(~ pid3) +
  scale_fill_manual(values = CONCERN_COLORS, name = NULL, breaks = names(CONCERN_COLORS)) +
  scale_x_continuous(labels = function(x) paste0(abs(x), "%")) +
  scale_y_discrete(labels = scales::label_wrap(40)) +
  labs(x = "Weighted %", y = NULL,
       title = str_wrap("Concern About the Midterm Election, Full Response Distribution, by Party ID", width = 65),
       subtitle = "Diverging stacked bars: share \"not concerned\" (left) vs. \"concerned\" (right) at each level") +
  theme(legend.position = "top", panel.grid.major.y = element_blank(), strip.text = element_text(face = "bold"))
p_concern_dist
