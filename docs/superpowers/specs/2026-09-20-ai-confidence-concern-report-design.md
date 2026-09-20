# EIS 2026 AI, Confidence, and Concern Report — Design

Date: 2026-09-20
Status: Approved by Jack, proceeding to implementation plan.

## 1. Purpose

Visualize 2026's weighted results for every question from the AI section through the end of the questionnaire (BPC35-BPC48): AI use and attitudes, voting experience, confidence votes will be counted as intended, concern about election problems (broken out by party ID), and the trailing noncitizen-voting / access-vs-integrity / USPS policy items. Where a 2024 counterpart exists, add the comparison; where none exists, show 2026 alone. Delivered as a plain R script (not an Rmd) meant to be run interactively, printing each chart to the graphics device in sequence — no files written, no knitting.

Audience: internal, exploratory — same as every other script/report in this project.

## 2. Data sources

All paths relative to `2026/` (the `.Rproj` working directory).

| File | Used for |
|---|---|
| `output/eis_2026_clean.rds` → `data` | Every 2026-current-state and by-party chart. Weight: `weight`. |
| `output/eis_cumulative.rds` → `cum` | Only the four ported trend charts (§4). Weight: `weight_common`. |

No `var.index` is loaded. Every item in this report is a single ordinal/nominal question, not a multiselect battery, so display labels are hand-typed — matching `eis_2026_05_trend_comparison_report.Rmd`'s own convention for these same kinds of items (`ai_labels`, `conf_labels`, `concern_labels`, `seek_labels`, all hand-typed there) rather than looked up from the codebook.

Confirmed directly against the data before writing this spec: `ai_prevalence`, `ai_tool_freq`, `ai_detect_conf` (BPC35-37), `concern_ice_deployment`, `concern_ballot_seizure`, `concern_eligible_blocked`, `concern_gerrymander` (the four concern items new to 2026), and `noncitizen_freq`, `noncitizen_alters`, `access_vs_integrity`, `usps_policy_support` (BPC45-48) do **not** exist in `eis_cumulative.rds`. I also checked `2024/raw/field1/question_codebook.csv` directly (the same per-column codebook `eis_2026_04_cumulative_data.R` already cross-references) and confirmed 2024 never asked any of these — so there is no missed opportunity here, and no pipeline edit could add a comparison that doesn't exist.

## 3. Deliverable

New file: `2026/process/eis_2026_09_ai_confidence_concern_report.R`. Plain script using the `##### #` / `#### #` nested banner-comment sections that scripts 01-04 and 07 use, with `# - - - -` subsection banners. No Rmd, no `knitr`/`rmarkdown`, no output file — every chart is a bare top-level ggplot-producing expression that auto-prints when the script is sourced or stepped through line-by-line, the same execution model `eis_2026_06_design_data.R` already documents ("Run it interactively") and relies on for its own worked examples. Multi-item views are built as one assembled long tibble (via `map_dfr()`) feeding a single top-level plot call, specifically so nothing depends on print-inside-a-loop/function behavior.

No other file is created, modified, or read. No pipeline changes — see §2's confirmation that no 2024 data exists for any item this report adds beyond what `eis_2026_05_trend_comparison_report.Rmd` already covers.

## 4. Content

### Part C — AI (BPC35-38)

- **C.1** (new, 2026-only): three bar charts, `factor_props()` + `plot_prop_bar()` (scale-ordered), one each for `ai_prevalence` (how much election info seen is AI-generated), `ai_tool_freq` (how often uses AI tools such as chatbots), `ai_detect_conf` (confidence detecting AI-generated election content). No 2024 counterpart for any of the three.
- **C.2** (ported verbatim, `cum`/`weight_common`): the existing AI good/bad-matrix trend charts (10 items) from the 05 report's "Attitudes Toward AI Use in Elections" section — the mean-score (1-5) dumbbell and the bad/neither/good stacked-by-year chart. Both already show 2026 (as one dot, or one year's stacked bar) next to 2024, so no separate 2026-only chart is built for these 10 items — that would just duplicate what's already on the page.

### Part D — Voting experience (BPC39)

- **D.1** (ported verbatim, `cum`/`weight_common`): the existing single trend dumbbell from the 05 report's "Voting Experience" section.

### Part E — Confidence (BPC40-43)

- **E.1** (new, 2026-only): full four-level distribution for all 4 items at once — `factor_props()` mapped over the 4 columns into one long tibble, then `plot_prop_stack_by(by_col = "item")` (the same construction the 05 report already uses for its seek-frequency chart). This is new relative to the 05 report, which shows only the collapsed top-2 trend for confidence.
- **E.2** (ported verbatim, `cum`/`weight_common`): the existing top-2 trend dumbbell from the 05 report's "Confidence Votes Will Be Counted as Intended" section.
- No party-ID breakdown (confirmed: concern only).

### Part F — Concern (BPC44, 14 items)

- **F.1** (new, 2026-only): full four-level distribution for all 14 items at once, same construction as E.1.
- **F.2** (new, 2026-only, by party): by-party top-2 breakdown for all 14 items. `svy_prop_by()` (script 06) mapped over each item's `_top2` indicator, grouped by `pid3`, assembled into one long tibble, plotted with `plot_battery_dodge_party()` (ported from report 08 — dodged, not stacked, because each item's "concerned" share is independent of the others, the same reasoning report 08 gives for its own dodge charts). `_top2` (`as.numeric(as.integer(x) >= 3)`) is built for all 14 items in this script's own setup, extending the 05 report's existing top-2 convention (which only builds it for 10 of the 14) rather than duplicating a separate convention.
- **F.3** (ported verbatim, `cum`/`weight_common`): the existing top-2 trend dumbbell for the 10 items comparable to 2024, from the 05 report's "Concern About Election Problems" section.
- **F.4** (new, 2026-only): one bar chart (`battery_props()` + `plot_battery_bar()`, on the same 4 items' `_top2` indicators built for F.2) for the 4 items new to 2026 — federal law enforcement (e.g. ICE) deployment in the respondent's community, federal/state seizure of ballots or voting machines, eligible voters being blocked or having valid ballots rejected, and gerrymandering — explicitly captioned as having no 2024 counterpart.

### Part G — Noncitizen voting, access-vs-integrity, and USPS policy (BPC45-48)

Four new 2026-only bar charts, `factor_props()` + `plot_prop_bar()`, none with a 2024 counterpart:

- **G.1**: `noncitizen_freq` — how often illegal noncitizen voting occurs (5-level ordinal: Never → Very frequently, scale-ordered).
- **G.2**: `noncitizen_alters` — whether illegal noncitizen voting changes election outcomes (Yes / No / Don't know).
- **G.3**: `access_vs_integrity` — priority: easier for eligible voters to vote vs. harder for ineligible voters to vote (two substantive options + "Don't know / No opinion").
- **G.4**: `usps_policy_support` — support for the Postal Service's August mail-ballot policy change (5-level ordinal: Strongly oppose → Strongly support, scale-ordered).

For G.2 and G.3, the catch-all option ("Don't know" / "Don't know / No opinion") is pinned to the bottom of the chart rather than sorted by its own weighted %, reusing `order_bottom_anchors()` (ported from report 08's most recent revision, "Pin Don't know/Not sure to the bottom") with each item's own exact catch-all wording passed as `anchors`.

## 5. Statistical methodology

- Every 2026-current-state and by-party estimate uses `weight` on `data`; every trend estimate uses `weight_common` on `cum` — never mixed within one chart, per the project's established rule (see script 06's "WHICH WEIGHT" note and identical language in the 08 spec).
- Proportions: `survey::svyciprop()` (logit method) for a single 0/1 indicator; `survey::svymean()` for a whole factor's distribution at once — `svy_prop()` / `factor_props()` / `battery_props()`, ported from script 06/07.
- By-party proportions (F.2): `svy_prop_by()` — script 06's `group_by()` + `group_modify()` pattern over base `survey`, not `srvyr`. Report 08 used `srvyr` for its own by-party cut, but that was working with `sources.long` (a long, multi-battery table where `srvyr`'s `group_by(item_tag, pid3)` genuinely simplified a two-dimensional group-by); this report's data is already wide (one row per respondent, one column per item), which is exactly the shape script 06's `svy_prop_by()` was written for, so it is reused as-is rather than introducing `srvyr` as a second dependency for the same job.
- Trend estimators (`prop_by_year()`, `factor_props_by_year()`, `mean_by_year()`, `prop_table_by_item()`) ported unchanged from the 05 report.
- Every chart's `n` is printed via `cat()` immediately after the plot — the plain-script equivalent of the project-wide captioning convention (every chart elsewhere states its `n` in Rmd prose or an HTML caption).

## 6. Visual conventions

- `theme_minimal(base_size = 12)`, `BAR_COLOR <- "#2a78d6"` — matches every other script in this project.
- `YEAR_COLORS`, `AI_STATUS_COLORS` — ported verbatim from the 05 report, used only by the ported AI-matrix trend charts (C.2).
- `PID_COLORS` (Dem `#2a78d6` / Ind `#898781` / Rep `#d03b3b`) — ported from report 08, used only by the concern-by-party chart (F.2).
- `RAMP_4PT <- c("#b7d3f6", "#6da7ec", "#2a78d6", "#0d366b")` — one shared light-to-dark blue ramp (the same ramp script 06 already uses for `CONF_RAMP`), applied to both confidence's (E.1) and concern's (F.1) full-distribution charts via `setNames()` with each item's own actual level text, rather than defining two near-identical ramps.
- `order_bottom_anchors()` — ported from report 08, used for G.2/G.3's catch-all handling.
- No custom fonts, branding, or `ggsave` sizing — plots auto-size to whatever device shows them, matching script 06's own worked examples exactly (no chart in that file sets an explicit width/height either).

## 7. Scope boundaries

- No pipeline changes. No file besides the new script is created or modified.
- No party-ID breakdown for AI or confidence — concern only, per explicit direction.
- No 2024 comparison for BPC35-37, BPC45-48, or the four concern items new to 2026 — confirmed none exist in the 2024 field (§2).
- Nothing is saved to disk; running the script produces plots in the current R graphics device only.
- This report does not re-derive or duplicate any chart that already exists unchanged in `eis_2026_05_trend_comparison_report.Rmd` — every trend chart here is a direct port (§4's "ported verbatim" items), not a rebuild.

## 8. Size estimate

Part C: 3 + 2 = 5. Part D: 1. Part E: 2. Part F: 4. Part G: 4. **Total: 16 charts.**
