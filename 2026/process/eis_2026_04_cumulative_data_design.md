# Design: EIS cumulative 2024↔2026 data file

_Written 2026-09-17. Companion to `eis_2026_README.md`; extends the `eis_2026_0N` pipeline with a
fourth script. Status: approved by Jack, ready for an implementation plan._

## Purpose

A single stacked (long) file, one row per respondent per year (1,891 + 3,144 = 5,035 rows), holding
only the demographics, weights, and substantive items that are genuinely comparable across the 2024
and 2026 Election Information Survey fields. Built for year-over-year trend analysis — the natural
next step after `eis_2026_03_common_weights.R`'s `compare_years()` helper, which already does this
comparison one item at a time but doesn't persist a combined dataset.

**Sourcing note, load-bearing:** every wording/scale comparison below was verified against the
*fielded* documents — 2024's `survey_instrument.md` cross-checked against `eis_2024_codebook_questions.csv`
and `eis_2024_field1_codebook_levels.csv`; 2026 against `2026 Survey Data/2026_BPC_Voter Information
Survey.docx` (via `textutil -convert txt`, since pandoc isn't on PATH outside RStudio) and the vendor's
own codebooks in that folder. **`2026_survey_revision.txt` (project root) is a stale pre-fielding draft
and was not relied on for any conclusion below** — see [[bpc-eis-2026-revision]] memory. Where the
fielded 2026 survey differs from that draft (it does, substantially, on several items below), the
fielded version is what's used.

**Demographics/attitudinal items were verified by an exhaustive column-by-column audit** of all 219
raw 2024 columns against the 2026 variable index (not pattern-matching against existing script 03
code, which is what missed several items on the first pass — see the project memory note on this if
resuming this work later). Treat the list below, not any earlier draft of it, as authoritative.

## Structure

Stacked respondent-level microdata, not an item-level summary table. Backbone columns on every row:

| Column | Contents |
|---|---|
| `year` | 2024 / 2026 |
| `resp_id` | Respondent identifier — 2024 has `ResponseID`, 2026 has `resp_id`; use both directly, no row-position fallback needed |
| `weight_native` | Each year's own delivered weight (`wts` / `weight`) |
| `weight_common` | The composition-free re-raked weight from `eis_2026_03_common_weights.R` (`weight24`/`weight26`, from `output/eis_common_weights.rds`) |

## Demographics

| Cumulative name | 2024 source | 2026 source | Note |
|---|---|---|---|
| `educ3` | `xeduc3` | `educ3` | |
| `race_white` | `xdemWhite` | `race_white` | Binary, kept for continuity with script 03's existing raking margins |
| `race4` | built from `xdemWhite` + `demBlackBin` + `xdemHispBin` + `demRaceOther` | `race4` | 2024 has the same four overlapping single-value race flags 2026 had pre-`race4` (confirmed via the level codebook — each flag defines only code 1, same structure). Build the identical White-non-Hispanic/Black-non-Hispanic/Hispanic/Other-non-Hispanic partition script 02 already uses for 2026 |
| `age4` | `age` | `age4` | |
| `generation` | `demAgeGeneration` | `generation` | |
| `gender` | `xdemGender` (2024 also has a second column, `demGender` — **resolve which to use before finalizing**, likely a vendor recode duplicate of `xdemGender` but not yet confirmed) | `gender` | |
| `region4` | `xreg4` | `region4` | |
| `state`, `county`, `town` | built from `demZIP` via `zipcodeR`, same method as 2026 | `state`, `county`, `town` (already built) | 2024 also delivers `county_fips` directly — use it to validate the ZIP-derived county, the same way 2026's ZIP-derived state was validated against delivered `region4` |
| `recalled_vote` | `xsubVote20O` (2020 vote) | `vote_2024` (2024 vote) | Different referent election by design; kept year-specific per script 03's existing precedent. (2024 also has `xsubVote22O`/`xsubVote18O` if a different anchor is ever wanted — not used here) |
| `pid3` | `xpid3` | `pid3` | `pid5`/`pid3_lean`/`pid7` out of scope — see note below |
| `ideo3` | `xdemIdeo3` | `ideo3` | |
| `income3` | `xdemInc3` | `income3` | |
| `rural_urban3` | `xdemUsr` | `rural_urban3` | |
| `employment` | `xdemEmploy` | `employment` | |
| `union_member` | `demUnion` | `union_member` | |
| `sexual_orientation` | `demLGBTQ1` | `sexual_orientation` | Confirmed via level codebook this is sexual-orientation-only (7 categories), not combined with gender identity — 2026's separate `trans_nonbinary` has no 2024 counterpart, stays 2026-only |
| `insured` | `demInsured` | `insured` | |
| `insurance_type` | `demInsType` | `insurance_type` | |
| `religion` | `xdemReligion` | `religion` | |
| `evangelical` | `xdemEvang` | `evangelical` | |
| `married` | `xdemMarried` | `married` | |

**`pid5`/`pid3_lean`/`pid7` are out of scope.** 2024's `BPCxdem1`-`BPCxdem5` (the presumed analogue of
2026's `BPCdem1`/`BPCdem2` MAGA/progressive routing used to build `pid5`) have no recorded question
text anywhere in the project — both `eis_2024_codebook_questions.csv` ("NO TEXT") and the actual
`2024_survey_instrument.docx` (demographics section lists category names only, no item text) are dead
ends. Decision: `pid3` only, for both years.

**Confirmed genuinely absent, not overlooked** (checked both directions in the full audit): disability
status (2026 has `disability_work`, 2024 has no disability column at all), parent/child status (neither
year's delivered data has it, despite both instrument outlines listing it), military/veteran household
(2024 has `xdemMilHH1`, 2026 has nothing), presidential approval (2024 has three variants — `nr2b`,
`xdemBidenApprove`, `xdemBidenApprove2` — 2026 has none, and it's administration-specific regardless).

## Substantive items — included

All items below were re-verified word-for-word against the fielded questionnaires or vendor codebooks,
not just the PROJECT_NOTES.md crosswalk (which is itself stale on several of these).

| Cumulative name(s) | 2024 source | 2026 source | Note |
|---|---|---|---|
| `country_direction` | `nr1` | `nr1` (same vendor qid both years, outside the BPC-numbered questionnaire in both fields) | Word-for-word identical: "Now, generally speaking, would you say that things in the country are going in the right direction, or have they pretty seriously gotten off on the wrong track?" |
| `top_issue` | `nr3` | `nr3` (same vendor qid) | Word-for-word identical stem |
| `seek_register`, `seek_elections_run`, `seek_who_won` | `BPC1_1`, `BPC1_2`, `BPC1_3` | `seek_register`, `seek_elections_run`, `seek_who_won` | Stem and 5-point frequency scale identical (2024's `BPC1_4` candidates / `BPC1_5` campaign-news rows have no 2026 counterpart, excluded) |
| 8 source indicators × 3 topics (register/how-run/who-won) = 24 columns: `reg_src_local_officials`, `reg_src_state_officials`, `reg_src_federal_site`, `reg_src_news_media`, `reg_src_social`, `reg_src_friends_family`, `reg_src_advocacy`, `reg_src_campaign` (+ same 8 under `run_src_*` and `won_src_*`) | `BPC3`/`BPC4`/`BPC5` (15-item lists; use each topic's within-15 count, since 2024 did **not** actually force exactly 3 — confirmed empirically: 55.5–59.3% of respondents selected fewer than 3 across all three questions, same pattern as `eis_2024_selectN_findings.md` already found for `BPC2`) | 2026's pooled `reg_src_*`/`run_src_*`/`won_src_*` columns (actual + hypothetical arms already combined by script 02 — matches 2024's unconditional single-version asking) | Ranking is 2026-only and is dropped for this comparison — flat selection indicators only. Of 2024's 15 options and 2026's 10, 7 match cleanly (local/county officials, state officials, federal agency/website, news media, social media influencer, friends/family, election-integrity/advocacy orgs) and 1 needs constructing (`*_src_campaign` = 2026's single "candidate, campaign, or political party" option ↔ OR of 2024's two separate options "your preferred candidate" and "a national political party organization"). 2024's 4 remaining options (fact-checking organizations, elected officials generally, favorite commentator/analyst, civic or religious organizations) and 2026's 2 remaining options (online search engine, AI-enabled chatbot) have no cross-year match and are **not** part of the comparable set — they stay in each year's own file only |
| `ai_ok_voter_candidate_info` … `ai_ok_cand_answer_questions` (10 items) | `BPC14_1`…`BPC14_10` | same names (`BPC38` rows) | 5-point good/bad scale, items word-for-word identical except item 1: 2024 "a candidate or an issue" → 2026 "...on the ballot" (approved as comparable). **Scale asymmetry:** 2024's `BPC14` has no "Don't know" option (confirmed via level codebook — 5 forced-choice levels only); 2026's `BPC38` adds a 6th DK. 2026's existing `_i` columns already drop DK as off-scale per established project convention, so build a matching `_i` for 2024 (straightforward 5-level recode, no DK to handle) and stack the two `_i` columns as the comparable measure; keep each year's raw/native levels available too |
| `vote_exp_positive` (raw, per year) + `vote_exp_positive_top2` (comparable) | `BPC19` | `vote_exp_positive` (`BPC39`) | Identical stem. **Scale asymmetry:** 2024 is 4-point forced-choice (no neutral); 2026 adds "neither agree nor disagree." Resolution: keep each year's full native distribution *and* add `vote_exp_positive_top2` (1 = strongly/somewhat agree, 0 = strongly/somewhat disagree; base excludes DK in both years and excludes "neither" in 2026 — 2024 has no "neither" to exclude, so this can only partly restore comparability). Script should also report 2026's raw "neither" share as a standalone number |
| `conf_own_vote` | `BPC20` | `conf_own_vote` (`BPC40`) | Identical apart from named election. **`BPC40` alone has an extra "I do not plan to vote" option 2024 doesn't have — excluded from this item's base, both years** |
| `conf_local_votes` | `BPC21` | `conf_local_votes` (`BPC41`) | Identical ("county or city") apart from named election |
| `conf_state_votes` | `BPC22` | `conf_state_votes` (`BPC42`) | Identical apart from named election |
| `conf_national_votes` | `BPC23` | `conf_national_votes` (`BPC43`) | Identical apart from named election |
| `concern_misinfo`, `concern_ai_disinfo`, `concern_ineligible_votes`, `concern_overturn`, `concern_biased_count`, `concern_mail_ballots`, `concern_guns_intimidation`, `concern_post_violence` | `BPC24_1,2,4,5,6,7,8,9` | same names (`BPC44` rows) | Word-for-word identical stems and scale |
| `concern_foreign` | `BPC24_3` | `concern_foreign` | "Interference from foreign entities (including countries)" → "Interference from foreign countries or entities" — reordered, same concept. Approved as comparable |
| `concern_polling_problems` | `BPC24_11` ("Long lines at the polls") | `concern_polling_problems` ("Problems at polling places, such as long lines or equipment failures") | Broadened wording, same core construct. Approved as comparable |

**Comparability caveat, source-seeking battery (`reg_src_*`/`run_src_*`/`won_src_*`): the retained 8 shared
indicators are rank-comparable, not level-comparable.** Both years cap selections at three, but the menus behind
that cap differ in size: 2024's BPC3/BPC4/BPC5 each offered 13 substantive options (of 15 total, excluding the two
anchors), while 2026's pooled batteries offer 10 substantive options (of 12 total). 2026 also added two options with
no 2024 counterpart, and both are heavily selected: `reg_src_search` (online search engine), picked by 34.5% of
2026 respondents — the single most-selected option in the entire battery — and `reg_src_chatbot` (AI-enabled
chatbot), picked by 8.0%. Because both years cap at three selections, those two new options do not simply add data
alongside the shared eight; they compete for the same fixed number of slots. The measured effect: 39.9% of 2026
respondents spent at least one of their three slots on a 2026-only option (search OR chatbot; the two overlap
slightly, so this is below the 42.5% naive sum of 34.5% + 8.0%), against 28.6% of 2024 respondents on a 2024-only
option (union of the four dropped options — fact-checking organizations, elected officials generally, favorite
commentator/analyst, civic or religious organizations — within `BPC3`), and mean selections among the shared eight
dropped from 1.60 (2024) to 1.39 (2026). Both percentages independently verified against the raw register-topic
columns (`BPC3_*NET` for 2024, `reg_src_search`/`reg_src_chatbot` for 2026) using the identical methodology on each
side. The mechanical
consequence is visible item by item — `reg_src_advocacy` fell from 15.9% to 6.3%, `reg_src_campaign` from 19.0% to
8.1%, and `reg_src_federal_site` from 24.6% to 16.8% — and some or all of each of those drops is menu displacement,
not opinion change. **A level change in any of the eight retained indicators therefore cannot be read as pure
opinion change: the menu itself changed what competes for the same three slots.** Recommendation: compare ranks
across years, not raw selection levels. `eis_2024_selectN_findings.md` already found, in this same battery family,
that rank orderings are far more robust to a wording/menu change of exactly this kind than selection levels are
(Spearman rho 0.97-0.99), so ranking each year's eight shared indicators separately and comparing the ranks — not
the percentages — is the recommended way to use these columns for trend analysis.

## Excluded (structurally changed, or absent one side — stay in each year's own file, not stacked)

- `BPC2` (2024: "choose the three you use most often") vs. `bpc2`/`BPC2`+`BPC2a` (2026: "select up to three" + separate ranking step) — already documented in `eis_2024_selectN_findings.md` as ~18% wording-inflated; not re-litigated here. (Note: this is a *different* question from the register/how-run/who-won source batteries above — `BPC2` asks the single "which sources do you use most for election news overall" question, not the three topic-specific ones.)
- `BPC11` (2024, "how frequently encounter AI-generated info") vs. `BPC35`/2026 ("how much of what you see is AI-generated") — frequency-of-exposure vs. perceived-share; different constructs.
- `BPC13` (2024, "encountered AI without knowing, yes/no probability") vs. `BPC37`/2026 ("confidence you could detect AI content") — past-experience-belief vs. detection-confidence; different constructs.
- `BPC12`/2024 vs. `ai_tool_freq`/`BPC36`/2026 ("AI tool use frequency") — **same stem, deliberately different scale.** 2026 fielded this with BPC1's 5-point "only around major election dates.../consistently throughout the year/never" scale rather than 2024's 6-point Daily/Weekly/Monthly/Rarely/Never/DK. Confirmed by Jack as an intentional design choice, not a fielding error. Different scale measuring a different framing either way, so excluded.
- `BPC24_10`, "Restrictive voting equipment" (2024-only) and 2026's `concern_eligible_blocked`/`concern_gerrymander`/`concern_ice_deployment`/`concern_ballot_seizure` (net-new, no 2024 counterpart).
- Everything else with no cross-year pairing at all: 2024's `BPC6`-`BPC10`, `BPC15`-`BPC18`; 2026's `BPC2a`-`g`/`BPC13`-`BPC34` drill-down follow-ups, `q_attn`, `BPC45`-`BPC48`, `BPCdem3`.
- `pid5`, `pid3_lean`, `pid7`, `trans_nonbinary`, `disability_work`, parent/child status, military household, presidential approval — see demographics section above.

## Corrections this design surfaced, filed separately (not part of this file's scope)

- PROJECT_NOTES.md §3's crosswalk table is stale on multiple points (confidence battery item count,
  concern battery item count/composition, the AI-tool-use scale, and the overall source-seeking
  question architecture) — spun off as background task `task_c06be1d0`, not yet actioned.
- `2026_survey_revision.txt` is superseded and should probably be labeled as such, or replaced — not
  actioned here; see [[bpc-eis-2026-revision]] memory.

## Open implementation questions (for the plan, not decisions needed from Jack)

- Resolve `demGender` vs. `xdemGender` (2024) before finalizing `gender` — likely a vendor recode
  duplicate, not yet confirmed which is which.
- Output naming/location: continuing the existing convention, this becomes
  `eis_2026_04_cumulative_data.R` in the project root, reading `output/eis_2026_clean.rds`,
  `eis_2024_field2_rvoter_data.csv`, and `output/eis_common_weights.rds`, writing
  `output/eis_cumulative.rds` / `.csv` (name open to bikeshedding, not a design question).
