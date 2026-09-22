# Design: extending the EIS cumulative file back to 2022 and 2023

_Written 2026-09-21. Companion to `eis_2026_04_cumulative_data_design.md`, which built the existing
`eis_cumulative` (2024↔2026 only). This document identifies which 2022 and 2023 variables are
comparable to the content already in `eis_cumulative`. Status: the demographics and prospective
confidence items below were implemented for 2022 in `eis_2026_11_cumulative_add_2022.R`
(2026-09-22, per Jack's explicit scope instruction); 2023 and the rest of 2022's comparable content
remain analysis only._

**Update, 2026-09-22:** between this audit and implementation, `eis_2026_04_cumulative_data.R` picked
up a 113th column, `ai_tool_used` (commit `0f95f8c`) — a binary "used an AI tool at all" trend built
from 2024's `BPC12` and 2026's `ai_tool_freq`, narrower than the full frequency scale this document's
companion (`eis_2026_04_cumulative_data_design.md`) originally excluded as a different-scale item.
2022 predates any AI-tool question entirely (fielded October 2022, before ChatGPT's public launch) and
has no counterpart — confirmed `ai_tool_used` is `NA` for all 2,002 2022 rows in the implemented file.
Noted here so this document stays accurate against the file's current shape; not re-litigating the
104 doc's own reasoning.

## Purpose

`eis_cumulative` currently holds one row per respondent for 2024 and 2026 (5,035 rows), covering only
demographics, weights, and substantive items verified comparable across those two fields. This
document audits the 2022 (`2022/raw/`) and 2023 (`2023/raw/`) source files column-by-column against
every variable already in `eis_cumulative` (ground truth: the actual header of
`output/eis_cumulative.csv`, 112 columns, not just the design doc's prose) to determine what could be
added if 2022 and 2023 were stacked in too.

**Sourcing note, load-bearing:** every wording/scale claim below was checked against the *fielded*
instruments — `2022/survey_instrument.md` and `2023/survey_instrument.md` — not just the vendor
question/level codebooks, cross-checked against `2022/raw/question_codebook.csv` +
`2022/raw/levels_codebook.csv` and the 2023 equivalents. This matters because the 04 design doc already
found the vendor codebooks mislabel at least one question stem (2024's BPC3/BPC4); the same class of
error is possible here and the instrument text is the tie-breaker where the two disagree. Sample sizes
and population descriptions are cross-checked against the project `README.md`.

## The finding that overrides everything else: 2023 is not a registered-voter sample

Before any item-level comparison: **2023's `xdemAll` is coded `1 = "Adults"` for all 2,203 respondents**
— there is no registered-voter screen, and no flag anywhere in the delivered 2023 file that would let one
be reconstructed after the fact. **Confirmed exhaustively (2026-09-21):** `xdemAll` is constant (no
variation at all) in both 2022 and 2023 — it's Morning Consult's own panel-screening designation for the
sample as a whole ("Registered Voters" vs. "Adults"), not a measured per-respondent survey response.
Searched both years' question codebooks, levels codebooks, survey instruments, and full raw-data column
lists for any registration-status item, voter-file-match flag, or screener column — none exists. The only
neighboring content is past-vote-choice recall (`xsubVote20O` etc.), which asks who a respondent voted
for in a specific past election, not whether they're currently registered — an imperfect proxy at best
(misses anyone registered since that election; conflates turnout with registration), not a real
substitute. **There is no way to recover a registered-voters subset from 2023 as delivered.** This is
different in kind from every other comparability question in this document. 2022 (`xdemAll = "Registered Voters"`), 2024, and 2026 are all registered-voter samples;
2023 is a general-population-adult sample. That difference shows up in *every* variable at once — it's a
compositional difference in who answered, not a wording or scale difference in any one question — and no
amount of item-level recoding fixes it. Recommend the cumulative file carry a clear population flag
(e.g. `sample_pop = "registered voters"` / `"adults"`) rather than silently pooling 2023 with the other
three years, and that any year-over-year comparison involving 2023 caveat this explicitly, the same way
this project already caveats the source-seeking menu-composition problem in the 2024↔2026 file.

## Backbone

| Column | 2022 | 2023 |
|---|---|---|
| `year` | 2022L | 2023L |
| `resp_id` | **No respondent ID column exists in `2022/raw/data.csv` at all** (confirmed against the full 138-column header — no `ResponseID` or equivalent). Would need a synthetic id (e.g. `paste0("2022_", row_number())`); every 2022 row is still unique, just not vendor-identified | `ResponseID` present, use directly like 2024/2026 |
| `weight_native` | `wts` (same column name as 2024) | `wts` |
| `weight_common` | Built 2026-09-22 — see "Out of scope" below | Not addressed here |

**2022 sample composition decision needed:** `2022/raw/data.csv` has 4,113 rows, not the 2,002 the
README describes as the national sample. `AUD` (1 = GENPOP n=2,002, 2 = Colorado oversample n=805, 3 =
Georgia oversample n=805, 4 = Wisconsin oversample n=501) shows the file bundles three state oversamples
in with the national sample, the same shape of problem the 04 design doc resolved for 2024's field1/
field2 overwrite. Recommend filtering to `AUD == 1` before stacking, since the oversamples are not part
of a national probability sample and would bias national trend estimates — but this is a call for Jack,
not assumed here.

## Demographics

Every row below was checked against both years' `levels_codebook.csv` for exact level count/order, not
just column presence — a same-named column with a different number of categories would silently corrupt
a trend line.

| Cumulative name | 2022 source | 2023 source | Note |
|---|---|---|---|
| `educ3` | `xeduc3` | `xeduc3` | Identical 3-level scheme both years, matches 2024/2026 exactly |
| `race_white` | `xdemWhite` | `xdemWhite` | Same single-value flag structure as 2024 |
| `race4` | buildable from `xdemHispBin`+`xdemWhite`+`demBlackBin`+`demRaceOther` | same | All four flags present both years, identical structure to 2024 — build with the same Hispanic > White > Black > Other precedence |
| `age4` | `age` | `age` | Already delivered pre-bucketed (18-34/35-44/45-64/65+, coded 1:4) in both years, same as 2024 — not a raw age field despite the codebook's generic "What is your age?" label |
| `generation` | `demAgeGeneration` | `demAgeGeneration` | Identical 4-level scheme, identical birth-year ranges |
| `gender` | `xdemGender` | `xdemGender` | Binary (Male/Female) both years — no `demGender` duplicate column exists in either year, so the 2024 ambiguity flagged in the 04 design doc doesn't recur here |
| `region4` | `xreg4` | `xreg4` | Identical 4-level scheme |
| `state`, `county`, `town` | **Not available — no `demZIP` or any other geography column in 2022** | Buildable via `demZIP` using the same `zipcodeR` crosswalk method script 04 used for 2024 (2023 has `demZIP`, confirmed in both the raw header and question codebook) | |
| `recalled_vote` | `xsubVote20O` (2020 vote: Biden/Trump/Other/Didn't vote, same 4-level scheme) | `xsubVote20O` (same scheme, confirmed via levels codebook and value counts: 989/718/71/425) | Same referent election (2020) as 2024 used, so no referent-mismatch issue like the 2024-vs-2026 case |
| `pid3` | `xpid3` | `xpid3` | Identical 3-level (Dem/Ind/Rep, no lean) |
| `ideo3` | `xdemIdeo3` | `xdemIdeo3` | Identical 3-level |
| `income3` | `xdemInc3` | `xdemInc3` | Identical 3-level |
| `rural_urban3` | `xdemUsr` | `xdemUsr` | Identical 3-level |
| `employment` | `xdemEmploy` | `xdemEmploy` | Identical 8-level |
| `evangelical` | `xdemEvang` | `xdemEvang` | Identical 2-level |
| `religion` | `xdemReligion` | `xdemReligion` | **Already the exact same 5-bucket scheme `eis_cumulative` uses** (All Christian / All Non-Christian / Atheist / Agnostic-Nothing in particular / Something Else) — simpler than 2026, which needed a 12→5 collapse |
| `country_direction` | `xnr1` | **Absent** — no `xnr1` column in 2023's data, codebook, or levels file, despite "Right direction/wrong track" appearing in 2023's own instrument-outline boilerplate (same outline-lists-it-but-vendor-didn't-deliver-it gap the 04 design doc already found for parent/child status in 2024) | Word-for-word identical stem to 2024/2026's `nr1`, same 2-level RD/WT scale |
| `top_issue` | `xnr3` | **Absent**, same outline-vs-delivered gap as `country_direction` | Identical 8-category composition and order to 2024/2026's `nr3` (Economy / Security / Health Care / Medicare-Social Security / Women's Issues / Education / Energy / Other) — verified against `eis_2026_codebook_combined.csv`'s full-text labels, not just 2022's abbreviated ones |

**Confirmed genuinely absent in both 2022 and 2023** (checked full column headers of both years'
`data.csv`, not just the codebooks): `union_member`, `sexual_orientation`, `insured`, `insurance_type`,
`married`. All five are listed in both years' survey-instrument "Standard Demo Questions" outline
(alongside "Parent" and "COVID-19 Vaccination Status", also absent) but none were actually delivered —
the same outline-vs-delivered-data gap the 04 design doc documented for 2024's parent/child status and
military household. Since these are also missing from at least one of 2024/2026 already, none of them
are part of `eis_cumulative` today anyway, so this is confirmation, not new loss.

## Substantive items

### Comparable, both years — confidence in vote counting (prospective)

| Cumulative name | 2022 source | 2023 source | Note |
|---|---|---|---|
| `conf_own_vote` | `BPC13` ("...will be counted accurately in the 2022 midterm election") | `BPC17` ("...in the 2024 presidential election") | Identical stem and identical 4-point + DK scale apart from the named election, same pattern as the existing 2024↔2026 handling |
| `conf_local_votes` | `BPC14` | `BPC18` | Same |
| `conf_state_votes` | `BPC15` | `BPC19` | Same |
| `conf_national_votes` | `BPC16` | `BPC20` | Same |

**Do not confuse with the retrospective version.** 2022 *also* has `BPC9`-`BPC12`, asking the identical
four-way question about whether votes **were** counted accurately in the 2020 election (past fact, not
future expectation) — a different construct from `conf_own_vote` etc., which is forward-looking
("will be"). 2023 has no retrospective equivalent. Flagging explicitly so a future pass doesn't grab
`BPC9`-`12` by pattern-matching the "confident... votes... counted accurately" stem.

### Comparable, 2023 only — concern battery

| Cumulative name | 2023 source (`BPC21_n`) | Note |
|---|---|---|
| `concern_misinfo` | `BPC21_1` | Word-for-word identical to 2024's stem ("Inaccurate or misleading information about elections") |
| `concern_foreign` | `BPC21_2` | Word-for-word identical to 2024's exact wording ("Interference from foreign entities (including countries)") — even closer than 2026's reordered version |
| `concern_ineligible_votes` | `BPC21_3` | Word-for-word identical |
| `concern_overturn` | `BPC21_4` | Word-for-word identical |
| `concern_biased_count` | `BPC21_5` | Word-for-word identical |
| `concern_mail_ballots` | `BPC21_6` | Word-for-word identical |
| `concern_guns_intimidation` | `BPC21_7` | Word-for-word identical |
| `concern_post_violence` | `BPC21_8` | Word-for-word identical |

All eight use the same 4-point "Very concerned...Not at all concerned" + DK scale as 2024/2026, confirmed
via the 2023 levels codebook. `concern_ai_disinfo` and `concern_polling_problems` have no 2023
counterpart — 2023's `BPC21` battery has only 9 rows, and the 9th ("Restrictive voting rules or
regulations") has no cumulative counterpart either (same fate as 2024's own dropped "restrictive voting
equipment" item — a 2024-only exclusion the 04 design doc already made, now matched by a 2023-only
analog).

**2022 has no comparable battery at all.** Its closest analog, `BPC17`, is a *conditional* multi-select
("select all that apply") asked only to respondents who answered "not confident" on `BPC16`, listing
*reasons* for that lack of confidence (worried about mail ballots, worried about overturn attempts,
worried about election-day violence, etc.). That's a different base population (conditional vs. universal),
a different response format (pick-all-reasons vs. a 4-point rating on every item), and materially
different item wording, even where the themes overlap. Recommend treating `concern_*` as unavailable for
2022 entirely rather than force a mapping from `BPC17`.

### Partially comparable, needs a judgment call — source-seeking batteries

**`reg_src_*` / `run_src_*` / `won_src_*` (8 shared indicators × 3 topics):** 2022 has the same three
topic-specific "select up to 3" questions (`BPC1` = register/vote, `BPC3` = how elections run, `BPC5` =
who won) at the same structural level as 2024/2026 — but the option list itself is a different kind of
list. 2022's 13 substantive options are almost entirely *media-channel* types (national TV, local TV,
radio, print publications, print-pub news websites, TV news website, social media, search engines,
podcasts, blogs/forums) plus two institutional options (state election office, local election office) and
friends/family. The cumulative's 8 shared indicators, by contrast, come from an *institution/people* list
(fact-checking orgs, election administrators, elected officials, advocacy orgs, a federal agency, a
candidate/party, news media professionals, commentators, civic/religious orgs, a social media influencer,
friends/family). The overlap is real but narrow:

- `reg_src_state_officials` / `run_src_state_officials` / `won_src_state_officials` ← "your state election office" — clean match
- `reg_src_local_officials` / etc. ← "your local election office" — clean match
- `reg_src_friends_family` / etc. ← "friends and family" — clean match
- `reg_src_social` / etc. ← "social media such as Facebook, Twitter, or Instagram" — approximate; 2022 asks about the *channel*, 2024/2026 ask specifically about a *social media influencer or content creator*, a narrower concept
- `reg_src_federal_site`, `reg_src_news_media`, `reg_src_advocacy`, `reg_src_campaign` (and the run/won equivalents) — **no 2022 counterpart at all.** 2022 never asks about a federal agency/website, an institutional "news media professionals" category (only fragmented TV/radio/print channel items), an advocacy/election-integrity-org category, or a candidate/party category, in this battery.

So at best 3 of 8 shared indicators map cleanly from 2022, one more maps approximately, and the other four
are absent — worth a real decision from Jack before building this rather than an implementation-time
default, the same way the menu-composition caveat in the existing file was surfaced rather than silently
absorbed.

2023 has the *right* option list — its `BPC2` ("select the three people or groups you would look to for
information about elections") uses what is essentially the same 12-item institution/people list 2024/2026
use for `reg_src_*`/`run_src_*`/`won_src_*` (fact-checking orgs, local/county election administrators,
state election administrators, elected officials, election-related organizations, a federal agency,
preferred candidate, national party organization, news media professionals, favorite commentator, civic/
religious organizations, favorite social media influencer — verified word-for-word against
`2023/survey_instrument.md`). But **2023 asks this only once, generally** — not once per topic
(register/how-elections-run/who-won) the way 2022, 2024, and 2026 all do. That means 2023 can support at
most a single general version of the 8 shared indicators, not three topic-specific ones. It also has no
"friends and family" option within `BPC2` at all — that item lives instead in 2023's separate `BPC1`
(the media-channel-type question, see below), so pulling a `src_friends_family` value for 2023 means
reaching into a different question than the one supplying the other seven indicators, which is a real
construct-boundary compromise to flag rather than paper over.

**`src_tv_local` … `src_friends_family` (general "which sources do you use most", 2024's `BPC2`):** 2023's
`BPC1` ("choose the three that you use most often to learn about elections") is a close match — confirmed
verbatim for national/local TV, radio, social media, search, podcasts, friends/family, newsletters-blogs-
forums, news aggregator apps, and AI chatbot. One split does **not** carry over cleanly: 2024's `BPC2`
separates "Local or state news outlets, print or online" from "National news outlets, print or online"
(`src_news_local` / `src_news_national`, confirmed via `2024/raw/question_codebook.csv`), while 2023's
`BPC1` has a single combined item, "News websites (including TV news and print publications websites)" —
which also folds in TV-news websites, not just print. A 2023 value can't be cleanly split into
`src_news_local`/`src_news_national`; it would need to either stand in for both (double-counting risk) or
be excluded from that specific pair. 2022 has no general (non-topic-specific) version of this question at
all — only the three topic-specific batteries above — so at most one of those three could stand in as an
approximation, which is a meaningfully different question ("sources for X topic" vs. "sources you use
most often overall") and not recommended as a real substitute.

### Not available in either 2022 or 2023

- `seek_register`, `seek_elections_run`, `seek_who_won` (5-point *frequency* of seeking each kind of
  information) — neither survey asks a frequency-of-seeking question; both jump directly to source
  selection. Do not construct a proxy from "did they select any source" — that's a binary sought-vs-not
  measure, a different construct from a 5-point frequency scale.
- `ai_ok_voter_candidate_info` … `ai_ok_cand_answer_questions` (10-item AI acceptability matrix) — 2022
  predates ChatGPT's public existence (fielded October 2022); 2023's only AI-related content is a single
  "AI-enabled chatbot" option inside the general source-type list, not an acceptability battery.
- `vote_exp_positive` / `vote_exp_positive_top2` — no "voting is easy" or similar statement in either
  instrument.

## Out of scope for this document

- **`weight_common` for 2022 — done, 2026-09-22, not by this document.** `eis_2026_03_common_weights.R`
  was extended to re-rake 2022 to the same common demographic target as 2024/2026 (it needed none of
  2022's missing ZIP-derived geography — the raking margins are education/race/age/gender/region/recalled
  vote, none of them geography). 2022's own delivered weight turned out to already be close to the
  target (the confidence items moved only ~0.2-0.3pts under the re-rake, far less than 2024's own
  correction), but it is now on the identical composition as the other two years rather than merely
  close. `eis_2026_11_cumulative_add_2022.R` populates `weight_common` from this directly. 2023's
  4-way extension (if ever done) remains a separate task — 2023's registered-voter problem (see
  [[eis-2023-survey-population-caveat]]) is unresolved and unrelated to this fix.
- Everything in 2022/2023 with no `eis_cumulative` counterpart to compare against — 2022's `BPC6a`-`f`
  (trust-messenger paired comparisons), `BPC7`/`xBPC7dem`/`xBPC7rep`/`xBPC7state` (statement-trust by
  messenger), `BPC8` (who should decide on contested ballots), `BPCdem1`/`BPCdem2` (MAGA/progressive
  self-identification); 2023's `BPC3`-`BPC16` and `BPC22`-`BPC24` (information-seeking factors, social
  media sentiment/moderation/platform use, messaging-platform behavior, political ad exposure, news
  consumption frequency, civic participation, self-identity statements). None of these have a home in the
  current `eis_cumulative` schema; whether any are worth adding as *new* cumulative content (rather than
  matched against what already exists) is a separate question from the one this document answers.

## Summary

| | 2022 | 2023 |
|---|---|---|
| Population | Registered voters (matches 2024/2026) — but file bundles 3 non-national oversamples, needs `AUD == 1` filter | **General adults, not registered voters — no way to reconstruct an RV screen after the fact** |
| Respondent ID | **None delivered** — needs a synthetic id | `ResponseID`, direct |
| ZIP / state / county / town | **Not delivered at all** | Available via `demZIP`, same method as 2024 |
| Demographics (educ3, race4, age4, generation, gender, region4, recalled_vote, pid3, ideo3, income3, rural_urban3, employment, evangelical, religion) | All comparable, same schemes as 2024/2026 | All comparable, same schemes as 2024/2026 |
| `country_direction`, `top_issue` | Comparable | Not delivered |
| `conf_own_vote`/`conf_local_votes`/`conf_state_votes`/`conf_national_votes` | Comparable (`BPC13`-`16`) | Comparable (`BPC17`-`20`) |
| `concern_*` (8 of 10) | **Not available** (structurally different follow-up question) | Comparable (`BPC21`) |
| `reg_src_*`/`run_src_*`/`won_src_*` (8 shared indicators) | 3 clean + 1 approximate of 8, per topic | Right item list, but only 1 general version, not 3 topic-specific; missing friends/family from that question |
| `src_*` general sources | Only via topic-specific proxy (not recommended) | Comparable except the local/national news-site split |
| `seek_*`, `ai_ok_*`, `vote_exp_positive*` | Not available | Not available |
