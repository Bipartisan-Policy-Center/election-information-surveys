

# EIS_2026_codebooks



########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####
####  Title: Building the combined codebook for the BPC Election Information Survey, 2026 field (registered voters)
####
####  Author: Jack Friedman
####
####  Overview: The 2026 Election Information Survey is cleaned in three companion .R files. This file, 'eis_2026_01_combine_codebooks.R',
####            joins Morning Consult's two delivered codebooks into a single response-level codebook and attaches the metadata needed to
####            clean the data. The second file, 'eis_2026_02_clean_data.R', reads that codebook and uses it to clean the raw data. The third
####            file, 'eis_2026_03_common_weights.R', re-weights the 2024 and 2026 files to one common demographic target so that a
####            year-over-year difference is free of composition effects.
####
####  File Description: Morning Consult delivers two separate codebooks — one listing question text by column, the other listing response
####                    option labels by column — and neither on its own describes a variable completely. This document merges them so that
####                    every response option carries its parent question's full text, and adds six pieces of metadata the cleaning script
####                    needs but the vendor does not supply:
####
####                      var_name        a short, intuitive name to replace the vendor's qid
####                      var_label       a plain-English description of what the variable measures
####                      var_type        id / weight / text / flag / binary / nominal / ordinal / multiselect / rank
####                      scale_family    a name for a shared response scale, so each of the ~40 distinct scales is declared exactly once
####                      ord_position    for ordinal items, the level's place in the substantive low-to-high ordering (NA off-scale)
####                      base_condition  the questionnaire's skip logic, in plain text
####
####                    This file is the SPECIFICATION for script 02. Names, labels, orderings, and skip logic are declared here and nowhere
####                    else, so a correction is made in one place.
####
####  Outputs (written to output/): eis_2026_codebook_combined.csv    one row per response option
####                                eis_2026_codebook_questions.csv   one row per variable
####
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# Loading packages
library(tidyverse)


##### #
#### #
### ################################################################################################################################################# #
# Part A. Loading and preparing the vendor codebooks ------------------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.1. File paths ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Setting the folder the vendor delivery sits in, and the folder all output is written to. Both are relative to
# 2026/ — the R project's working directory whenever its .Rproj is open, regardless of which subfolder a script
# itself is saved in (this file lives in 2026/process/, but that is irrelevant to where relative paths resolve).
data.dir <- "input"
out.dir  <- "output"

# Creating the output folder if this is a first run (showWarnings = F so a re-run is silent)
dir.create(out.dir, showWarnings = FALSE)

# Building the two codebook paths. Morning Consult job number 2608077.
#
# THESE ARE THE V2 CODEBOOKS, delivered 2026-09-11 after we asked for the demographics in their original form. V2 is a
# strict superset of the first delivery: the same 3,144 respondents, every V1 value carried through unchanged, and 13
# new columns. The V1 files are still in the folder but nothing reads them any more.
path.questions <- file.path(data.dir, "2608077_BPC_question codebook_Voter Information Survey_RVs_V2.csv")
path.levels    <- file.path(data.dir, "2608077_BPC_level codebook_Voter Information Survey_RVs_V2.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.2. Reading in the two vendor codebooks ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Reading in the question codebook (one row per delivered column: qid, full question text).
# Both vendor files are UTF-8 with a byte-order mark, so the first column name arrives as "<U+FEFF>qid" rather than "qid".
# Renaming positionally with set_names() rather than by name, because the invisible BOM makes a by-name rename fail.
questions.orig <- read.csv(path.questions) %>%
  set_names(c("qid", "qid_full"))

# Reading in the level codebook (one row per response option: numeric code, label, and the qid it belongs to)
levels.orig <- read.csv(path.levels) %>%
  set_names(c("value", "label", "qid"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.3. Splitting the question text into a stem and an item ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The vendor packs every matrix and multi-select item into one string as "STEM --- ITEM". Single-response questions
# have no " --- " and therefore no item text. "NO TEXT" is the vendor's placeholder for administrative columns.
# Splitting the two apart here, because script 02 needs the stem and the item separately.
questions <- questions.orig %>%
  mutate(qid_full      = case_when(qid_full == "NO TEXT" ~ NA_character_,       # dropping the placeholder
                                   TRUE ~ qid_full),
         has_item      = str_detect(coalesce(qid_full, ""), " --- "),           # coalesce so NA does not propagate into the test
         question_text = case_when(has_item ~ str_trim(str_extract(qid_full, "^.*?(?= --- )")),   # everything BEFORE the separator
                                   TRUE ~ qid_full),
         item_text     = case_when(has_item ~ str_trim(str_replace(qid_full, "^.*? --- ", "")),   # everything AFTER the separator
                                   TRUE ~ NA_character_),
         base          = str_remove(qid, "_[0-9]+$"),                           # "BPC11_4" -> "BPC11", the question family
         item_index    = as.integer(str_extract(qid, "(?<=_)[0-9]+$"))) %>%     # "BPC11_4" -> 4, the item's position in the battery
  select(-has_item)

# The weight column `wts` appears in the level codebook but is missing from the question codebook, so the delivered file has
# a column the vendor's own question codebook does not describe. Adding it here so the codebook is a complete description of the data.
questions <- questions %>%
  add_row(qid = "wts", qid_full = NA_character_, question_text = NA_character_,
          item_text = NA_character_, base = "wts", item_index = NA_integer_)




##### #
#### #
### ################################################################################################################################################# #
# Part B. Item vocabularies ------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# This instrument reuses ten item lists across 90 question families, and position k means the same item in every battery that shares a
# list (verified in Part F below against the vendor's own question codebook). Declaring each list once here is what keeps the variable
# names consistent across, for example, BPC11 / BPC12 / BPC19 / BPC20 / BPC27 / BPC28, which all ask the same ten options.
#
# Each vector maps the vendor's verbatim item text (the name) to the short tag used in the variable name (the value), so that
# BPC11_4 becomes reg_act_src_news_media rather than something that has to be looked up.

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.1. Kinds of election information sought (BPC1) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
vocab.seek <- c(
  "Information about how to register and vote" = "register",
  "Information about how elections are run"    = "elections_run",
  "Information about who won an election"      = "who_won"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.2. Main election-news sources (BPC2) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
vocab.source <- c(
  "Local or regional television"                                     = "tv_local",
  "National television"                                              = "tv_national",
  "Radio"                                                            = "radio",
  "Local or state news outlets, print or online"                     = "news_local",
  "National news outlets, print or online"                           = "news_national",
  "Social media"                                                     = "social",
  "Search through Google or other search engines"                    = "search",
  "Podcasts"                                                         = "podcast",
  "Newsletters, blogs, or online forums"                             = "newsletter",
  "News aggregator apps (e.g. NewsBreak, Smart News, Apple News)"    = "aggregator",
  "AI-enabled chatbot (e.g. ChatGPT, Gemini, or Claude)"             = "chatbot",
  "Friends and/or family"                                            = "friends_family",
  "Other, please specify"                                            = "other",
  "I am not interested in election news or information"              = "not_interested",
  "Not sure"                                                         = "not_sure"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.3. National television networks (BPC3) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
vocab.tvnet <- c(
  "ABC News"                = "abc",
  "CBS News"                = "cbs",
  "NBC News"                = "nbc",
  "CNN"                     = "cnn",
  "FOX News"                = "fox",
  "MS NOW (formerly MSNBC)" = "msnow",
  "Newsmax"                 = "newsmax",
  "PBS News"                = "pbs",
  "One America News (OAN)"  = "oan",
  "NewsNation"              = "newsnation",
  "CNBC"                    = "cnbc",
  "Noticias Telemundo"      = "telemundo",
  "N+ Univision"            = "univision",
  "Other, please specify"   = "other",
  "Not sure"                = "not_sure"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.4. Radio (BPC4) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
vocab.radio <- c(
  "Public radio news (e.g., NPR or your local public radio station)"     = "public",
  "Conservative talk radio (e.g., Sean Hannity, Mark Levin, Glenn Beck)" = "conservative",
  "Liberal or progressive talk radio"                                    = "liberal",
  "A local AM/FM news station"                                           = "local_amfm",
  "Other, please specify"                                                = "other",
  "Not sure"                                                             = "not_sure"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.5. Print and online news outlets (BPC5) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
vocab.outlet <- c(
  "The New York Times"                          = "nyt",
  "The Wall Street Journal"                     = "wsj",
  "The Washington Post"                         = "wapo",
  "USA Today"                                   = "usatoday",
  "The Atlantic (theatlantic.com)"              = "atlantic",
  "The Associated Press"                        = "ap",
  "Reuters"                                     = "reuters",
  "Yahoo News"                                  = "yahoo",
  "BBC News (bbc.com)"                          = "bbc",
  "NPR News (npr.org)"                          = "npr",
  "CNN.com"                                     = "cnn",
  "Fox News (foxnews.com)"                      = "fox",
  "New York Post (nypost.com)"                  = "nypost",
  "The Guardian (theguardian.com)"              = "guardian",
  "Bloomberg (bloomberg.com)"                   = "bloomberg",
  "Politico (politico.com)"                     = "politico",
  "Axios"                                       = "axios",
  "The Hill (thehill.com)"                      = "thehill",
  "NBC News (nbcnews.com)"                      = "nbc",
  "Huffington Post (huffingtonpost.com)"        = "huffpost",
  "One America News (oann.com)"                 = "oan",
  "Daily Caller (dailycaller.com)"              = "dailycaller",
  "Newsmax.com"                                 = "newsmax",
  "Breitbart.com"                               = "breitbart",
  "Univision (univision.com/noticias)"          = "univision",
  "Noticias Telemundo (telemundo.com/noticias)" = "telemundo",
  "Al Jazeera (aljazeera.com)"                  = "aljazeera",
  "A local newspaper or its website"            = "local_paper",
  "Other, please specify"                       = "other",
  "Not sure"                                    = "not_sure"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.6. Social media platforms (BPC6) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
vocab.social <- c(
  "Facebook"              = "facebook",
  "YouTube"               = "youtube",
  "Instagram"             = "instagram",
  "TikTok"                = "tiktok",
  "Reddit"                = "reddit",
  "Snapchat"              = "snapchat",
  "Threads"               = "threads",
  "X (formerly Twitter)"  = "x_twitter",
  "Bluesky"               = "bluesky",
  "LinkedIn"              = "linkedin",
  "Nextdoor"              = "nextdoor",
  "Discord"               = "discord",
  "WhatsApp"              = "whatsapp",
  "Truth Social"          = "truth_social",
  "Other, please specify" = "other",
  "Not sure"              = "not_sure"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.7. AI chatbots (BPC10) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
vocab.bot <- c(
  "ChatGPT"               = "chatgpt",
  "Google Gemini"         = "gemini",
  "Microsoft Copilot"     = "copilot",
  "Claude"                = "claude",
  "Meta AI"               = "meta_ai",
  "Grok"                  = "grok",
  "Perplexity AI"         = "perplexity",
  "Other, please specify" = "other",
  "Not sure"              = "not_sure"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.8. Sources of election-administration information (BPC11/12/19/20/27/28) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
vocab.infosource <- c(
  "Local election officials (town, city, or county)"                                     = "local_officials",
  "State election officials"                                                             = "state_officials",
  "Federal election website (e.g. the U.S. Election Assistance Commission or vote.gov)"  = "federal_site",
  "News media (TV, print, online, or radio)"                                             = "news_media",
  "Online search engine"                                                                 = "search",
  "Social media or online influencers"                                                   = "social",
  "Friends and/or family"                                                                = "friends_family",
  "Candidate, campaign, or political party"                                              = "campaign",
  "Advocacy organization (e.g. election integrity or voting rights group)"               = "advocacy",
  "AI-enabled chatbot (e.g. ChatGPT, Gemini, or Claude)"                                 = "chatbot",
  "Other, please specify"                                                                = "other",
  "Don't know"                                                                           = "dont_know"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.9. Election concerns (BPC44) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
vocab.concern <- c(
  "Inaccurate or misleading information about elections"                         = "misinfo",
  "Use of AI to spread disinformation or manipulate public opinion"              = "ai_disinfo",
  "Interference from foreign countries or entities"                              = "foreign",
  "Counting ineligible votes (e.g. duplicate, non-citizen, or deceased voters)"  = "ineligible_votes",
  "Eligible voters being prevented from voting or having valid ballots rejected" = "eligible_blocked",
  "Biased or inaccurate counting of ballots"                                     = "biased_count",
  "Illegal or improper use of mail-in ballots or drop boxes"                     = "mail_ballots",
  "Attempts to overturn the results of a fair election"                          = "overturn",
  "Election outcomes being distorted by unfairly drawn districts"                = "gerrymander",
  "Presence of guns, violence, or intimidation at voting locations"              = "guns_intimidation",
  "Violence or civil unrest after election day"                                  = "post_violence",
  "Problems at polling places, such as long lines or equipment failures"         = "polling_problems",
  "Federal law enforcement, such as ICE, being deployed in your community"       = "ice_deployment",
  "Federal or state law enforcement seizing ballots or voting machines"          = "ballot_seizure"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.10. Uses of AI in elections (BPC38) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -
vocab.ai.use <- c(
  "Voters using AI chatbots to find out information about a candidate or an issue on the ballot" = "voter_candidate_info",
  "Voters using AI chatbots to find out information about how to cast their ballot"              = "voter_how_to_vote",
  "Voters using AI chatbots to find information on how to register to vote"                      = "voter_how_to_register",
  "Voters using AI chatbots to decide which candidate to vote for"                               = "voter_decide",
  "Voters using AI chatbots to learn about how candidates' positions align with their personal values and priorities" = "voter_values_align",
  "Political campaigns using AI to create content, including advertisements for voters, without disclosing that AI was used in the process" = "campaign_undisclosed",
  "Political campaigns using AI to create content, including advertisements for voters, with clear disclosure that AI was used in the process" = "campaign_disclosed",
  "Candidates using AI to edit or touch-up photos or videos for their political advertisements"  = "cand_photo_edit",
  "Candidates using AI to tailor their political advertisements to individual voters"            = "cand_microtarget",
  "Candidates using AI chatbots to answer voters' questions about campaigns"                     = "cand_answer_questions"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## B.11. Collecting the vocabularies into one lookup list ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Naming each element after the `vocab` value used in the base map in Part C, so that a question family declares its item list by name
vocab.lookup <- list(seek       = vocab.seek,
                     source     = vocab.source,
                     tvnet      = vocab.tvnet,
                     radio      = vocab.radio,
                     outlet     = vocab.outlet,
                     social     = vocab.social,
                     bot        = vocab.bot,
                     infosource = vocab.infosource,
                     concern    = vocab.concern,
                     ai_use     = vocab.ai.use)




##### #
#### #
### ################################################################################################################################################# #
# Part C. Variable naming and description maps ------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.1. Item batteries: question family -> name prefix, item vocabulary, type, and skip logic ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# For every battery, `prefix` + "_" + the item's short tag gives the variable name. So BPC11_4 (whose item text is
# "News media (TV, print, online, or radio)") becomes reg_act_src + "_" + news_media = reg_act_src_news_media.
#
# How to read a variable name:
#   src_*                                the master "where do you get election news" battery (BPC2)
#   tvnet_ / radio_ / outlet_ / social_ / bot_*   the source-specific follow-ups to BPC2
#   reg_ / run_ / won_*                  the three information-need blocks — how to REGister and vote, how elections are RUN,
#                                        and who WON — each split into an `_act_` arm (respondents who do seek that information)
#                                        and a `_hyp_` arm (respondents who never seek it, asked hypothetically)
#   *_use_ / *_src_                      a select-one-or-more indicator
#   *_rank_                              the drag-and-drop rank follow-up to a select battery
#
# `base_condition` is the questionnaire's own [IF ...] line, carried through so that any percentage can be computed on the
# right denominator without going back to the .docx.

base.map <- tribble(
  ~base,    ~prefix,          ~vocab,       ~var_type,     ~scale_family, ~base_condition,
  "BPC1",   "seek",           "seek",       "ordinal",     "freq_seek5",  "All respondents",
  "BPC2",   "src_use",        "source",     "multiselect", "select2",     "All respondents",
  "BPC2a",  "src_rank",       "source",     "rank",        "rank12",      "Items selected at BPC2 (carry forward)",
  "BPC3",   "tvnet_use",      "tvnet",      "multiselect", "select2",     "BPC2_2 = 1 (national television)",
  "BPC3a",  "tvnet_rank",     "tvnet",      "rank",        "rank13",      "BPC2_2 = 1; items selected at BPC3",
  "BPC4",   "radio_use",      "radio",      "multiselect", "select2",     "BPC2_3 = 1 (radio)",
  "BPC4a",  "radio_rank",     "radio",      "rank",        "rank4",       "BPC2_3 = 1; items selected at BPC4",
  "BPC5",   "outlet_use",     "outlet",     "multiselect", "select2",     "BPC2_4 = 1 or BPC2_5 = 1 (print/online outlets)",
  "BPC6",   "social_use",     "social",     "multiselect", "select2",     "BPC2_6 = 1 (social media)",
  "BPC10",  "bot_use",        "bot",        "multiselect", "select2",     "BPC2_11 = 1 (AI chatbot). NOTE: the delivered stem says 'select all that apply' but the questionnaire specifies SELECT UP TO 2, and the data confirms the cap of 2 was programmed",
  "BPC11",  "reg_act_src",    "infosource", "multiselect", "select2",     "BPC1_1 in 1:4 (does seek registration info)",
  "BPC11a", "reg_act_rank",   "infosource", "rank",        "rank10",      "Items selected at BPC11",
  "BPC12",  "reg_hyp_src",    "infosource", "multiselect", "select2",     "BPC1_1 = 5 (never seeks registration info)",
  "BPC12a", "reg_hyp_rank",   "infosource", "rank",        "rank10",      "Items selected at BPC12",
  "BPC15",  "reg_act_social", "social",     "multiselect", "select2",     "BPC11_6 selected (social media)",
  "BPC16",  "reg_hyp_social", "social",     "multiselect", "select2",     "BPC12_6 selected (social media)",
  "BPC17",  "reg_act_bot",    "bot",        "multiselect", "select2",     "BPC11_10 selected (AI chatbot)",
  "BPC18",  "reg_hyp_bot",    "bot",        "multiselect", "select2",     "BPC12_10 selected (AI chatbot)",
  "BPC19",  "run_act_src",    "infosource", "multiselect", "select2",     "BPC1_2 in 1:4 (does seek election-administration info)",
  "BPC19a", "run_act_rank",   "infosource", "rank",        "rank10",      "Items selected at BPC19",
  "BPC20",  "run_hyp_src",    "infosource", "multiselect", "select2",     "BPC1_2 = 5 (never seeks election-administration info)",
  "BPC20a", "run_hyp_rank",   "infosource", "rank",        "rank10",      "Items selected at BPC20",
  "BPC23",  "run_act_social", "social",     "multiselect", "select2",     "BPC19_6 selected (social media)",
  "BPC24",  "run_hyp_social", "social",     "multiselect", "select2",     "BPC20_6 selected (social media)",
  "BPC25",  "run_act_bot",    "bot",        "multiselect", "select2",     "BPC19_10 selected (AI chatbot)",
  "BPC26",  "run_hyp_bot",    "bot",        "multiselect", "select2",     "BPC20_10 selected (AI chatbot)",
  "BPC27",  "won_act_src",    "infosource", "multiselect", "select2",     "BPC1_3 in 1:4 (does seek results info)",
  "BPC27a", "won_act_rank",   "infosource", "rank",        "rank10",      "Items selected at BPC27",
  "BPC28",  "won_hyp_src",    "infosource", "multiselect", "select2",     "BPC1_3 = 5 (never seeks results info)",
  "BPC28a", "won_hyp_rank",   "infosource", "rank",        "rank10",      "Items selected at BPC28",
  "BPC31",  "won_act_social", "social",     "multiselect", "select2",     "BPC27_6 selected (social media)",
  "BPC32",  "won_hyp_social", "social",     "multiselect", "select2",     "BPC28_6 selected (social media)",
  "BPC33",  "won_act_bot",    "bot",        "multiselect", "select2",     "BPC27_10 selected (AI chatbot)",
  "BPC34",  "won_hyp_bot",    "bot",        "multiselect", "select2",     "BPC28_10 selected (AI chatbot)",
  "BPC38",  "ai_ok",          "ai_use",     "ordinal",     "goodbad5_dk", "All respondents (items 4 and 5 are a hidden 50/50 split)",
  "BPC44",  "concern",        "concern",    "ordinal",     "concern4_dk", "All respondents"
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.2. Single-response variables: qid -> name, type, and skip logic ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Every delivered column that is not part of an item battery. `xdem*`, `x*` and `seg_*` are the vendor's own precomputed
# recodes and segments rather than questions the respondent saw, which is why several of them have no question text.
single.map <- tribble(
  ~qid,               ~var_name,               ~var_type, ~scale_family,     ~base_condition,
  "ResponseID",       "resp_id",               "id",      "none",            "All respondents",
  "wts",              "weight",                "weight",  "none",            "All respondents",
  "xdemAll",          "sample_rv",             "flag",    "flag1",           "All respondents",
  "demZIP",           "zip",                   "text",    "none",            "All respondents",
  "BPCdem1",          "maga_support",          "ordinal", "endorse4_dk",     "Republicans, including Republican leaners (demPidNoLn = 1 or demPidClos = 2)",
  "BPCdem2",          "progressive_id",        "ordinal", "endorse4_dk",     "Democrats, including Democratic leaners (demPidNoLn = 2 or demPidClos = 1)",
  "BPCdem3",          "vote_likelihood",       "ordinal", "likely10_dk",     "All respondents",
  "BPC13",            "reg_act_official_mode", "nominal", "mode_act5",       "BPC11_1 or BPC11_2 selected (election officials)",
  "BPC14",            "reg_hyp_official_mode", "nominal", "mode_hyp5",       "BPC12_1 or BPC12_2 selected (election officials)",
  "BPC21",            "run_act_official_mode", "nominal", "mode_act5",       "BPC19_1 or BPC19_2 selected (election officials)",
  "BPC22",            "run_hyp_official_mode", "nominal", "mode_hyp5",       "BPC20_1 or BPC20_2 selected (election officials)",
  "BPC29",            "won_act_official_mode", "nominal", "mode_act5",       "BPC27_1 or BPC27_2 selected (election officials)",
  "BPC30",            "won_hyp_official_mode", "nominal", "mode_hyp5",       "BPC28_1 or BPC28_2 selected (election officials)",
  "BPC35",            "ai_prevalence",         "ordinal", "amount5_dk",      "All respondents",
  "BPC36",            "ai_tool_freq",          "ordinal", "freq_seek5",      "All respondents",
  "BPC37",            "ai_detect_conf",        "ordinal", "conf4_dk",        "All respondents",
  "BPC39",            "vote_exp_positive",     "ordinal", "agree5_dk",       "All respondents",
  "BPC40",            "conf_own_vote",         "ordinal", "conf4_novote_dk", "All respondents",
  "BPC41",            "conf_local_votes",      "ordinal", "conf4_dkno",      "All respondents",
  "BPC42",            "conf_state_votes",      "ordinal", "conf4_dkno",      "All respondents",
  "BPC43",            "conf_national_votes",   "ordinal", "conf4_dkno",      "All respondents",
  "BPC45",            "noncitizen_freq",       "ordinal", "freq5_dk",        "All respondents",
  "BPC46",            "noncitizen_alters",     "binary",  "yesno_dk",        "BPC45 in 1:4 (thinks it occurs at all)",
  "BPC47",            "access_vs_integrity",   "nominal", "priority3_dk",    "All respondents",
  "BPC48",            "usps_policy_support",   "ordinal", "support5_dk",     "All respondents",
  "xdemGender",       "gender",                "nominal", "gender2",         "All respondents",
  "age",              "age4",                  "ordinal", "age4",            "All respondents",
  "demAgeGeneration", "generation",            "ordinal", "generation4",     "All respondents",
  "xpid3",            "pid3",                  "nominal", "pid3",            "All respondents",
  "xpidGender",       "pid_gender",            "nominal", "pidgender6",      "All respondents",
  "xdemIdeo3",        "ideo3",                 "ordinal", "ideo3",           "All respondents",
  "xeduc3",           "educ3",                 "ordinal", "educ3",           "All respondents",
  "xdemInc3_us",      "income3",               "ordinal", "income3",         "All respondents",
  "xdemWhite",        "race_white",            "flag",    "flag1",           "All respondents",
  "xdemHispBin",      "race_hispanic",         "flag",    "flag1",           "All respondents",
  "demBlackBin",      "race_black",            "flag",    "flag1",           "All respondents",
  "demRaceOther",     "race_other",            "flag",    "flag1",           "All respondents",
  "xdemUsr",          "rural_urban3",          "ordinal", "urban3",          "All respondents",
  "xdemEmploy",       "employment",            "nominal", "employ8",         "All respondents",
  "demUnion",         "union_member",          "binary",  "yesno",           "All respondents",
  "xsubVote24O",      "vote_2024",             "nominal", "vote24_4",        "All respondents",
  "xreg4",            "region4",               "nominal", "region4",         "All respondents",
  "demRelig",         "religion",              "nominal", "relig12",         "All respondents",
  "demEvang",         "evangelical",           "binary",  "yesno",           "demRelig in {Protestant, Roman Catholic, Something else}",
  "MCEP7",            "disability_work",       "binary",  "yesno",           "Not currently employed (xdemEmploy in 4:8)",
  "nr1",              "country_direction",     "nominal", "direction2",      "All respondents",
  "nr3",              "top_issue",             "nominal", "issue8",          "All respondents",
  "demLGBTQ1",        "sexual_orientation",    "nominal", "lgbtq7",          "All respondents",
  "demLGBTQ2",        "trans_nonbinary",       "binary",  "yesno",           "All respondents",
  "demInsured",       "insured",               "binary",  "insured2",        "All respondents",
  "demInsType",       "insurance_type",        "nominal", "instype7",        "demInsured = 1 (covered)",
  "BPCxdem1",         "seg_likely_voter",      "flag",    "flag1",           "Vendor segment: BPCdem3 >= 8",
  "BPCxdem2",         "seg_maga_rep",          "flag",    "flag1",           "Vendor segment: BPCdem1 in 1:2",
  "BPCxdem3",         "seg_prog_dem",          "flag",    "flag1",           "Vendor segment: BPCdem2 in 1:2",

  # The thirteen columns added in the V2 delivery (2026-09-11). Eight of them are the uncollapsed source items behind
  # recodes that were already in the file, so each now sits next to the collapsed version it was built from:
  # age_years/age4, educ9/educ3, income6/income3, ideo7/ideo3, race5 and the four race flags, insured_recode/insured.
  # Both versions are kept deliberately — only the collapsed ones exist in the 2024 file, so a year-over-year comparison
  # can only be made at the coarse level, while anything 2026-only should use the full detail.
  #
  # NOTE the two party columns are named the opposite way round from what their names suggest. demPidLean is the
  # STRENGTH item ("Would you call yourself a strong ... or a not very strong ...?"), and demPidClos is the LEAN item
  # ("Do you think of yourself as closer to ...?"). They are named here for what they measure, not for what the vendor
  # called them.
  "demPidLean",       "pid_strength",          "ordinal", "strength2",       "Partisans (xpid3 = 1 or 3)",
  "demPidClos",       "pid_lean",              "nominal", "pidclos3",        "Independents (xpid3 = 2)",
  "xdemRealAge",      "age_years",             "numeric", "age_years",       "All respondents",
  "demEduFull",       "educ9",                 "ordinal", "educ9",           "All respondents",
  "demInc",           "income6",               "ordinal", "income6",         "All respondents",
  "demInc2",          "income_top4",           "ordinal", "income_top4",     "demInc = 6 ($100 thousand or more)",
  "demPolIdeo",       "ideo7",                 "ordinal", "ideo7",           "All respondents",
  "demRace",          "race5",                 "nominal", "race5",           "All respondents",
  "demState",         "state",                 "nominal", "state51",         "All respondents",
  "demMarital",       "marital",               "nominal", "marital6",        "All respondents",
  "xdemMarried",      "married",               "binary",  "married2",        "All respondents",
  "Q156",             "job_fulltime",          "nominal", "fulltime3",       "Currently employed (xdemEmploy in 1:3)",
  "xdemInsured",      "insured_recode",        "binary",  "insured_x2",      "All respondents"
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## C.3. Plain-English descriptions of every question family ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The point of this table is that a short variable name should never have to be decoded. `var_label_stem` is a one-line
# description of what a question family measures, written so that it can be read on its own and dropped straight into a
# table heading. Part G pastes it together with the item text to build the `var_label` column:
#
#   var_name   reg_act_src_news_media
#   base       BPC11
#   stem       "Sources used to find information about how to register and vote"
#   item_text  "News media (TV, print, online, or radio)"
#   var_label  "Sources used to find information about how to register and vote: News media (TV, print, online, or radio)"
#
# So the abbreviation in the middle of that name — `reg_act_src` — is spelled out as "sources ACTually used for
# REGistration information", and the variable no longer needs interpreting.
#
# The verbatim vendor question text is kept alongside it in `question_text`; this column is the readable summary, not a
# replacement for the source text.

label.map <- tribble(
  ~base,              ~var_label_stem,
  # The three information needs, and the master source battery
  "BPC1",             "How often seeks out each kind of election information",
  "BPC2",             "Main sources for election news and information",
  "BPC2a",            "Rank among main sources for election news and information",
  # Source-specific follow-ups to BPC2
  "BPC3",             "National television networks used for election news",
  "BPC3a",            "Rank among national television networks used for election news",
  "BPC4",             "Radio programming used for election news",
  "BPC4a",            "Rank among radio programming used for election news",
  "BPC5",             "Print and online news outlets used for election news",
  "BPC6",             "Social media platforms used for election news",
  "BPC10",            "AI chatbots used for election news",
  # Information need 1: how to register and vote
  "BPC11",            "Sources used to find information about how to register and vote",
  "BPC11a",           "Rank among sources used to find information about how to register and vote",
  "BPC12",            "Sources would use to find information about how to register and vote",
  "BPC12a",           "Rank among sources would use to find information about how to register and vote",
  "BPC13",            "How contacts election officials about how to register and vote",
  "BPC14",            "How would contact election officials about how to register and vote",
  "BPC15",            "Social media platforms used for information about how to register and vote",
  "BPC16",            "Social media platforms would use for information about how to register and vote",
  "BPC17",            "AI chatbots used for information about how to register and vote",
  "BPC18",            "AI chatbots would use for information about how to register and vote",
  # Information need 2: how elections are run
  "BPC19",            "Sources used to find information about how elections are run",
  "BPC19a",           "Rank among sources used to find information about how elections are run",
  "BPC20",            "Sources would use to find information about how elections are run",
  "BPC20a",           "Rank among sources would use to find information about how elections are run",
  "BPC21",            "How contacts election officials about how elections are run",
  "BPC22",            "How would contact election officials about how elections are run",
  "BPC23",            "Social media platforms used for information about how elections are run",
  "BPC24",            "Social media platforms would use for information about how elections are run",
  "BPC25",            "AI chatbots used for information about how elections are run",
  "BPC26",            "AI chatbots would use for information about how elections are run",
  # Information need 3: who won
  "BPC27",            "Sources used to find information about who won an election",
  "BPC27a",           "Rank among sources used to find information about who won an election",
  "BPC28",            "Sources would use to find information about who won an election",
  "BPC28a",           "Rank among sources would use to find information about who won an election",
  "BPC29",            "How contacts election officials about who won an election",
  "BPC30",            "How would contact election officials about who won an election",
  "BPC31",            "Social media platforms used for information about who won an election",
  "BPC32",            "Social media platforms would use for information about who won an election",
  "BPC33",            "AI chatbots used for information about who won an election",
  "BPC34",            "AI chatbots would use for information about who won an election",
  # Artificial intelligence
  "BPC35",            "How much of the election information seen is AI-generated",
  "BPC36",            "How frequently uses AI tools such as chatbots",
  "BPC37",            "Confidence could tell whether election content was AI-generated",
  "BPC38",            "Whether a given use of AI in elections is a good or a bad thing",
  # Voting experience, confidence, and integrity
  "BPC39",            "Agreement that own voting experience is a positive one",
  "BPC40",            "Confidence own vote will be counted as intended",
  "BPC41",            "Confidence votes in own community will be counted as intended",
  "BPC42",            "Confidence votes in own state will be counted as intended",
  "BPC43",            "Confidence votes nationwide will be counted as intended",
  "BPC44",            "Level of concern about each possible election problem",
  "BPC45",            "How often illegal noncitizen voting occurs",
  "BPC46",            "Whether illegal noncitizen voting changes election outcomes",
  "BPC47",            "Higher priority: easier for eligible voters, or harder for ineligible voters",
  "BPC48",            "Support for the Postal Service's August mail-ballot policy change",
  # Political identity and turnout intent
  "BPCdem1",          "Supports the MAGA movement",
  "BPCdem2",          "Considers self a progressive",
  "BPCdem3",          "Self-rated likelihood of voting in the November 2026 midterm elections",
  "BPCxdem1",         "Vendor segment: likely midterm voter",
  "BPCxdem2",         "Vendor segment: MAGA Republican",
  "BPCxdem3",         "Vendor segment: progressive Democrat",
  # Demographics
  "age",              "Age, four bands",
  "demAgeGeneration", "Generation",
  "demBlackBin",      "Race: Black",
  "demEvang",         "Describes self as evangelical or born-again Christian",
  "demInsType",       "Type of health insurance coverage",
  "demInsured",       "Covered by health insurance",
  "demLGBTQ1",        "Sexual orientation",
  "demLGBTQ2",        "Transgender or non-binary",
  "demRaceOther",     "Race: other than White, Black, or Hispanic",
  "demRelig",         "Religious affiliation",
  "demUnion",         "Member of a labor union household",
  "demZIP",           "Five-digit ZIP code of residence",
  "MCEP7",            "Has a disability that prevents working",
  "xdemEmploy",       "Employment status",
  "xdemGender",       "Gender",
  "xdemHispBin",      "Ethnicity: Hispanic",
  "xdemIdeo3",        "Political ideology, three categories",
  "xdemInc3_us",      "Household income, three bands",
  "xdemUsr",          "Community type: urban, suburban, or rural",
  "xdemWhite",        "Race: White",
  "xeduc3",           "Educational attainment, three categories",
  "xpid3",            "Party identification, three categories, leaners not assigned",
  "xpidGender",       "Party identification crossed with gender",
  "xreg4",            "Census region, four categories",
  "xsubVote24O",      "Recalled 2024 presidential vote",
  # The uncollapsed demographics added in the V2 delivery, each named for what it measures rather than for the
  # vendor's column name. Where one is the source of a collapsed variable already in the file, the description says so.
  "demPidLean",       "Party identification strength among partisans",
  "demPidClos",       "Party an independent leans toward",
  "xdemRealAge",      "Age in completed years",
  "demEduFull",       "Educational attainment, nine categories",
  "demInc",           "Household income, six bands",
  "demInc2",          "Household income within the top band, four bands",
  "demPolIdeo",       "Political ideology, seven-point scale",
  "demRace",          "Race, five categories",
  "demState",         "State of residence",
  "demMarital",       "Marital status",
  "xdemMarried",      "Married",
  "Q156",             "Whether the respondent's job is full-time or part-time",
  "xdemInsured",      "Covered by health insurance, vendor recode duplicating demInsured",
  # Other substantive questions
  "nr1",              "Whether the country is headed in the right direction",
  "nr3",              "Most important issue",
  # Administrative
  "ResponseID",       "Respondent identifier",
  "wts",              "Morning Consult survey weight, registered-voter target",
  "xdemAll",          "In the registered-voter sample"
)




##### #
#### #
### ################################################################################################################################################# #
# Part D. Response scales --------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.1. Declaring each scale family once ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# WARNING, worth reading once. Twenty-six of the 46 ordinal scales in this instrument run BACKWARDS in the raw file:
# 1 = Very confident, 1 = Very concerned, 1 = Strongly agree, 1 = Very good thing. Taking a mean of the raw codes
# therefore gives the opposite of what the variable name says, and taking a mean with "Don't know" left in as its numeric
# code (usually 5 or 6, i.e. off the top of the scale) corrupts it further.
#
# `low_to_high` fixes both problems at once. It lists the substantive codes from LEAST to MOST of the construct the
# variable is named after, so the integer companion built in script 02 always runs in the intuitive direction — higher
# means more confident, more concerned, more frequent, more supportive.
#
# `nonscale` lists the codes that are not points on the scale at all, so they can be set to missing rather than sorted to
# the top of it. These are declared by CODE and never by matching the label text, because the instrument uses two
# different don't-know wordings — plain "Don't know" on 20 variables and "Don't know / No opinion" on 24 — plus "Not sure"
# as a multi-select anchor. Any recode driven by a text pattern would silently miss about half the instrument.

scale.spec <- tribble(
  ~scale_family,     ~low_to_high,                      ~nonscale, ~scale_note,
  "none",            "",                                "",        "Administrative column; no value labels",
  "flag1",           "",                                "",        "Indicator: only code 1 is labelled; missing means not in group",
  "select2",         "",                                "",        "1 = Selected, 2 = Not Selected (not 1/0)",
  "rank4",           "1,2,3,4",                         "",        "Rank position among carried-forward items",
  "rank10",          "1,2,3,4,5,6,7,8,9,10",            "",        "Rank position among carried-forward items",
  "rank12",          "1,2,3,4,5,6,7,8,9,10,11,12",      "",        "Rank position among carried-forward items",
  "rank13",          "1,2,3,4,5,6,7,8,9,10,11,12,13",   "",        "Rank position among carried-forward items",
  "freq_seek5",      "5,1,2,3,4",                       "",        "All five codes are substantive: 5 = Never is the LOW end of the scale, not a missing code",
  "concern4_dk",     "4,3,2,1",                         "5",       "Reversed in the raw file: 1 = Very concerned",
  "goodbad5_dk",     "5,4,3,2,1",                       "6",       "Reversed in the raw file: 1 = Very good thing. Integer runs bad -> good",
  "conf4_dk",        "4,3,2,1",                         "5",       "Reversed in the raw file: 1 = Very confident",
  "conf4_dkno",      "4,3,2,1",                         "5",       "Reversed in the raw file: 1 = Very confident",
  "conf4_novote_dk", "4,3,2,1",                         "5,6",     "Reversed; code 5 is 'I do not plan to vote', a separate universe rather than a scale point",
  "agree5_dk",       "5,4,3,2,1",                       "6",       "Reversed in the raw file: 1 = Strongly agree. Integer runs disagree -> agree",
  "support5_dk",     "5,4,3,2,1",                       "6",       "Reversed in the raw file: 1 = Strongly support. Integer runs oppose -> support",
  "endorse4_dk",     "4,3,2,1",                         "5",       "Reversed in the raw file: 1 = Yes, strongly. Integer runs no -> strong yes",
  "likely10_dk",     "1,2,3,4,5,6,7,8,9,10",            "11",      "Self-reported 0-10 style vote likelihood; already ascending",
  "amount5_dk",      "5,4,3,2,1",                       "6",       "Reversed in the raw file: 1 = Almost all of it. Integer runs none -> almost all",
  "freq5_dk",        "5,4,3,2,1",                       "6",       "Reversed in the raw file: 1 = Very frequently. Integer runs never -> very frequently",
  "yesno",           "",                                "",        "1 = Yes, 2 = No",
  "yesno_dk",        "",                                "3",       "1 = Yes, 2 = No, 3 = Don't know",
  "insured2",        "",                                "",        "1 = Covered, 2 = Not covered",
  "direction2",      "",                                "",        "1 = Right Direction, 2 = Wrong Track",
  "mode_act5",       "",                                "4,5",     "Nominal mode of contact; 4 = Other, 5 = Don't know",
  "mode_hyp5",       "",                                "4,5",     "Nominal mode of contact, hypothetical wording; 4 = Other, 5 = Don't know",
  "priority3_dk",    "",                                "3",       "Nominal forced choice; 3 = Don't know / No opinion",
  "pid3",            "",                                "",        "Party identification with leaners NOT assigned to a party. Vendor labels read 'PID: Dem (no lean)' and are cleaned in Part E to Dem / Ind / Rep. For the version that assigns leaners, see pid5 and pid3_lean in script 02",
  "pidgender6",      "",                                "",        "Party identification crossed with gender",
  "gender2",         "",                                "",        "Binary only: no non-binary category was delivered",
  "age4",            "1,2,3,4",                         "",        "Ascending age band",
  "generation4",     "1,2,3,4",                         "",        "Ascending age: Gen Z < Millennials < Gen X < Baby Boomers. Birth years (Gen Z 1997-2012, Millennials 1981-1996, Gen X 1965-1980, Boomers 1946-1964) are trimmed from the labels in Part E. No pre-1946 category exists",
  "ideo3",           "1,2,3",                           "",        "Left to right: Liberal < Moderate < Conservative. Collapsed by the vendor from a 7-point scale, where Liberal = 1-3, Moderate = 4, and Conservative = 5-7; those ranges are trimmed from the labels in Part E",
  "educ3",           "1,2,3",                           "",        "Ascending attainment",
  "income3",         "1,2,3",                           "",        "Ascending income band",
  "urban3",          "1,2,3",                           "",        "Ordered by decreasing density: Urban < Suburban < Rural. Treat as ordinal only if that density gradient is the construct you want; otherwise read it as nominal",
  "employ8",         "",                                "8",       "Nominal; 8 = Other",
  "relig12",         "",                                "11,12",   "Nominal; 11 = Something else, 12 = Nothing in particular",
  "lgbtq7",          "",                                "7",       "Nominal; 7 = Something else",
  "instype7",        "",                                "",        "Nominal plan type",
  "issue8",          "",                                "8",       "Nominal top issue; 8 = Other",
  "vote24_4",        "",                                "",        "Nominal recalled 2024 presidential vote, with a non-voter category",
  "region4",         "",                                "",        "Nominal census region",

  # The scales behind the thirteen V2 columns
  "strength2",       "2,1",                             "",        "Party identification strength among partisans. Reversed in the raw file: 1 = Strong. The integer runs not-very-strong -> strong. The vendor's labels pipe the respondent's own party into the text ('Strong ${q://QID70/...}'), which Part E replaces with 'Strong partisan' / 'Not very strong partisan' — read it alongside pid3 to know which party",
  "pidclos3",        "",                                "",        "Which party an independent leans toward. All three codes are substantive; 'Neither' is a real answer, not a missing code, and is what leaves a respondent a pure independent in pid7",
  "age_years",       "",                                "",        "Age in completed years, 18 to 92. IMPORTANT: the vendor's level codebook lists 74 codes for this variable mapping 1 = 18 up to 74 = 92, but the delivered data holds the AGE ITSELF, not those codes. Applying the level map would destroy the variable, so it is typed 'numeric' and script 02 leaves it alone. Verified in script 02 Part G against the age4 bands",
  "educ9",           "1,2,3,4,5,6,7,8,9",               "",        "Ascending attainment, the uncollapsed source of educ3 (educ3 collapses 1-6 / 7 / 8-9). This is the version that can be cut at the 'diploma divide' — high school or less (1-3) against any college (4-9) — which educ3 cannot express",
  "income6",         "1,2,3,4,5,6",                     "",        "Ascending household income band, the uncollapsed source of income3. Code 6 ($100 thousand or more) is broken out further by income_top4",
  "income_top4",     "1,2,3,4",                         "",        "Ascending band within '$100 thousand or more'. Asked only of the 551 respondents at income6 = 6, so it is a follow-up rather than a separate measure; combine the two for a nine-band income variable",
  "ideo7",           "1,2,3,4,5,6,7",                   "8",       "Left to right: Very liberal ... Very conservative. The uncollapsed source of ideo3 (which collapses 1-3 / 4 / 5-7). Code 8 is 'Don't Know', and it is exactly the 157 respondents who are missing on ideo3",
  "race5",           "",                                "",        "Nominal race, the uncollapsed source of the four race flags. American Indian (41), Asian American (118) and Other (94) were all folded into race_other (253), so this is the only way to identify Asian American respondents. Hispanic origin is asked separately and overlays all five categories",
  "state51",         "",                                "",        "Nominal state of residence, including the District of Columbia. Code 52 ('I do not live in the continental United States') exists in the codebook but no respondent took it",
  "marital6",        "",                                "",        "Nominal marital status",
  "married2",        "",                                "",        "Vendor recode of marital: 1 = Married, 2 = everything else",
  "fulltime3",       "",                                "3",       "Nominal; 3 = Don't Know, declared by the vendor but taken by no respondent",
  "insured_x2",      "",                                "",        "Vendor recode of health insurance status. An exact duplicate of `insured` (demInsured) on all 3,144 rows, verified in script 02 Part G; it is kept only because the delivery contains it. Use `insured`"
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## D.2. Expanding the scale families into one row per response code ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Turning the comma-separated `low_to_high` and `nonscale` strings above into a long table of one row per
# (scale_family, value), which is what joins onto the level codebook in Part G.
# Splitting into list columns first, then dropping the empty strings that str_split() leaves behind for a blank field.
scale.split <- scale.spec %>%
  transmute(scale_family,
            ord = str_split(low_to_high, ","),
            non = str_split(nonscale, ",")) %>%
  mutate(ord = map(ord, ~ as.numeric(.x[.x != ""])),
         non = map(non, ~ as.numeric(.x[.x != ""])))

# Substantive codes: `ord_position` is the code's place in the low-to-high ordering, which is simply its position in the vector
scale.ordinal <- scale.split %>%
  transmute(scale_family,
            value        = ord,
            ord_position = map(ord, seq_along)) %>%
  unnest(c(value, ord_position)) %>%
  mutate(ord_position = as.integer(ord_position),
         is_nonscale  = FALSE)

# Off-scale codes: no ordinal position by construction
scale.nonscale <- scale.split %>%
  transmute(scale_family, value = non) %>%
  unnest(value) %>%
  mutate(ord_position = NA_integer_,
         is_nonscale  = TRUE)

# Stacking the two
scale.map <- bind_rows(scale.ordinal, scale.nonscale) %>%
  select(scale_family, value, ord_position, is_nonscale)




##### #
#### #
### ################################################################################################################################################# #
# Part E. Cleaning the vendor's response labels ----------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.1. Stripping variable-name prefaces from response labels ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Morning Consult prefaces the response labels of its own demographic recodes with the variable's name: the levels of
# xdemGender arrive as "Gender: Male" and "Gender: Female", age as "Age: 18-34", xpid3 as "PID: Dem (no lean)". That is
# useful in a vendor banner table, where a column heading has to identify itself, and useless in a data frame, where the
# column name is already sitting on top of the values. It also makes every cross-tab and plot legend wider than it needs
# to be. Stripping the prefaces here, in the codebook, so script 02 never sees them.
#
# The rule is deliberately data-driven rather than a list of prefaces to delete, because a hard-coded list would go stale
# the moment the vendor adds a recode:
#
#   strip "<preface>: " from a variable's labels only if EVERY labelled level of that variable
#   begins with the SAME preface.
#
# That condition is what makes the rule safe. It catches all 15 genuine prefaces (Age, Gender, PID, PID/Gender, Ideo,
# Educ, Income, Community, Employ, Ethnicity, 4-Region, 2024 Vote), and it automatically protects the one variable where
# the text before the colon is the substantive label rather than a preface: demAgeGeneration, whose four levels read
# "GenZers: 1997-2012", "Millennials: 1981-1996", "GenXers: 1965-1980", and "Baby Boomers: 1946-1964". Those four
# prefaces all differ, the condition fails, and nothing is stripped — which is correct, since stripping would leave
# behind four bare year ranges. E.2 below shortens those four by hand instead.

# Identifying the preface on every label that has one. Requiring at least one character and no more than 14 before the
# colon, and no colon inside the preface itself, so that a long label that merely contains a colon is not misread.
labels.prefaced <- levels.orig %>%
  filter(!is.na(label), label != "NO TEXT") %>%
  mutate(preface = case_when(str_detect(label, "^[^:]{1,14}: ") ~ str_extract(label, "^[^:]{1,14}(?=: )"),
                             TRUE ~ NA_character_))

# Reducing to one row per variable and asking the two questions the rule depends on:
#   do ALL of this variable's labels carry a preface, and is it always the SAME one?
qids.to.strip <- labels.prefaced %>%
  group_by(qid) %>%
  summarise(n_levels     = n(),                                  # how many labelled levels the variable has
            n_prefaced   = sum(!is.na(preface)),                  # how many of them carry a preface
            n_prefaces   = n_distinct(preface, na.rm = TRUE),        # how many DIFFERENT prefaces appear
            .groups = "drop") %>%
  filter(n_prefaced == n_levels,                                 # every level is prefaced ...
         n_prefaces == 1) %>%                                    # ... and always with the same preface
  pull(qid)

# Doing the strip
levels.stripped <- levels.orig %>%
  mutate(label = case_when(qid %in% qids.to.strip ~ str_remove(label, "^[^:]{1,14}: "),
                           TRUE ~ label))


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## E.2. Hand fixes to individual labels ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Four kinds of leftover, all of them cases the general rule in E.1 cannot and should not handle:
#
#   1. pid3. Once "PID: " is gone the labels read "Dem (no lean)", "Ind (no lean)", "Rep (no lean)". The parenthetical is
#      a note about how the variable was derived, not part of any respondent's answer, so it belongs in the codebook's
#      scale_note (where Part D now records it) rather than on all 3,144 rows. Cutting it leaves Dem / Ind / Rep.
#   2. ideo3. Same situation: "Liberal (1-3)" records that the vendor collapsed a 7-point scale, which is now in scale_note.
#   3. generation. Shortening the four labels to the generation name and dropping the birth years, which are in scale_note.
#   4. educ3. "< College" is the vendor's wording. Spelling it out, because "<" reads badly in a table and in a plot axis.
#
# Note what is NOT touched: parentheses that are part of the question as the respondent read it, such as
# "AI-enabled chatbot (e.g. ChatGPT, Gemini, or Claude)" or "MS NOW (formerly MSNBC)". This is why the fixes are an
# explicit list of eleven labels rather than a regular expression that deletes trailing parentheses — such an expression
# would quietly damage roughly forty substantive item labels.
#
# This tribble is the single place to change any response label. Add a row to relabel anything.

label.fix <- tribble(
  ~qid,               ~label_old,                ~label_new,
  "xpid3",            "Dem (no lean)",           "Dem",
  "xpid3",            "Ind (no lean)",           "Ind",
  "xpid3",            "Rep (no lean)",           "Rep",
  "xdemIdeo3",        "Liberal (1-3)",           "Liberal",
  "xdemIdeo3",        "Moderate (4)",            "Moderate",
  "xdemIdeo3",        "Conservative (5-7)",      "Conservative",
  "demAgeGeneration", "GenZers: 1997-2012",      "Gen Z",
  "demAgeGeneration", "Millennials: 1981-1996",  "Millennials",
  "demAgeGeneration", "GenXers: 1965-1980",      "Gen X",
  "demAgeGeneration", "Baby Boomers: 1946-1964", "Baby Boomers",
  "xeduc3",           "< College",               "No bachelor's degree",
  "xeduc3",           "Bachelors degree",        "Bachelor's degree",

  # Three further kinds of leftover, all introduced by the V2 delivery:
  #
  #   5. demPidLean. The vendor authored this question with Qualtrics piping, so that a Republican read "a strong
  #      Republican" and a Democrat "a strong Democrat". The piping was never resolved in the codebook, so the labels
  #      arrive as the raw template text "Strong ${q://QID70/ChoiceGroup/SelectedChoices}". Replacing it with the
  #      party-neutral wording; read pid_strength alongside pid3 to know which party a respondent was strong in.
  #   6. demPidClos. Cutting the leading "the" so the three labels read as category names in a table rather than as
  #      the tail of the question's sentence.
  #   7. xdemAll. V2 relabelled this "Adults". It is wrong — this is the registered-voter file, the filename says RVs,
  #      and the banner book's own column header reads "Registered Voters". Restoring it, because the label would
  #      otherwise end up in a chart footnote describing the wrong population.
  "demPidLean",       "Strong ${q://QID70/ChoiceGroup/SelectedChoices}",          "Strong partisan",
  "demPidLean",       "Not very strong ${q://QID70/ChoiceGroup/SelectedChoices}", "Not very strong partisan",
  "demPidClos",       "the Democratic Party",    "Democratic Party",
  "demPidClos",       "the Republican Party",    "Republican Party",
  "demPidClos",       "Neither Party",           "Neither",
  "xdemAll",          "Adults",                  "Registered Voters"
)

# Applying the fixes by joining them on, then taking the replacement wherever one exists
levels.clean <- levels.stripped %>%
  left_join(label.fix, by = c("qid", "label" = "label_old")) %>%
  mutate(label = coalesce(label_new, label)) %>%
  select(-label_new)

# Every hand fix must have actually matched something. If the vendor rewords a label, this stops the script rather than
# letting the fix silently become a no-op and the old label survive into the data.
fixes.unmatched <- label.fix %>%
  anti_join(levels.stripped, by = c("qid", "label_old" = "label"))

if (nrow(fixes.unmatched) > 0) {
  stop("Hand fixes in label.fix that matched no delivered label (check whether the vendor reworded them):\n",
       paste0("  ", fixes.unmatched$qid, ": ", fixes.unmatched$label_old, collapse = "\n"))
}

# No label should still carry a preface. Allowing the handful of substantive labels that legitimately contain ": ",
# which are checked by hand rather than assumed.
labels.still.prefaced <- levels.clean %>%
  filter(!is.na(label), label != "NO TEXT") %>%
  filter(str_detect(label, "^(Age|Gender|PID|PID/Gender|Ideo|Educ|Income|Community|Employ|Ethnicity|4-Region|2024 Vote|Insurance|Married): "))

if (nrow(labels.still.prefaced) > 0) {
  stop("Response labels still carrying a variable-name preface:\n",
       paste0("  ", labels.still.prefaced$qid, ": ", labels.still.prefaced$label, collapse = "\n"))
}

message("Part E: stripped variable-name prefaces from the labels of ", length(qids.to.strip),
        " variables and applied ", nrow(label.fix), " hand fixes.")




##### #
#### #
### ################################################################################################################################################# #
# Part F. Assembling the variable names -------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.1. Naming the item batteries ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Joining the base map onto the questions, then looking each item's verbatim text up in its declared vocabulary to get
# the short tag. inner_join drops the single-response questions, which F.2 picks up separately.
items.named <- questions %>%
  inner_join(base.map, by = "base") %>%
  mutate(item_tag = map2_chr(vocab, item_text,
                             function(v, txt) {
                               lookup <- vocab.lookup[[v]]                            # the vocabulary this battery declared
                               if (!txt %in% names(lookup)) NA_character_             # item text absent from it -> flagged below
                               else unname(lookup[[txt]])                             # otherwise the short tag
                             }))

# Guard 1. Any item whose text is absent from its declared vocabulary means the vendor reworded an item or a new item was
# added. Stopping rather than producing a column named prefix_NA.
items.unmatched <- items.named %>% filter(is.na(item_tag))

if (nrow(items.unmatched) > 0) {
  stop("Item text not found in its declared vocabulary (add it to the vocabulary in Part B):\n",
       paste0("  ", items.unmatched$qid, ": ", items.unmatched$item_text, collapse = "\n"))
}

# Guard 2. Within a vocabulary, position k must always carry the same item. This is the assumption that lets six
# different batteries share one item list, and it is what makes a positional comparison across them valid. If it ever
# fails, items from different batteries would be silently mismatched.
index.conflicts <- items.named %>%
  distinct(vocab, item_index, item_text) %>%
  count(vocab, item_index) %>%
  filter(n > 1)

if (nrow(index.conflicts) > 0) {
  stop("Position k does not mean the same item across batteries sharing a vocabulary, so a positional join would ",
       "mismatch items:\n",
       paste0("  ", index.conflicts$vocab, " position ", index.conflicts$item_index, collapse = "\n"))
}

# Building the variable name, and selecting the columns the codebook carries
items.named <- items.named %>%
  mutate(var_name = paste(prefix, item_tag, sep = "_")) %>%
  select(qid, var_name, base, item_index, item_tag, vocab, question_text, item_text, qid_full,
         var_type, scale_family, base_condition)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.2. Naming the single-response variables ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# These have no item vocabulary, so vocab and item_tag are empty by construction
singles.named <- questions %>%
  inner_join(single.map, by = "qid") %>%
  mutate(vocab    = NA_character_,
         item_tag = NA_character_) %>%
  select(qid, var_name, base, item_index, item_tag, vocab, question_text, item_text, qid_full,
         var_type, scale_family, base_condition)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## F.3. Combining, attaching descriptions, and checking coverage ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Stacking the two, restoring the delivered column order, and building the var_label described in C.3:
# the family's plain-English stem, plus the item text where there is one.
var.index <- bind_rows(items.named, singles.named) %>%
  left_join(label.map, by = "base") %>%
  mutate(var_label = case_when(is.na(var_label_stem) ~ NA_character_,                          # nothing declared -> flagged below
                               is.na(item_text)      ~ var_label_stem,                          # single-response: the stem alone
                               TRUE ~ paste0(var_label_stem, ": ", item_text))) %>%             # battery item: stem, then the item
  select(-var_label_stem) %>%
  arrange(match(qid, questions$qid))

# Every delivered column must be named exactly once, and no two columns may end up with the same name
stopifnot(setequal(var.index$qid, questions$qid),
          !any(duplicated(var.index$qid)),
          !any(duplicated(var.index$var_name)))

# Every question family must have a description in C.3, so that no variable reaches the index without one
labels.missing <- var.index %>% filter(is.na(var_label)) %>% distinct(base)

if (nrow(labels.missing) > 0) {
  stop("Question families with no description in label.map (add a row to C.3):\n",
       paste0("  ", labels.missing$base, collapse = "\n"))
}

# Every scale family named in the maps must be declared in D.1
scales.undeclared <- setdiff(var.index$scale_family, scale.spec$scale_family)

if (length(scales.undeclared) > 0) {
  stop("Scale family used in the naming maps but not declared in scale.spec: ",
       paste(scales.undeclared, collapse = ", "))
}




##### #
#### #
### ################################################################################################################################################# #
# Part G. Building the combined codebook ------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## G.1. Response-level codebook (one row per response option) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Joining the cleaned level labels onto the variable index, then attaching the scale notes and the ordinal positions.
# right_join keeps every variable even where it has no response options at all (resp_id, zip, weight), which a plain
# left_join from the levels side would drop.
codebook.combined <- levels.clean %>%
  mutate(label = case_when(label == "NO TEXT" ~ NA_character_, TRUE ~ label)) %>%
  right_join(var.index, by = "qid") %>%
  left_join(scale.spec %>% select(scale_family, scale_note), by = "scale_family") %>%
  left_join(scale.map, by = c("scale_family", "value")) %>%
  group_by(qid) %>%
  mutate(n_levels     = sum(!is.na(value)),
         is_nonscale  = coalesce(is_nonscale, FALSE),                                  # not declared off-scale -> on-scale
         ord_position = case_when(var_type == "ordinal" ~ ord_position,            # an unordered variable has no ordinal
                                  TRUE ~ NA_integer_)) %>%                         # position, by construction
  ungroup() %>%
  select(qid, var_name, var_label, base, item_index, item_tag, vocab,
         question_text, item_text, qid_full,
         var_type, scale_family, scale_note, base_condition,
         value, label, ord_position, is_nonscale, n_levels) %>%
  arrange(match(qid, questions$qid), value)

# Every substantive code of every ordinal variable must have received a position, or script 02 would drop it to NA
ord.gaps <- codebook.combined %>%
  filter(var_type == "ordinal", !is_nonscale, is.na(ord_position))

if (nrow(ord.gaps) > 0) {
  stop("Ordinal codes with no declared position (fix low_to_high in scale.spec):\n",
       paste0("  ", ord.gaps$qid, " value ", ord.gaps$value, " = ", ord.gaps$label, collapse = "\n"))
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## G.2. Question-level codebook (one row per variable) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Collapsing the response-level codebook to one row per variable, with the response options pasted into a single readable
# field. This is the file to open when the question is simply "what is this variable and what are its categories".
codebook.questions <- codebook.combined %>%
  group_by(qid) %>%
  summarise(var_name       = first(var_name),
            var_label      = first(var_label),
            base           = first(base),
            item_index     = first(item_index),
            item_tag       = first(item_tag),
            question_text  = first(question_text),
            item_text      = first(item_text),
            var_type       = first(var_type),
            scale_family   = first(scale_family),
            scale_note     = first(scale_note),
            base_condition = first(base_condition),
            n_levels       = first(n_levels),
            # All response options, as "code = label" pairs
            levels         = paste(na.omit(paste0(value, " = ", label)), collapse = " | "),
            # The substantive levels in low-to-high order, for ordinal variables only
            ordering_low_to_high = {
              keep <- !is.na(ord_position)
              case_when(!any(keep) ~ NA_character_,
                        TRUE ~ paste(label[keep][order(ord_position[keep])], collapse = " < "))
            },
            # The codes set to missing on the ordered version
            nonscale_codes = paste(value[is_nonscale], collapse = ", "),
            .groups = "drop") %>%
  arrange(match(qid, questions$qid))




##### #
#### #
### ################################################################################################################################################# #
# Part H. Writing the codebooks --------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

write.csv(x = codebook.combined,
          file = file.path(out.dir, "eis_2026_codebook_combined.csv"),
          row.names = FALSE,
          na = "")

write.csv(x = codebook.questions,
          file = file.path(out.dir, "eis_2026_codebook_questions.csv"),
          row.names = FALSE,
          na = "")

# Printing a summary so a run reports what it built rather than finishing silently
message("\nCombined codebook written to ", out.dir, "/\n",
        "  variables:         ", nrow(codebook.questions), "\n",
        "  response options:  ", sum(!is.na(codebook.combined$value)), "\n",
        "  question families: ", n_distinct(codebook.combined$base), "\n",
        "  scale families:    ", n_distinct(codebook.combined$scale_family), "\n",
        "  item vocabularies: ", n_distinct(na.omit(codebook.combined$vocab)), "\n",
        "  by var_type:       ",
        codebook.questions %>%
          count(var_type) %>%
          mutate(txt = paste0(var_type, "=", n)) %>%
          pull(txt) %>%
          paste(collapse = ", "))




# The end.
