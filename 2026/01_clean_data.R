

# 01_clean_data.R



########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
####
####  Title: Cleaning the BPC Election Information Survey, 2026 field (registered voters)
####
####  Author: Jack Friedman
####
####  Overview: THIS FILE IS SELF-CONTAINED. It reads Morning Consult's delivery and nothing else — no other script
####            has to be run first, and no derived file has to be present. Everything needed to clean the data is
####            declared inside it: the item vocabularies (Part B), the variable names and descriptions (Part C), the
####            response scales and their orderings (Part D), the label corrections (Part E), and the combined codebook
####            those build (Parts F and G), which then drives the cleaning in Parts H onward.
####
####            Two companion scripts build on this file's output. '02_common_weights.R' re-weights the 2022, 2024, and
####            2026 files to one common demographic target so that a year-over-year difference is free of composition
####            effects, and '03_cumulative_data.R' stacks the three years into one file. Run this script first.
####
####  File Description: This document reads the three files Morning Consult delivered — the raw data
####                    ('..._RVs_V2.csv', 3,144 registered voters x 496 columns), its two codebooks, and the open-end
####                    workbook — and writes a typed, labelled, analysis-ready data frame, a long table for the select
####                    and rank batteries, the combined codebook, and an index of every variable in the output.
####
####                    WHY THE SPECIFICATION IS IN HERE RATHER THAN IN A SEPARATE FILE. Morning Consult delivers two
####                    codebooks — one listing question text by column, the other listing response labels by column —
####                    and neither on its own describes a variable completely. Neither supplies a usable variable name,
####                    says which direction an ordinal scale runs, or marks which codes are off-scale. Parts B through G
####                    add all of that and join the two codebooks into one. Keeping it here rather than in a companion
####                    script means this file can be run, read, and corrected on its own.
####
#### ------------------------------------------------------------------------------------------------------------------
####
####  TYPE CONVENTIONS IN THE OUTPUT
####
####    id / text / weight   Left as delivered. resp_id and zip stay character so that a leading zero survives
####                         (08849 must not become 8849).
####
####    flag                 Factor with levels 0 and 1. The vendor labels only code 1 and leaves the rest missing, so
####                         missing is recoded to 0. Part H.1 verifies that missing really does mean "not in this
####                         group" and never "not asked" before doing so.
####
####    multiselect          Factor with levels 0 and 1, from the vendor's 1 = Selected / 2 = Not Selected. NA is
####                         PRESERVED and means the respondent never saw the item because they were out of the
####                         question's base. So a proportion must always be taken within base, which the NA enforces.
####
####    binary / nominal     Factor, levels in the vendor's code order, labelled. "Don't know" stays a level, because
####                         it belongs in the published denominator.
####
####    ordinal              THREE columns, because no single column can serve all three uses:
####                           <name>    Ordered factor, substantive levels only, ALWAYS running low to high in the
####                                     direction the variable's name implies. Off-scale codes ("Don't know",
####                                     "I do not plan to vote") become NA, because they are not points on the scale
####                                     and must not sort to the top of it.
####                           <name>_i  Integer companion, equal to as.integer(<name>). For means, correlations,
####                                     and regressions.
####                           <name>_f  Unordered factor carrying EVERY delivered label, "Don't know" included. Use
####                                     this for a published frequency table, so that no response is silently dropped.
####
####    rank                 Collapsed from 89 per-item columns into 36 slot columns; see Part I.
####
#### ------------------------------------------------------------------------------------------------------------------
####
####  WORKING WITH THE 0/1 FACTORS
####
####    The 364 indicator variables are factors with levels "0" and "1", not logicals, so arithmetic on them needs an
####    explicit comparison. The three idioms:
####
####      sum(data$src_use_social == "1", na.rm = TRUE)                            # count
####      mean(data$src_use_social == "1", na.rm = TRUE)                           # proportion within base
####      weighted.mean(data$src_use_social == "1", data$weight, na.rm = TRUE)      # weighted proportion within base
####
####    The counts that DO need row arithmetic (how many sources a respondent named, straightlining, and so on) are all
####    computed in Part J while the indicators are still logical, and the conversion to factor happens afterwards in
####    Part K. So nothing downstream has to reconstruct them.
####
####    Note that na.rm = TRUE is doing real work in those idioms: for every battery below the master source question, NA
####    means out of base, so dropping it is what puts the percentage on the right denominator.
####
####### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# Loading packages
library(tidyverse)
library(zipcodeR)   # for the ZIP code -> state / county / town crosswalk used in Part J.1



##### #
#### #
### ################################################################################################################################################# #
# Part A. Loading the vendor delivery ----------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.1. File paths ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Setting the folder the vendor delivery sits in, and the folder all output is written to. Both are relative to R's
# working directory, which should be the folder containing this script and the "data" folder (set it with setwd() or
# by opening the folder as an RStudio project, whichever you prefer). Output goes to an "output" folder, which is
# created if it does not exist.
data.dir <- "data"
out.dir  <- "output"

# Creating the output folder if this is a first run (showWarnings = FALSE so a re-run is silent)
dir.create(out.dir, showWarnings = FALSE)

# The four files Morning Consult delivered, job number 2608077. These are the ONLY inputs this script has.
#
# THE V2 DELIVERY (2026-09-11), which added the demographics in their original, uncollapsed form. It is a strict superset
# of the first delivery: the same 3,144 respondents, every original value carried through unchanged, and 13 new columns
# holding the uncollapsed age, education, income, ideology, race and marital items plus state, party strength, party
# lean, and two vendor recodes. Nothing reads the V1 files.
path.raw       <- file.path(data.dir, "2608077_BPC_raw data_Voter Information Survey_RVs_V2.csv")
path.questions <- file.path(data.dir, "2608077_BPC_question codebook_Voter Information Survey_RVs_V2.csv")
path.levels    <- file.path(data.dir, "2608077_BPC_level codebook_Voter Information Survey_RVs_V2.csv")
path.open.ends <- file.path(data.dir, "bipartisan_policy_center_open_ends.xlsx")

# Stopping with a readable message if a file is missing, rather than letting read.csv() report a failed connection
paths.missing <- c(path.raw, path.questions, path.levels, path.open.ends) %>%
  keep(~ !file.exists(.x))

if (length(paths.missing) > 0) {
  stop("Input files not found. Check that R's working directory is the folder that contains the 'data' folder:\n",
       paste0("  ", paths.missing, collapse = "\n"))
}


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
# Splitting the two apart here, because the cleaning steps below need the stem and the item separately.
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






# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.4. Reading in the raw data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Reading every column in as character, for two reasons. First, the vendor writes missing values as an unquoted NA token
# in 1,019,189 of its 1,560,192 cells while real values arrive quoted, so the file has to be taken literally and converted
# deliberately in A.5. Second, demZIP is a five-digit ZIP code, and reading it as a number would silently delete the
# leading zero from every New England and New Jersey respondent.
#
#   colClasses = "character"  every column read as text
#   na.strings = NULL         no token treated as missing on read; A.4 does that explicitly
#   check.names = F           column names kept exactly as delivered, so the qids still match the codebook
raw.orig <- read.csv(path.raw,
                     colClasses  = "character",
                     na.strings  = NULL,
                     check.names = FALSE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## A.5. Converting the vendor's missing-value tokens to real NA ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The tokens that mean missing in this delivery. Listing more than the file actually uses, so that a future field that
# writes missing differently is still handled.
na.tokens <- c("", "NA", "N/A", "NaN", "NULL", ".", "-", "#NULL!")

# Counting them first, so the run reports how much of the file was missing rather than converting silently
n.na.tokens <- raw.orig %>%
  summarise(across(everything(), ~ sum(str_trim(.x) %in% na.tokens))) %>%
  unlist() %>%
  sum()

raw <- raw.orig %>%
  mutate(across(everything(), ~ case_when(str_trim(.x) %in% na.tokens ~ NA_character_,
                                          TRUE ~ str_trim(.x))))

message("Part A: converted ", format(n.na.tokens, big.mark = ","),
        " placeholder cells (chiefly the literal string \"NA\") to real NA.")

# Every column other than the two genuinely textual ones must be numeric. Anything left over would be a value the
# codebook cannot describe, so this stops the script rather than letting as.numeric() quietly turn it into NA.
residual.text <- raw %>%
  select(-ResponseID, -demZIP) %>%
  summarise(across(everything(), ~ sum(!is.na(.x) & is.na(suppressWarnings(as.numeric(.x)))))) %>%
  unlist()

if (any(residual.text > 0)) {
  stop("Non-numeric values found in coded columns: ",
       paste(names(residual.text)[residual.text > 0], collapse = ", "))
}


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
# variable is named after, so the integer companion built in Part H always runs in the intuitive direction — higher
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
  "pid3",            "",                                "",        "Party identification with leaners NOT assigned to a party. Vendor labels read 'PID: Dem (no lean)' and are cleaned in Part E to Dem / Ind / Rep. For the version that assigns leaners, see pid5 and pid3_lean in Part J.2",
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
  "age_years",       "",                                "",        "Age in completed years, 18 to 92. IMPORTANT: the vendor's level codebook lists 74 codes for this variable mapping 1 = 18 up to 74 = 92, but the delivered data holds the AGE ITSELF, not those codes. Applying the level map would destroy the variable, so it is typed 'numeric' and Part H leaves it alone. Verified in Part M against the age4 bands",
  "educ9",           "1,2,3,4,5,6,7,8,9",               "",        "Ascending attainment, the uncollapsed source of educ3 (educ3 collapses 1-6 / 7 / 8-9). This is the version that can be cut at the 'diploma divide' — high school or less (1-3) against any college (4-9) — which educ3 cannot express",
  "income6",         "1,2,3,4,5,6",                     "",        "Ascending household income band, the uncollapsed source of income3. Code 6 ($100 thousand or more) is broken out further by income_top4",
  "income_top4",     "1,2,3,4",                         "",        "Ascending band within '$100 thousand or more'. Asked only of the 551 respondents at income6 = 6, so it is a follow-up rather than a separate measure; combine the two for a nine-band income variable",
  "ideo7",           "1,2,3,4,5,6,7",                   "8",       "Left to right: Very liberal ... Very conservative. The uncollapsed source of ideo3 (which collapses 1-3 / 4 / 5-7). Code 8 is 'Don't Know', and it is exactly the 157 respondents who are missing on ideo3",
  "race5",           "",                                "",        "Nominal race, the uncollapsed source of the four race flags. American Indian (41), Asian American (118) and Other (94) were all folded into race_other (253), so this is the only way to identify Asian American respondents. Hispanic origin is asked separately and overlays all five categories",
  "state51",         "",                                "",        "Nominal state of residence, including the District of Columbia. Code 52 ('I do not live in the continental United States') exists in the codebook but no respondent took it",
  "marital6",        "",                                "",        "Nominal marital status",
  "married2",        "",                                "",        "Vendor recode of marital: 1 = Married, 2 = everything else",
  "fulltime3",       "",                                "3",       "Nominal; 3 = Don't Know, declared by the vendor but taken by no respondent",
  "insured_x2",      "",                                "",        "Vendor recode of health insurance status. An exact duplicate of `insured` (demInsured) on all 3,144 rows, verified in Part M; it is kept only because the delivery contains it. Use `insured`"
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
# to be. Stripping the prefaces here, in the codebook, so the cleaning steps never see them.
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

# Every substantive code of every ordinal variable must have received a position, or the cleaning step would drop it to NA
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






# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## G.3. Reducing the codebook to one row per variable, and renaming the data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# `codebook.combined` has one row per response option. Everything from here on needs one row per VARIABLE instead: the
# qid it arrived as, the name it takes, and the type it should end up as.
var.meta <- codebook.combined %>%
  distinct(qid, var_name, var_label, base, item_index, item_tag, vocab, var_type, scale_family, base_condition)

# Pulling out the variable names belonging to each type, once, so that the code below never re-filters the codebook.
# Saving these as plain character vectors rather than looking them up repeatedly keeps each mutate() readable.
vars.flag        <- var.meta %>% filter(var_type == "flag")        %>% pull(var_name)
vars.multiselect <- var.meta %>% filter(var_type == "multiselect") %>% pull(var_name)
vars.rank        <- var.meta %>% filter(var_type == "rank")        %>% pull(var_name)
vars.ordinal     <- var.meta %>% filter(var_type == "ordinal")     %>% pull(var_name)
vars.categorical <- var.meta %>% filter(var_type %in% c("binary", "nominal")) %>% pull(var_name)

# Recovering each item vocabulary's tag order from the codebook rather than from the vocabularies in Part B directly, so
# that the order used below is provably the same one the codebook was built with
vocab.tags <- codebook.combined %>%
  filter(!is.na(vocab)) %>%
  distinct(vocab, item_index, item_tag) %>%
  arrange(vocab, item_index) %>%
  group_by(vocab) %>%
  summarise(tags = list(item_tag), .groups = "drop") %>%
  deframe()

# The delivered file and the codebook must describe exactly the same set of columns, in both directions
stopifnot("the raw file and the codebook do not describe the same columns" =
            setequal(names(raw), var.meta$qid))

# Renaming every column from its qid to its new name in one step. The rename is driven by the codebook rather than by a
# list repeated here.
data0 <- raw %>%
  rename_with(~ var.meta$var_name[match(.x, var.meta$qid)]) %>%
  mutate(across(-c(resp_id, zip), as.numeric))

stopifnot("respondent identifiers are not unique" = !any(duplicated(data0$resp_id)))

message("Part G: built a combined codebook of ", nrow(codebook.combined), " response options across ",
        nrow(var.meta), " variables, and renamed the delivered columns.")


##### #
#### #
### ################################################################################################################################################# #
# Part H. Typing the delivered variables ------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# Every indicator in this Part is built as a LOGICAL and stays logical until Part K, which converts all of them to
# factors with levels 0 and 1. The reason for the delay is that Part J needs row arithmetic across these columns (how
# many sources a respondent named, whether they straightlined a matrix), and rowSums() over a factor is not defined.

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## H.1. Flags ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The vendor's flag columns label only code 1 and leave everyone else missing. Before recoding that missing to 0, it has
# to be established that missing means "not in this group" rather than "was not asked" — if it were the latter, recoding
# it to 0 would be inventing data.
#
# Two things establish it. First, the four race flags partition the sample, so every respondent should carry at least
# one; a respondent carrying none would suggest the flags were not asked of everyone. Second, the three seg_* columns
# are deterministic recodes of BPCdem1, BPCdem2, and BPCdem3, which everyone answered.
race.flag.coverage <- data0 %>%
  transmute(n_flags = rowSums(cbind(race_white, race_hispanic, race_black, race_other) == 1, na.rm = TRUE)) %>%
  count(n_flags)

if (any(race.flag.coverage$n_flags == 0)) {
  warning("Some respondents carry no race flag at all, so missing may not mean 'not in this group'. ",
          "Check before treating a missing flag as 0.", call. = FALSE)
}

data1 <- data0 %>%
  mutate(across(all_of(vars.flag), ~ coalesce(.x == 1, FALSE)))


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## H.2. Multi-select indicators ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The vendor codes these 1 = Selected, 2 = Not Selected — NOT 1/0, which is the trap here: treating the raw code as an
# indicator would count every non-selection as two selections. Checking that no other value appears before recoding.
bad.multiselect <- data1 %>%
  summarise(across(all_of(vars.multiselect), ~ sum(!is.na(.x) & !.x %in% c(1, 2)))) %>%
  unlist()

if (any(bad.multiselect > 0)) {
  stop("Multi-select columns holding a value outside {1, 2}: ",
       paste(names(bad.multiselect)[bad.multiselect > 0], collapse = ", "))
}

# Unlike the flags, missing is left missing here. For every battery below the master source question, missing means the
# respondent was out of that battery's base, and recoding it to 0 would put the whole sample in the denominator of a
# percentage that belongs only to the people who were asked.
data2 <- data1 %>%
  mutate(across(all_of(vars.multiselect), ~ .x == 1))


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## H.3. Building the value-to-label lookups from the codebook ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# factor() takes `levels` (the codes, in the order the levels should appear) and `labels` (the text for each). Building
# those two vectors per variable here, from the codebook, so that B.4 and B.5 can call factor() directly and no
# variable's labels are written out anywhere in this script.

# Nominal and binary variables: every labelled code, in the vendor's numeric order
codes.categorical <- codebook.combined %>%
  filter(var_name %in% vars.categorical, !is.na(value)) %>%
  arrange(var_name, value) %>%
  group_by(var_name) %>%
  summarise(value = list(value), label = list(label), .groups = "drop")

levels.categorical <- codes.categorical %>% select(var_name, value) %>% deframe()
labels.categorical <- codes.categorical %>% select(var_name, label) %>% deframe()

# Ordinal variables, ordered version: substantive codes only (ord_position is NA for an off-scale code such as
# "Don't know"), sorted into the low-to-high ordering declared in Part D
codes.ordinal <- codebook.combined %>%
  filter(var_name %in% vars.ordinal, !is.na(value), !is.na(ord_position)) %>%
  arrange(var_name, ord_position) %>%
  group_by(var_name) %>%
  summarise(value = list(value), label = list(label), .groups = "drop")

levels.ordinal <- codes.ordinal %>% select(var_name, value) %>% deframe()
labels.ordinal <- codes.ordinal %>% select(var_name, label) %>% deframe()

# Ordinal variables, full version: every label including the off-scale codes, in the vendor's numeric order
codes.ordinal.full <- codebook.combined %>%
  filter(var_name %in% vars.ordinal, !is.na(value)) %>%
  arrange(var_name, value) %>%
  group_by(var_name) %>%
  summarise(value = list(value), label = list(label), .groups = "drop")

levels.ordinal.full <- codes.ordinal.full %>% select(var_name, value) %>% deframe()
labels.ordinal.full <- codes.ordinal.full %>% select(var_name, label) %>% deframe()


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## H.4. Nominal and binary variables into labelled factors ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# cur_column() returns the name of the column across() is currently working on, which is what lets one expression apply
# a different set of levels to each of the 24 variables.
data3 <- data2 %>%
  mutate(across(all_of(vars.categorical),
                ~ factor(.x,
                         levels = levels.categorical[[cur_column()]],
                         labels = labels.categorical[[cur_column()]])))


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## H.5. Ordinal variables into three columns each ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Building the three versions described in the header. Order matters: the _f version is built first, from the still-numeric
# column, because once <name> has been overwritten with the substantive-only ordered factor the off-scale codes are gone.
#
#   1. <name>_f  every label, unordered                        -> for frequency tables
#   2. <name>    substantive levels only, ordered low to high  -> for anything that relies on the ordering
#   3. <name>_i  as.integer() of the ordered version           -> for means and models
data4 <- data3 %>%
  mutate(across(all_of(vars.ordinal),
                ~ factor(.x,
                         levels = levels.ordinal.full[[cur_column()]],
                         labels = labels.ordinal.full[[cur_column()]]),
                .names = "{.col}_f")) %>%
  mutate(across(all_of(vars.ordinal),
                ~ factor(.x,
                         levels  = levels.ordinal[[cur_column()]],
                         labels  = labels.ordinal[[cur_column()]],
                         ordered = TRUE))) %>%
  mutate(across(all_of(vars.ordinal), as.integer, .names = "{.col}_i"))




##### #
#### #
### ################################################################################################################################################# #
# Part I. Reshaping the select and rank batteries ---------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.1. The battery information table ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# One row per select battery. `rank_base` names the battery's rank follow-up where it has one, which is what pairs the
# 89 rank columns to their parents. Everything here is keyed on the vendor's question number, so no variable name has to
# be reconstructed from a string.
#
# `oob` records what it MEANS for a respondent to be out of a battery's base, because that decides how a net percentage
# across the whole sample should be computed:
#
#   "none"       Asked of everyone. No out-of-base cases exist.
#   "no"         Out of base is a structural zero. A respondent who never named social media at BPC2 uses no social
#                platform for election news, so the right denominator for a net percentage is all 3,144 and treating
#                their missing as a 0 is correct.
#   "other_arm"  Out of base means the respondent took the OTHER arm of the same question and was asked an equivalent
#                item there, so pooling against all 3,144 would count them twice. Report within base, or use the
#                pooled reg_src_* / run_src_* / won_src_* columns built in Part J.5.

battery.info <- tribble(
  ~base,   ~battery,         ~rank_base, ~battery_label,                                      ~oob,        ~net_universe,
  "BPC2",  "src",            "BPC2a",    "Main election-news sources",                        "none",      "All registered voters",
  "BPC3",  "tvnet",          "BPC3a",    "National TV networks",                              "no",        "Named national television at BPC2",
  "BPC4",  "radio",          "BPC4a",    "Radio programming",                                 "no",        "Named radio at BPC2",
  "BPC5",  "outlet",         NA,         "Print/online news outlets",                         "no",        "Named local/state or national print-online outlets at BPC2",
  "BPC6",  "social",         NA,         "Social media platforms",                            "no",        "Named social media at BPC2",
  "BPC10", "bot",            NA,         "AI chatbots",                                       "no",        "Named an AI chatbot at BPC2",
  "BPC11", "reg_act",        "BPC11a",   "Registration info: sources used",                   "other_arm", "Seeks registration information (BPC1_1 not Never)",
  "BPC12", "reg_hyp",        "BPC12a",   "Registration info: sources would use",              "other_arm", "Never seeks registration information",
  "BPC15", "reg_act_social", NA,         "Registration info: platforms used",                 "other_arm", "Named social media at BPC11",
  "BPC16", "reg_hyp_social", NA,         "Registration info: platforms would use",            "other_arm", "Named social media at BPC12",
  "BPC17", "reg_act_bot",    NA,         "Registration info: chatbots used",                  "other_arm", "Named an AI chatbot at BPC11",
  "BPC18", "reg_hyp_bot",    NA,         "Registration info: chatbots would use",             "other_arm", "Named an AI chatbot at BPC12",
  "BPC19", "run_act",        "BPC19a",   "Election administration info: sources used",        "other_arm", "Seeks election-administration information",
  "BPC20", "run_hyp",        "BPC20a",   "Election administration info: sources would use",   "other_arm", "Never seeks election-administration information",
  "BPC23", "run_act_social", NA,         "Election administration info: platforms used",      "other_arm", "Named social media at BPC19",
  "BPC24", "run_hyp_social", NA,         "Election administration info: platforms would use", "other_arm", "Named social media at BPC20",
  "BPC25", "run_act_bot",    NA,         "Election administration info: chatbots used",       "other_arm", "Named an AI chatbot at BPC19",
  "BPC26", "run_hyp_bot",    NA,         "Election administration info: chatbots would use",  "other_arm", "Named an AI chatbot at BPC20",
  "BPC27", "won_act",        "BPC27a",   "Results info: sources used",                        "other_arm", "Seeks results information",
  "BPC28", "won_hyp",        "BPC28a",   "Results info: sources would use",                   "other_arm", "Never seeks results information",
  "BPC31", "won_act_social", NA,         "Results info: platforms used",                      "other_arm", "Named social media at BPC27",
  "BPC32", "won_hyp_social", NA,         "Results info: platforms would use",                 "other_arm", "Named social media at BPC28",
  "BPC33", "won_act_bot",    NA,         "Results info: chatbots used",                       "other_arm", "Named an AI chatbot at BPC27",
  "BPC34", "won_hyp_bot",    NA,         "Results info: chatbots would use",                  "other_arm", "Named an AI chatbot at BPC28"
)

# The four item tags that are response-set anchors rather than substantive options. Kept as variables (a respondent who
# picks "Not sure" has told you something) but excluded from selection counts and rankings.
anchor.tags <- c("other", "not_sure", "not_interested", "dont_know")

# Every delivered select and rank column must belong to exactly one registered battery, so that nothing is quietly
# left out of the long table below
stopifnot("a multi-select column belongs to no battery in battery.info" =
            setequal(var.meta %>% filter(var_type == "multiselect") %>% pull(base), battery.info$base),
          "a rank column belongs to no battery in battery.info" =
            setequal(var.meta %>% filter(var_type == "rank") %>% pull(base), na.omit(battery.info$rank_base)))


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.2. Reshaping the selections and the ranks into long form ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Rather than looping over the 24 batteries, all 313 multi-select columns are pivoted at once and the battery each
# belongs to is recovered by joining the codebook. One pipeline, and the metadata cannot drift from the data.
# Result: 3,144 respondents x 313 options = 983,472 rows.
select.long <- data4 %>%
  select(resp_id, weight, all_of(vars.multiselect)) %>%
  pivot_longer(all_of(vars.multiselect), names_to = "var_name", values_to = "selected") %>%
  left_join(var.meta %>% select(var_name, base, item_tag, vocab, var_label), by = "var_name") %>%
  left_join(battery.info %>% select(base, battery, rank_base, oob), by = "base")

# Same treatment for the 89 rank columns. values_drop_na drops the unranked items, which is the great majority of cells:
# a respondent ranks at most three of the ten or more options they were shown.
# `rank_prefix` is the variable name with its item tag cut off the end, which is the stem the collapsed slot columns in
# C.4 are named after (reg_act_rank_local_officials, tag local_officials -> prefix reg_act_rank).
rank.long <- data4 %>%
  select(resp_id, all_of(vars.rank)) %>%
  pivot_longer(all_of(vars.rank), names_to = "var_name", values_to = "rank", values_drop_na = TRUE) %>%
  left_join(var.meta %>% select(var_name, base, item_tag, vocab), by = "var_name") %>%
  mutate(rank        = as.integer(rank),
         rank_prefix = str_sub(var_name, 1, nchar(var_name) - nchar(item_tag) - 1)) %>%
  rename(rank_base = base) %>%
  # Attaching the parent select battery, so a rank can be matched to the selection it refers to
  left_join(battery.info %>% filter(!is.na(rank_base)) %>% select(rank_base, base), by = "rank_base")


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.3. Verifying that the ranks are a clean permutation ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The collapse in C.4 keeps only "which item came 1st, 2nd, 3rd" and discards the per-item columns, which is lossless
# only if each respondent's ranks within a battery are exactly 1..k with no ties and no gaps. Checking that here, across
# all nine rank batteries at once, rather than assuming it.
rank.integrity <- rank.long %>%
  group_by(resp_id, rank_base) %>%
  summarise(is_permutation = identical(sort(rank), seq_len(n())), .groups = "drop") %>%
  filter(!is_permutation)

if (nrow(rank.integrity) > 0) {
  stop(nrow(rank.integrity), " respondent-by-battery combinations have ranks that are not a permutation of 1..k ",
       "(ties or gaps), so collapsing them would lose a response. Inspect rank.integrity before continuing.")
}

# No item may be ranked without having been selected in its parent battery
rank.orphans <- rank.long %>%
  left_join(select.long %>% select(resp_id, base, item_tag, selected), by = c("resp_id", "base", "item_tag")) %>%
  filter(!coalesce(selected, FALSE))

if (nrow(rank.orphans) > 0) {
  stop(nrow(rank.orphans), " items are ranked without having been selected in their parent battery.")
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## I.4. Collapsing the ranks from 89 columns into 36 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Each rank battery carries forward only the items its parent battery selected, and every "select up to three" parent
# caps the ranking at three. So the 89 delivered rank columns hold at most three facts per respondent per battery, and
# 89 columns become 9 batteries x (3 slots + a count) = 36. The per-item ranks are not lost: they remain in the long
# table written out in Part L, where a Borda score or a "ranked in my top three" indicator is a one-liner.

# Names for the rank positions. Longer than the cap of three, so a future field that allows more ranks still works.
slot.names <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")

# Pivoting the ranks into one column per (battery, position), holding the tag of the item that took that position
rank.wide <- rank.long %>%
  mutate(slot = paste(rank_prefix, slot.names[rank], sep = "_")) %>%
  select(resp_id, slot, item_tag) %>%
  pivot_wider(names_from = slot, values_from = item_tag) %>%
  select(resp_id, sort(setdiff(names(.), "resp_id")))

# Making each slot a factor over its battery's full item list, so that a table of the slot shows every option the
# respondents could have ranked first, including the ones nobody did. Looking the vocabulary up by slot column name.
slot.vocab <- rank.long %>%
  mutate(slot = paste(rank_prefix, slot.names[rank], sep = "_")) %>%
  distinct(slot, vocab) %>%
  deframe()

rank.wide <- rank.wide %>%
  mutate(across(-resp_id, ~ factor(.x, levels = vocab.tags[[ slot.vocab[[cur_column()]] ]])))

# The count of items ranked. Zero for a respondent the parent battery reached who ranked nothing; missing for a
# respondent who was never in that battery's base at all, since "ranked none" and "never asked" are different facts.
rank.counts <- select.long %>%
  filter(!is.na(rank_base)) %>%
  group_by(resp_id, rank_base) %>%
  summarise(in_base = any(!is.na(selected)), .groups = "drop") %>%
  left_join(rank.long %>% count(resp_id, rank_base, name = "n_ranked"), by = c("resp_id", "rank_base")) %>%
  left_join(rank.long %>% distinct(rank_base, rank_prefix), by = "rank_base") %>%
  mutate(n_ranked = case_when(in_base ~ coalesce(n_ranked, 0L),
                              TRUE    ~ NA_integer_),
         count_col = paste0(rank_prefix, "_n")) %>%
  select(resp_id, count_col, n_ranked) %>%
  pivot_wider(names_from = count_col, values_from = n_ranked)

# Dropping the 89 per-item rank columns and joining the 36 collapsed ones in their place
data5 <- data4 %>%
  select(-all_of(vars.rank)) %>%
  left_join(rank.wide,   by = "resp_id") %>%
  left_join(rank.counts, by = "resp_id")

message("Part I: collapsed ", length(vars.rank), " rank columns into ",
        ncol(rank.wide) + ncol(rank.counts) - 2, ".")




##### #
#### #
### ################################################################################################################################################# #
# Part J. Derived variables --------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## J.1. Geography from the ZIP code ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# STATE IS NOW DELIVERED. The V2 file contains demState, which is `state` here — the respondent's own answer to "In
# which state do you currently reside?", so it is authoritative and is what any analysis should use. The ZIP code
# crosswalk below is kept for two reasons all the same:
#
#   1. It gives county and town, which the delivered state does not. Those are the only sub-state geography available.
#   2. It is an INDEPENDENT measurement of state, so comparing it against the delivered answer is a free check on both.
#      D.1.iii runs that comparison.
#
# WHAT A ZIP CODE CAN AND CANNOT GIVE YOU. A ZIP code is a postal delivery route, not a unit of geography, so the three
# derived columns are not equally reliable:
#
#   state_zip  Very close to exact, but not exact: roughly one ZIP code in a hundred straddles a state line, and the
#              crosswalk names the state holding most of its addresses. This is why the delivered `state` is preferred.
#   county     The PRIMARY county only. Roughly one ZIP code in twenty spans a county line, and the crosswalk names the
#              county containing most of its addresses. Fine for describing a sample; do not merge county-level
#              administrative data onto it and treat the join as exact.
#   town       The USPS primary place name for the ZIP code, which is the post office's name for the delivery area
#              rather than a municipal boundary. A respondent in an unincorporated area gets the nearest post office's
#              town.
#
# The crosswalk comes from the zipcodeR package, which bundles a static ZIP code database, so this runs offline and
# gives the same answer on every run. D.1.iv writes the crosswalk out so the join can be audited without the package.

### J.1.i. Reading in the ZIP code crosswalk ----

# zip_code_db is a data frame shipped inside zipcodeR, one row per ZIP code
zip.db <- zipcodeR::zip_code_db %>%
  filter(!is.na(state), state != "") %>%
  select(zip = zipcode, town = major_city, county, state)

# A fallback for ZIP codes absent from the crosswalk, built from the crosswalk itself rather than hard-coded. The first
# three digits of a ZIP code are its sectional center facility, which sits inside one state for 910 of the 914 prefixes
# in use, so the modal state of a prefix recovers the state for a ZIP code the table does not list.
zip3.map <- zip.db %>%
  mutate(zip3 = str_sub(zip, 1, 3)) %>%
  count(zip3, state, name = "n_zips") %>%
  group_by(zip3) %>%
  slice_max(n_zips, n = 1, with_ties = FALSE) %>%          # the state holding most of the prefix's ZIP codes
  ungroup() %>%
  select(zip3, state_zip3 = state)

### J.1.ii. Joining the crosswalk onto the respondents ----

# Building a state-abbreviation-to-name lookup from base R's own state data, adding the District of Columbia, which
# state.name omits because it is not a state
state.names <- tibble(state      = c(state.abb, "DC"),
                      state_name = c(state.name, "District of Columbia"))

# Naming the ZIP-derived columns `state_zip` and `state_zip_name` rather than `state`, because `state` is now the
# respondent's own delivered answer and the two must not be confused. The delivered one wins wherever they disagree.
geography <- data5 %>%
  select(resp_id, zip) %>%
  left_join(zip.db, by = "zip") %>%
  mutate(zip3 = str_sub(zip, 1, 3)) %>%
  left_join(zip3.map, by = "zip3") %>%
  mutate(# Recording which of the two sources the state came from, so the fallback cases can be filtered if needed
         zip_match = case_when(!is.na(state)      ~ "ZIP code",
                               !is.na(state_zip3) ~ "ZIP prefix",
                               TRUE               ~ "unmatched"),
         # Taking the exact match where there is one, the prefix otherwise
         state     = coalesce(state, state_zip3)) %>%
  left_join(state.names, by = "state") %>%
  mutate(across(c(state, state_name, county, town), as.factor),
         zip_match = factor(zip_match, levels = c("ZIP code", "ZIP prefix", "unmatched"))) %>%
  select(resp_id, state_zip = state, state_zip_name = state_name, county, town, zip_match)

data6 <- data5 %>%
  left_join(geography, by = "resp_id")

### J.1.iii. Checking the ZIP-derived state against the delivered one ----

# Two independent measurements of the same thing: the respondent's own answer at demState, and the state the crosswalk
# assigns to the ZIP code they typed. Disagreement is expected at a low rate, because a ZIP code that straddles a state
# line is assigned to whichever state holds most of its addresses.
state.check <- data6 %>%
  select(resp_id, state, state_zip_name) %>%
  mutate(agrees = as.character(state) == as.character(state_zip_name)) %>%
  summarise(n_compared = sum(!is.na(agrees)),
            n_agree    = sum(agrees, na.rm = TRUE),
            n_differ   = sum(!agrees, na.rm = TRUE),
            pct_agree  = 100 * mean(agrees, na.rm = TRUE))

# Below 97% would mean something is wrong with the join rather than with the odd border ZIP code
if (state.check$pct_agree < 97) {
  warning("The state derived from the ZIP code agrees with the delivered demState on only ",
          round(state.check$pct_agree, 2), "% of respondents. Inspect state.check before using either.", call. = FALSE)
}

# Flagging the disagreements on the data itself, because state's main use is grouping states into policy categories
# (mail-voting regime, voter-ID strictness, competitiveness) and a respondent in the wrong state lands in the wrong
# group. Thirteen cases, so this changes nothing material — but it is worth being able to exclude them.
data6 <- data6 %>%
  mutate(flag_state_zip_mismatch = as.character(state) != as.character(state_zip_name),
         flag_state_zip_mismatch = coalesce(flag_state_zip_mismatch, FALSE))

# WHAT THE THIRTEEN LOOK LIKE. They are not careless respondents: they straightline BPC38 LESS than the rest of the
# sample (7.7% against 27.0%), straightline all three matrices at the same rate (7.7% against 7.9%), and none of them
# trips either internal-contradiction flag. Two benign explanations cover 10 of the 13:
#
#   Dropdown mis-click.  Four of the thirteen name a state ADJACENT IN THE ALPHABETICAL DROPDOWN to the one their ZIP
#                        code implies — Maryland/Massachusetts twice, South Dakota/Tennessee, Iowa/Kansas. Chance would
#                        put 0.5 of 13 there, so this is roughly an eightfold enrichment: the respondent clicked one row
#                        off in a 52-item list. For these, the ZIP code is the more reliable of the two.
#   Border or recent move. Six name a state that physically borders the ZIP code's state (Maryland/Virginia,
#                        Oregon/Washington, Pennsylvania/New Jersey, Virginia/West Virginia, Alabama/Tennessee,
#                        Arkansas/Oklahoma). A ZIP code that straddles a state line, or someone who has moved, explains
#                        these without anyone answering carelessly.
#
# That leaves three unexplained. With n = 13 none of this is worth a significance test; it is recorded so that the
# thirteen do not get mistaken for a quality problem.
state.disagreements <- data6 %>%
  select(resp_id, zip, state, state_zip_name, county, town) %>%
  filter(as.character(state) != as.character(state_zip_name))

### J.1.iv. Checking the delivered state against the delivered census region ----

# A second, independent check: region4 was built by the vendor from state, so the two must agree exactly. Mapping states
# to census regions here only to run that check; region4 itself is what gets used.
census.regions <- tibble(state  = c(state.abb, "DC"),
                         region = c("South", "West", "West", "South", "West", "West", "Northeast", "South",        # AL AK AZ AR CA CO CT DE
                                    "South", "South", "West", "West", "Midwest", "Midwest", "Midwest", "Midwest", # FL GA HI ID IL IN IA KS
                                    "South", "South", "Northeast", "South", "Northeast", "Midwest", "Midwest",    # KY LA ME MD MA MI MN
                                    "South", "Midwest", "West", "Midwest", "West", "Northeast", "Northeast",      # MS MO MT NE NV NH NJ
                                    "West", "Northeast", "South", "Midwest", "Midwest", "South", "West",          # NM NY NC ND OH OK OR
                                    "Northeast", "Northeast", "South", "Midwest", "South", "South", "West",       # PA RI SC SD TN TX UT
                                    "Northeast", "South", "West", "South", "Midwest", "West", "South"))           # VT VA WA WV WI WY DC

# Joining on the state abbreviation, which is what census.regions is keyed by; state.names carries the crosswalk between
# the delivered full name and the abbreviation
region.check <- data6 %>%
  select(resp_id, state, region4) %>%
  left_join(state.names, by = c("state" = "state_name")) %>%
  rename(state_abb = state.y) %>%
  left_join(census.regions, by = c("state_abb" = "state")) %>%
  mutate(agrees = as.character(region4) == region) %>%
  summarise(n_compared = sum(!is.na(agrees)),
            n_agree    = sum(agrees, na.rm = TRUE),
            pct_agree  = 100 * mean(agrees, na.rm = TRUE))

# region4 is the vendor's own recode of the delivered state, so anything short of exact agreement is a vendor error
if (region.check$pct_agree < 100) {
  warning("The delivered state and the delivered region4 disagree for ",
          region.check$n_compared - region.check$n_agree, " respondents (", round(region.check$pct_agree, 2),
          "% agree). region4 is the vendor's own recode of state, so these should match exactly.", call. = FALSE)
}

### J.1.v. Writing the crosswalk out ----

# Saving the respondent-level crosswalk so the geography can be checked, or re-used, without installing zipcodeR.
# Including the delivered state alongside the derived one so the two can be compared outside R.
write.csv(x = data6 %>% select(resp_id, zip, state, state_zip, state_zip_name, county, town, zip_match),
          file = file.path(out.dir, "eis_2026_zip_geography.csv"),
          row.names = FALSE,
          na = "")

message("Part J.1: state is now delivered (demState), covering all ", sum(!is.na(data6$state)), " respondents. ",
        "The ZIP crosswalk adds county and town for ", sum(!is.na(data6$county)), " of them and agrees with the ",
        "delivered state on ", round(state.check$pct_agree, 2), "% (", state.check$n_differ,
        " disagreements). Delivered state and region4 agree on ", round(region.check$pct_agree, 2), "%.")


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## J.2. Party identification: recovering the leaners ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# THE FULL BATTERY IS NOW DELIVERED. A standard 7-point party identification measure is built from three items, and the
# V2 file has all of them:
#
#   xpid3         party identification without leaners        -> pid3          Dem / Ind / Rep
#   demPidLean    strength among partisans                    -> pid_strength  Strong / Not very strong
#   demPidClos    which party an independent leans toward     -> pid_lean      Democratic Party / Republican Party / Neither
#
# So pid7 below is the real measure, built from the respondent's own answers rather than inferred.
#
# THE RECONSTRUCTION WE MADE BEFORE THE ITEMS ARRIVED WAS EXACTLY RIGHT, and it is kept because it is still the only
# way to read the 2024 file, where these items were also never delivered. The questionnaire routed BPCdem1 (MAGA
# support) to Republicans INCLUDING Republican leaners and BPCdem2 (progressive identity) to Democrats INCLUDING
# Democratic leaners, using demPidClos to decide which. So WHICH OF THE TWO ITEMS AN INDEPENDENT WAS SHOWN reveals the
# lean. That inference gave 227 Democratic leaners, 200 Republican leaners and 413 pure independents; the delivered
# demPidClos returns 227 / 200 / 413. D.2.v checks the two against each other respondent by respondent, not just on the
# marginals.
#
# WHICH OF THE FOUR PARTY VARIABLES TO USE:
#
#   pid7        The headline measure. Strong Dem ... Strong Rep, seven ordered categories.
#   pid3        How people label themselves, leaners left independent. For describing self-identification.
#   pid3_lean   The usual reporting cut, leaners folded into the party they lean toward. For anything predictive.
#   pid5        pid3_lean's five-category parent, keeping the leaners visible as their own categories.
#
# pid3 and pid3_lean are DIFFERENT MEASURES, not two codings of one: pid3 is 1,191 / 840 / 1,113 and pid3_lean is
# 1,418 / 413 / 1,313.
#
# ONE RESIDUAL GAP. The questionnaire's party question offered four options — Republican, Democrat, Independent, and
# "Something else" — but the vendor's xpid3 has only three and no missing values, so "Something else" respondents were
# folded in. They were shown the lean probe, which means they sit inside the 840 independents and, if they answered
# "Neither", inside pid7's Independent category. The four-category source item was asked for and has not been
# delivered, so the size of that group is still unknown.

### J.2.i. Which of the two follow-up items each respondent was shown ----

# The routing itself is the measurement, so it is worth having as a variable in its own right. Testing the _f version
# because it keeps every label, including "Don't know" — a respondent who was shown the item and said "Don't know" was
# still routed there, and so still revealed a lean.
data7 <- data6 %>%
  mutate(asked_maga = !is.na(maga_support_f),
         asked_prog = !is.na(progressive_id_f))

### J.2.ii. The 5-point party identification ----

# Independents are assigned a lean by which item they were routed to; independents shown neither stay independent
data7 <- data7 %>%
  mutate(pid5 = case_when(pid3 == "Dem" ~ "Dem",
                          pid3 == "Rep" ~ "Rep",
                          pid3 == "Ind" & asked_maga ~ "Lean Rep",
                          pid3 == "Ind" & asked_prog ~ "Lean Dem",
                          pid3 == "Ind"              ~ "Ind",
                          TRUE ~ NA_character_),
         pid5 = factor(pid5,
                       levels  = c("Dem", "Lean Dem", "Ind", "Lean Rep", "Rep"),
                       ordered = TRUE),
         pid5_i = as.integer(pid5))

### J.2.iii. The 3-point party identification WITH leaners assigned ----

# This is the usual reporting cut in survey research, and it is NOT the same variable as pid3. pid3 leaves every
# independent an independent; pid3_lean moves the 427 independents whose lean is recoverable into the party they lean
# toward. The two therefore have different marginals:
#
#                   pid3          pid3_lean
#   Democratic      1,191         1,418   (1,191 Democrats + 227 Democratic leaners)
#   Independent       840           413   (only the independents who lean neither way)
#   Republican      1,113         1,313   (1,113 Republicans + 200 Republican leaners)
#
# The difference matters because leaners vote very much like the party they lean toward, so pid3_lean is the better
# variable for anything predictive, while pid3 is the one to use for a straight description of how people label
# themselves. D.2.iv checks that the reassignment did exactly this and nothing else. If only one is wanted, drop the
# other here — but they are different measures, not two codings of one measure.
data7 <- data7 %>%
  mutate(pid3_lean = fct_collapse(factor(pid5, ordered = FALSE),
                                  `Dem/Lean Dem` = c("Dem", "Lean Dem"),
                                  `Ind`          = "Ind",
                                  `Rep/Lean Rep` = c("Rep", "Lean Rep")))

### J.2.iv. Checking the lean reassignment ----

# Nobody may cross the aisle: a Democrat must stay Democratic, a Republican Republican, and only independents may move
pid.crossings <- data7 %>%
  count(pid3, pid3_lean) %>%
  filter((pid3 == "Dem" & pid3_lean != "Dem/Lean Dem") |
           (pid3 == "Rep" & pid3_lean != "Rep/Lean Rep"))

if (nrow(pid.crossings) > 0) {
  stop("Assigning leaners moved a self-identified partisan out of their own party:\n",
       paste0("  ", pid.crossings$pid3, " -> ", pid.crossings$pid3_lean, ": ", pid.crossings$n, collapse = "\n"))
}

# An independent may not have been shown both follow-up items, which would make the lean ambiguous
stopifnot("an independent was routed to both the Republican and the Democratic follow-up item" =
            !any(data7$pid3 == "Ind" & data7$asked_maga & data7$asked_prog, na.rm = TRUE))

### J.2.v. Checking the earlier reconstruction against the delivered lean ----

# pid5 above was built from the routing; pid_lean is the respondent's own answer. Comparing them respondent by
# respondent, not on the marginals, because two variables can have identical marginals and still disagree on individuals.
lean.check <- data7 %>%
  filter(pid3 == "Ind") %>%
  mutate(reconstructed = as.character(pid5),
         delivered     = case_when(pid_lean == "Democratic Party" ~ "Lean Dem",
                                   pid_lean == "Republican Party" ~ "Lean Rep",
                                   pid_lean == "Neither"          ~ "Ind",
                                   TRUE                           ~ "coding error")) %>%
  count(reconstructed, delivered)

lean.mismatches <- lean.check %>%
  filter(reconstructed != delivered)

if (nrow(lean.mismatches) > 0) {
  warning("The lean reconstructed from the BPCdem1/BPCdem2 routing disagrees with the delivered demPidClos for ",
          sum(lean.mismatches$n), " independents. The delivered answer is the respondent's own and wins; pid5 is built ",
          "from the routing, so inspect lean.check before relying on the same inference in the 2024 file.",
          call. = FALSE)
}

### J.2.vi. The 7-point party identification ----

# The standard measure, from the three delivered items. Partisans split by strength, independents by lean, and an
# independent who leans neither way stays in the middle.
data7 <- data7 %>%
  mutate(pid7 = case_when(pid3 == "Dem" & pid_strength == "Strong partisan"          ~ "Strong Dem",
                          pid3 == "Dem" & pid_strength == "Not very strong partisan" ~ "Not very strong Dem",
                          pid3 == "Ind" & pid_lean     == "Democratic Party"         ~ "Lean Dem",
                          pid3 == "Ind" & pid_lean     == "Neither"                  ~ "Ind",
                          pid3 == "Ind" & pid_lean     == "Republican Party"         ~ "Lean Rep",
                          pid3 == "Rep" & pid_strength == "Not very strong partisan" ~ "Not very strong Rep",
                          pid3 == "Rep" & pid_strength == "Strong partisan"          ~ "Strong Rep",
                          TRUE                                                       ~ NA_character_),
         pid7 = factor(pid7,
                       levels  = c("Strong Dem", "Not very strong Dem", "Lean Dem", "Ind",
                                   "Lean Rep", "Not very strong Rep", "Strong Rep"),
                       ordered = TRUE),
         pid7_i = as.integer(pid7))

# Every respondent must land in one of the seven categories. A missing value here means a partisan with no strength
# answer or an independent with no lean answer, which the delivery should not contain.
pid7.unplaced <- data7 %>%
  filter(is.na(pid7)) %>%
  count(pid3, pid_strength, pid_lean)

if (nrow(pid7.unplaced) > 0) {
  stop("Respondents who could not be placed on the 7-point scale:\n",
       paste0("  pid3 = ", pid7.unplaced$pid3, ", strength = ", pid7.unplaced$pid_strength,
              ", lean = ", pid7.unplaced$pid_lean, ": ", pid7.unplaced$n, collapse = "\n"))
}

# pid7 must collapse back onto pid3_lean exactly, since both assign leaners the same way
stopifnot("pid7 does not collapse back onto pid3_lean" =
            identical(as.character(fct_collapse(factor(data7$pid7, ordered = FALSE),
                                                `Dem/Lean Dem` = c("Strong Dem", "Not very strong Dem", "Lean Dem"),
                                                `Ind`          = "Ind",
                                                `Rep/Lean Rep` = c("Lean Rep", "Not very strong Rep", "Strong Rep"))),
                      as.character(data7$pid3_lean)))

message("Part J.2: built pid7 from the delivered items (",
        paste(paste0(levels(data7$pid7), " ", as.integer(table(data7$pid7))), collapse = ", "),
        "). The lean reconstructed before these items arrived matches the delivered answer for ",
        nrow(data7 %>% filter(pid3 == "Ind")) - sum(lean.mismatches$n), " of ",
        nrow(data7 %>% filter(pid3 == "Ind")), " independents.")


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## J.3. Race and ethnicity, made mutually exclusive ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The four delivered race flags overlap, because Hispanic origin is asked separately from race: every Hispanic
# respondent also carries a race flag (199 White, 41 Black, 83 Other). Summing the four would therefore double-count
# 323 people. Giving Hispanic origin precedence, which is the standard construction and the only ordering of the
# case_when() below that partitions the sample.
data8 <- data7 %>%
  mutate(race4 = case_when(race_hispanic ~ "Hispanic",
                           race_white    ~ "White, non-Hispanic",
                           race_black    ~ "Black, non-Hispanic",
                           race_other    ~ "Other, non-Hispanic",
                           TRUE          ~ NA_character_),
         race4 = factor(race4, levels = c("White, non-Hispanic", "Black, non-Hispanic",
                                          "Hispanic", "Other, non-Hispanic")))


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## J.4. Recovering the undocumented split sample at BPC38 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The questionnaire marks [RANDOM SET SPLIT = 1:2] before BPC38, but no split-assignment flag was delivered. Only two of
# the battery's ten items are actually split, and because the two are perfectly complementary — every respondent has
# exactly one of them — which one a respondent answered recovers the assignment for the whole sample.
data8 <- data8 %>%
  mutate(ai_ok_split = case_when(!is.na(ai_ok_voter_decide_f)       ~ "A: decide which candidate",
                                 !is.na(ai_ok_voter_values_align_f) ~ "B: values alignment",
                                 TRUE                               ~ NA_character_),
         ai_ok_split = factor(ai_ok_split, levels = c("A: decide which candidate", "B: values alignment")))


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## J.5. Which arm of each information block a respondent took ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# BPC1 splits each of the three information needs into an actual arm (respondents who do seek that information, asked
# where they look) and a hypothetical arm (respondents who never seek it, asked where they would look). The two arms ask
# the same options with different wording, so they arrive as two separate sets of columns of which any given respondent
# has exactly one. Building an arm indicator plus a pooled column per option makes either analysis possible: pooled for
# "where would people turn", arm-specific for "does actual behavior differ from stated intention".

### J.5.i. The arm indicator ----

data9 <- data8 %>%
  mutate(reg_basis = case_when(seek_register_f      == "Never" ~ "Hypothetical", TRUE ~ "Actual"),
         run_basis = case_when(seek_elections_run_f == "Never" ~ "Hypothetical", TRUE ~ "Actual"),
         won_basis = case_when(seek_who_won_f       == "Never" ~ "Hypothetical", TRUE ~ "Actual"),
         across(c(reg_basis, run_basis, won_basis),
                ~ factor(.x, levels = c("Actual", "Hypothetical"))))

### J.5.ii. Pooling the two arms ----

# For each of the three blocks and each of the ten options, coalescing the actual-arm column with the hypothetical-arm
# column. Exactly one of the two is non-missing for any respondent, so the pooled column has no missing values and needs
# no priority rule. Naming these reg_src_*, run_src_*, won_src_* — the same stem as the arm-specific columns without the
# _act_ / _hyp_ marker.
#
# Building the three sets by stacking the pooled result in long form and pivoting back, rather than writing out 36
# coalesce() calls or looping. `basis` is carried along so the arm is recoverable from the long table too.
pooled.sources <- select.long %>%
  filter(base %in% c("BPC11", "BPC12", "BPC19", "BPC20", "BPC27", "BPC28")) %>%
  mutate(block = str_sub(battery, 1, 3),                                  # "reg_act" -> "reg"
         pooled_name = paste(block, "src", item_tag, sep = "_")) %>%      # reg_src_local_officials
  filter(!is.na(selected)) %>%                                            # keeping only the arm the respondent took
  select(resp_id, pooled_name, selected) %>%
  pivot_wider(names_from = pooled_name, values_from = selected)

data9 <- data9 %>%
  left_join(pooled.sources, by = "resp_id")


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## J.6. How many options each respondent selected ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Counting substantive selections per battery, with the "Other", "Not sure", "I am not interested", and "Don't know"
# anchors excluded, since selecting an anchor is not naming a source. Computing this from the long table rather than with
# rowSums() across the wide columns, because the long table already knows which items are anchors and which battery each
# column belongs to.
#
# The count is missing, not zero, for a respondent out of the battery's base: "named no sources" and "was never asked"
# are different facts and must not collapse into the same number.
selection.counts <- select.long %>%
  filter(!item_tag %in% anchor.tags,
         battery %in% c("src", "outlet", "social", "bot")) %>%
  group_by(resp_id, battery) %>%
  summarise(n_selected = case_when(all(is.na(selected)) ~ NA_integer_,      # out of base entirely
                                   TRUE ~ as.integer(sum(selected, na.rm = TRUE))),
            .groups = "drop") %>%
  mutate(battery = paste0("n_", battery, "_selected")) %>%
  pivot_wider(names_from = battery, values_from = n_selected)

data10 <- data9 %>%
  left_join(selection.counts, by = "resp_id")


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## J.7. Data-quality indicators ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

### J.7.i. Straightlining ----

# A respondent straightlines when they give the identical answer to every item in a matrix, which is the signature of
# not reading the items. Testing this on the _f versions rather than the ordered ones, because answering "Don't know"
# straight down a matrix is also non-differentiation — and on the ordered versions those cases are all NA and would be
# missed entirely.
#
# Reshaping the three matrices to long form, counting distinct answers per respondent per matrix, and pivoting back.
# Dropping missing values first means BPC38's two split items exclude themselves automatically: whichever of the pair a
# respondent did not see is NA for them, so it never enters their distinct count.
straightlining <- data10 %>%
  select(resp_id,
         all_of(paste0("concern_", vocab.tags$concern, "_f")),
         all_of(paste0("ai_ok_",   vocab.tags$ai_use,  "_f")),
         all_of(paste0("seek_",    vocab.tags$seek,    "_f"))) %>%
  pivot_longer(-resp_id, names_to = "var_name", values_to = "answer",
               values_transform = as.character, values_drop_na = TRUE) %>%
  mutate(matrix = case_when(str_starts(var_name, "concern_") ~ "sl_concern",   # BPC44, the election-concern battery
                            str_starts(var_name, "ai_ok_")   ~ "sl_ai_ok",     # BPC38, the AI good/bad battery
                            str_starts(var_name, "seek_")     ~ "sl_seek")) %>% # BPC1, the information-seeking battery
  group_by(resp_id, matrix) %>%
  summarise(is_straightline = n_distinct(answer) == 1, .groups = "drop") %>%
  pivot_wider(names_from = matrix, values_from = is_straightline)

### J.7.ii. Off-scale answering, and two internal contradictions ----

# The off-scale count is taken over the ten scale items everyone was asked. On the ordered versions an off-scale answer
# ("Don't know", "I do not plan to vote") is NA, so counting NA across them counts off-scale answers.
vars.universal.scales <- c("ai_prevalence", "ai_detect_conf", "vote_exp_positive", "conf_own_vote",
                           "conf_local_votes", "conf_state_votes", "conf_national_votes",
                           "noncitizen_freq", "usps_policy_support", "vote_likelihood")

data11 <- data10 %>%
  left_join(straightlining, by = "resp_id") %>%
  mutate(n_offscale = as.integer(rowSums(across(all_of(vars.universal.scales), is.na))),

         # Two internal contradictions. Carrying them as flags rather than editing the responses away, because either
         # could be a real respondent who is simply inconsistent, and that is the analyst's call rather than this
         # script's.
         #
         # Contradiction 1: says at BPC40 they do not plan to vote, yet rates their own likelihood of voting 8 or
         # higher at BPCdem3 — which is what puts them in the vendor's "likely midterm voter" segment.
         flag_vote_contradiction =
           conf_own_vote_f == "I do not plan to vote in the November 2026 midterm elections" & vote_likelihood_i >= 8,

         # Contradiction 2: took BPC2's exclusive "I am not interested in election news or information" anchor, yet
         # reported at BPC1 that they do seek at least one of the three kinds of election information.
         flag_interest_contradiction =
           src_use_not_interested &
           (seek_register_f != "Never" | seek_elections_run_f != "Never" | seek_who_won_f != "Never"),

         # A contradiction flag is a statement about a respondent, so missing means "no contradiction", not "unknown"
         across(starts_with("flag_"), ~ coalesce(.x, FALSE)))




##### #
#### #
### ################################################################################################################################################# #
# Part K. Converting the indicators to 0/1 factors --------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# Every indicator built above — the flags, the 313 multi-select items, the 30 pooled source columns, the straightlining
# and contradiction flags — has been a logical until now, so that Part J could do row arithmetic over them. Converting
# all of them to factors with levels "0" and "1" here, in one step and as the last thing that happens to them.
#
# This is deliberately the LAST operation on these columns. Anything that needs to count across them has already run.
# See the note in the file header for the three idioms (x == "1" inside sum, mean, or weighted.mean) that arithmetic on
# these columns now requires.
#
# NA is preserved by the conversion, and still means out of base.
data <- data11 %>%
  mutate(across(where(is.logical), ~ factor(as.integer(.x), levels = c(0, 1))))

# No logical column may survive into the output
stopifnot("a logical column survived the conversion in Part K" = !any(map_lgl(data, is.logical)))




##### #
#### #
### ################################################################################################################################################# #
# Part L. The long table for the select and rank batteries ------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# One row per respondent x battery x option, which is the shape that makes per-item ranks, Borda-style point scores, and
# comparisons across batteries straightforward. This is where the detail the rank collapse in Part I dropped still lives.
#
# The columns:
#   in_base       whether the respondent was asked this battery at all
#   selected      1 / 0 within base, missing out of base
#   selected_net  selected with out-of-base treated as a 0, defined only for the batteries where that is a structural
#                 zero (oob == "no" or "none"). Missing for the two-arm batteries, where a net across the whole sample
#                 would double-count; use the pooled reg_src_* / run_src_* / won_src_* columns for those instead.
#   is_anchor     whether the option is "Other", "Not sure", "I am not interested", or "Don't know"
#   rank          the position the respondent gave this item, where the battery had a rank follow-up
#   points        Borda-style: 1st = 3, 2nd = 2, 3rd = 1, a selection that was not ranked = 0, out of base = missing

sources.long <- select.long %>%
  # Attaching each item's rank, where its battery had a rank follow-up
  left_join(rank.long %>% select(resp_id, base, item_tag, rank), by = c("resp_id", "base", "item_tag")) %>%
  left_join(battery.info %>% select(base, battery_label), by = "base") %>%
  mutate(in_base      = !is.na(selected),
         is_anchor    = item_tag %in% anchor.tags,
         points       = case_when(!is.na(rank)             ~ 4L - rank,
                                  coalesce(selected, FALSE)    ~ 0L,
                                  TRUE                     ~ NA_integer_),
         selected_net = case_when(oob == "other_arm" ~ NA,
                                  TRUE ~ coalesce(selected, FALSE)),
         # Matching Part K: the two indicators are 0/1 factors here too
         across(c(selected, selected_net), ~ factor(as.integer(.x), levels = c(0, 1))),
         battery  = factor(battery, levels = battery.info$battery),
         item_tag = factor(item_tag)) %>%
  select(resp_id, weight, battery, battery_label, item_tag, item_label = var_label,
         in_base, selected, selected_net, is_anchor, rank, points)




##### #
#### #
### ################################################################################################################################################# #
# Part M. Validation ---------------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# Each check below is a claim about the DATA, not about the code, so a failure here means the delivery changed rather
# than that something in this script broke. Using stopifnot() with named conditions, so the name is the error message.

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## M.1. The shape of the delivery ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

stopifnot("expected 3,144 registered-voter interviews" = nrow(data) == 3144,
          "every row should be flagged as a registered voter" = all(data$sample_rv == "1"),
          "weights must be complete" = !any(is.na(data$weight)),
          "weights should sum to the unweighted n" = abs(sum(data$weight) - nrow(data)) < 1e-6)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## M.2. The three versions of every ordinal variable must agree ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The integer companion must equal as.integer() of its ordered factor, for all 46 ordinal variables
ordinal.mismatch <- vars.ordinal %>%
  keep(~ !identical(as.integer(data[[.x]]), data[[paste0(.x, "_i")]]))

if (length(ordinal.mismatch) > 0) {
  stop("The integer companion disagrees with as.integer() of its ordered factor for: ",
       paste(ordinal.mismatch, collapse = ", "))
}

# The re-orientation declared in Part D must actually have flipped the reversed scales. These three are spot checks
# on the scales most likely to be read the wrong way round, one from each direction of reversal.
stopifnot("conf_state_votes_i is not oriented so that higher means more confident" =
            identical(unique(data$conf_state_votes_i[data$conf_state_votes_f == "Very confident"]), 4L),
          "concern_misinfo_i is not oriented so that higher means more concerned" =
            identical(unique(data$concern_misinfo_i[data$concern_misinfo_f == "Very concerned"]), 4L),
          "seek_register_i should put 'Never' at the bottom of the frequency scale" =
            identical(unique(data$seek_register_i[data$seek_register_f == "Never"]), 1L))

# Off-scale codes must be missing on the ordered version but still present on the _f version
stopifnot("Don't know leaked into the ordered version of conf_own_vote" =
            all(is.na(data$conf_own_vote[data$conf_own_vote_f == "Don't know / No opinion"])),
          "the 'do not plan to vote' code should survive on conf_own_vote_f" =
            sum(data$conf_own_vote_f == "I do not plan to vote in the November 2026 midterm elections", na.rm = TRUE) > 0)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## M.3. The derived variables must partition the sample ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

stopifnot("race4 should cover every respondent" = !any(is.na(data$race4)),
          "ai_ok_split should cover every respondent" = !any(is.na(data$ai_ok_split)),
          "pid5 should cover every respondent" = !any(is.na(data$pid5)),
          "pid3_lean should cover every respondent" = !any(is.na(data$pid3_lean)),
          # Every respondent takes exactly one arm of each of the three information blocks
          "reg_basis has missing values; every respondent takes one arm" = !any(is.na(data$reg_basis)),
          "run_basis has missing values; every respondent takes one arm" = !any(is.na(data$run_basis)),
          "won_basis has missing values; every respondent takes one arm" = !any(is.na(data$won_basis)))

# The pooled source columns exist precisely because exactly one arm is non-missing per respondent, so none of them may
# be missing
pooled.gaps <- data %>%
  summarise(across(matches("^(reg|run|won)_src_"), ~ sum(is.na(.x)))) %>%
  unlist()

if (any(pooled.gaps > 0)) {
  stop("Pooled source columns with missing values, so the two arms do not partition the sample: ",
       paste(names(pooled.gaps)[pooled.gaps > 0], collapse = ", "))
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## M.4. Geography ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

stopifnot("the delivered state is missing for some respondents" =
            !any(is.na(data$state)),
          "more than 1% of respondents have no state derived from their ZIP code" =
            mean(is.na(data$state_zip)) < 0.01,
          "the delivered state and the delivered census region disagree" =
            region.check$pct_agree == 100,
          "the ZIP-derived state disagrees with the delivered state too often" =
            state.check$pct_agree > 97)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## M.6. The uncollapsed demographics must collapse back onto the vendor's recodes ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Six of the thirteen V2 columns are the source items behind recodes that were already in the file. Collapsing each one
# by hand and comparing it against the vendor's own recode checks both at once: if they agree everywhere, the fine
# version can be trusted and so can the collapse rule documented in the codebook.
#
# Each row is (fine variable, collapsed variable, the rule), and the check is that the crosstab is diagonal.
recode.checks <- list(
  educ9_to_educ3   = data %>% count(educ9, educ3) %>%
    filter(!(as.integer(educ9) %in% 1:6 & educ3 == "No bachelor's degree") &
             !(as.integer(educ9) == 7 & educ3 == "Bachelor's degree") &
             !(as.integer(educ9) %in% 8:9 & educ3 == "Post-grad")),
  ideo7_to_ideo3   = data %>% count(ideo7_f, ideo3) %>%
    filter(!(ideo7_f %in% c("Very liberal", "Liberal", "Slightly liberal") & ideo3 == "Liberal") &
             !(ideo7_f == "Moderate" & ideo3 == "Moderate") &
             !(ideo7_f %in% c("Slightly conservative", "Conservative", "Very conservative") & ideo3 == "Conservative") &
             !(ideo7_f == "Don't Know" & is.na(ideo3))),
  income6_to_income3 = data %>% count(income6, income3) %>%
    filter(!(as.integer(income6) %in% 1:3 & income3 == "Under 50k") &
             !(as.integer(income6) %in% 4:5 & income3 == "50k-100k") &
             !(as.integer(income6) == 6 & income3 == "100k+")),
  race5_to_race4   = data %>% count(race5, race_white, race_black, race_other) %>%
    filter(!(race5 == "White" & race_white == "1") &
             !(race5 == "Black" & race_black == "1") &
             !(race5 %in% c("American Indian", "Asian American", "Other") & race_other == "1")),
  age_years_to_age4 = data %>% count(age4, in_band = case_when(age4 == "18-34" ~ age_years %in% 18:34,
                                                              age4 == "35-44" ~ age_years %in% 35:44,
                                                              age4 == "45-64" ~ age_years %in% 45:64,
                                                              age4 == "65+"   ~ age_years >= 65,
                                                              TRUE            ~ NA)) %>%
    filter(!in_band),
  insured_duplicate = data %>% count(insured, insured_recode) %>%
    filter(!(insured == "Covered by health insurance" & insured_recode == "Has Health Insurance") &
             !(insured == "Not covered by health insurance" & insured_recode == "No Health Insurance"))
)

recode.failures <- recode.checks %>%
  keep(~ nrow(.x) > 0)

if (length(recode.failures) > 0) {
  stop("An uncollapsed demographic does not collapse onto the vendor's own recode, so one of the two is wrong:\n",
       paste0("  ", names(recode.failures), ": ", map_int(recode.failures, nrow), " off-diagonal cells", collapse = "\n"))
}

message("Part M.6: all six uncollapsed demographics collapse exactly onto the vendor's recodes ",
        "(educ9 -> educ3, ideo7 -> ideo3, income6 -> income3, race5 -> the race flags, age_years -> age4, ",
        "insured_recode duplicates insured).")


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## M.5. The 0/1 factors ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Every 0/1 factor must have exactly the two levels, in that order, even where only one of them occurs in the data
indicator.vars <- names(data) %>%
  keep(~ is.factor(data[[.x]]) && identical(levels(data[[.x]]), c("0", "1")))

stopifnot("the flags and multi-select items should all be 0/1 factors" =
            all(c(vars.flag, vars.multiselect) %in% indicator.vars))

message("Part M: all validation checks passed (", length(indicator.vars), " variables are 0/1 factors).")




##### #
#### #
### ################################################################################################################################################# #
# Part N. The variable index -------------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# An index of every column in the output. The three columns that matter for reading it:
#
#   var_label     a plain-English description of what the variable measures, so that no abbreviated column name has to
#                 be decoded. For a delivered variable this comes from the combined codebook built in Parts F and G; for a derived
#                 one it is written in H.1 or generated in H.2 from its parent's label.
#   derived_from  for a derived variable, exactly which columns it was built from and how. Empty for delivered columns,
#                 whose source is the qid.
#   origin        delivered / collapsed rank / derived.

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## N.1. Descriptions of the derived variables ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Every variable this script creates rather than reads. `derived_from` names the inputs, so a derivation never has to be
# reverse-engineered out of the code.
derived.map <- tribble(
  ~var_name,                     ~var_label,                                                                            ~derived_from,
  # Geography, from Part J.1. `state` itself is now delivered (demState) and so is described by the codebook, not here.
  "state_zip",                   "State of residence derived from the ZIP code, two-letter abbreviation (a check on the delivered state, not a replacement for it)", "zip, via the zipcodeR ZIP code crosswalk; ZIP prefix fallback for 16 unlisted ZIP codes",
  "state_zip_name",              "State of residence derived from the ZIP code, full name",                              "state_zip, via base R's state.abb / state.name plus the District of Columbia",
  "county",                      "County of residence (the ZIP code's primary county)",                                  "zip, via the zipcodeR ZIP code crosswalk",
  "town",                        "Town of residence (the ZIP code's USPS primary place name)",                           "zip, via the zipcodeR ZIP code crosswalk",
  "zip_match",                   "Whether the geography came from the full ZIP code or the three-digit ZIP prefix",       "zip",
  "flag_state_zip_mismatch",     "The delivered state disagrees with the state implied by the ZIP code (13 respondents)", "state and state_zip_name",
  # Party identification, from Part J.2
  "asked_maga",                  "Was routed to the MAGA-support follow-up, i.e. is a Republican or Republican leaner",   "maga_support_f (non-missing)",
  "asked_prog",                  "Was routed to the progressive-identity follow-up, i.e. is a Democrat or Democratic leaner", "progressive_id_f (non-missing)",
  "pid5",                        "Party identification, five categories, with independents' leans recovered",            "pid3, plus asked_maga / asked_prog to assign a lean to independents",
  "pid5_i",                      "Party identification, five categories, as an integer running Dem (1) to Rep (5)",      "as.integer(pid5)",
  "pid3_lean",                   "Party identification, three categories, with leaners assigned to the party they lean toward", "pid5, collapsing Lean Dem into Dem and Lean Rep into Rep",
  "pid7",                        "Party identification, standard seven-point scale from Strong Democrat to Strong Republican", "pid3, pid_strength (splits partisans) and pid_lean (splits independents)",
  "pid7_i",                      "Party identification, seven-point scale, as an integer running Strong Dem (1) to Strong Rep (7)", "as.integer(pid7)",
  # Race, split sample, and information-block arms, from Parts D.3 to D.5
  "race4",                       "Race and ethnicity, four mutually exclusive categories",                               "race_hispanic, race_white, race_black, race_other, with Hispanic origin taking precedence",
  "ai_ok_split",                 "Which arm of the hidden 50/50 split at BPC38 the respondent was assigned to",          "ai_ok_voter_decide_f and ai_ok_voter_values_align_f (whichever is non-missing)",
  "reg_basis",                   "Whether the how-to-register source question was asked about actual or hypothetical behavior", "seek_register_f (Never = hypothetical arm)",
  "run_basis",                   "Whether the how-elections-are-run source question was asked about actual or hypothetical behavior", "seek_elections_run_f (Never = hypothetical arm)",
  "won_basis",                   "Whether the who-won source question was asked about actual or hypothetical behavior",  "seek_who_won_f (Never = hypothetical arm)",
  # Selection counts, from Part J.6
  "n_src_selected",              "Number of main election-news sources named, excluding the anchor options",             "src_use_* (BPC2), anchors excluded",
  "n_outlet_selected",           "Number of print/online news outlets named, excluding the anchor options",              "outlet_use_* (BPC5), anchors excluded",
  "n_social_selected",           "Number of social media platforms named, excluding the anchor options",                 "social_use_* (BPC6), anchors excluded",
  "n_bot_selected",              "Number of AI chatbots named, excluding the anchor options",                            "bot_use_* (BPC10), anchors excluded",
  # Data quality, from Part J.7
  "sl_concern",                  "Straightlined the election-concern matrix (same answer to every item)",                "concern_*_f (BPC44)",
  "sl_ai_ok",                    "Straightlined the AI good/bad matrix (same answer to every item)",                     "ai_ok_*_f (BPC38)",
  "sl_seek",                     "Straightlined the information-seeking matrix (same answer to every item)",             "seek_*_f (BPC1)",
  "n_offscale",                  "Number of off-scale answers ('Don't know' and similar) across the ten scale items everyone was asked", "the ten variables listed in vars.universal.scales",
  "flag_vote_contradiction",     "Says they do not plan to vote, yet rates their own likelihood of voting 8 or higher",   "conf_own_vote_f and vote_likelihood_i",
  "flag_interest_contradiction", "Took the exclusive 'not interested in election news' anchor, yet seeks election information", "src_use_not_interested and the three seek_*_f variables"
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## N.2. Descriptions generated from a parent variable ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The remaining derived columns are all systematic variants of a delivered variable — the two companions to each ordinal
# variable, the 36 pooled source columns, and the 36 collapsed rank slots — so their descriptions are built from their
# parent's rather than written out 118 times.

# For each ordinal variable, the two ends of its substantive scale and how many points it has, so that the integer
# companion's description states what 1 and k actually mean rather than a vague "higher = more"
ordinal.ends <- codebook.combined %>%
  filter(var_type == "ordinal", !is.na(ord_position)) %>%
  arrange(var_name, ord_position) %>%
  group_by(var_name) %>%
  summarise(scale_low  = first(label),
            scale_high = last(label),
            scale_k    = n(),
            .groups = "drop")

# The off-scale labels for each ordinal variable, which is what the _f version adds back. Empty for the scales that
# have none, such as the demographic bands.
ordinal.offscale <- codebook.combined %>%
  filter(var_type == "ordinal", !is.na(label)) %>%
  group_by(var_name) %>%
  summarise(offscale = paste(label[is_nonscale], collapse = ", "), .groups = "drop")

# The two companions to every ordinal variable
labels.ordinal.variants <- bind_rows(
  var.meta %>%
    filter(var_type == "ordinal") %>%
    left_join(ordinal.ends, by = "var_name") %>%
    transmute(var_name     = paste0(var_name, "_i"),
              var_label    = paste0(var_label, " [integer: 1 = ", scale_low, " ... ", scale_k, " = ", scale_high, "]"),
              derived_from = paste0("as.integer(", var_name, ")")),
  var.meta %>%
    filter(var_type == "ordinal") %>%
    left_join(ordinal.offscale, by = "var_name") %>%
    transmute(var_name     = paste0(var_name, "_f"),
              var_label    = case_when(offscale == "" ~ paste0(var_label, " [all response options]"),
                                       TRUE ~ paste0(var_label, " [all response options, including ", offscale, "]")),
              derived_from = paste0(qid, " (every delivered label retained)"))
)

# The pooled source columns from D.5, whose label has to describe both arms at once
labels.pooled <- select.long %>%
  filter(base %in% c("BPC11", "BPC12", "BPC19", "BPC20", "BPC27", "BPC28")) %>%
  distinct(battery, base, item_tag, var_label) %>%
  mutate(block = str_sub(battery, 1, 3),
         need  = case_when(block == "reg" ~ "how to register and vote",
                           block == "run" ~ "how elections are run",
                           block == "won" ~ "who won an election")) %>%
  distinct(block, need, item_tag, .keep_all = TRUE) %>%
  transmute(var_name     = paste(block, "src", item_tag, sep = "_"),
            var_label    = paste0("Sources used or would use to find information about ", need,
                                  ", both arms pooled: ", str_remove(var_label, "^.*?: ")),
            derived_from = paste0(block, "_act_src_", item_tag, " and ", block, "_hyp_src_", item_tag,
                                  ", coalesced (exactly one is non-missing per respondent)"))

# The collapsed rank slots and counts from C.4
# One description per rank battery: the family's stem with the item text cut off, so "Rank among sources used to find
# information about who won an election: Local election officials" becomes just the part before the colon.
labels.rank.stem <- var.meta %>%
  filter(var_type == "rank") %>%
  mutate(var_label = str_remove(var_label, ": .*$")) %>%
  distinct(base, var_label)

labels.rank <- bind_rows(
  rank.long %>%
    distinct(rank_base, rank_prefix, rank) %>%
    left_join(labels.rank.stem, by = c("rank_base" = "base")) %>%
    transmute(var_name     = paste(rank_prefix, slot.names[rank], sep = "_"),
              var_label    = paste0(var_label, ": item ranked ", slot.names[rank]),
              derived_from = paste0("the ", rank_base, " per-item rank columns, collapsed in Part I.4")),
  rank.long %>%
    distinct(rank_base, rank_prefix) %>%
    left_join(labels.rank.stem, by = c("rank_base" = "base")) %>%
    transmute(var_name     = paste0(rank_prefix, "_n"),
              var_label    = paste0(var_label, ": number of items ranked"),
              derived_from = paste0("the ", rank_base, " per-item rank columns, counted in Part I.4"))
) %>%
  distinct(var_name, .keep_all = TRUE)

# Stacking all of the derived descriptions together
derived.labels <- bind_rows(derived.map, labels.ordinal.variants, labels.pooled, labels.rank)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## N.3. Building the index ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

variable.index <- tibble(var_name = names(data),
                         class    = map_chr(data, ~ class(.x)[1]),
                         n_miss   = map_int(data, ~ sum(is.na(.x))),
                         n_unique = map_int(data, ~ n_distinct(.x, na.rm = TRUE))) %>%
  # Descriptions and metadata for the delivered variables, from the codebook
  left_join(var.meta %>% select(var_name, var_label, qid, base, var_type, scale_family, item_tag, base_condition),
            by = "var_name") %>%
  # Descriptions for the derived ones, from H.1 and H.2
  left_join(derived.labels %>% rename(var_label_derived = var_label), by = "var_name") %>%
  mutate(var_label = coalesce(var_label, var_label_derived),
         origin    = case_when(!is.na(qid) ~ "delivered",
                               str_detect(var_name, "_(1st|2nd|3rd|[0-9]+th|n)$") ~ "collapsed rank",
                               TRUE ~ "derived"),
         pct_miss  = round(100 * n_miss / nrow(data), 2)) %>%
  select(var_name, var_label, origin, class, derived_from, qid, base, var_type, scale_family,
         item_tag, base_condition, n_miss, pct_miss, n_unique)

# Every column in the output must carry a description, or the index fails at its one job
labels.missing <- variable.index %>% filter(is.na(var_label))

if (nrow(labels.missing) > 0) {
  stop("Columns in the output with no description in the variable index (add them to H.1):\n",
       paste0("  ", labels.missing$var_name, collapse = "\n"))
}




##### #
#### #
### ################################################################################################################################################# #
# Part O. Design summary and writing the output ------------------------------------------------------------------------------------------------------ ----
### ################################################################################################################################################# #
#### #
##### #

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## O.1. Design summary ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The weights are unequal, so the sample carries less information than its 3,144 interviews suggest. Kish's effective n
# is what a simple random sample of the same precision would have to be, and the margin of error should be quoted off
# that rather than off the nominal n.
design.summary <- data %>%
  summarise(n          = n(),
            weight_sum = sum(weight),
            weight_min = min(weight),
            weight_max = max(weight),
            weight_cv  = sd(weight) / mean(weight),
            deff_cv    = 1 + (sd(weight) / mean(weight))^2,             # design effect from the weight variation
            n_eff_kish = sum(weight)^2 / sum(weight^2),                 # Kish's effective sample size
            moe_50_pct = 1.96 * sqrt(0.25 / (sum(weight)^2 / sum(weight^2))) * 100)


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## O.2. Writing ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The .rds files are the ones to analyze, because they keep the factor levels and orderings. The .csv files are for
# reading and for handing to someone who is not using R; factors are written as their labels.
saveRDS(object = data,         file = file.path(out.dir, "eis_2026_clean.rds"))
saveRDS(object = sources.long, file = file.path(out.dir, "eis_2026_sources_long.rds"))

# Writing out the combined codebook that Parts F and G built. It costs nothing, since it exists in memory either way,
# and it is what makes the cleaning auditable: every name, label, response ordering and off-scale code this script
# applied is in one table, so a reader can check any recode without reading the code that produced it.
write.csv(x = codebook.combined,
          file = file.path(out.dir, "eis_2026_codebook_combined.csv"),
          row.names = FALSE,
          na = "")

write.csv(x = data %>% mutate(across(where(is.factor), as.character)),
          file = file.path(out.dir, "eis_2026_clean.csv"),
          row.names = FALSE,
          na = "")

# Just under a million rows, so gzipped. read.csv() and readr::read_csv() both read a .csv.gz transparently.
write.csv(x = sources.long %>% mutate(across(where(is.factor), as.character)),
          file = gzfile(file.path(out.dir, "eis_2026_sources_long.csv.gz")),
          row.names = FALSE,
          na = "")

write.csv(x = battery.info,
          file = file.path(out.dir, "eis_2026_batteries.csv"),
          row.names = FALSE,
          na = "")

write.csv(x = variable.index,
          file = file.path(out.dir, "eis_2026_variable_index.csv"),
          row.names = FALSE,
          na = "")


# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## O.3. Run summary ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

message("\nClean data written to ", out.dir, "/\n",
        "  respondents:              ", nrow(data), "\n",
        "  columns:                  ", ncol(data),
        "  (delivered ", sum(variable.index$origin == "delivered"),
        ", collapsed rank ", sum(variable.index$origin == "collapsed rank"),
        ", derived ", sum(variable.index$origin == "derived"), ")\n",
        "  0/1 indicator factors:    ", length(indicator.vars), "\n",
        "  long source table rows:   ", format(nrow(sources.long), big.mark = ","), "\n",
        "  states represented:       ", n_distinct(data$state, na.rm = TRUE), "\n",
        "  counties represented:     ", n_distinct(data$county, na.rm = TRUE), "\n",
        "  design effect (1+CV^2):   ", round(design.summary$deff_cv, 3), "\n",
        "  effective n (Kish):       ", round(design.summary$n_eff_kish), "\n",
        "  margin of error at 50%:   +/-", round(design.summary$moe_50_pct, 2), " points\n",
        "  straightlined BPC44:      ", sum(data$sl_concern == "1"), " (",
        round(100 * mean(data$sl_concern == "1"), 1), "%)\n",
        "  straightlined BPC38:      ", sum(data$sl_ai_ok == "1"), " (",
        round(100 * mean(data$sl_ai_ok == "1"), 1), "%)\n",
        "  straightlined BPC1:       ", sum(data$sl_seek == "1"), " (",
        round(100 * mean(data$sl_seek == "1"), 1), "%)\n",
        "  vote-intent contradiction:   ", sum(data$flag_vote_contradiction == "1"), "\n",
        "  interest contradiction:      ", sum(data$flag_interest_contradiction == "1"))




##### #
#### #
### ################################################################################################################################################# #
# Part P. The open-end responses ---------------------------------------------------------------------------------------------------------------------- ----
### ################################################################################################################################################# #
#### #
##### #

# WHAT THIS FILE IS, AND WHAT IT IS NOT. Morning Consult sent 'bipartisan_policy_center_open_ends.xlsx' with the V2
# delivery. It is NOT a verbatim file. It holds aggregated counts of cleaned response text, in two sheets:
#
#   summary_response_counts  one row per (question, distinct response text), with the number of mentions
#   single_word_counts       the same, after splitting each response into individual words
#
# THREE THINGS TO KNOW BEFORE USING IT:
#
#   1. There is NO ResponseID. The counts cannot be joined back to a respondent, so an open-end answer cannot be crossed
#      against party identification, platform use, or anything else in the main file. This is the biggest limitation.
#   2. `n` counts MENTIONS, not respondents. Where a respondent's text was split into more than one coded mention the
#      count exceeds the number of people: BPC5_29 shows 68 mentions from the 63 respondents who ticked "Other". So `n`
#      cannot be read as a respondent count, and `pct_of_demo` is the share of MENTIONS within that question.
#   3. It covers the "Other, please specify" boxes ONLY. BPC7, BPC8 and BPC9 — the three standalone open ends, on the
#      accounts and influencers people follow, the podcasts they listen to, and the newsletters they read — produced no
#      columns in the raw data and appear nowhere here either. Neither do the 121 "Other" answers at nr3, the
#      most-important-issue question. Those are still outstanding with the vendor.
#
# What this part does is make the file readable: the workbook keys questions by raw qid with a _TEXT suffix, so joining
# the codebook onto it replaces BPC5_29_TEXT with outlet_use_other and its plain-English label.

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## P.1. Reading and labelling the open ends ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Reading both sheets and stacking them, keeping a column that says which sheet each row came from
open.ends <- bind_rows(
  readxl::read_excel(path.open.ends, sheet = "summary_response_counts") %>% mutate(sheet = "full response"),
  readxl::read_excel(path.open.ends, sheet = "single_word_counts")      %>% mutate(sheet = "single word")
) %>%
  # Stripping _TEXT leaves the key of the option that opened the text box, and the key takes two forms:
  #   BPC5_29_TEXT  -> BPC5_29, a multi-select "Other" checkbox, which IS a delivered column
  #   BPC13_4_TEXT  -> BPC13 code 4, a single-select "Other" option, where the column is BPC13
  # Taking the whole key as the qid where that is a delivered column, and otherwise dropping the trailing code.
  mutate(key       = str_remove(question, "_TEXT$"),
         qid       = if_else(key %in% var.meta$qid, key, str_remove(key, "_[0-9]+$")),
         # The response code that opens the box, needed to count the right respondents in J.2
         other_code = if_else(key %in% var.meta$qid, 1L, as.integer(str_extract(key, "(?<=_)[0-9]+$")))) %>%
  left_join(var.meta %>% select(qid, var_name, var_label), by = "qid") %>%
  select(sheet, qid, other_code, var_name, var_label, text, n, pct_of_mentions = pct_of_demo) %>%
  arrange(sheet, var_name, desc(n))

# Every question in the workbook must map onto a delivered column, or the join has gone wrong
open.ends.unmatched <- open.ends %>%
  filter(is.na(var_name)) %>%
  distinct(qid)

if (nrow(open.ends.unmatched) > 0) {
  stop("Open-end questions that match no delivered column:\n",
       paste0("  ", open.ends.unmatched$qid, collapse = "\n"))
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - -
## P.2. Reconciling the mention counts against the data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - -

# For each open end, comparing the mentions in the workbook against the number of respondents who actually chose
# "Other" in the main file. They should be equal or the workbook should be slightly higher (see note 2 above); the
# workbook being LOWER would mean verbatims are missing.
open.ends.reconciled <- open.ends %>%
  filter(sheet == "full response") %>%
  group_by(qid, other_code, var_name) %>%
  summarise(n_mentions = sum(n), n_distinct_texts = n(), .groups = "drop") %>%
  mutate(n_chose_other = map2_int(var_name, other_code, ~ sum(data0[[.x]] == .y, na.rm = TRUE)),
         shortfall     = n_chose_other - n_mentions)

if (any(open.ends.reconciled$shortfall > 0)) {
  warning("Open ends where fewer verbatims were delivered than respondents who chose 'Other':\n",
          paste0("  ", open.ends.reconciled$var_name[open.ends.reconciled$shortfall > 0], ": ",
                 open.ends.reconciled$shortfall[open.ends.reconciled$shortfall > 0], " missing", collapse = "\n"),
          call. = FALSE)
}

write.csv(x = open.ends,
          file = file.path(out.dir, "eis_2026_open_ends.csv"),
          row.names = FALSE,
          na = "")

message("Part P: read ", nrow(open.ends), " open-end rows covering ", n_distinct(open.ends$var_name),
        " 'Other, please specify' boxes and ", sum(open.ends.reconciled$n_mentions), " mentions from ",
        sum(open.ends.reconciled$n_chose_other), " respondents who chose 'Other'. ",
        "BPC7, BPC8, BPC9 and the 121 'Other' answers at nr3 are still not delivered.")




# The end.
