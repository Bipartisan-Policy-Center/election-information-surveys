# BPC Election Information Surveys

This repository contains the data, analysis, and visualizations for surveys conducted by the Bipartisan Policy Center on where Americans seek election information. Four surveys—similar but with slight variations—were carried out by Morning Consult in October 2022, December 2023, October 2024, and September 2026.

## September 2026

Our 2026 poll was conducted by Morning Consult between September 2-4, 2026 among a sample of 3,144 registered voters nationally. Results from the full survey have a margin of error of plus or minus 2 percentage points.

The data, codebooks, survey instrument, and R code that reproduce the figures and statistics in the BPC article "[What Voters Think Ahead of the 2026 Midterms: Election Information, Confidence, and Concerns](https://bipartisanpolicy.org/explainer/what-voters-think-ahead-of-the-2026-midterms-election-information-confidence-and-concerns/)" are in the [2026 folder](2026/). See the [2026 README](2026/README.md) for instructions.

## October 2024

2024 data includes results from two surveys conducted by Morning Consult. The first was fielded from October 11-12 among a sample of 1,743 registered voters. The second was fielded from October 16-17 among a sample of 1,891 registered voters. 

One question (“BPC3: In the United States, there is information voters need to register and vote. Where are you most likely to look for this information?”) was accidentally overwritten by another question (“BPC4: If you wanted to know more about how elections are run in the United States, where are you most likely to look for that information?”) in the first field. As such, we combined responses from the two surveys for all questions except BPC3. Accordingly, the total sample size for BPC3 is 1,891 and the sample size for all other questions is 3,634.

Results and analysis here: Rachel Orey, William T. Adler, Theo Menon, Julianne Lampert. (October 31, 2024). [Where Voters Look for Information Ahead of the 2024 Election](https://bipartisanpolicy.org/blog/where-voters-look-for-information-ahead-of-the-2024-election). _Bipartisan Policy Center._

## December 2023

Our 2023 poll was conducted by Morning Consult between December 13- 15, 2023 among a sample of 2,203 adults.

Results and analysis here: Jeff Allen, Katie Harbath, Rachel Orey, Thania Sanchez. (February 26, 2024). [Who Voters Trust for Election Information in 2024](https://bipartisanpolicy.org/explainer/who-voters-trust-election-information-2024/). _Bipartisan Policy Center._

## October 2022

Our 2022 poll was conducted by Morning Consult between October 14-15, 2022 among a sample of 2,002 registered voters nationally. Results from the full survey have a margin of error of plus or minus 3 percentage points.

* *CO voters oversample*: Additional poll conducted between October 14-20, 2022 among a sample of 805 Colorado voters. 

* *GA voters oversample*: Additional poll conducted between October 14-17, 2022 among a sample of 809 Georgia voters.

* *WI voters oversample*: Additional poll conducted between October 14-24 2022 among a sample of 501 Wisconsin voters.


Results and analysis here: Katie Harbath, Collier Fernekes, Rachel Orey, Mara Suttmann-Lea, Michael Wagner. (November 2, 2022). [New Survey Data on Who Americans Look to For Election Information](https://bipartisanpolicy.org/blog/new-survey-data-election-information/). _Bipartisan Policy Center._



## Folder structure

The repository is organized by year, with each year containing folders for raw and processed data, as well as codebooks and analysis files specific to that survey year.

The 2026 folder is the exception. It is a self-contained set of replication materials, with numbered R scripts, a `data/` folder, and a `documentation/` folder, described in its own [README](2026/README.md).

The general structure of the other years' folders is as follows.
``` yml
BPC-ELECTION-INFORMATION-SURVEYS/
├── year/
│   ├── raw/
│   │   ├── data.csv                 # Raw survey data
│   │   ├── levels_codebook.csv      # Codebook for categorical levels
│   │   └── question_codebook.csv    # Codebook for questions asked
│   └── processed/                   # Processed and cleaned data in .csv format

│   ├── mc_deck.pdf                  # PDF of the survey analysis deck provided by Morning Consult
│   ├── survey_instrument.docx       # Survey questions document in Word format
│   ├── survey_instrument.md         # Markdown version of survey questions
│   └── analyses.ipynb               # Jupyter Notebook with analysis
```
2024 has additional subfolders for multiple survey fields, detailed below. 
```yml
├── 2024/
│   ├── raw/
│   │   ├── field1/                  # Data for first survey field with BPC3 overwritten by BPC4 (general population)
│   │   │   ├── data.csv
│   │   │   ├── levels_codebook.csv 
│   │   │   └── question_codebook.csv
│   │   ├── field2/                  # Data for second survey field with corrected BPC3
│   │   │   ├── genpop/              
│   │   │   │   └── data.csv         # General population sample data for field2
│   │   │   ├── rvoter/              
│   │   │   │   └── data.csv         # Registered voters sample data for field2 (filtered and reweighted)
│   │   ├── stacked/                 
│   │   │   └── data.csv             # Combined data from field1 and field2 (registered voters only)

```
## Survey methodology

The interviews were conducted online with data weighted to approximate a target sample of registered voters based on age and gender, educational attainment, race, marital status, homeownership, race by education, 2020 presidential vote, and region. The results have a margin of error of ±2 percentage points, except for the 2022 state oversamples which have a margin of error of ±3-4 percentage points.

For 2026, the data were weighted to approximate a target sample of registered voters based on gender, age, race, educational attainment, region, gender by age, and race by educational attainment.
