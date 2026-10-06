# 2026 Election Information Survey: Replication Materials

This folder contains the data, documentation, and R code needed to reproduce the figures and statistics in the Bipartisan Policy Center's article "[What Voters Think Ahead of the 2026 Midterms: Election Information, Confidence, and Concerns.](https://bipartisanpolicy.org/explainer/what-voters-think-ahead-of-the-2026-midterms-election-information-confidence-and-concerns/)" The article draws on a 2026 survey of 3,144 registered voters, conducted by Morning Consult for BPC, and compares the results with BPC's 2022 and 2024 surveys of registered voters.

## Contents

```
2026/
├── README.md
├── 01_clean_data.R
├── 02_common_weights.R
├── 03_cumulative_data.R
├── 04_analysis.R
├── data/            Files the scripts read
├── documentation/   Reference material that no script reads
├── embeds/          HTML for the confidence chart grid (not needed to run the analysis)
└── output/          Created when the scripts run (not included in this repository)
```

## Requirements

The scripts were developed and tested with R 4.5.2, tidyverse 2.0.0, survey 4.4.8, and zipcodeR 0.4.0. Install the packages with:

```r
install.packages(c("tidyverse", "survey", "zipcodeR"))
```

The readxl and scales packages that the scripts also use are installed with the tidyverse.

## Instructions

1. Download this folder and keep its structure as is, with the scripts next to the `data/` folder.
2. Set R's working directory to this folder, for example with `setwd()` or by opening the folder as a project in RStudio. All file paths in the scripts are relative to the working directory.
3. Run the four scripts in numerical order. Each script after the first reads files created by the earlier ones.

To run a script from a terminal, use `Rscript 01_clean_data.R` from this folder, and so on. In RStudio, run `04_analysis.R` line by line, or use Source with Echo, so that each figure is displayed. (With `Rscript`, the figures are saved to a file named `Rplots.pdf`.)

To confirm that everything ran, check that `03_cumulative_data.R` prints a message that all validation checks passed (n = 7,037), and that `n_2026_respondents` in `04_analysis.R` equals 3,144.

## Scripts

| Script | What it does |
|---|---|
| `01_clean_data.R` | Cleans Morning Consult's 2026 data using its codebooks, adding variable names, labels, and derived variables |
| `02_common_weights.R` | Re-weights the 2022, 2024, and 2026 data to one common demographic target, so that year-to-year differences are not driven by differences in weighting targets |
| `03_cumulative_data.R` | Stacks the three years into one dataset holding the demographics, weights, and questions that are comparable across years |
| `04_analysis.R` | Reproduces Figures 1 through 4 and the standalone statistics in the article, plus supplementary figures with 95% confidence intervals |

## Data

The scripts read these files from the `data/` folder.

| File | Description |
|---|---|
| `2608077_BPC_raw data_Voter Information Survey_RVs_V2.csv` | 2026 survey responses from 3,144 registered voters |
| `2608077_BPC_question codebook_Voter Information Survey_RVs_V2.csv` | Question text for each column in the raw data |
| `2608077_BPC_level codebook_Voter Information Survey_RVs_V2.csv` | Response labels for each coded value |
| `bipartisan_policy_center_open_ends.xlsx` | Counts of write-in ("Other, please specify") responses |
| `eis_2024_field2_rvoter_data.csv` | 2024 survey responses from 1,891 registered voters |
| `eis_2022_data.csv` | 2022 survey responses (the scripts use only the national sample of 2,002 registered voters, not the three state oversamples) |

## Documentation

The `documentation/` folder holds reference material. No script reads these files.

| File | Description |
|---|---|
| `2026_BPC_Voter Information Survey.docx` | The 2026 survey questionnaire |
| `2608077_BPC_Voter Information Survey_Analysis.pdf` | Morning Consult's report on the 2026 survey results |
| `2608077_BPC_banners_Voter Information Survey_RVs.xlsx` | Morning Consult's crosstabs by demographic group, with one tab per question |

Questionnaires and data for the 2022 and 2024 surveys are also in the `2022/` and `2024/` folders at the top level of this repository.

## Output

The scripts create an `output/` folder holding the cleaned and combined datasets, the common weights, and, in `output/plots/`, the data behind each figure as CSV files. Nothing in `output/` is included in this repository. Running the scripts regenerates it.

## Notes

- Estimates for a single year use the survey weights Morning Consult delivered. Most cross-year comparisons use the common weights built by `02_common_weights.R`, and the comments in `04_analysis.R` identify the weight used for each estimate.
- In Figures 3 and 4, "confident" and "concerned" combine the "very" and "somewhat" response options.
- The 2024 comparisons use the second of BPC's two October 2024 surveys. See the top-level README for details on both.
- `04_analysis.R` prints several warnings reading "glm.fit: algorithm did not converge" in its appendix. They are expected and do not affect the figures in the article. The note at the top of the script explains why.

## Questions

To report a problem or ask a question, open an issue on this repository.
