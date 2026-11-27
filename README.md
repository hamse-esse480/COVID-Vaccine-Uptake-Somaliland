# COVID-19 Vaccine Uptake in Somaliland

## Project Overview

This project analyzes COVID-19 vaccine uptake and its associated factors among the study participants in Somaliland. The analysis uses survey data to describe participant characteristics, examine factors associated with vaccine uptake, and identify independent predictors using multivariable logistic regression.

## Study Objectives

1. To describe COVID-19 vaccine uptake among study participants.
2. To assess the association between selected sociodemographic, health-related, and perception-related characteristics and vaccine uptake.
3. To identify factors independently associated with COVID-19 vaccine uptake using multivariable logistic regression.

## Dataset

- Total observations: 704
- Total variables: 20
- Outcome variable: COVID-19 vaccine uptake
- Outcome distribution:
  - No: 445 (63.21%)
  - Yes: 259 (36.79%)

## Methods of Analysis

- Data importation and cleaning
- Exploratory data analysis
- Descriptive analysis
- Bivariate analysis
- Multivariable binary logistic regression
- Model diagnostics and performance assessment

## Key Findings

Factors significantly associated with COVID-19 vaccine uptake in the adjusted logistic regression model included:

- Age group
- Educational level
- Vaccine availability
- Previous COVID-19 testing
- Perceived COVID-19 threat

The final model had an AUC of 0.759 (95% CI: 0.722–0.795).

These findings represent statistical associations and should not be interpreted as evidence of causation.

## Software and Packages

- R
- RStudio
- tidyverse
- gtsummary
- broom
- car
- ResourceSelection
- pROC
- here
- ggplot2

- 
## Project Structure

```text
COVID_Vaccine_Uptake_Somaliland/
│
├── COVID_Vaccine_Uptake_Somaliland.Rproj
├── README.md
│
├── Documentation/
│   ├── Dataset_Codebook
│   └── Project_Summary
│
├── figures/
│   └── Final_Forest_Plot
│
├── final_results/
│   ├── Table1_Participant_Characteristics
│   ├── Table2_Bivariate_Analysis
│   └── Table3_Multivariable_Logistic_Regression
│
├── reports/
│   └── Final_Analysis_Report
│
├── tables/
│   └── Final_Analysis_Tables
│
└── R scripts/
    ├── 01_import_data.R
    ├── 02_data_assessment.R
    ├── 03_data_cleaning.R
    ├── 04_Exploratory Data Analysis.R
    ├── 05_descriptive_analysis.R
    ├── 06_bivariate_analysis.R
    ├── 07_multivariable_analysis.R
    ├── 08_results_presentation.R
    ├── 09_model_diagnostics.R
    └── 10_final_project_review.R
```
```

## Note

This project was developed for epidemiological data analysis and research purposes. Interpretation of findings should consider the study design, sampling approach, and limitations of the dataset.
