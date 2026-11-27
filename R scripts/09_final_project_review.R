# Check required R packages

required_packages <- c(
  "tidyverse",
  "here",
  "readxl",
  "janitor",
  "gtsummary",
  "broom",
  "car",
  "ResourceSelection",
  "pROC",
  "ggplot2"
)

package_status <- data.frame(
  package = required_packages,
  installed = vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
)

package_status




# Check data file availability

data_files <- c(
  here("Data", "Raw", "covid_vaccine_raw.rds"),
  here("Data", "Cleaned", "covid_vaccine_cleaning_stage1.rds"),
  here("Data", "Cleaned", "covid_vaccine_cleaned.rds"),
  here("Data", "Cleaned", "covid_vaccine_cleaned.csv")
)

data_file_status <- data.frame(
  file = basename(data_files),
  exists = file.exists(data_files)
)

data_file_status


# Load raw and cleaned datasets

raw_data <- readRDS(
  here("Data", "Raw", "covid_vaccine_raw.rds")
)

cleaned_data <- readRDS(
  here("Data", "Cleaned", "covid_vaccine_cleaned.rds")
)

# Compare dataset dimensions

data_integrity_check <- data.frame(
  dataset = c("Raw data", "Cleaned data"),
  observations = c(nrow(raw_data), nrow(cleaned_data)),
  variables = c(ncol(raw_data), ncol(cleaned_data))
)

data_integrity_check



# Check final outcome variable

table(
  cleaned_data$covid_vaccine_uptake,
  useNA = "ifany"
)

# Check outcome proportions

prop.table(
  table(cleaned_data$covid_vaccine_uptake)
) * 100

# Load final logistic regression model

final_model_check <- readRDS(
  here("Outputs", "models", "final_logistic_regression_model.rds")
)

# Check model information

summary(final_model_check)

# Check model convergence

final_model_check$converged

# Check final model formula

formula(final_model_check)

# Count model predictors

length(attr(terms(final_model_check), "term.labels"))

# Check model sample size

nobs(final_model_check)

# Check final model performance

final_model_performance <- data.frame(
  AIC = AIC(final_model_check),
  Null_Deviance = final_model_check$null.deviance,
  Residual_Deviance = deviance(final_model_check),
  Degrees_of_Freedom = df.residual(final_model_check)
)

final_model_performance




# Calculate predicted probabilities

predicted_probabilities <- predict(
  final_model_check,
  type = "response"
)

# Generate ROC curve

roc_check <- roc(
  cleaned_data$covid_vaccine_uptake,
  predicted_probabilities,
  levels = c("No", "Yes")
)

# Calculate AUC and 95% CI

auc(roc_check)
ci.auc(roc_check)

# Hosmer-Lemeshow goodness-of-fit test

hl_check <- hoslem.test(
  as.numeric(cleaned_data$covid_vaccine_uptake == "Yes"),
  predicted_probabilities,
  g = 10
)

hl_check

# Verify final tables and figures

final_output_files <- c(
  here("Outputs", "final_results", "Table1_Participant_Characteristics.csv"),
  here("Outputs", "final_results", "Table2_Bivariate_Analysis.csv"),
  here("Outputs", "final_results", "Table3_Multivariable_Logistic_Regression.csv"),
  here("Outputs", "final_results", "Figure1_Forest_Plot.png")
)

file.info(final_output_files)[, c("size", "mtime")]



# Load required packages

library(tidyverse)
library(here)

# Import cleaned dataset

data_check <- readRDS(
  here("Data", "Cleaned", "covid_vaccine_cleaned.rds")
)

# Verify imported dataset

dim(data_check)




# Load final logistic regression model

model_check <- readRDS(
  here("Outputs", "models", "final_logistic_regression_model.rds")
)

# Verify model

formula(model_check)

# Check sample size

nobs(model_check)

nobs(model_check)



# Import final results tables

table1_check <- read.csv(
  here("Outputs", "final_results",
       "Table1_Participant_Characteristics.csv")
)

table2_check <- read.csv(
  here("Outputs", "final_results",
       "Table2_Bivariate_Analysis.csv")
)

table3_check <- read.csv(
  here("Outputs", "final_results",
       "Table3_Multivariable_Logistic_Regression.csv")
)

# Check table dimensions

dim(table1_check)
dim(table2_check)
dim(table3_check)

# Verify final forest plot

forest_plot_file <- here(
  "Outputs",
  "final_results",
  "Figure1_Forest_Plot.png"
)

file.exists(forest_plot_file)

file.info(forest_plot_file)$size

# Final Verification

file_check

file.info(
  file.path(final_results_dir, expected_files)
)$size


# Verify main project folders

main_folders <- c(
  "Data/Raw",
  "Data/Cleaned",
  "Outputs/figures",
  "Outputs/tables",
  "Outputs/models",
  "Outputs/reports",
  "Outputs/final_results",
  "R scripts"
)

folder_check <- data.frame(
  Folder = main_folders,
  Exists = dir.exists(
    here::here(main_folders)
  )
)

print(folder_check)






# Verify all project R scripts

script_files <- sprintf(
  "%02d_%s.R",
  1:8,
  c(
    "import_data",
    "data_cleaning",
    "exploratory_data_analysis",
    "descriptive_analysis",
    "bivariate_analysis",
    "multivariable_logistic_regression",
    "model_diagnostics",
    "results_presentation"
  )
)

script_check <- data.frame(
  Script = script_files,
  Exists = file.exists(
    here::here("R scripts", script_files)
  )
)

print(script_check)




# List all R scripts in the project

list.files(
  here::here("R scripts"),
  pattern = "\\.R$",
  full.names = FALSE
)


# Verify all existing R scripts

script_files <- list.files(
  here::here("R scripts"),
  pattern = "\\.R$",
  full.names = FALSE
)

script_check <- data.frame(
  Script = script_files,
  Exists = file.exists(
    here::here("R scripts", script_files)
  )
)

print(script_check)

# Count verified scripts
sum(script_check$Exists)



# Verify essential project files

essential_files <- c(
  "COVID_Vaccine_Uptake_Somaliland.Rproj",
  "Data/Raw/covid_vaccine_raw.rds",
  "Data/Cleaned/covid_vaccine_cleaning_stage1.rds",
  "Data/Cleaned/covid_vaccine_cleaned.rds",
  "Data/Cleaned/covid_vaccine_cleaned.csv",
  "Outputs/models/final_logistic_regression_model.rds",
  "Outputs/reports/COVID_Vaccine_Uptake_Results.docx"
)

essential_check <- data.frame(
  File = essential_files,
  Exists = file.exists(here::here(essential_files))
)

print(essential_check)



# Create project README file

readme_file <- here::here("README.md")

file.create(readme_file)

file.exists(readme_file)












