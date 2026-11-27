
# ============================================================
# : FINAL PROJECT DOCUMENTATION AND SUBMISSION PREPARATION
# Project: COVID-19 Vaccine Uptake in Somaliland
# ============================================================

library(tidyverse)
library(here)

# 10.1 README Documentation
# README.md created in the project root.

readme_file <- here("README.md")

# 10.2 Dataset Codebook

data <- readRDS(
  here("Data", "Cleaned", "covid_vaccine_cleaned.rds")
)

codebook <- data.frame(
  Variable = names(data),
  Description = c(
    "Unique participant identification number",
    "Language used for the survey",
    "Mode of survey administration",
    "COVID-19 vaccine uptake status",
    "Willingness to receive COVID-19 vaccination",
    "Participant sex",
    "Participant age group",
    "Educational level",
    "Marital status",
    "Region of residence",
    "Employment status",
    "Healthcare worker status",
    "Pre-existing health condition",
    "Availability of COVID-19 vaccine",
    "Perceived vulnerability to COVID-19",
    "Previous COVID-19 testing",
    "Result of previous COVID-19 test",
    "Perceived threat of COVID-19",
    "Perceived vaccine safety",
    "Duration of social media use"
  ),
  Data_Type = sapply(data, function(x) class(x)[1]),
  Categories = sapply(data, function(x) {
    if (is.factor(x) || is.character(x)) {
      paste(sort(unique(na.omit(x))), collapse = "; ")
    } else {
      "Numeric"
    }
  })
)

write.csv(
  codebook,
  here("Outputs", "tables", "Dataset_Codebook.csv"),
  row.names = FALSE
)

# 10.3 Final Submission Folder

submission_dir <- here("Final_Submission")

dir.create(
  submission_dir,
  showWarnings = FALSE
)

subfolders <- c(
  "Final_Results",
  "Report",
  "Documentation",
  "Analysis_Scripts"
)

for (folder in subfolders) {
  dir.create(
    file.path(submission_dir, folder),
    showWarnings = FALSE,
    recursive = TRUE
  )
}

# Copy final results

final_results <- c(
  "Table1_Participant_Characteristics.csv",
  "Table2_Bivariate_Analysis.csv",
  "Table3_Multivariable_Logistic_Regression.csv",
  "Figure1_Forest_Plot.png"
)

file.copy(
  file.path(here("Outputs", "final_results"), final_results),
  file.path(submission_dir, "Final_Results"),
  overwrite = TRUE
)

# Copy final report

file.copy(
  here("Outputs", "reports", "COVID_Vaccine_Uptake_Results.docx"),
  file.path(submission_dir, "Report"),
  overwrite = TRUE
)

# Copy documentation

documentation_files <- c(
  "README.md",
  "Dataset_Codebook.csv",
  "Project_Summary.md"
)

documentation_sources <- c(
  here("README.md"),
  here("Outputs", "tables", "Dataset_Codebook.csv"),
  file.path(submission_dir, "Documentation", "Project_Summary.md")
)

file.copy(
  documentation_sources,
  file.path(submission_dir, "Documentation"),
  overwrite = TRUE
)

# Copy analysis scripts

scripts <- list.files(
  here("R scripts"),
  pattern = "\\.R$",
  full.names = TRUE
)

file.copy(
  scripts,
  file.path(submission_dir, "Analysis_Scripts"),
  overwrite = TRUE
)

# Copy RStudio project file

project_file <- here("COVID_Vaccine_Uptake_Somaliland.Rproj")

file.copy(
  project_file,
  submission_dir,
  overwrite = TRUE
)

# 10.4 Project Summary
# Project_Summary.md prepared and saved in Documentation.

# 10.5 Submission Checklist

# Verify documentation

documentation_check <- file.exists(
  file.path(
    submission_dir,
    "Documentation",
    documentation_files
  )
)

print(documentation_check)

# Verify final results and report

final_submission_files <- c(
  "Final_Results/Table1_Participant_Characteristics.csv",
  "Final_Results/Table2_Bivariate_Analysis.csv",
  "Final_Results/Table3_Multivariable_Logistic_Regression.csv",
  "Final_Results/Figure1_Forest_Plot.png",
  "Report/COVID_Vaccine_Uptake_Results.docx"
)

final_submission_check <- file.exists(
  file.path(submission_dir, final_submission_files)
)

print(final_submission_check)

# Verify analysis scripts and project file

script_check <- list.files(
  file.path(submission_dir, "Analysis_Scripts"),
  pattern = "\\.R$"
)

project_check <- file.exists(
  file.path(submission_dir, "COVID_Vaccine_Uptake_Somaliland.Rproj")
)

# Final package quality check

all_submission_files <- list.files(
  submission_dir,
  recursive = TRUE,
  full.names = TRUE
)

submission_quality <- data.frame(
  File = basename(all_submission_files),
  Size_KB = round(
    file.info(all_submission_files)$size / 1024,
    2
  ),
  Non_Empty = file.info(all_submission_files)$size > 0
)

print(submission_quality)

cat("Total files:", nrow(submission_quality), "\n")
cat("Non-empty files:", sum(submission_quality$Non_Empty), "\n")
cat("Empty files:", sum(!submission_quality$Non_Empty), "\n")

