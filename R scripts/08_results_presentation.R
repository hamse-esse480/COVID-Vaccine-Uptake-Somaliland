# ============================================================
 # RESULTS PRESENTATION AND REPORTING
# Project: COVID-19 Vaccine Uptake in Somaliland
# ============================================================


# 1. Load required packages

library(tidyverse)
library(gtsummary)
library(here)
library(broom)


# 2. Load cleaned dataset

data <- readRDS(
  here("Data", "Cleaned", "covid_vaccine_cleaned.rds")
)

# Check dataset

dim(data)
names(data)


# 3. Generate Table 1: Participant Characteristics

table1_data <- data %>%
  select(
    sex,
    age_group,
    educational_level,
    marital_status,
    region_of_residence,
    employment_status,
    healthcare_worker_status,
    preexisting_health_condition,
    vaccine_availability,
    perceived_vulnerability_over_45,
    previous_covid19_testing,
    perceived_covid19_threat,
    perceived_vaccine_safety,
    social_media_use_duration
  )

# Generate descriptive table

table1 <- table1_data %>%
  tbl_summary(
    type = all_categorical() ~ "categorical",
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ c(0, 1),
    missing = "no",
    percent = "column",
    value = list(
      healthcare_worker_status ~ "Yes",
      preexisting_health_condition ~ "Yes",
      vaccine_availability ~ "Yes",
      perceived_vulnerability_over_45 ~ "Yes",
      previous_covid19_testing ~ "Yes"
    )
  ) %>%
  modify_header(
    label = "**Participant characteristics**",
    stat_0 = "**N = 704**"
  ) %>%
  bold_labels()

# Convert Table 1 to a data frame

table1_export <- as_tibble(
  table1,
  col_labels = TRUE
)


# 4. Prepare Table 2: Bivariate Analysis

bivariate_results <- read_csv(
  here(
    "Outputs",
    "tables",
    "final_bivariate_results.csv"
  ),
  show_col_types = FALSE
)

# Check bivariate results

dim(bivariate_results)
names(bivariate_results)

# Prepare final bivariate table

table2 <- bivariate_results


# 5. Prepare Table 3: Multivariable Logistic Regression

adjusted_results <- read_csv(
  here(
    "Outputs",
    "tables",
    "final_adjusted_odds_ratios.csv"
  ),
  show_col_types = FALSE
)

# Check adjusted results

dim(adjusted_results)
names(adjusted_results)

# Prepare final multivariable table

table3 <- adjusted_results


# 6. Load Final Logistic Regression Model

final_model <- readRDS(
  here(
    "Outputs",
    "models",
    "final_logistic_regression_model.rds"
  )
)

# Check final model

summary(final_model)

nobs(final_model)


# 7. Prepare Forest Plot Data

forest_data <- broom::tidy(
  final_model,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    label = case_when(
      str_starts(term, "sex") ~
        str_replace(term, "sex", "Sex: "),
      
      str_starts(term, "age_group") ~
        str_replace(term, "age_group", "Age: "),
      
      str_starts(term, "educational_level") ~
        str_replace(term, "educational_level", "Education: "),
      
      str_starts(term, "marital_status_group") ~
        str_replace(term, "marital_status_group", "Marital status: "),
      
      str_starts(term, "region_group") ~
        str_replace(term, "region_group", "Region: "),
      
      str_starts(term, "employment_status") ~
        str_replace(term, "employment_status", "Employment: "),
      
      str_starts(term, "healthcare_worker_status") ~
        str_replace(term, "healthcare_worker_status", "Healthcare worker: "),
      
      str_starts(term, "preexisting_health_condition") ~
        str_replace(term, "preexisting_health_condition", "Health condition: "),
      
      str_starts(term, "vaccine_availability") ~
        str_replace(term, "vaccine_availability", "Vaccine availability: "),
      
      str_starts(term, "perceived_vulnerability_over_45") ~
        str_replace(term, "perceived_vulnerability_over_45", "Perceived vulnerability: "),
      
      str_starts(term, "previous_covid19_testing") ~
        str_replace(term, "previous_covid19_testing", "Previous COVID-19 testing: "),
      
      str_starts(term, "perceived_covid19_threat") ~
        str_replace(term, "perceived_covid19_threat", "Perceived COVID-19 threat: "),
      
      str_starts(term, "perceived_vaccine_safety") ~
        str_replace(term, "perceived_vaccine_safety", "Perceived vaccine safety: "),
      
      str_starts(term, "social_media_use_duration") ~
        str_replace(term, "social_media_use_duration", "Social media use: "),
      
      TRUE ~ term
    )
  )

# Check forest plot data

dim(forest_data)
head(forest_data)


# 8. Generate Forest Plot

forest_plot <- ggplot(
  forest_data,
  aes(
    x = estimate,
    y = reorder(label, estimate)
  )
) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    width = 0.2
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed"
  ) +
  scale_x_log10() +
  labs(
    title = "Factors Associated with COVID-19 Vaccine Uptake",
    x = "Adjusted Odds Ratio (95% CI)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    axis.text.y = element_text(size = 9)
  )

# Display forest plot

print(forest_plot)


# 9. Create Final Results Directory

final_results_dir <- here(
  "Outputs",
  "final_results"
)

dir.create(
  final_results_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# 10. Export Final Tables

# Export Table 1

write_csv(
  table1_export,
  here(
    "Outputs",
    "final_results",
    "Table1_Participant_Characteristics.csv"
  )
)

# Export Table 2

write_csv(
  table2,
  here(
    "Outputs",
    "final_results",
    "Table2_Bivariate_Analysis.csv"
  )
)

# Export Table 3

write_csv(
  table3,
  here(
    "Outputs",
    "final_results",
    "Table3_Multivariable_Logistic_Regression.csv"
  )
)


# 11. Export Forest Plot

ggsave(
  filename = here(
    "Outputs",
    "final_results",
    "Figure1_Forest_Plot.png"
  ),
  plot = forest_plot,
  width = 10,
  height = 8,
  dpi = 300
)


# 12. Final Verification

# Check exported files

final_files <- list.files(
  final_results_dir,
  full.names = TRUE
)

print(final_files)

# Check whether all expected files exist

expected_files <- c(
  "Table1_Participant_Characteristics.csv",
  "Table2_Bivariate_Analysis.csv",
  "Table3_Multivariable_Logistic_Regression.csv",
  "Figure1_Forest_Plot.png"
)

file_check <- file.exists(
  file.path(
    final_results_dir,
    expected_files
  )
)

data.frame(
  File = expected_files,
  Exists = file_check
)

# Check file sizes

file.info(
  file.path(
    final_results_dir,
    expected_files
  )
)$size
