# Bivariate Analysis

library(tidyverse)

# Load cleaned dataset
data <- readRDS(
  here("data", "cleaned", "covid_vaccine_cleaned.rds")
)

# Check outcome variable

table(data$covid_vaccine_uptake)
# Select categorical independent variables

independent_variables <- c(
  "sex",
  "age_group",
  "educational_level",
  "marital_status",
  "region_of_residence",
  "employment_status",
  "healthcare_worker_status",
  "preexisting_health_condition",
  "vaccine_availability",
  "perceived_vulnerability_over_45",
  "previous_covid19_testing",
  "perceived_covid19_threat",
  "perceived_vaccine_safety",
  "social_media_use_duration"
)
# Check categories and missing values

data %>%
  select(all_of(independent_variables)) %>%
  summarise(
    across(
      everything(),
      ~ n_distinct(.x, na.rm = TRUE)
    )
  )

# Display all variable category counts

category_counts <- data %>%
  select(all_of(independent_variables)) %>%
  summarise(
    across(
      everything(),
      ~ n_distinct(.x, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "categories"
  )

print(category_counts, n = Inf)


# Chi-square test for all independent variables

chi_square_results <- map_dfr(
  independent_variables,
  function(var) {
    
    tab <- table(
      data[[var]],
      data$covid_vaccine_uptake
    )
    
    test <- chisq.test(tab, correct = FALSE)
    
    tibble(
      variable = var,
      chi_square = unname(test$statistic),
      df = unname(test$parameter),
      p_value = test$p.value,
      minimum_expected = min(test$expected)
    )
  }
)

chi_square_results

# Assess Chi-square assumptions

chi_square_assumptions <- map_dfr(
  independent_variables,
  function(var) {
    
    tab <- table(
      data[[var]],
      data$covid_vaccine_uptake
    )
    
    expected <- chisq.test(
      tab,
      correct = FALSE
    )$expected
    
    tibble(
      variable = var,
      total_cells = length(expected),
      cells_below_5 = sum(expected < 5),
      percentage_below_5 = round(
        mean(expected < 5) * 100, 1
      ),
      minimum_expected = round(
        min(expected), 2
      )
    )
  }
)

chi_square_assumptions

# Fisher's Exact Test: Region of Residence

region_table <- table(
  data$region_of_residence,
  data$covid_vaccine_uptake
)

region_fisher <- fisher.test(
  region_table,
  workspace = 2e7
)

region_fisher


# Final Bivariate Analysis Results

final_bivariate_results <- chi_square_results %>%
  mutate(
    test = "Pearson Chi-square",
    p_value = round(p_value, 5)
  ) %>%
  mutate(
    test = if_else(
      variable == "region_of_residence",
      "Fisher's Exact Test",
      test
    ),
    p_value = if_else(
      variable == "region_of_residence",
      round(region_fisher$p.value, 5),
      p_value
    ),
    result = if_else(
      p_value < 0.05,
      "Significant",
      "Not significant"
    )
  ) %>%
  select(
    variable,
    test,
    chi_square,
    df,
    p_value,
    result
  )

final_bivariate_results


# Export final bivariate results

write_csv(
  final_bivariate_results,
  "outputs/tables/final_bivariate_results.csv"
)
file.exists(
  "outputs/tables/final_bivariate_results.csv"
)





