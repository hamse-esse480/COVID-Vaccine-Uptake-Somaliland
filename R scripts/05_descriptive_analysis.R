
# Descriptive Analysis

library(tidyverse)
library(here)

cleaned_data <- readRDS(
  here("data", "cleaned", "covid_vaccine_cleaned.rds")
)

View(cleaned_data)

names(cleaned_data)


# Frequency and percentage of categorical variables

categorical_variables <- c(
  "survey_language",
  "survey_mode",
  "covid_vaccine_uptake",
  "vaccination_willingness",
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
  "previous_covid19_test_result",
  "perceived_covid19_threat",
  "perceived_vaccine_safety",
  "social_media_use_duration"
)

frequency_tables <- cleaned_data %>%
  select(all_of(categorical_variables)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "category"
  ) %>%
  filter(!is.na(category)) %>%
  group_by(variable, category) %>%
  summarise(frequency = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(
    percentage = round(
      frequency / sum(frequency) * 100, 1
    )
  ) %>%
  ungroup()

frequency_tables

# Export frequency tables

write_csv(
  frequency_tables,
  "outputs/tables/frequency_tables.csv"
)

#  Overall COVID-19 vaccine uptake
uptake_summary<-cleaned_data%>%
  count(covid_vaccine_uptake)%>%
  mutate(
    
    percentage=round(n/sum(n)*100,1)
  )
uptake_summary


# save uptake_summary
write_csv(uptake_summary,"outputs/tables/overall_vaccine_uptake.csv")


# Descriptive summary by participant characteristics

uptake_by_characteristics <- cleaned_data %>%
  pivot_longer(
    cols = -c(participant_id, covid_vaccine_uptake),
    names_to = "variable",
    values_to = "category"
  ) %>%
  group_by(variable, category, covid_vaccine_uptake) %>%
  summarise(
    frequency = n(),
    .groups = "drop"
  )

uptake_by_characteristics
  

# Calculate row percentages

uptake_by_characteristics <- uptake_by_characteristics %>%
  group_by(variable, category) %>%
  mutate(
    percentage = round(
      frequency / sum(frequency) * 100, 1
    )
  ) %>%
  ungroup()

uptake_by_characteristics

# Export descriptive summary by characteristics

write_csv(
  uptake_by_characteristics,
  "outputs/tables/uptake_by_characteristics.csv"
)
# Verify exported files

list.files("outputs/tables")


