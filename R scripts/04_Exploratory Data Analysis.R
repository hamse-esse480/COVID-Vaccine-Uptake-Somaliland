library(tidyverse)
library(janitor)
library(here)

cleaned_data <- readRDS(
  here("Data", "Cleaned", "covid_vaccine_cleaned.rds")
)
# Dataset dimensions
dim(cleaned_data)

# Number of participants
nrow(cleaned_data)

# Number of variables
ncol(cleaned_data)

# Variable names
names(cleaned_data)

# Dataset structure
glimpse(cleaned_data)


# Frequency of vaccine uptake
uptake_frequency <- cleaned_data %>%
  count(covid_vaccine_uptake) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 2)
  )

uptake_frequency

write_csv(
  uptake_frequency,
  here("outputs", "tables", "vaccine_uptake_frequency.csv")
)


ggplot(cleaned_data, aes(x = covid_vaccine_uptake)) +
  geom_bar() +
  labs(
    title = "COVID-19 Vaccine Uptake Among Study Participants",
    x = "COVID-19 Vaccine Uptake",
    y = "Number of Participants"
  ) +
  theme_minimal()

ggsave(
  here("outputs", "figures", "vaccine_uptake_distribution.png"),
  width = 8,
  height = 6,
  dpi = 300
)
file.exists(
  here("outputs", "figures", "vaccine_uptake_distribution.png")
)

# Categorical variable distributions

categorical_summary <- cleaned_data %>%
  select(-participant_id) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "category"
  ) %>%
  group_by(variable, category) %>%
  summarise(
    frequency = n(),
    .groups = "drop"
  ) %>%
  group_by(variable) %>%
  mutate(
    percentage = round(frequency / sum(frequency) * 100, 2)
  ) %>%
  ungroup()

categorical_summary

write_csv(
  categorical_summary,
  here("outputs", "tables", "categorical_summary.csv")
)

# Generate visualization

ggplot(categorical_summary, aes(x = category, y = frequency)) +
  geom_col() +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(
    title = "Distribution of Study Variables",
    x = "Category",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )
ggsave(
  here("outputs", "figures", "categorical_variables_distribution.png"),
  width = 16,
  height = 12,
  dpi = 300
)
# Generate descriptive uptake cross-tabulation

uptake_by_characteristics <- cleaned_data %>%
  select(-participant_id) %>%
  pivot_longer(
    cols = -covid_vaccine_uptake,
    names_to = "variable",
    values_to = "category"
  ) %>%
  group_by(variable, category, covid_vaccine_uptake) %>%
  summarise(
    frequency = n(),
    .groups = "drop"
  ) %>%
  group_by(variable, category) %>%
  mutate(
    row_percentage = round(
      frequency / sum(frequency) * 100, 2
    )
  ) %>%
  ungroup()

uptake_by_characteristics
write_csv(
  uptake_by_characteristics,
  here("outputs", "tables", "uptake_by_characteristics.csv")
)


ggplot(
  uptake_by_characteristics,
  aes(x = category, y = row_percentage, fill = covid_vaccine_uptake)
) +
  geom_col(position = "stack") +
  facet_wrap(~variable, scales = "free_x", ncol = 3) +
  labs(
    title = "COVID-19 Vaccine Uptake by Participant Characteristics",
    x = "Category",
    y = "Percentage",
    fill = "Vaccine Uptake"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

ggsave(
  here("outputs", "figures", "uptake_by_characteristics.png"),
  width = 16,
  height = 14,
  dpi = 300
)











