
library(tidyverse)
library(janitor)
library(skimr)
library(here)

raw_data <- readRDS(
  here("data", "raw", "covid_vaccine_raw.rds")
)


dim(raw_data)

nrow(raw_data)

ncol(raw_data)

names(raw_data)
# Check duplicate data
sum(duplicated(raw_data))
raw_data%>% filter(duplicated(raw_data))

# Check unique identifiers
n_distinct(raw_data$id)

# Check duplicate IDs
sum(duplicated(raw_data$id))

# Display duplicated IDs if any
raw_data %>%
  filter(duplicated(id) | duplicated(id, fromLast = TRUE))

# check missing data
colSums(is.na(raw_data))
raw_data %>%
  summarise(across(everything(), ~sum(is.na(.x))))

# Missing data percentage
missing_summary <- data.frame(
  Variable = names(raw_data),
  Missing = colSums(is.na(raw_data)),
  Missing_Percentage = round(
    colSums(is.na(raw_data)) / nrow(raw_data) * 100,
    2
  )
)

missing_summary

# check dta types 
str(raw_data)
skim(raw_data)
# Check categorical variable distributions
raw_data %>%
  summarise(across(where(is.character), ~n_distinct(.x)))


# Check categorical values individually
lapply(raw_data, function(x) {
  if (is.character(x) || is.factor(x)) {
    table(x, useNA = "ifany")
  }
})
       
# Check numerical variable distributions

raw_data %>%
  summarise(across(where(is.numeric), list(
    Min = ~min(.x, na.rm = TRUE),
    Max = ~max(.x, na.rm = TRUE),
    Mean = ~mean(.x, na.rm = TRUE),
    SD = ~sd(.x, na.rm = TRUE)
  )))

# Inspect unique values of categorical variables
lapply(raw_data %>% select(-id), unique)

# Uptake and willingness
table(raw_data$UPTAKE, raw_data$WILLING, useNA = "ifany")

# Uptake and health worker status
table(raw_data$UPTAKE, raw_data$HEALTHWORK, useNA = "ifany")

# Test and result
table(raw_data$TEST, raw_data$RESULT, useNA = "ifany")


