library(tidyverse)
library(janitor)
library(here)

raw_data <- readRDS(
  here("data", "raw", "covid_vaccine_raw.rds")
)
 Cleaned_data<- raw_data
 # Standardize whitespace
 Cleaned_data <- raw_data
 
 Cleaned_data <- Cleaned_data %>%
   mutate(across(where(is.character), trimws))
 # Standardize inconsistent labels
 cleaned_data <- Cleaned_data %>%
   mutate(
     SAFETY = case_when(
       SAFETY == "Strongly Agree" ~ "Strongly agree",
       TRUE ~ SAFETY
     ),
     
     HISTORY = case_when(
       HISTORY == "Nohealthcondition" ~ "No health condition",
       HISTORY == "healthcondition" ~ "Health condition",
       TRUE ~ HISTORY
     )
   )
 # Verify recoding
 table(cleaned_data$SAFETY, useNA = "ifany")
 
 table(cleaned_data$HISTORY, useNA = "ifany")
 # Save cleaned working dataset
 saveRDS(
   cleaned_data,
   here("data", "cleaned", "covid_vaccine_cleaning_stage1.rds")
 )
 
 
 # Missing Values Imputation (Mode)
 
 # Mode function
 get_mode <- function(x) {
   freq <- table(x)
   names(freq)[which.max(freq)]
 }
 
 # Identify missing values before imputation
 missing_before <- cleaned_data %>%
   summarise(across(where(is.character), ~sum(is.na(.x))))
 
 # Mode imputation for categorical variables (excluding ID)
 cleaned_data <- cleaned_data %>%
   mutate(
     across(
       -id,
       ~ if (is.character(.x)) {
         replace(.x, is.na(.x), get_mode(.x))
       } else {
         .x
       }
     )
   )
 
 # Check missing values
 colSums(is.na(cleaned_data))
 
 # Verify categorical distributions
 lapply(cleaned_data %>% select(-id), table)
 
 saveRDS(
   cleaned_data,
   here("data", "cleaned", "covid_vaccine_cleaned.rds")
 )
 
 write_csv(
   cleaned_data,
   here("data", "cleaned", "covid_vaccine_cleaned.csv")
 )
 
 
 
 # Imputation audit summary
 
 imputation_summary <- tibble(
   Variable = names(missing_before),
   Missing_before = as.numeric(missing_before[1, ]),
   Missing_after = colSums(is.na(cleaned_data))[names(missing_before)],
   Imputed = Missing_before - Missing_after
 )
 
 imputation_summary
 
 write_csv(
   imputation_summary,
   here("outputs", "tables", "imputation_summary.csv")
 )
# Rename Columns names
 cleaned_data <- cleaned_data %>%
   rename(
     participant_id = id,
     survey_language = LANG,
     survey_mode = MODE,
     covid_vaccine_uptake = UPTAKE,
     vaccination_willingness = WILLING,
     sex = GENDER,
     age_group = AGE,
     educational_level = EDU,
     marital_status = MARITAL,
     region_of_residence = RESIDE,
     employment_status = EMPLOY,
     healthcare_worker_status = HEALTHWORK,
     preexisting_health_condition = HISTORY,
     vaccine_availability = AVAILABLE,
     perceived_vulnerability_over_45 = ABOVE45,
     previous_covid19_testing = TEST,
     previous_covid19_test_result = RESULT,
     perceived_covid19_threat = THREAT,
     perceived_vaccine_safety = SAFETY,
     social_media_use_duration = SOCIALM
   )
 names(cleaned_data)
 
 # Verify dataset
 names(cleaned_data)
 
 # Save updated dataset
 saveRDS(
   cleaned_data,
   here("data", "cleaned", "covid_vaccine_cleaned.rds")
 )
 write.csv(
   cleaned_data,
   here("data", "cleaned", "covid_vaccine_cleaned.csv"),
   row.names = FALSE
 )
 view(cleaned_data)

 
 
 
 
 
 
 
 
 
 
 