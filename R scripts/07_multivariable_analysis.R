#  Multivariable Logistic Regression

library(tidyverse)
library(broom)

# Load cleaned dataset

data <- readRDS(
  here("data", "cleaned", "covid_vaccine_cleaned.rds")
)

# Check outcome variable

table(data$covid_vaccine_uptake, useNA = "ifany")

# Check outcome proportions

prop.table(
  table(data$covid_vaccine_uptake)
) * 100

# Check total sample size

nrow(data)
# Set outcome variable as a factor

data$covid_vaccine_uptake <- factor(
  data$covid_vaccine_uptake,
  levels = c("No", "Yes")
)

# Check factor levels

levels(data$covid_vaccine_uptake)

# Check outcome distribution

table(data$covid_vaccine_uptake)


# Define candidate independent variables

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

# Check missing values

missing_summary <- data %>%
  summarise(
    across(
      all_of(independent_variables),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_n"
  ) %>%
  mutate(
    missing_percent = round(missing_n / nrow(data) * 100, 1)
  )

print(missing_summary)

# Check category frequencies by vaccination status

category_distribution <- map_dfr(
  independent_variables,
  function(var) {
    
    data %>%
      transmute(
        category = as.character(.data[[var]]),
        covid_vaccine_uptake
      ) %>%
      count(category, covid_vaccine_uptake) %>%
      pivot_wider(
        names_from = covid_vaccine_uptake,
        values_from = n,
        values_fill = 0
      ) %>%
      mutate(
        variable = var,
        total = No + Yes
      ) %>%
      select(variable, category, No, Yes, total)
  }
)

# Display category distribution

print(category_distribution, n = Inf)


# Set reference categories for categorical predictors

data <- data %>%
  mutate(
    sex = relevel(factor(sex), ref = "Male"),
    age_group = relevel(factor(age_group), ref = "18-24"),
    educational_level = relevel(factor(educational_level), ref = "No education"),
    marital_status = relevel(factor(marital_status), ref = "Single"),
    region_of_residence = relevel(factor(region_of_residence), ref = "Maroodi Jeeh"),
    employment_status = relevel(factor(employment_status), ref = "No"),
    healthcare_worker_status = relevel(factor(healthcare_worker_status), ref = "No"),
    preexisting_health_condition = relevel(factor(preexisting_health_condition), ref = "No health condition"),
    vaccine_availability = relevel(factor(vaccine_availability), ref = "No"),
    perceived_vulnerability_over_45 = relevel(factor(perceived_vulnerability_over_45), ref = "No"),
    previous_covid19_testing = relevel(factor(previous_covid19_testing), ref = "No"),
    perceived_covid19_threat = relevel(factor(perceived_covid19_threat), ref = "Strongly disagree"),
    perceived_vaccine_safety = relevel(factor(perceived_vaccine_safety), ref = "Strongly disagree"),
    social_media_use_duration = relevel(factor(social_media_use_duration), ref = "Less than 2 hours")
  )

# Check reference categories

lapply(data[independent_variables], levels)


# Install car package if not already installed

if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}

library(car)

# Fit temporary model for VIF assessment

vif_model <- lm(
  as.numeric(covid_vaccine_uptake) ~
    sex +
    age_group +
    educational_level +
    marital_status +
    region_of_residence +
    employment_status +
    healthcare_worker_status +
    preexisting_health_condition +
    vaccine_availability +
    perceived_vulnerability_over_45 +
    previous_covid19_testing +
    perceived_covid19_threat +
    perceived_vaccine_safety +
    social_media_use_duration,
  data = data
)

# Calculate VIF

vif_results <- car::vif(vif_model)

print(vif_results)

# Group region of residence

data <- data %>%
  mutate(
    region_group = if_else(
      region_of_residence == "Maroodi Jeeh",
      "Maroodi Jeeh",
      "Other surveyed regions"
    )
  )

# Set reference category

data$region_group <- factor(
  data$region_group,
  levels = c("Maroodi Jeeh", "Other surveyed regions")
)

# Check region distribution

table(data$region_group, data$covid_vaccine_uptake)

# Check percentages

prop.table(
  table(data$region_group, data$covid_vaccine_uptake),
  margin = 1
) * 100


# Fit initial multivariable logistic regression model

full_model <- glm(
  covid_vaccine_uptake ~
    sex +
    age_group +
    educational_level +
    marital_status +
    region_group +
    employment_status +
    healthcare_worker_status +
    preexisting_health_condition +
    vaccine_availability +
    perceived_vulnerability_over_45 +
    previous_covid19_testing +
    perceived_covid19_threat +
    perceived_vaccine_safety +
    social_media_use_duration,
  data = data,
  family = binomial(link = "logit")
)

# Display model summary

summary(full_model)

# Calculate adjusted odds ratios and 95% confidence intervals

aor_results <- broom::tidy(
  full_model,
  exponentiate = TRUE,
  conf.int = TRUE
)

# Prepare regression results table

final_regression_table <- aor_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    AOR = round(estimate, 2),
    CI_lower = round(conf.low, 2),
    CI_upper = round(conf.high, 2),
    p_value = if_else(
      p.value < 0.001,
      "<0.001",
      sprintf("%.3f", p.value)
    )
  ) %>%
  select(term, AOR, CI_lower, CI_upper, p_value)

# Display results

print(final_regression_table, n = Inf)





# Likelihood Ratio Test

null_model <- glm(
  covid_vaccine_uptake ~ 1,
  data = data,
  family = binomial
)

anova(null_model, full_model, test = "Chisq")


# Hosmer-Lemeshow Test

if (!requireNamespace("ResourceSelection", quietly = TRUE)) {
  install.packages("ResourceSelection")
}

library(ResourceSelection)

observed_outcome <- ifelse(
  data$covid_vaccine_uptake == "Yes", 1, 0
)

hosmer_lemeshow <- hoslem.test(
  observed_outcome,
  fitted(full_model),
  g = 10
)

print(hosmer_lemeshow)


# ROC-AUC

if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC")
}

library(pROC)

roc_model <- roc(
  observed_outcome,
  fitted(full_model),
  levels = c(0, 1),
  direction = "<"
)

print(auc(roc_model))

plot(
  roc_model,
  main = "ROC Curve - COVID-19 Vaccine Uptake"
)



# Model influence diagnostics

n <- nrow(data)
p <- length(coef(full_model))

# Calculate diagnostic measures

diagnostic_data <- data %>%
  mutate(
    cooks_distance = cooks.distance(full_model),
    leverage = hatvalues(full_model),
    deviance_residual = rstandard(full_model, type = "deviance")
  )

# Define diagnostic thresholds

cook_threshold <- 4 / n
leverage_threshold <- 2 * p / n

# Identify potentially influential observations

diagnostic_data <- diagnostic_data %>%
  mutate(
    high_cooks_distance = cooks_distance > cook_threshold,
    high_leverage = leverage > leverage_threshold,
    large_residual = abs(deviance_residual) > 3
  )

# Summarize diagnostic findings

diagnostic_summary <- diagnostic_data %>%
  summarise(
    total_observations = n(),
    high_cooks_distance = sum(high_cooks_distance),
    high_leverage = sum(high_leverage),
    large_residuals = sum(large_residual)
  )

print(diagnostic_summary)

# Display observations flagged by any diagnostic

influential_observations <- diagnostic_data %>%
  filter(
    high_cooks_distance |
      high_leverage |
      large_residual
  ) %>%
  select(
    participant_id,
    covid_vaccine_uptake,
    cooks_distance,
    leverage,
    deviance_residual,
    high_cooks_distance,
    high_leverage,
    large_residual
  )

print(influential_observations, n = Inf)




# Sensitivity analysis: Exclude observations with high Cook's distance

sensitivity_data <- diagnostic_data %>%
  filter(!high_cooks_distance)

# Fit the sensitivity model

sensitivity_model <- glm(
  covid_vaccine_uptake ~
    sex +
    age_group +
    educational_level +
    marital_status +
    region_group +
    employment_status +
    healthcare_worker_status +
    preexisting_health_condition +
    vaccine_availability +
    perceived_vulnerability_over_45 +
    previous_covid19_testing +
    perceived_covid19_threat +
    perceived_vaccine_safety +
    social_media_use_duration,
  data = sensitivity_data,
  family = binomial(link = "logit")
)

# Compare sample sizes and model convergence

cat("Original sample size:", nrow(data), "\n")
cat("Sensitivity sample size:", nrow(sensitivity_data), "\n")

cat("Original model converged:", full_model$converged, "\n")
cat("Sensitivity model converged:", sensitivity_model$converged, "\n")

# Compare adjusted odds ratios

original_aor <- broom::tidy(
  full_model,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  select(term, original_AOR = estimate)

sensitivity_aor <- broom::tidy(
  sensitivity_model,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  select(term, sensitivity_AOR = estimate)

aor_comparison <- original_aor %>%
  left_join(sensitivity_aor, by = "term") %>%
  mutate(
    percentage_change = round(
      ((sensitivity_AOR - original_AOR) / original_AOR) * 100,
      1
    )
  )

print(aor_comparison, n = Inf)





# Inspect warnings from the sensitivity model

warnings()

# Examine model coefficients and standard errors

summary(sensitivity_model)

# Check outcome distribution across age groups

table(
  sensitivity_data$age_group,
  sensitivity_data$covid_vaccine_uptake
)

# Check outcome distribution across education levels

table(
  sensitivity_data$educational_level,
  sensitivity_data$covid_vaccine_uptake
)

# Check outcome distribution across marital status

table(
  sensitivity_data$marital_status,
  sensitivity_data$covid_vaccine_uptake
)

# Check outcome distribution across perceived COVID-19 threat

table(
  sensitivity_data$perceived_covid19_threat,
  sensitivity_data$covid_vaccine_uptake
)


# Calculate DFBETAS from the original model

dfbetas_matrix <- dfbetas(full_model)

# Define the diagnostic threshold

dfbetas_threshold <- 2 / sqrt(nrow(data))

# Convert diagnostic results into a tidy dataset

dfbetas_data <- as.data.frame(dfbetas_matrix) %>%
  mutate(
    participant_id = data$participant_id
  ) %>%
  pivot_longer(
    cols = -participant_id,
    names_to = "term",
    values_to = "dfbetas"
  )

# Summarize influential observations for each coefficient

dfbetas_summary <- dfbetas_data %>%
  group_by(term) %>%
  summarise(
    maximum_absolute_dfbetas = max(abs(dfbetas), na.rm = TRUE),
    flagged_observations = sum(
      abs(dfbetas) > dfbetas_threshold,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(maximum_absolute_dfbetas))

print(dfbetas_summary, n = Inf)

# Identify observations with the largest overall DFBETAS

dfbetas_by_observation <- dfbetas_data %>%
  group_by(participant_id) %>%
  summarise(
    maximum_absolute_dfbetas = max(abs(dfbetas), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(maximum_absolute_dfbetas))

print(head(dfbetas_by_observation, 15))


# Examine sparse categories in the original dataset

# Marital status
table(
  data$marital_status,
  data$covid_vaccine_uptake
)

# Educational level
table(
  data$educational_level,
  data$covid_vaccine_uptake
)

# Age group
table(
  data$age_group,
  data$covid_vaccine_uptake
)

# Calculate total observations per category

sparse_category_summary <- bind_rows(
  data %>%
    count(marital_status, name = "frequency") %>%
    mutate(variable = "Marital status",
           category = as.character(marital_status)) %>%
    select(variable, category, frequency),
  
  data %>%
    count(educational_level, name = "frequency") %>%
    mutate(variable = "Educational level",
           category = as.character(educational_level)) %>%
    select(variable, category, frequency),
  
  data %>%
    count(age_group, name = "frequency") %>%
    mutate(variable = "Age group",
           category = as.character(age_group)) %>%
    select(variable, category, frequency)
)

print(sparse_category_summary, n = Inf)



# Combine divorced and widowed categories

data <- data %>%
  mutate(
    marital_status_group = case_when(
      marital_status == "Single" ~ "Single",
      marital_status == "Married" ~ "Married",
      marital_status %in% c("Divorced", "Widowed") ~ "Previously married"
    )
  )

# Set reference category

data$marital_status_group <- factor(
  data$marital_status_group,
  levels = c("Single", "Married", "Previously married")
)

# Examine grouped marital status

table(
  data$marital_status_group,
  data$covid_vaccine_uptake
)

# Check total frequencies

table(data$marital_status_group)





# Set reference category for grouped marital status

data$marital_status_group <- relevel(
  data$marital_status_group,
  ref = "Single"
)

# Fit revised multivariable logistic regression model

revised_model <- glm(
  covid_vaccine_uptake ~
    sex +
    age_group +
    educational_level +
    marital_status_group +
    region_group +
    employment_status +
    healthcare_worker_status +
    preexisting_health_condition +
    vaccine_availability +
    perceived_vulnerability_over_45 +
    previous_covid19_testing +
    perceived_covid19_threat +
    perceived_vaccine_safety +
    social_media_use_duration,
  data = data,
  family = binomial(link = "logit")
)

# Examine model results

summary(revised_model)

# Check model convergence

revised_model$converged


# Generate adjusted odds ratios and 95% confidence intervals

revised_aor_results <- broom::tidy(
  revised_model,
  exponentiate = TRUE,
  conf.int = TRUE
)

# Prepare the final regression table

revised_regression_table <- revised_aor_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    AOR = round(estimate, 2),
    CI_lower = round(conf.low, 2),
    CI_upper = round(conf.high, 2),
    p_value = if_else(
      p.value < 0.001,
      "<0.001",
      sprintf("%.3f", p.value)
    )
  ) %>%
  select(
    term,
    AOR,
    CI_lower,
    CI_upper,
    p_value
  )

print(revised_regression_table, n = Inf)

# Export revised regression results

write_csv(
  revised_regression_table,
  here("outputs", "tables", "revised_multivariable_results.csv")
)







# Revised model influence diagnostics

n <- nrow(data)
p <- length(coef(revised_model))

# Calculate diagnostic measures

revised_diagnostic_data <- data %>%
  mutate(
    cooks_distance = cooks.distance(revised_model),
    leverage = hatvalues(revised_model),
    deviance_residual = rstandard(
      revised_model,
      type = "deviance"
    )
  )

# Define diagnostic thresholds

cook_threshold <- 4 / n
leverage_threshold <- 2 * p / n

# Identify potentially influential observations

revised_diagnostic_data <- revised_diagnostic_data %>%
  mutate(
    high_cooks_distance = cooks_distance > cook_threshold,
    high_leverage = leverage > leverage_threshold,
    large_residual = abs(deviance_residual) > 3
  )

# Summarize diagnostic findings

revised_diagnostic_summary <- revised_diagnostic_data %>%
  summarise(
    total_observations = n(),
    high_cooks_distance = sum(high_cooks_distance),
    high_leverage = sum(high_leverage),
    large_residuals = sum(large_residual)
  )

print(revised_diagnostic_summary)

# Check fitted probabilities

range(fitted(revised_model))

# Check model convergence

revised_model$converged





# Identify observations flagged by any diagnostic

revised_influential_observations <- revised_diagnostic_data %>%
  filter(
    high_cooks_distance |
      high_leverage |
      large_residual
  )

# Count unique flagged observations

nrow(revised_influential_observations)

# Calculate DFBETAS

revised_dfb <- dfbetas(revised_model)

# Define DFBETAS threshold

dfbetas_threshold <- 2 / sqrt(n)

# Identify coefficients with the largest DFBETAS

dfbetas_summary <- tibble(
  coefficient = colnames(revised_dfb),
  max_abs_dfbeta = apply(
    abs(revised_dfb),
    2,
    max
  ),
  observations_flagged = colSums(
    abs(revised_dfb) > dfbetas_threshold
  )
) %>%
  arrange(desc(max_abs_dfbeta))

print(dfbetas_summary)

# Display the 10 most influential observations

revised_top_influential <- revised_diagnostic_data %>%
  mutate(
    max_abs_dfbeta = apply(abs(revised_dfb), 1, max)
  ) %>%
  arrange(desc(max_abs_dfbeta)) %>%
  select(
    participant_id,
    cooks_distance,
    leverage,
    max_abs_dfbeta
  ) %>%
  slice_head(n = 10)

print(revised_top_influential)


nrow(revised_influential_observations)
 print(dfbetas_summary, n = Inf)




 
 # Revised model goodness-of-fit
 
 # Likelihood ratio test
 
 null_deviance <- revised_model$null.deviance
 residual_deviance <- revised_model$deviance
 
 deviance_difference <- null_deviance - residual_deviance
 
 df_difference <- revised_model$df.null -
   revised_model$df.residual
 
 model_lrt_p <- pchisq(
   deviance_difference,
   df = df_difference,
   lower.tail = FALSE
 )
 
 # Display likelihood ratio test results
 
 revised_model_fit <- tibble(
   null_deviance = null_deviance,
   residual_deviance = residual_deviance,
   df_difference = df_difference,
   likelihood_ratio_p = model_lrt_p
 )
 
 print(revised_model_fit)
 
 # Hosmer-Lemeshow goodness-of-fit test
 
 library(ResourceSelection)
 
 hl_test_revised <- hoslem.test(
   as.numeric(data$covid_vaccine_uptake == "Yes"),
   fitted(revised_model),
   g = 10
 )
 
 print(hl_test_revised)
 
 
 # Revised model ROC curve and AUC
 
 library(pROC)
 
 revised_roc <- roc(
   response = data$covid_vaccine_uptake,
   predictor = fitted(revised_model),
   levels = c("No", "Yes"),
   direction = "<",
   quiet = TRUE
 )
 
 # Calculate AUC
 
 revised_auc <- auc(revised_roc)
 
 # Calculate 95% confidence interval
 
 revised_auc_ci <- ci.auc(revised_roc)
 
 # Display AUC results
 
 print(revised_auc)
 print(revised_auc_ci)
 
 # Plot ROC curve
 
 plot(
   revised_roc,
   main = "ROC Curve - Revised Logistic Regression Model",
   col = "blue",
   lwd = 2
 )
 
 abline(
   a = 0,
   b = 1,
   lty = 2,
   col = "gray"
 )
 

 
 # Compare initial and revised models
 
 model_comparison <- tibble(
   model = c("Initial full model", "Revised model"),
   observations = c(
     nobs(full_model),
     nobs(revised_model)
   ),
   AIC = c(
     AIC(full_model),
     AIC(revised_model)
   ),
   residual_deviance = c(
     deviance(full_model),
     deviance(revised_model)
   ),
   AUC = c(
     as.numeric(auc(roc(
       data$covid_vaccine_uptake,
       fitted(full_model),
       levels = c("No", "Yes"),
       direction = "<",
       quiet = TRUE
     ))),
     as.numeric(revised_auc)
   )
 )
 
 print(model_comparison)
 

 
 # Final adjusted odds ratios table
 
 final_aor_table <- broom::tidy(
   revised_model,
   exponentiate = TRUE,
   conf.int = TRUE
 ) %>%
   filter(term != "(Intercept)") %>%
   mutate(
     AOR = round(estimate, 2),
     CI_lower = round(conf.low, 2),
     CI_upper = round(conf.high, 2),
     p_value = case_when(
       p.value < 0.001 ~ "<0.001",
       TRUE ~ sprintf("%.3f", p.value)
     ),
     adjusted_95_CI = paste0(
       CI_lower, "–", CI_upper
     )
   ) %>%
   select(
     term,
     AOR,
     adjusted_95_CI,
     p_value
   )
 
 print(final_aor_table, n = Inf)
 
 # Save final AOR table
 
 write_csv(
   final_aor_table,
   here("outputs", "tables", "final_adjusted_odds_ratios.csv")
 )
 

