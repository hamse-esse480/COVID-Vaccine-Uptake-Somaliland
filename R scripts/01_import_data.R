install.packages(c(
  "janitor",
  "skimr",
  "here"
))
library(readxl)
library(tidyverse)
library(janitor)
library(skimr)
install.packages("here")
library(here)
raw_data <- read_excel(
  here("Data", "raw", "Factors Associated with COVID-19 Vaccine Uptake and Hesitancy in Somaliland .xlsx")
)
getwd
# View the dataset
View(raw_data)

# Check dimensions
dim(raw_data)

# Check variable names
names(raw_data)
nrow(raw_data)

ncol(raw_data)

# Check structure
str(raw_data)

# Check first observations
head(raw_data)

# Get a statistical overview
summary(raw_data)

saveRDS(
  raw_data,
  here("Data", "raw", "covid_vaccine_raw.rds")
)












