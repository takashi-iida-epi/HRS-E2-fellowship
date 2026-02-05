## "EP808: Lab 1: Data Management and Reshape to Long Format"
# ************************************************************************ #
# Project:
# Author: 
# Population:
# Exposure:
# Outcome:
# Time:
# ************************************************************************* #
# ===============
# Library Needed:
# ===============
libs <- c("haven", "tidyverse")

for (pkg in libs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(haven)
library(tidyverse)

rm(list = ls())

# ==============
# Set directory:
# ==============
getwd()

base_directory <- "C:/from_d/New Volume F/New Volume/MPH-BUSPH/Spring 2026/EP808 Seminar/01_data/1.1 raw_data/randhrs1992_2022v1_SAS"
setwd("C:/from_d/New Volume F/New Volume/MPH-BUSPH/Spring 2026/EP808 Seminar/01_data/1.1 raw_data/randhrs1992_2022v1_SAS")

# ============
# Import data:
# ============
data<-read_sas ("randhrs1992_2022v1.sas7bdat")

# =============
# Explore data:
# =============
dim(data)  # n. of observation and n. of variables
nrow(data) #observation
ncol(data) #variables
names(data) # names of variables

# NOTE:
# The RAND HRS file is a WIDE longitudinal dataset.
# Repeated measures are stored with wave-specific prefixes:
# S1 = Wave 1 (1992), S2 = Wave 2 (1994), ..., S16 = Wave 16
# Each row = one individual, multiple columns = repeated waves

# Key identifiers:
# HHIDPN = unique respondent ID
# S1... S16... = wave-specific variables
# INW1–INW16 = interview indicators (whether interviewed)


# ====================
# Examine Sample Size: (wide format)
# ====================
# Number of unique participants
nrow(data)  
## In the wide RAND file, each row is one person.

colSums(data %>% select(starts_with("INW")))
## Number of completed interviews per wave

sum(data %>% select(starts_with("INW")))
## Total number of completed interviews (all waves combined)


# =============================
# Number of Years of Follow-up:
# =============================
# Select interview indicators (INW1–INW16)
inw_vars <- data %>% select(starts_with("INW"))

# Count number of waves each participant completed
followup <- inw_vars %>%
  mutate(n_waves = rowSums(.))  # sum 1s across waves

# Add participant ID
followup <- cbind(HHIDPN = data$HHIDPN, followup)

# Approximate years of follow-up (waves are biennial, 2 years apart)
followup <- followup %>%
  mutate(years_followup = ifelse(n_waves > 1, (n_waves - 1) * 2, 0))

# Summary of follow-up for all participants
summary(followup$years_followup)


# ============================
# Check Variable Distribution: (wide format)
# ============================
## Continuous variables
summary(data$R1AGEY_E)
hist (data$R1AGEY_E)


## Categorical variables
table(data$R1URBRUR, useNA = "ifany")
prop.table(table(data$R1URBRUR))


# ========================
# Choosing your variables:
# ========================
constant_vars<-c("HHIDPN") # These are the variables that don't change (ID variables)
changing_vars<-c("variable1", "variable2", "variable3") # Replace with your actual time-varying variables

## Create sub-dataset to void memory error
vars_needed <- c("HHIDPN", "variable1", "variable2", "variable3")

### Subset dataset
hrs_all <- data[, vars_needed]

# ================
# Reshape to Long:
# ================
# NOTE:
# The RAND file is WIDE (one row per person).
# For longitudinal summaries and plots across waves,
# it is often easier to reshape the data into LONG format:
# - One row per person per wave
# - One column indicating wave number

long_hrs <-hrs_all %>%
  pivot_longer( cols = all_of(changing_vars), # Select only the columns that are time-varying
                names_to = c("wave", "variable"), # Create two new columns: 'wave' (e.g., 8, 9, etc.) and 'variable' (the name of the measurement)
                names_pattern = "R([0-9]+)(.*)", # Extract the wave number and the variable name from column names 
                values_to = "value" # Place the values from each variable into the 'value' column
                )

## Save
saveRDS(long_hrs, 
        file = "C:/from_d/New Volume F/New Volume/MPH-BUSPH/Spring 2026/EP808 Seminar/01_data/1.2  temporary datas/randhrs_long.rds")

# ============================================
# Examine Distributions of Exposure & Outcome: (long format)
# ============================================
# Because we stacked the data, exposure and outcome values are stored in one 
# column called value, and the type of measurement is identified by the variable column
# Always filter by variable

## Continuous variable
stress_data <- long_hrs %>%
  filter(variable == "LBONCHRSTR")

summary(stress_data$value)


## Categorical variable
heart_long <- long_hrs %>%
  filter(variable == "HEART")

heart_summary <- heart_long %>%
  group_by(value) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n))

heart_summary