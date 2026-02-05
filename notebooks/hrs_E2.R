# data import
# install.packages("haven")

library(tidyverse)
library(haven)

# ==============
# Set directory:
# ==============
getwd()

base_directory <- "C:/from_d/New Volume F/New Volume/MPH-BUSPH/Spring 2026/EP808 Seminar/01_data/1.1 raw_data/randhrs1992_2022v1_SAS"
setwd("C:/from_d/New Volume F/New Volume/MPH-BUSPH/Spring 2026/EP808 Seminar/01_data/1.1 raw_data/randhrs1992_2022v1_SAS")

# do it later--------------------

# ============
# Import data:
# ============
data<-read_sas ("randhrs1992_2022v1.sas7bdat")


# =============
# Explore data:
# =============
head(data)
dim(data)  # n. of observation and n. of variables
nrow(data) #observation
ncol(data) #variables
names(data) # names of variables



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



# ============================
# Check Variable Distribution: (wide format)
# ============================
## Continuous variables
summary(data$R1AGEY_E)
hist (data$R1AGEY_E)


# ============================
# Check Ongoing housing preoblems
# ============================
## Ongoing housing preoblems: Categorical variables
table(data$R8LBHOUSEPRB, useNA = "ifany")
prop.table(table(data$R8LBHOUSEPRB))

# check cols for Ongoing housing preoblems
data %>% select(contains("HOUSEPRB"))

colSums(!is.na(data %>% select(contains("HOUSEPRB"))))
## Number of completed interviews per wave

sum(data %>% select(contains("HOUSEPRB")), na.rm = TRUE)
## Total number of completed interviews (all waves combined)


# integrate into vector
all_house_values <- unlist(data %>% select(contains("HOUSEPRB")))

# distribution
table(all_house_values, useNA = "ifany")

# proportion
prop.table(table(all_house_values))


# ============================
# Check outcome variables
# this might be key factors: what aspect I focus on in cognitive functions
# ============================
data %>% select(contains("COGTOT"))

colSums(!is.na(data %>% select(contains("COGTOT"))))
## Number of completed interviews per wave

sum(data %>% select(contains("COGTOT")), na.rm = TRUE)
## Total number of completed interviews (all waves combined)
 


