## Welcome!

## This is your project's main script file and together with
## manuscript.Rmd it provides and entry point for you and other people
## coming to the project. The code in this file should give an outline
## of the different steps conducted in your study, from importing data
## to producing results.

## This file should be relatively short, and most of the heavy
## lifting should be done by specialised functions. These functions
## live in the folder functions/ and you create a new function using
## create_function().

## Feel free to remove this introductory text as you get started.

## Source all functions (if you tick the box "Source on save" in
## RStudio functions will be automatically sourced when you save
## them). They all need to be sourced however when you compile your
## manuscript file or run this file as a job, as that happens in a
## clean R session.
noacsr::source_all_functions()

# Load packages
library(rofi)
library(dplyr)
library(labelled)
library(gtsummary)
library(ggplot2)

## Import data
data <- import_data(test = TRUE)

# Merge data
merged.data <- merge_data(data, test = TRUE)

# New add ofi categories
merged.data <- add_ofi_categories(merged.data)

# Add the OFI outcome
merged.data$ofi <- create_ofi(merged.data)

# Select variables, this is just an example. The function select comes from the 
# dplyr package
study.data <- merged.data |> 
  select(pt_age_yrs, 
         pt_Gender, 
         inj_mechanism, 
         pt_asa_preinjury, 
         ed_gcs_sum,
         ed_sbp_value,
         ed_rr_value,
         ed_be_art,
         ISS,
         host_care_level,
         res_survival,
         ofi,
         ed_sbp_rtscat,
         ed_inr,
         ofi.categories.broad,
         ofi.categories.detailed
         )

# Exclude patients who were not reviewed for the presence of OFI
study.sample <- study.data |>
  filter(!is.na(ofi))

# Function for converting to numeric
convert_number <- function(x){
  x <- as.character(x)
  x <- gsub(pattern = ",", replacement = ".",x = x, fixed = TRUE)
  x <- as.numeric(x)
  return(x)
}

# Converting ed_be_art to numeric
BEnum <- convert_number(study.sample$ed_be_art)

# BE shock classification
study.sample <- study.sample %>%
  mutate(BE_class = case_when(
    BEnum < (-10) ~ "Class 4 (severe)",
    BEnum >= (-10) & BEnum < (-6) ~ "Class 3 (moderate)",
    BEnum >= (-6) & BEnum < (-2) ~ "Class 2 (mild)",  
    BEnum >= (-2) ~ "Class 1 (no shock)",   
    is.na(BEnum) ~ NA_character_,
  ))

# Re-add the BE column as numeric to `study.sample`
study.sample <- study.sample %>%
  mutate(ed_be_art_numeric = BEnum)

# SBP shock classifiaction
study.sample <- study.sample %>%
  mutate(SBP_class = case_when(
    ed_sbp_value < (90) ~ "Class 2",
    ed_sbp_value >= (90) & ed_sbp_value < (110) ~ "Class 1",
    is.na(ed_sbp_value) ~ NA_character_,        
    TRUE ~ "no shock"
  ))

# Convert gender from numeric to categorical
study.sample <- study.sample %>%
  mutate(pt_Gender = case_when(
    pt_Gender == 1 ~ "Male",
    pt_Gender == 2 ~ "Female",
    TRUE ~ NA_character_
  ))

# Converting 999 to unknown for pt_asa_preinjury
study.sample <- study.sample %>%
  mutate(pt_asa_preinjury = case_when(
    pt_asa_preinjury == 999 ~ NA_character_,
    TRUE ~ as.character(pt_asa_preinjury)
  ))

# Converting 999 to unknown for ed_sbp_rtscat
study.sample <- study.sample %>%
  mutate(ed_sbp_rtscat = case_when(
    ed_sbp_rtscat == 999 ~ NA_character_,
    TRUE ~ as.character(ed_sbp_rtscat)
  ))

# Reclassify those without ed_sbp_rtscat class
study.sample <- study.sample %>%
  mutate(ed_sbp_rtscat = case_when(
    ed_sbp_rtscat != "999" & !is.na(ed_sbp_rtscat) ~ ed_sbp_rtscat,  
    ed_sbp_value >89 ~ "4",
    ed_sbp_value <= 89 & ed_sbp_value >= 76 ~ "3",                                   
    ed_sbp_value <= 75 & ed_sbp_value >= 50 ~ "2",
    ed_sbp_value <= 49 & ed_sbp_value >= 1 ~ "1",
    ed_sbp_value >= 110 ~ "no shock",                               
    TRUE ~ NA_character_                                              
  ))

# Converting INR to numeric
inr_num <- convert_number(study.sample$ed_inr)

# Re-add INR as numeric to `study.sample`
study.sample <- study.sample %>%
  mutate(ed_inr_numeric = inr_num)

# Classify ISS
# Classify age
# Classify INR

# Remove unused variables. 
study.sample <- study.sample |> 
  select(-inj_mechanism, 
         -ed_gcs_sum,
         -ed_rr_value,
         -ed_be_art,
         -host_care_level,
         -res_survival,
         -ed_inr)

# Label variables
var_label(study.sample$pt_age_yrs) <- "Age (Years)"
var_label(study.sample$ofi) <- "Opportunities for improvement (Y/N)"
var_label(study.sample$ed_be_art_numeric) <- "Base Excess (BE)"
var_label(study.sample$SBP_class) <- "Shock class classified according to SBP"
var_label(study.sample$BE_class) <- "Shock class classified according to BE"
var_label(study.sample$pt_asa_preinjury) <- "Pre-injury ASA"
var_label(study.sample$ISS) <- "Injury Severity Score"
var_label(study.sample$pt_Gender) <- "Gender (M/F)"
var_label(study.sample$ed_sbp_value) <- "Systolic blood pressure (mmhg)"
var_label(study.sample$ed_sbp_rtscat) <- "SBP registery categorised"

# Create a table of sample characteristics
sample.characteristics.table <- tbl_summary(study.sample,
                                            by = ofi)

# Display table
sample.characteristics.table

# Testing P
add_p(sample.characteristics.table)

# Print
print(sample.characteristics.table)


# !Plot and Regression!

# Make ofi numerical
study.sample$ofinum <- ifelse(study.sample$ofi == "Yes", 1, 0)

# No NA database
study.sample_noNA <- study.sample %>%
  filter(!is.na(ofi) & !is.na(BE_class))

# Relevel, "no shock" reference category
study.sample_noNA$BE_class <- relevel(factor(study.sample_noNA$BE_class), ref = "Class 1 (no shock)")

# Logistic regression
Reg <- glm(formula= ofinum ~ BE_class, family = "binomial", data = study.sample_noNA)
summary(Reg)

# Generate predicted probabilities
study.sample_noNA$predicted_prob <- predict(Reg, type = "response")

# Create a plot
ggplot(study.sample_noNA, aes(x = BE_class, y = predicted_prob)) +
  geom_point() +  
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) + 
  labs(title = "Predicted Probability of OFI by BE Class",
       x = "BE Class",
       y = "Predicted Probability of OFI") +
  theme_minimal()

# Dataframe that regression models uses  - put in the beginning when using real data.
true_noNA <- na.omit(study.sample)




# hantera missingdata - Hur hantera de 33%? kör listwise deletion och sedan diskutera det i diskussionen
# tolka värdena och läsa på