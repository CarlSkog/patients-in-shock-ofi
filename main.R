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
library(nnet)
library(broom.helpers)

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
         pt_asa_preinjury, 
         ed_sbp_value,
         ed_be_art,
         ISS,
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

# Make ofi numerical
study.sample$ofinum <- ifelse(study.sample$ofi == "Yes", 1, 0)

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

# Converting INR to numeric
ed_inr_numeric <- convert_number(study.sample$ed_inr)

# Re-add INR as numeric to `study.sample`
study.sample <- study.sample %>%
  mutate(ed_inr_numeric = ed_inr_numeric)

# V4 SBP class
study.sample <- study.sample %>%
  mutate(V4SBP_class = case_when(
    ed_sbp_value < (90) ~ "Class 4 - severe",
    ed_sbp_value >= 90 & ed_sbp_value < (100) ~"Class 3 - moderate",
    ed_sbp_value >= 100 & ed_sbp_value < (110) ~ "Class 2 - mild",
    is.na(ed_sbp_value) ~ NA_character_,        
    TRUE ~ "Class 1 - no shock"
  ))

# Binary logistic regression model
log_reg <- glm(ofinum ~ pt_age_yrs + pt_Gender + ISS + ed_inr_numeric + pt_asa_preinjury + V4SBP_class + BE_class, 
               family = binomial, 
               data = study.sample)

# Summary of the model
summary(log_reg)

# Remove unused variables. 
study.sample <- study.sample |> 
  select(-ed_be_art,
         -ed_inr,
         -ofinum
         )

# Label variables
var_label(study.sample$pt_age_yrs) <- "Age (Years)"
var_label(study.sample$ofi) <- "Opportunities for improvement (Y/N)"
var_label(study.sample$ed_be_art_numeric) <- "Base Excess (BE)"
var_label(study.sample$BE_class) <- "Shock class classified according to BE"
var_label(study.sample$pt_asa_preinjury) <- "Pre-injury ASA"
var_label(study.sample$ISS) <- "Injury Severity Score"
var_label(study.sample$pt_Gender) <- "Gender (M/F)"
var_label(study.sample$ed_sbp_value) <- "Systolic blood pressure (mmhg)"
var_label(study.sample$ed_sbp_rtscat) <- "SBP registery categorised"
var_label(study.sample$ed_inr_numeric) <- "INR"
var_label(study.sample$V4SBP_class) <- "Shock class V4 - SBP"

# Create a table of sample characteristics
sample.characteristics.table <- tbl_summary(study.sample,
                                            by = ofi)

# Create a table of regression of sample

log_reg_sample.characteristics.tabel <- tbl_regression(log_reg, exponentiate = TRUE)

# Display tables
sample.characteristics.table
log_reg_sample.characteristics.tabel

# Print
print(sample.characteristics.table)
print(log_reg_sample.characteristics.tabel)




# hantera missingdata - Hur hantera de 33%? kör listwise deletion och sedan diskutera det i diskussionen
# tolka värdena och läsa på