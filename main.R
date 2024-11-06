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
  select(
    pt_age_yrs,
    pt_Gender,
    pt_asa_preinjury,
    ed_sbp_value,
    ed_be_art,
    ISS,
    ofi,
    ed_inr,
    ofi.categories.broad,
    ofi.categories.detailed,
    Deceased,
  )

# Those excluded
excluded <- study.data |>
  filter(Deceased == "False",
         pt_age_yrs >= 15
  )

# Exclude patients who were not reviewed for the presence of OFI
study.sample <- study.data |>
  filter(!is.na(ofi),
         Deceased == "False",
         pt_age_yrs >= 15
         )

# Function for converting to numeric
convert_number <- function(x) {
  x <- as.character(x)
  x <- gsub(pattern = ",", replacement = ".", x = x, fixed = TRUE)
  x <- as.numeric(x)
  return(x)
}

# Make ofi numerical
study.sample$ofinum <- ifelse(study.sample$ofi == "Yes", 1, 0)

# Converting ed_be_art to numeric
BEnum <- convert_number(study.sample$ed_be_art)

#Re-add the BE column as numeric to `study.sample`
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

# BE shock classification
study.sample <- study.sample %>%
  mutate(BE_class = case_when(
    BEnum < (-10) ~ "Class 4",
    BEnum >= (-10) & BEnum < (-6) ~ "Class 3",
    BEnum >= (-6) & BEnum < (-2) ~ "Class 2",
    BEnum >= (-2) ~ "Class 1",
    is.na(BEnum) ~ NA_character_,
  ))

# V4 SBP class
study.sample <- study.sample %>%
  mutate(V4SBP_class = case_when(
    ed_sbp_value < (90) ~ "Class 4",
    ed_sbp_value >= 90 & ed_sbp_value < (100) ~ "Class 3",
    ed_sbp_value >= 100 & ed_sbp_value < (110) ~ "Class 2",
    is.na(ed_sbp_value) ~ NA_character_,
    TRUE ~ "Class 1"
  ))

# Binary logistic regression model - BE
# Note that you cannot adjust for both ways to define shock in the same model, because they are just different ways to define the same thing. I suggest you create separate models for each.
log_regBE <- glm(ofinum ~ pt_age_yrs + pt_Gender + pt_asa_preinjury + ed_inr_numeric + ISS + BE_class,
  family = binomial,
  data = study.sample
)

# Binary logistic regression model - SBP
log_regSBP <- glm(ofinum ~ pt_age_yrs + pt_Gender + pt_asa_preinjury + ed_inr_numeric + ISS + V4SBP_class,
               family = binomial,
               data = study.sample
)

# Binary logistic regression model unadjusted - BE
log_regBEun <- glm(ofinum ~ BE_class,
                 family = binomial,
                 data = study.sample
)

# Binary logistic regression model unadjusted - SBP
log_regSBPun <- glm(ofinum ~ V4SBP_class,
                  family = binomial,
                  data = study.sample
)

# Remove unused variables.
# I suggest removing them from the lines 40-52 where the study data is created instead
study.sample <- study.sample |>
  select(
    -ed_be_art,
    -ed_inr,
    -ofinum,
    -Deceased
  )
# Label variables
var_label(study.sample$pt_age_yrs) <- "Age (Years)"
var_label(study.sample$ofi) <- "Opportunities for improvement (Y/N)"
var_label(study.sample$ed_be_art_numeric) <- "Base Excess (BE)"
var_label(study.sample$BE_class) <- "Shock classification - BE"
var_label(study.sample$pt_asa_preinjury) <- "Pre-injury ASA"
var_label(study.sample$ISS) <- "Injury Severity Score"
var_label(study.sample$pt_Gender) <- "Gender (M/F)"
var_label(study.sample$ed_sbp_value) <- "Systolic blood pressure (mmhg)"
var_label(study.sample$ed_inr_numeric) <- "INR"
var_label(study.sample$V4SBP_class) <- "Shock classification - SBP"

# Counting log reg BE
log_reg_dataBE <- study.sample %>% 
  filter(!is.na(pt_age_yrs) & 
           !is.na(pt_Gender) & 
           !is.na(ISS) & 
           !is.na(ed_inr_numeric) & 
           !is.na(pt_asa_preinjury) & 
           !is.na(BE_class))

# Total rows used in the logistic regression
log_reg_countBE <- nrow(log_reg_dataBE)

# Counting log reg SBP
log_reg_dataSBP <- study.sample %>% 
  filter(!is.na(pt_age_yrs) & 
           !is.na(pt_Gender) & 
           !is.na(ISS) & 
           !is.na(ed_inr_numeric) & 
           !is.na(pt_asa_preinjury) & 
           !is.na(V4SBP_class))

# Total rows used in the logistic regression
log_reg_countSBP <- nrow(log_reg_dataSBP)


# Create a table of sample characteristics
sample.characteristics.table <- tbl_summary(study.sample,
  by = ofi
) |>
  add_overall() |>
  add_p()

# Create a table of sample characteristics - BE
sample.characteristics.tableBE <- tbl_summary(log_reg_dataBE,
                                            by = ofi
) |>
  add_overall() |>
  add_p()

# Create a table of sample characteristics - SBP
sample.characteristics.tableSBP <- tbl_summary(log_reg_dataSBP,
                                              by = ofi
) |>
  add_overall() |>
  add_p()

# Create a table of regression of sample - BE
log_regBE_sample.characteristics.table <- tbl_regression(log_regBE,
  exponentiate = TRUE,
  label = list(
    pt_age_yrs ~ "Age (Years)",
    pt_Gender ~ "Gender (M/F)",
    pt_asa_preinjury ~ "Pre-injury ASA",
    ed_inr_numeric ~ "INR",
    ISS ~ "Injury Severity Score",
    BE_class ~ "Shock classification - BE"
  )
)

# Create a table of regression of sample - SBP
log_regSBP_sample.characteristics.table <- tbl_regression(log_regSBP,
  exponentiate = TRUE,
  label = list(
    pt_age_yrs ~ "Age (Years)",
    pt_Gender ~ "Gender (M/F)",
    pt_asa_preinjury ~ "Pre-injury ASA",
    ed_inr_numeric ~ "INR",
    ISS ~ "Injury Severity Score",
    V4SBP_class ~ "Shock classification - SBP"
  )
)

# Create a table of regression of sample unadjusted - BE
log_regBE_sample.characteristics.table_unadjusted <- tbl_regression(log_regBEun,
                                                         exponentiate = TRUE,
                                                         label = list(
                                                           BE_class ~ "Shock classification - BE"
                                                         )
)

# Create a table of regression of sample unadjusted - SBP
log_regSBP_sample.characteristics.table_unadjusted <- tbl_regression(log_regSBPun,
                                                          exponentiate = TRUE,
                                                          label = list(
                                                            V4SBP_class ~ "Shock classification - SBP"
                                                          )
)

# ggplot2
ofi_counts <- study.sample %>%
  group_by(BE_class, ofi.categories.broad) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(BE_class) %>%
  mutate(percent = count / sum(count) * 100)

p <- ggplot(ofi_counts, aes(x = BE_class, y = percent, fill = ofi.categories.broad)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of OFI Broad Categories by BE Shock Class",
    x = "BE Shock Class",
    y = "Percentage of OFI Broad Categories"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set1", name = "OFI Categories")

print(p)

# ggplot2 SBP
ofi_counts <- study.sample %>%
  group_by(V4SBP_class, ofi.categories.broad) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(V4SBP_class) %>%
  mutate(percent = count / sum(count) * 100)

pSBP <- ggplot(ofi_counts, aes(x = V4SBP_class, y = percent, fill = ofi.categories.broad)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of OFI Broad Categories by SBP Shock Class",
    x = "SBP Shock Class",
    y = "Percentage of OFI Broad Categories"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set2", name = "OFI Categories")

print(pSBP)

# Print
print(sample.characteristics.table)
print(sample.characteristics.tableBE)
print(sample.characteristics.tableSBP)
print(log_regBE_sample.characteristics.table)
print(log_regSBP_sample.characteristics.table)
print(log_regBE_sample.characteristics.table_unadjusted)
print(log_regSBP_sample.characteristics.table_unadjusted)

# Create objects for descriptive data
ofi <- paste0(sum(study.sample$ofi == "Yes"), " (", round(sum(study.sample$ofi == "Yes") / nrow(study.sample) * 100, 1), "%)")
age <- inline_text(sample.characteristics.table, variable = pt_age_yrs, column = stat_0)
male <- inline_text(sample.characteristics.table, variable = pt_Gender, column = stat_0, level = "Male")
merged <- nrow(merged.data)
sample <- nrow(study.sample)
excluded <- nrow(excluded)

