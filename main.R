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

## Import data
data <- import_data(test = TRUE)

# Merge data
merged.data <- merge_data(data, test = TRUE)

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
         ofi)

OfiBE <- merged.data |> 
  select(ed_be_art,
         ofi)

# Exclude patients who were not reviewed for the presence of OFI
study.sample <- study.data |>
  filter(!is.na(ofi))

study.sampleBE <- study.sample |>
  filter(!is.na(ed_be_art))

OfiBEO <- OfiBE |>
  filter(!is.na(ofi))

OfiBEOB <- OfiBEO |>
  filter(!is.na(ed_be_art))

# Label variables
var_label(study.sample$pt_age_yrs) <- "Age in years"
var_label(study.sample$ofi) <- "Opportunities for improvement"

# Create a table of sample characteristics
sample.characteristics.table <- tbl_summary(study.sample,
                                            by = ofi)

## Whatever you do next, maybe clean data?

function(x){
  x <- as.character(x)
  x <- gsub(pattern = ",", replacement = ".",x = x, fixed = TRUE)
  x <- as.numeric(x)
  return(x)
}

BE <- convert_number(study.sampleBE$ed_be_art)

BEclass1 <- study.sampleBE[BE<0 & BE>=(-2), ]
BEclass2 <- study.sampleBE[BE<(-2) & BE>=(-6), ]
BEclass3 <- study.sampleBE[BE<(-6) & BE>=-10, ]
BEclass4 <- study.sampleBE[BE<(-10), ]

BEclass1num <- convert_number(BEclass1$ed_be_art)
BEclass2num <- convert_number(BEclass2$ed_be_art)
BEclass3num <- convert_number(BEclass3$ed_be_art)
BEclass4num <- convert_number(BEclass4$ed_be_art)

antalBE1 <- sum(length(BEclass1num))
antalBE2 <- sum(length(BEclass2num))
antalBE3 <- sum(length(BEclass3num))
antalBE4 <- sum(length(BEclass4num))


ofi10 <- ifelse(study.sample$ofi == "Yes", 1, 0)


BEc1Y <- filter(BEclass1, ofi == "Yes")
BEc1N <- filter(BEclass1, ofi == "No")
BEc2Y <- filter(BEclass2, ofi == "Yes")
BEc2N <- filter(BEclass2, ofi == "No")
BEc3Y <- filter(BEclass3, ofi == "Yes")
BEc3N <- filter(BEclass3, ofi == "No")
BEc4Y <- filter(BEclass4, ofi == "Yes")
BEc4N <- filter(BEclass4, ofi == "No")

tab <- as.table(rbind(c(sum(length(convert_number(BEc1Y$ed_be_art))), sum(length(convert_number(BEc2Y$ed_be_art))), sum(length(convert_number(BEc3Y$ed_be_art))), sum(length(convert_number(BEc4Y$ed_be_art)))), 
                      c(sum(length(convert_number(BEc1N$ed_be_art))), sum(length(convert_number(BEc2N$ed_be_art))), sum(length(convert_number(BEc3N$ed_be_art))), sum(length(convert_number(BEc4N$ed_be_art))))))
dimnames(tab) <- list(OFI = c("Yes", "No"),
                      Shock_class = c("Class 1", "Class 2", "Class 3", "Class 4"))
tab
