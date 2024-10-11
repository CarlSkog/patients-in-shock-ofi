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

# Exclude patients who were not reviewed for the presence of OFI
study.sample <- study.data |>
  filter(!is.na(ofi))

# Exclude patients who did not have BE data
study.sampleBE <- study.sample |>
  filter(!is.na(ed_be_art))

# The display sample
display.sample <- study.sample |> 
  select(pt_age_yrs, 
         pt_Gender, 
         pt_asa_preinjury,
         ed_sbp_value,
         ISS,
         ofi)

# Adding shock classification

convert_number <- function(x){
  x <- as.character(x)
  x <- gsub(pattern = ",", replacement = ".",x = x, fixed = TRUE)
  x <- as.numeric(x)
  return(x)
}

BEnum <- convert_number(study.sample$ed_be_art)

study.sample <- study.sample %>%
  mutate(BE_class = case_when(
    BEnum < (-10) ~ "Class 4",
    BEnum >= (-10) & BEnum < (-6) ~ "Class 3",
    BEnum >= (-6) & BEnum < (-2) ~ "Class 2",  
    BEnum >= (-2) & BEnum < (0) ~ "Class 1",   
    is.na(BEnum) ~ NA_character_,        
    TRUE ~ "no shock"
  ))

# Remove unused variables. 
study.sample <- study.sample |> 
  select( -inj_mechanism, 
         -ed_gcs_sum,
         -ed_rr_value,
         -ed_be_art,
         -host_care_level,
         -res_survival)

# Label variables
var_label(study.sample$pt_age_yrs) <- "Age in years"
var_label(study.sample$ofi) <- "Opportunities for improvement"

# Create a table of sample characteristics
sample.characteristics.table <- tbl_summary(study.sample,
                                            by = ofi)

## Whatever you do next, maybe clean data?

convert_number <- function(x){
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

tabell <- as.table(rbind(c(sum(length(convert_number(BEc1Y$ed_be_art))), sum(length(convert_number(BEc2Y$ed_be_art))), sum(length(convert_number(BEc3Y$ed_be_art))), sum(length(convert_number(BEc4Y$ed_be_art)))), 
                      c(sum(length(convert_number(BEc1N$ed_be_art))), sum(length(convert_number(BEc2N$ed_be_art))), sum(length(convert_number(BEc3N$ed_be_art))), sum(length(convert_number(BEc4N$ed_be_art))))))
dimnames(tabell) <- list(OFI = c("Yes", "No"),
                      Shock_class = c("Class 1", "Class 2", "Class 3", "Class 4"))
tabell
chisq.test(tabell)


BEclass0 <- study.sampleBE[BE<0, ]
BEclass999 <- study.sampleBE[BE>= 0, ]
BEc0Y <- filter(BEclass0, ofi == "Yes")
BEc0N <- filter(BEclass0, ofi == "No")
BEc999Y <- filter(BEclass999, ofi == "Yes")
BEc999N <- filter(BEclass999, ofi == "No")

tabell <- as.table(rbind(c(sum(length(convert_number(BEc0Y$ed_be_art))), sum(length(convert_number(BEc999Y$ed_be_art)))),
                         c(sum(length(convert_number(BEc0N$ed_be_art))), sum(length(convert_number(BEc999N$ed_be_art))))))
dimnames(tabell) <- list(OFI = c("Yes", "No"),
                         Shock = c("Yes shock", "no shock"))
tabell
chisq.test(tabell)


SBP <- convert_number(study.sample$ed_sbp_value)
SBPclass0 <- study.sample[SBP<110, ]
SBPclass999 <- study.sample[SBP>= 110, ]
SBPc0Y <- filter(SBPclass0, ofi == "Yes")
SBPc0N <- filter(SBPclass0, ofi == "No")
SBPc999Y <- filter(SBPclass999, ofi == "Yes")
SBPc999N <- filter(SBPclass999, ofi == "No")

tabell <- as.table(rbind(c(sum(length(convert_number(SBPc0Y$ed_be_art))), sum(length(convert_number(SBPc999Y$ed_be_art)))),
                         c(sum(length(convert_number(SBPc0N$ed_be_art))), sum(length(convert_number(SBPc999N$ed_be_art))))))
dimnames(tabell) <- list(OFI = c("Yes", "No"),
                         Shock = c("Yes shock", "no shock"))
tabell
chisq.test(tabell)


SBP <- convert_number(study.sample$ed_sbp_value)
SBPclass1 <- study.sample[SBP>=110, ]
SBPclass2 <- study.sample[SBP>=110, ]
SBPclass3 <- study.sample[SBP <110 & SBP>=(90), ]
SBPclass4 <- study.sample[SBP < 90, ]
SBPc1Y <- filter(SBPclass1, ofi == "Yes")
SBPc1N <- filter(SBPclass1, ofi == "No")
SBPc2Y <- filter(SBPclass2, ofi == "Yes")
SBPc2N <- filter(SBPclass2, ofi == "No")
SBPc3Y <- filter(SBPclass3, ofi == "Yes")
SBPc3N <- filter(SBPclass3, ofi == "No")
SBPc4Y <- filter(SBPclass4, ofi == "Yes")
SBPc4N <- filter(SBPclass4, ofi == "No")

tabell <- as.table(rbind(c(sum(length(convert_number(SBPc1Y$ed_be_art))), sum(length(convert_number(SBPc2Y$ed_be_art))), sum(length(convert_number(SBPc3Y$ed_be_art))), sum(length(convert_number(SBPc4Y$ed_be_art)))), 
                         c(sum(length(convert_number(SBPc1N$ed_be_art))), sum(length(convert_number(SBPc2N$ed_be_art))), sum(length(convert_number(SBPc3N$ed_be_art))), sum(length(convert_number(SBPc4N$ed_be_art))))))
dimnames(tabell) <- list(OFI = c("Yes", "No"),
                         Shock_class = c("Class 1", "Class 2", "Class 3", "Class 4"))
tabell
chisq.test(tabell)



BE <- convert_number(study.sampleBE$ed_be_art)

BEclass1 <- study.sampleBE[BE<=0 & BE>(-2), ]
BEclass2 <- study.sampleBE[BE<=(-2) & BE>(-6), ]
BEclass3 <- study.sampleBE[BE<=(-6) & BE>-10, ]
BEclass4 <- study.sampleBE[BE<=(-10), ]

BEclass1num <- convert_number(BEclass1$ed_be_art)
BEclass2num <- convert_number(BEclass2$ed_be_art)
BEclass3num <- convert_number(BEclass3$ed_be_art)
BEclass4num <- convert_number(BEclass4$ed_be_art)

antalBE1 <- sum(length(BEclass1num))
antalBE2 <- sum(length(BEclass2num))
antalBE3 <- sum(length(BEclass3num))
antalBE4 <- sum(length(BEclass4num))

BEc1Y <- filter(BEclass1, ofi == "Yes")
BEc1N <- filter(BEclass1, ofi == "No")
BEc2Y <- filter(BEclass2, ofi == "Yes")
BEc2N <- filter(BEclass2, ofi == "No")
BEc3Y <- filter(BEclass3, ofi == "Yes")
BEc3N <- filter(BEclass3, ofi == "No")
BEc4Y <- filter(BEclass4, ofi == "Yes")
BEc4N <- filter(BEclass4, ofi == "No")

tabell <- as.table(rbind(c(sum(length(convert_number(BEc1Y$ed_be_art))), sum(length(convert_number(BEc2Y$ed_be_art))), sum(length(convert_number(BEc3Y$ed_be_art))), sum(length(convert_number(BEc4Y$ed_be_art)))), 
                         c(sum(length(convert_number(BEc1N$ed_be_art))), sum(length(convert_number(BEc2N$ed_be_art))), sum(length(convert_number(BEc3N$ed_be_art))), sum(length(convert_number(BEc4N$ed_be_art))))))
dimnames(tabell) <- list(OFI = c("Yes", "No"),
                         Shock_class = c("Class 1", "Class 2", "Class 3", "Class 4"))
tabell
chisq.test(tabell)

#recodar variabler - tidy verse
#conituerlig av BE i tabellen o.s.v 
#add p - gt summary 