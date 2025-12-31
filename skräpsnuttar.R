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


# SBP shock classifiaction
study.sample <- study.sample %>%
  mutate(SBP_class = case_when(
    ed_sbp_value < (90) ~ "Class 2",
    ed_sbp_value >= (90) & ed_sbp_value < (110) ~ "Class 1",
    is.na(ed_sbp_value) ~ NA_character_,        
    TRUE ~ "no shock"
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


# !Plot and Regression!

# Make ofi numerical
study.sample$ofinum <- ifelse(study.sample$ofi == "Yes", 1, 0)

# Dataframe that regression models uses
true_noNA <- na.omit(study.sample)

# Relevel, "no shock" reference category
true_noNA$BE_class <- relevel(factor(true_noNA$BE_class), ref = "Class 1 (no shock)")

# Logistic regression
Reg <- glm(formula= ofinum ~ BE_class, family = "binomial", data = true_noNA)
summary(Reg)

# Generate predicted probabilities
true_noNA$predicted_prob <- predict(Reg, type = "response")


# Dataframe that regression models uses
true_noNA <- na.omit(study.sample)

# Generate predicted probabilities
study.sample$predicted_prob <- predict(log_reg, type = "response")

# Create a plot
ggplot(true_noNA, aes(x = BE_class, y = predicted_prob)) +
  geom_point() +  
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) + 
  labs(title = "Predicted Probability of OFI by BE Class",
       x = "BE Class",
       y = "Predicted Probability of OFI") +
  theme_minimal()





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

There were a total of  patients in the trauma registry. After excluding the patients younger than 15 and/or were dead on arrival, there were `r excludednum` patients left. Out of those, `r sample` patients had been reviewed for the presence of OFI. A total of `r sample - log_reg_count` patients were excluded due to missing data, resulting in `r log_reg_count` patients for the final analysis.

The variable with most missing data is BE which lack the data for 1721 patients.

#TEST
master_combined_table_stepSBP <- {
  
  # ---- EVENT RISK (leftmost column) ----
  event_risk_SBP <-
    tbl_regression(
      log_regSBPun,
      exponentiate = TRUE,
      label = list(
        V4SBP_class ~ "Shock classification – SBP"
      )
    ) |>
    add_nevent(location = "level") |>
    add_n(location = "level") |>
    modify_table_body(
      ~ .x |>
        mutate(
          stat_event_risk =
            ifelse(
              !is.na(stat_nevent),
              paste0(
                stat_nevent, " / ", stat_n,
                " (",
                style_sigfig(stat_nevent / stat_n, scale = 100),
                "%)"
              ),
              NA_character_
            )
        )
    ) |>
    modify_table_body(
      ~ .x |>
        select(row_type, variable, label, stat_event_risk)
    ) |>
    modify_header(stat_event_risk = "**Event risk**")
  
  # ---- MERGE ALL TABLES ----
  tbl_merge(
    tbls = list(
      event_risk_SBP,
      log_regSBP_sample.characteristics.table_unadjusted,
      SBPstep1,
      log_regSBP_sample.characteristics.table
    ),
    tab_spanner = c(
      "**Event risk**",
      "**Unadjusted**",
      "**Without ISS**",
      "**Fully adjusted**"
    )
  )
}

print(master_combined_table_stepSBP)

