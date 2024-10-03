function(x){
  x <- as.character(x)
  x <- gsub(pattern = ",", replacement = ".",x = x, fixed = TRUE)
  x <- as.numeric(x)
  return(x)
}

BE <- convert_number(data$swetrau_scrambled$ed_be_art)

BEclass1 <- swetrau_scrambled[BE<0 & BE>-2, ]
BEclass2 <- swetrau_scrambled[BE<(-2) & BE>(-6), ]
BEclass3 <- swetrau_scrambled[BE<(-6) & BE>-10, ]
BEclass4 <- swetrau_scrambled[BE<(-10), ]

BEclass1num <- convert_number(BEclass1$ed_be_art)
BEclass2num <- convert_number(BEclass2$ed_be_art)
BEclass3num <- convert_number(BEclass3$ed_be_art)
BEclass4num <- convert_number(BEclass4$ed_be_art)

newNiss <- as.numeric(NISS)
newISS <- as.numeric(ISS)
newhost_care_level <- as.numeric(host_care_level)

DataFrame1 <- data.frame(host_care_level, newNiss, newISS)