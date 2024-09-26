function(x){
  x <- as.character(x)
  x <- gsub(pattern = ",", replacement = ".",x = x, fixed = TRUE)
  x <- as.numeric(x)
  return(x)
}

BE <- convert_number(data$swetrau_scrambled$ed_be_art)

BEclass1 <- swetrau_scrambled[BE<0 & BE>-2, ]
