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

## Import data
data <- import_data(test = TRUE)

## Whatever you do next, maybe clean data?

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