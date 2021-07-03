#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: combine-data.R
## Desc: create a clean dataset for downstream analyses: combine proportion of
##       prematurely abscised seeds with plant and seed trait datasets
## Date: July 2021

# Load packages ---------------------------

library("groundhog")
groundhog_day = "2021-05-01"
groundhog.library("tidyverse", groundhog_day)
groundhog.library("rdryad", groundhog_day)

# Get plant attribute data from Dryad ---------------------------

# download from Dryad
file_paths <- rdryad::dryad_download(doi = "10.5061/dryad.230j5ch")

# function to put downloaded files in the right place
move_files <- function(x){
  file.rename( from = file.path(x) ,
               to = file.path(here::here("data", "raw"), basename(x)) )
}

# apply the function to all files
lapply(file_paths, move_files)

# load TidyTrait.csv
plant_trait <- read.csv(here::here("data", "raw","TidyTrait.csv"),
                        header=TRUE, stringsAsFactors = FALSE)

# Get seed abscission rate data ---------------------------

fruit_drop <- read.csv(here::here("data", "clean","fruit_drop.csv"),
                        header=TRUE, stringsAsFactors = FALSE)

# Clean up plant traits data ---------------------------

# select columns and only keep if n=>200
plant_trait %>%
  subset(TotUnits_collected >= 200) %>%
  rename(sp4 = Codigo, sp6 = Code6) %>%
  select(sp4, sp6, Plant_species19, Family, HEIGHT_AVG, seed_dry,
         cvseed,  CoFruit, BCIReproductive, Endocarp_investment,
         SeedPred_pres) -> plant_trait

# make colnames lowercase
colnames(plant_trait) <- tolower(colnames(plant_trait))

# Join data ---------------------------

# join
fruit_traits <- left_join(fruit_drop, plant_trait, by = "sp4", all.x = TRUE)

# only include sp x year data points if there were at least 10 parts found
fruit_traits <- subset(fruit_traits, sum_parts >= 10)

# need factors when fitting models later
fruit_traits$year <- as.factor(fruit_traits$year)
fruit_traits$sp4 <- as.factor(fruit_traits$sp4)

# save as rds
saveRDS(fruit_traits, file = here::here("data", "clean", "fruit_traits.rds"))
