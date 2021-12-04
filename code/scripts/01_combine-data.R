#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 01_combine-data.R
## Desc: create a clean dataset for downstream analyses: combine proportion of
##       prematurely abscised seeds with plant trait dataset
## Date: July 2021

# Load packages -----------------------------------------------------------

library("tidyverse") # v1.3.1
library("rdryad") # v1.0.0
library("here") # v1.0.1

# Get plant trait data from Dryad -------------------------------------

# download from Dryad
trait_download <- rdryad::dryad_download(doi = "10.5061/dryad.230j5ch")

# function to put downloaded files in the right place
move_files <- function(old_path, new_path){
  file.rename( from = file.path(old_path) ,
               to = file.path(new_path, basename(old_path)) )
}

# apply the function to all files
lapply(trait_download, move_files, new_path = here::here("data", "raw"))

# load TidyTrait.csv
plant_trait <- read.csv(here::here("data", "raw","TidyTrait.csv"),
                        header=TRUE, stringsAsFactors = FALSE)

# Get seed abscission rate data -------------------------------------------

# download from Dryad
fruit_drop_download <- rdryad::dryad_download(doi = "10.5061/dryad.4mw6m909j")

# move files
lapply(fruit_drop_download, move_files, new_path = here::here("data", "clean"))

# load fruit_drop.csv
fruit_drop <- read.csv(here::here("data", "clean","fruit_drop.csv"),
                        header=TRUE, stringsAsFactors = FALSE)

# Clean up plant trait data -----------------------------------------------

# select columns and only keep if n=>200
plant_trait %>%
  subset(TotUnits_collected >= 200) %>%
  rename(sp4 = Codigo, sp6 = Code6) %>%
  select(sp4, sp6, Plant_species19, HEIGHT_AVG, seed_dry,
         cvseed,  CoFruit, BCIReproductive, Endocarp_investment,
         SeedPred_pres) -> plant_trait

# make colnames lowercase
colnames(plant_trait) <- tolower(colnames(plant_trait))

# Join data ---------------------------------------------------------------

# join
fruit_traits <- left_join(fruit_drop, plant_trait, by = "sp4", all.x = TRUE)

# only include sp x year data points if there were at least 10 parts found
fruit_traits <- subset(fruit_traits, sum_parts >= 10)

# need factors when fitting models later
fruit_traits$year <- as.factor(fruit_traits$year)
fruit_traits$sp4 <- as.factor(fruit_traits$sp4)

# save as rds
saveRDS(fruit_traits, file = here::here("data", "clean", "fruit_traits.rds"))
