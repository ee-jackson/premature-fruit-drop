#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: proportionAbscised.R
## Desc: calculate proportion prematurely abscised, per year, per sp
## Date: November 2019

rm(list = ls())

#load packages
require(tidyverse)

seedRain<-read.table("../data/BCI_TRAP200_20190215_spcorrected.txt", header=TRUE, stringsAsFactors = FALSE)

seedTrait<-read.csv("../data/20120227_seedsMassForTraits.csv", header=TRUE, stringsAsFactors = FALSE)

tidyTraits <- read.csv("../data/TidyTrait.csv", header=TRUE, stringsAsFactors = FALSE)
tidyTraits <- subset(tidyTraits, TotUnits_collected >= 200)

#convert fetcha to class Date
seedRain$fecha <- as.character(seedRain$fecha)
seedRain$fecha <- as.Date(seedRain$fecha, "%Y-%m-%d")
seedRain$year <- format(as.Date(seedRain$fecha), "%Y")

seedDat <- left_join(seedRain, seedTrait, by = c("sp" = "SP4"))

# subset dataframe to only woody plants and parts 1 (mature fruits), 2 (single diaspores), and 5 (immature fruit)
seedDat <- subset(seedDat, part==1|part==2|part==5)
seedDat <- subset(seedDat,LIFEFORM== "LIANA"|LIFEFORM== "MIDSTORY"|LIFEFORM== "SHRUB"|LIFEFORM== "TREE"|LIFEFORM=="UNDERSTORY")

# date is from 1987-01-12 to 2019-01-29, we only want complete years so remove year=2019 and year=1987
seedDat <- subset(seedDat, seedDat$year != "1987" & seedDat$year != "2019")

# remove sp where there is no data for N_SEEDFULL
seedDat <- seedDat %>% 
	drop_na("N_SEEDFULL")

tbl_df(seedDat)

#############################################

# create a df that sums quantity by year, sp and part
absdat <- seedDat %>% 
	group_by(part,sp, year, N_SEEDFULL) %>%
	summarise(quantity_sum= sum(quantity, na.rm = TRUE)) %>%
	ungroup()

# Viable seeds

absdat.v <- subset(absdat, part== 1 | part== 2) %>% 
	replace(., is.na(.), 0) %>%
	rowwise() %>% 
	mutate(viable_seeds = ifelse(part==1, quantity_sum*N_SEEDFULL, quantity_sum)) %>%
	ungroup() %>%
	group_by(sp, year) %>%
	summarise(viable_seeds= sum(viable_seeds, na.rm = TRUE)) %>%
	ungroup()

###############################################

# Abscised seeds

absdat.a <- subset(absdat, part== 5) %>% 
	replace(., is.na(.), 0) %>%
	rowwise() %>% 
	mutate(abscised_seeds = quantity_sum*N_SEEDFULL) %>%
	ungroup() %>%
	group_by(sp,year) %>%
	summarise(abscised_seeds= sum(abscised_seeds, na.rm = TRUE)) %>%
	ungroup()

# join and calculate proportion abcised
propDat<- full_join(absdat.a, absdat.v, by= c("sp", "year"))

propDat <- propDat %>%
	replace(., is.na(.), 0) %>%
	rowwise() %>% 
	mutate(total_seeds = sum(abscised_seeds, viable_seeds, na.rm = TRUE), proportion_abscised = abscised_seeds / total_seeds) %>%
	ungroup()



write.csv(propDat.y,"../output/tables/proportionAbscisedPerYear.csv")

############ create a dataset to use with the traits that I want

propDat.y <- left_join(propDat, seedTrait, by = c("sp" = "SP4"), all.x=TRUE)

propDat.traits <- dplyr::left_join(propDat.y, tidyTraits, by = c("sp" = "Codigo"), all.x=TRUE)

plantTraits.y <- subset(propDat.traits, select = c(sp, year, proportion_abscised, abscised_seeds, viable_seeds, total_seeds, FAMILY, GENUS, SPECIES, LIFEFORM, N_SEEDFULL, SEED_DRY, HEIGHT_AVG, cvseed, CoFruit, BCIReproductive, Endocarp_investment, Plant_species19, SeedPredationRate, SeedPred_n, SeedPred_pres, Coleo_pres, Hymeno_pres, Lepid_pres))

colnames(plantTraits.y) <- tolower(colnames(plantTraits.y))
plantTraits.y$lifeform <- str_to_title(plantTraits.y$lifeform, locale = "en")
plantTraits.y$lifeform <- as.factor(plantTraits.y$lifeform)
#plantTraits.y$x <- NULL

write.csv(plantTraits.y,"../output/tables/fullCleanData.csv")

# get a weighted average so one value per species

write.csv(propDat,"../output/tables/proportionAbscisedPerSp.csv")

propDat.sp <- propDat %>%
	group_by(sp) %>% 
	summarise(proportion_abscised = mean(proportion_abscised), abscised_seeds = sum(abscised_seeds), viable_seeds = sum(viable_seeds), total_seeds = sum(total_seeds)) %>%
	ungroup()

propDat.sp.50 <- subset(propDat.sp, total_seeds >= 50)

propDat.sp.50.t <- left_join(propDat.sp.50, seedTrait, by = c("sp" = "SP4"), all.x=TRUE)

propDat.traits <- dplyr::left_join(propDat.sp.50.t, tidyTraits, by = c("sp" = "Codigo"), all.x=TRUE)

plantTraits <- subset(propDat.traits, select = c(sp, proportion_abscised, abscised_seeds, viable_seeds, total_seeds, FAMILY, GENUS, SPECIES, LIFEFORM, N_SEEDFULL, SEED_DRY, HEIGHT_AVG, cvseed, CoFruit, BCIReproductive, Endocarp_investment, Plant_species19, SeedPredationRate, SeedPred_n, SeedPred_pres, Coleo_pres, Hymeno_pres, Lepid_pres))

colnames(plantTraits) <- tolower(colnames(plantTraits))
plantTraits$lifeform <- str_to_title(plantTraits$lifeform, locale = "en")
plantTraits$lifeform <- as.factor(plantTraits$lifeform)
plantTraits$x <- NULL

write.csv(plantTraits,"../output/tables/cleanData.csv")


