#!/usr/bin/env Rscript

## Author: Eleanor Jackson eleanor.elizabeth.j@gmail.com
## Script: summarizeSeedRain.R
## Desc: generate a summary table of the SeedRain dataset
## Date: November 2019

rm(list = ls())

#load packages
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
library(data.table)
library(ggExtra)
library(purrr)

seedRain<-read.table("../data/BCI_TRAP200_20190215_spcorrected.txt", header=TRUE, stringsAsFactors = FALSE) # this is the trap data

seedTrait<-read.csv("../data/20120227_seedsMassForTraits.csv", header=TRUE, stringsAsFactors = FALSE) # these are traits of sp prodived by Joe

# select useful columns from seed trait data
seedTrait <- subset(seedTrait,select = c(SP4, SP6, FAMILY, GENUS, SPECIES, LIFEFORM, N_SEEDFULL))

# add traits to seed rain dataset by species
seedDat <- dplyr::left_join(seedRain, seedTrait, by = c("sp" = "SP4"))

# subset datafrane to only woody plants and parts 1 (mature fruits), 2 (single diaspores), and 5 (immature fruit)
seedDat <- subset(seedDat, part==1|part==2|part==5)
seedDat <- subset(seedDat,LIFEFORM== "LIANA"|LIFEFORM== "MIDSTORY"|LIFEFORM== "SHRUB"|LIFEFORM== "TREE"|LIFEFORM=="UNDERSTORY")

#convert fetcha to class Date
seedDat$fecha <- as.character(seedDat$fecha)
seedDat$fecha <- as.Date(seedDat$fecha, "%Y-%m-%d")
seedDat$year <- format(as.Date(seedDat$fecha), "%Y")

# remove sp where there is no data for N_SEEDFULL
seedDat <- seedDat %>% 
	drop_na("N_SEEDFULL")

##### calculate proportion abscised by sp #####

# create a df that sums quantity by sp and part
absdat <- seedDat %>% 
	group_by(part,sp, N_SEEDFULL) %>%
	summarise(quantity_sum= sum(quantity, na.rm = TRUE)) %>%
	ungroup()

# calculate Viable seeds

# multiply quantity of mature fruits by average seeds per fruit (N_SEEDFULL), but keep single diaspores, then sum viable seeds per sp

absdat.v <- subset(absdat, part== 1 | part== 2) %>% 
	replace(., is.na(.), 0) %>%
	rowwise() %>% 
	mutate(viable_seeds = ifelse(part==1, quantity_sum*N_SEEDFULL, quantity_sum)) %>%
	ungroup() %>%
	group_by(sp) %>%
	summarise(viable_seeds= sum(viable_seeds, na.rm = TRUE)) %>%
	ungroup()

# calculate Abscised seeds, immature fruit * N_SEEDFULL

absdat.a <- subset(absdat, part== 5) %>% 
	replace(., is.na(.), 0) %>%
	rowwise() %>% 
	mutate(abscised_seeds = quantity_sum*N_SEEDFULL) %>%
	ungroup() %>%
	group_by(sp) %>%
	summarise(abscised_seeds= sum(abscised_seeds, na.rm = TRUE)) %>%
	ungroup()

# join and calculate proportion abcised
propDat<- full_join(absdat.a, absdat.v, by= c("sp"))

propDat <- propDat %>%
	replace(., is.na(.), 0) %>%
	rowwise() %>% 
	mutate(total_seeds = sum(abscised_seeds, viable_seeds, na.rm = TRUE), proportion_abscised = abscised_seeds / total_seeds) %>%
	ungroup()


#### summarise trap data ####

seedDat.sumParts <- seedDat %>% 
	group_by(part,sp) %>%
	summarise(quantity_sum= sum(quantity, na.rm=TRUE)) %>%
	spread(part, quantity_sum)%>%
	ungroup() %>%
	replace(., is.na(.), 0)

seedDat.nTrapsYears <- seedDat %>% 
	group_by(sp) %>%
	summarise(n_traps= n_distinct(trap, na.rm = TRUE), n_years= n_distinct(year, na.rm = TRUE))%>%
	ungroup() %>%
	replace(., is.na(.), 0) 

fullSummary <- list(propDat, seedDat.sumParts, seedDat.nTrapsYears) %>% 
	reduce(full_join, by = "sp")

#fullSummary2 <- left_join(fullSummary, propDat.sp, by= "sp")

seedDat.summary <- left_join(fullSummary, seedTrait, by = c("sp" = "SP4"))

write.csv(seedDat.summary, "../output/tables/summarizeSeedRain.csv")
