#!/usr/bin/env Rscript

## Author: Eleanor Jackson eleanor.elizabeth.j@gmail.com
## Script: proportionAbscised.R
## Desc: calculate proportion prematurely abscised, per year, per sp
## Date: November 2019

rm(list = ls())

#load packages
library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(tidyr)
library(data.table)
library(ggExtra)
library(purrr)

seedRain<-read.table("../data/BCI_TRAP200_20190215_spcorrected.txt", header=TRUE, stringsAsFactors = FALSE)

seedTrait<-read.csv("../data/20120227_seedsMassForTraits.csv", header=TRUE, stringsAsFactors = FALSE)

#convert fetcha to class Date
seedRain$fecha <- as.character(seedRain$fecha)
seedRain$fecha <- as.Date(seedRain$fecha, "%Y-%m-%d")
seedRain$year <- format(as.Date(seedRain$fecha), "%Y")

seedDat <- left_join(seedRain, seedTrait, by = c("sp" = "SP4"))


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

write.csv(propDat,"../data/proportionAbscised.csv")

