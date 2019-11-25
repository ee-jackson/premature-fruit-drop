#!/usr/bin/env Rscript
rm(list = ls())

#load packages
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
library(data.table)
library(ggpubr)
library(ggExtra)
library(pivottabler)
library(purrr)
theme_set(theme_bw())

seedRain<-read.table("../data/BCI_TRAP200_20190215_spcorrected.txt", header=TRUE, stringsAsFactors = FALSE) # this is the trap data
seedTrait<-read.csv("../data/20120227_seedsMassForTraits.csv", header=TRUE, stringsAsFactors = FALSE) # these are traits of sp prodived by Joe
propDat <- read.csv("../data/proportionAbscised.csv", header=TRUE, stringsAsFactors = FALSE) # this is the prop abscised dataset you made, it's by year


seedTrait <- subset(seedTrait,select = c(SP4, SP6, FAMILY, GENUS, SPECIES, LIFEFORM))
seedDat <- dplyr::left_join(seedRain, seedTrait, by = c("sp" = "SP4"))

seedDat$fecha <- as.character(seedDat$fecha)
seedDat$fecha <- as.Date(seedDat$fecha, "%Y-%m-%d")
seedDat$year <- format(as.Date(seedDat$fecha), "%Y")
seedDat <- subset(seedDat, part==1|part==2|part==5)
seedDat <- subset(seedDat,LIFEFORM== "LIANA"|LIFEFORM== "MIDSTORY"|LIFEFORM== "SHRUB"|LIFEFORM== "TREE"|LIFEFORM=="UNDERSTORY")

seedDat.nYears <- seedDat %>% 
	group_by(sp) %>%
	summarise(n_years= n_distinct(year, na.rm = TRUE))%>%
	ungroup()

seedDat.sumParts <- seedDat %>% 
	group_by(part,sp) %>%
	summarise(quantity_sum= sum(quantity, na.rm=TRUE)) %>%
	spread(part, quantity_sum)%>%
	ungroup()

seedDat.nTraps <- seedDat %>% 
	group_by(sp) %>%
	summarise(n_traps= n_distinct(trap, na.rm = TRUE))%>%
	ungroup()

propDat.sp <- propDat %>% 
	group_by(sp) %>%
	summarise(proportion_abscised_median= median(proportion_abscised,na.rm=TRUE), total_seeds_sum= sum(total_seeds, na.rm=TRUE),proportion_abscised_mean= mean(proportion_abscised, na.rm=TRUE)) %>%
	ungroup()

fullSummary <- list(seedDat.sumParts, seedDat.nYears, seedDat.nTraps) %>% 
	reduce(full_join, by = "sp")

fullSummary2 <- left_join(fullSummary, propDat.sp, by= "sp")

seedDat.summary <- left_join(fullSummary2, seedTrait, by = c("sp" = "SP4"))

write.csv(seedDat.summary, "../results/summarizeSeedRain.csv")
