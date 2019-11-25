#!/usr/bin/env Rscript
rm(list = ls())

#load packages
library(plyr)
library(dplyr)
library(reshape2)
library(data.table)

seedRain<-read.table("../data/BCI_TRAP200_20190215_spcorrected.txt", header=TRUE, stringsAsFactors = FALSE)

seedTrait<-read.csv("../data/20120227_seedsMassForTraits.csv", header=TRUE, stringsAsFactors = FALSE)

#convert fetcha to class Date
seedRain$fecha <- as.character(seedRain$fecha)
seedRain$fecha <- as.Date(seedRain$fecha, "%Y-%m-%d")

seedDat <- left_join(seedRain, seedTrait, by = c("sp" = "SP4"))
seedDat$year <- format(as.Date(seedDat$fecha), "%Y")

tbl_df(seedDat)

#############################################

# create a df that sums quantity by year, sp and part
absdat <- seedDat %>% 
	group_by(year,part,sp, N_SEEDFULL) %>%
	summarise(quantity_sum= sum(quantity)) %>%
	ungroup()

# Viable seeds

# subset to part1 + part 2
absdat.v <- subset(absdat, part== 1 | part==2)

# multiply quantity of mature fruits by average seeds per fruit
for(i in 1:length(absdat.v$part)) {
	if (i == 1) {
		absdat.v$quantity_sum2 <- absdat.v$quantity_sum* absdat.v$N_SEEDFULL
	}
	else {
		absdat.v$quantity_sum2 <- absdat.v$quantity_sum
	}
}

absdat.v <- absdat.v %>% 
		group_by(year,sp) %>%
		summarise(viable_seeds= sum(quantity_sum2)) %>%
		ungroup()

###############################################

# Abscised seeds

absdat.a <- subset(absdat, part== 5) %>% 
	group_by(year,sp) %>%
	summarise(abscised_seeds= sum(quantity_sum)) %>%
	ungroup() 

# join and calculate proportion abcised
propdat<- full_join(absdat.a, absdat.v, by= c("year", "sp"))

propdat$proportion_abscised <- propdat$abscised_seeds/(propdat$viable_seeds+propdat$abscised_seeds)

propdat$total_seeds <- propdat$abscised_seeds + propdat$viable_seeds

write.csv(propdat,"../data/proportionAbscised.csv")

