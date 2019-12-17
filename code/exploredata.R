#!/usr/bin/env Rscript
rm(list = ls())

#variable and function should be snake
#load packages

library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(ggpubr)
library(ggExtra)
theme_set(theme_bw())

seedTrait <- read.csv("../data/20120227_seedsMassForTraits.csv", header=TRUE, stringsAsFactors = FALSE)
tidyTraits <- read.csv("../data/TidyTrait.csv", header=TRUE, stringsAsFactors = FALSE)
seedRain<-read.table("../data/BCI_TRAP200_20190215_spcorrected.txt", header=TRUE, stringsAsFactors = FALSE)
propDat <- read.csv("../output/tables/proportionAbscised.csv", header=TRUE, stringsAsFactors = FALSE)
sumDat <- read.csv("../output/tables/summarizeSeedRain.csv", header=TRUE, stringsAsFactors = FALSE)


#convert fetcha to class Date
seedRain$fecha <- as.character(seedRain$fecha)
seedRain$fecha <- as.Date(seedRain$fecha, "%Y %m %d")

seeddat <- left_join(seedRain, seedTrait, by = c("sp" = "SP4"))

tbl_df(seeddat)

##########################################
# plotting

# plot abscised against viable
ggplot(data= propDat, aes(x=abscised_seeds, y=viable_seeds, colour= year)) +
	geom_point(position= position_dodge(3)) +
	facet_wrap(~sp) +
	scale_y_continuous(expand= c(0.05, 0.05))
#	geom_path()
ggsave("../results/abscised_vs_viable.png")

ggplot(data= propDat, aes(x=year, y=proportion_abscised, colour= sp)) +
	geom_point(position= position_dodge(1), alpha= 0.5) +
	facet_wrap(~sp) +
	scale_y_continuous(expand= c(0.01, 0.01))
ggsave("../results/prop_abscised_byyear.png")

ggplot(data= propDat, aes(x=viable_seeds, y=proportion_abscised, colour= sp)) +
	geom_point(position= position_dodge(3)) +
	scale_y_continuous(expand= c(0.05, 0.05))
ggsave("../results/abscised_vs_viable.png")

ggplot(data= propTraits, aes(x=reorder(sp, proportion_abscised), y=proportion_abscised)) +
	geom_boxplot(position= position_dodge(1), alpha= 0.5, outlier.colour = NULL, width=0.1) +
	scale_y_continuous(expand= c(0.05, 0.05))
ggsave("../results/prop_abscised.png")	

#############################################

# look at seasons

#############################################

seeddat$month <- format(as.Date(seeddat$fecha), "%m")

seeddat.s <- seeddat %>% 
		group_by(year,sp,month,part) %>%
		summarise(quantity= sum(quantity)) %>%
		ungroup()

ggplot(data= subset(seeddat.s, sp=="OCOS" & (part== 5| part== 1)), aes(x=month, y=quantity, fill= as.factor(part))) +
	geom_col(position = "stack") +
	facet_wrap(~year) +
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
	scale_y_continuous(expand= c(0.01, 0.01))
ggsave("../results/OCOS_parts_1_5_months.png")	

#############################################

# look at investigate relationship between proportion_abscised and abundance of fruit and trees

#############################################

#calculate total fruit each year and plot against proportion_abscised each year

propDat$total_seeds <- propDat$abscised_seeds + propDat$viable_seeds

propDat.g <- propDat %>% 
  group_by(year) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE)

ggplot(data= propDat.g, aes(x=total_seeds, y=proportion_abscised, colour= as.factor(year))) +
	geom_point(position= position_dodge(1), alpha=0.8, size=2.5) +
	scale_y_continuous(expand= c(0.01, 0.01)) +
	scale_x_continuous(expand= c(0.01, 0.01))
ggsave("../results/totalseeds_proportionabscised_fullDat.png")	

propDat.g.s <- propDat %>% 
  group_by(year, sp) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE)

ggplot(data= subset(propDat.g.s, total_seeds>1000), aes(x=total_seeds, y=proportion_abscised, colour= as.factor(year))) +
	geom_point(position= position_dodge(1), alpha=0.8, size=2.5) +
	scale_y_continuous(expand= c(0.01, 0.01)) +
	scale_x_continuous(expand= c(0.01, 0.01))
ggsave("../results/totalseeds_proportionabscised_sp.png")	

# tree abundance from BCI servey data

treeAbun <- read.csv("../data/bci_tree_abundance.csv", header=TRUE, stringsAsFactors = FALSE, check.names=FALSE)

treeAbun.m <- melt(treeAbun, id.vars=c("gs","genus","species","family","SP4","SP6"), measure.vars= c("1981","1985","1990","1995","2000","2005","2010","2015"), value.name= "abundance", variable.name="year")

treeAbun.y <- treeAbun.m %>% 
  group_by(year) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup()

propDat.g$year <- as.factor(propDat.g$year)

treeAbun.j <- left_join(treeAbun.y, propDat.g, by="year")

ggplot(data= na.omit(treeAbun.j), aes(x=abundance, y=total_seeds, colour= as.factor(year))) +
	geom_point(position= position_dodge(1), alpha=0.8, size=4) +
	scale_y_continuous(expand= c(0.01, 0.01)) +
	scale_x_continuous(expand= c(0.01, 0.01))
ggsave("../results/abundance_totalSeeds.png")	

#total tree abundance over years
ggplot(data= treeAbun.y, aes(x=year, y=abundance)) +
	geom_point(position= position_dodge(1), alpha=0.8, size=2.5) 

###### species level
propDat$year <- as.factor(propDat$year)
treeAbun.j.s <- left_join(propDat, treeAbun.m, by=c("year","sp"="SP4"))

ggplot(data= na.omit(treeAbun.j.s), aes(x=abundance, y=proportion_abscised, colour= as.factor(year))) +
	geom_point(position= position_dodge(1), alpha=0.8, size=3) +
	#facet_wrap(~sp) +
	scale_y_continuous(expand= c(0.01, 0.01)) +
	scale_x_continuous(expand= c(0.01, 0.01))
ggsave("../results/abundance_proportionabscised_sp.png")

#use boxplots to show variation of sp in each year
ggplot(data= na.omit(treeAbun.j.s), aes(x=abundance, y=proportion_abscised, colour= as.factor(year))) +
	geom_boxplot(width=0.1) +
	scale_y_continuous(expand= c(0.01, 0.01)) +
	scale_x_continuous(expand= c(0.01, 0.01))

ggplot(data= na.omit(treeAbun.j.s), aes(x=abundance, y=proportion_abscised, colour= as.factor(sp))) +
	geom_boxplot(varwidth=FALSE) +
	scale_y_continuous(expand= c(0.01, 0.01)) +
	scale_x_continuous(expand= c(0.01, 0.01))


ggscatterhist(
  na.omit(treeAbun.j.s), x = "abundance", y = "proportion_abscised",
  color = "sp", size = 3,
  group = "sp", palette = c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4'), shape=16,
  margin.plot = "boxplot",
  margin.params = list(fill = "sp", size = 0.2),
  ggtheme = theme_bw()
  )





