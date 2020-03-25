#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: compareProportionAbscised.R
## Desc: compare mean proportion abscised with a wieghted mean 
## Date: January 2020

rm(list = ls())

#load packages
require(tidyverse)
require(ggpubr)
require(reshape2)

theme_set(theme_bw())

sumDat <- read_csv("../output/tables/summarizeSeedRain.csv")
tidyTraits <-read.csv("../data/../data/TidyTrait.csv")

seedDat <- dplyr::left_join(sumDat, tidyTraits, by = c("sp" = "Codigo"), all.x=TRUE)

seedDat.m <- subset(seedDat, n_years=!1) %>%
	mutate(diff = proportion_abscised - proportion_abscised_w) #%>%
	#mutate(diff= abs(diff))

ggplot(dat=seedDat.m, aes(x=sp, y=diff, fill= LIFEFORM)) +
	geom_col() +
	theme(axis.text.y=element_blank())+
	coord_flip() 
ggsave("../output/plots/20200107/diffInMeans_sp.png")

p1 <- ggplot(dat=seedDat.m, aes(x=n_years, y=diff)) +
	geom_jitter(position=position_jitter(0.4), alpha=0.5, size=1)

p2 <- ggplot(dat=seedDat.m, aes(x=n_traps, y=diff)) +
	geom_jitter(position=position_jitter(0.2), alpha=0.5, size=1)

p3 <- ggplot(dat=subset(seedDat.m, total_seeds<1000), aes(x=total_seeds, y=diff)) +
	geom_jitter(position=position_jitter(0.2), alpha=0.5, size=1)

ggarrange(p1,p2, p3)
ggsave("../output/plots/20200107/diffInMeans_corr.png")