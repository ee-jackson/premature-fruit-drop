#!/usr/bin/env Rscript
rm(list = ls())

#load packages
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(ggpubr)
library(ggExtra)
library(ggrepel)
theme_set(theme_bw())

disperseDat <- read.csv("../data/species_20070228_DispersalModes.csv", header=TRUE, stringsAsFactors = FALSE)

disperseDat <- subset(disperseDat,select = c(sp, dsp_ant, dsp_bat, dsp_bird, dsp_bbird, dsp_explo, dsp_mam, dsp_water, dsp_wind))

disperseDat.m <- melt(disperseDat, id.vars="sp")
disperseDat.m <- subset(disperseDat.m, value==TRUE)

disperseDat.f <- disperseDat.m %>% 
	add_count(sp, sort=TRUE) %>%
	glimpse() %>%
	rename(dispersal_mode = variable) %>%
	mutate(bio = ifelse(dispersal_mode=="dsp_explo"|dispersal_mode=="dsp_wind"|dispersal_mode=="dsp_water", "abiotic", "biotic")) %>%
	subset(select = c(sp,dispersal_mode, n, bio))

summ <- read.csv("../output/tables/summarizeSeedRain.csv", header=TRUE, stringsAsFactors = FALSE)

disperse <- inner_join(disperseDat.f, summ, by = c("sp" = "sp"))

write.csv(disperse, "../output/tables/dispersalMode.csv")




# ggplot(data=subset(disperse, total_seeds>100), aes(x=proportion_abscised, fill=as.factor(n))) +
# 			geom_histogram(binwidth=.05, position="stack", colour="black", alpha=0.8) + 
# 			facet_wrap(~dispersal_mode) 

# ggsave("../output/plots/20191126/dispersal_mode_histogram_duplicates_greater100.png")
