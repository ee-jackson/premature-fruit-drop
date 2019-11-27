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


seedTrait<-read.csv("../data/20120227_seedsMassForTraits.csv", header=TRUE, stringsAsFactors = FALSE)
tidyTraits <- read.csv("../data/TidyTrait.csv", header=TRUE, stringsAsFactors = FALSE)
propDat <- read.csv("../data/proportionAbscised.csv", header=TRUE, stringsAsFactors = FALSE)

#merge datasets

TraitDat <- dplyr::left_join(propDat, seedTrait, by = c("sp" = "SP4"), all.x=TRUE)

traitDat <- dplyr::left_join(TraitDat, tidyTraits, by = c("sp" = "Codigo"), all.x=TRUE)

#############################################

# look at traits in seedsMassForTraits data

#############################################

# DSPR_DRY – mean entire diaspore dry mass after drying at 60 C (g)
# hypothesise that smaller seeds can be produced in greater quantities

p1 <- ggplot(data= traitDat, aes(x=FRUIT_FRSH, y=proportion_abscised, colour= sp)) +
		geom_boxplot(position= position_dodge(1), alpha= 0.8, outlier.colour = NULL, width=0.5) +
		scale_y_continuous(expand= c(0.05, 0.05))

p2 <- ggplot(data= traitDat, aes(x=FRUIT_DRY, y=proportion_abscised, colour= sp)) +
		geom_boxplot(position= position_dodge(1), alpha= 0.8, outlier.colour = NULL, width=0.3) +
		scale_y_continuous(expand= c(0.05, 0.05))

p3 <- ggplot(data= traitDat, aes(x=SEED_FRESH, y=proportion_abscised, colour= sp)) +
		geom_boxplot(position= position_dodge(1), alpha= 0.8, outlier.colour = NULL, width=0.3) +
		scale_y_continuous(expand= c(0.05, 0.05))

p4 <- ggplot(data= traitDat, aes(x=SEED_DRY, y=proportion_abscised, colour= sp)) +
		geom_boxplot(position= position_dodge(1), alpha= 0.8, outlier.colour = NULL, width=0.2) +
		scale_y_continuous(expand= c(0.05, 0.05))


ggarrange(p1,p2,p3,p4, common.legend=TRUE) 
ggsave("../results/prop_traits1.png")	


p7 <- ggplot(data= traitDat, aes(x=DSPR_DRY, y=proportion_abscised, colour= sp)) +
		geom_boxplot(position= position_dodge(1), alpha= 0.8, outlier.colour = NULL, width=0.3) +
		scale_y_continuous(expand= c(0.05, 0.05))

p8 <- ggplot(data= traitDat, aes(x=DSPR_FRESH, y=proportion_abscised, colour= sp)) +
		geom_boxplot(position= position_dodge(1), alpha= 0.8, outlier.colour = NULL, width=0.3) +
		scale_y_continuous(expand= c(0.05, 0.05))

ggarrange(p7,p8, common.legend=TRUE)
ggsave("../results/prop_traits2.png")

#############################################

# look at traits in TidyTraits data

#############################################

# mean prop abscised

pd <- propDat %>% 
        group_by(sp) %>% 
        summarise(proportion_abscised_mean = mean(proportion_abscised))

traitDat.m <- dplyr::inner_join(pd, tidyTraits, by = c("sp" = "Codigo"), all.x=TRUE)

ggplot(data= traitDat.m, aes(x=HEIGHT_AVG, y=proportion_abscised_mean, colour=Lifeform)) +
	geom_point(alpha= 0.8, size=3) +
	scale_colour_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999'))+
	scale_y_continuous(expand= c(0.05, 0.05))

ggplot(data= subset(traitDat, Lifeform=="Tree"), aes(x=HEIGHT_AVG, y=proportion_abscised, colour=FAMILY, group=sp)) +
	geom_boxplot(alpha= 0.8, width=0.5, varwidth=FALSE,position =  position_dodge2(width = 0.75, preserve = "single")) +
	scale_y_continuous(expand= c(0.05, 0.05)) 
ggsave("../results/HEIGHT_AVG_Tree.png")

ggplot(data= traitDat.m, aes(x=HEIGHT_AVG, y=proportion_abscised_mean, colour=Lifeform, group=sp)) +
	geom_point(alpha= 0.8, size=3) +
	facet_wrap(~Lifeform)+
	scale_y_continuous(expand= c(0.05, 0.05))
ggsave("../results/HEIGHT_AVG_Tree.png")

ggplot(data= subset(traitDat.m, Lifeform=="Understory"), aes(x=Endocarp_investment, y=proportion_abscised_mean, colour=Lifeform)) +
	geom_point(alpha= 0.8, size=3) +
	scale_colour_manual(values=c('#f781bf'))+
	scale_y_continuous(expand= c(0.05, 0.05))
ggsave("../results/endocarpInvestment_understory.png")

ggplot(data= traitDat.m, aes(x=Endocarp_investment, y=proportion_abscised_mean, colour=Lifeform)) +
	geom_point(alpha= 0.8, size=3) +
	facet_wrap(~Lifeform)+
	scale_colour_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999'))+
	scale_y_continuous(expand= c(0.05, 0.05), limits = c(0,1))
ggsave("../results/endocarpInvestment.png")

ggplot(data= subset(traitDat, Lifeform=="Understory"|Lifeform=="Midstory"), aes(x=as.factor(CoFruit), y=proportion_abscised, colour=Lifeform, group=sp)) +
	geom_boxplot(alpha= 0.8, width=1) +
	facet_wrap(~Lifeform)+
	scale_colour_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999'))+
	scale_y_continuous(expand= c(0.05, 0.05), limits = c(0,1))

ggplot(data= subset(traitDat, FAMILY=="Meliaceae"), aes(x=CoFruit, y=proportion_abscised, colour=sp, group=sp)) +
	geom_boxplot(alpha= 0.8, width=1) +
	facet_wrap(~FAMILY)+
	scale_y_continuous(expand= c(0.05, 0.05), limits = c(0,1))

ggplot(data= subset(traitDat.m, Lifeform=="Tree"), aes(x=Endocarp_investment, y=proportion_abscised_mean, colour=FAMILY, group=sp)) +
	geom_point(alpha= 0.8, size=3) +
	scale_y_continuous(expand= c(0.05, 0.05))

ggplot(data= subset(traitDat.m, Lifeform=="Tree"), aes(x=CoFruit, y=proportion_abscised_mean, colour=FAMILY, group=sp)) +
	geom_point(alpha= 0.8, size=3) +
	scale_y_continuous(expand= c(0.05, 0.05))

ggplot(data= traitDat.m, aes(x=BCIReproductive, y=proportion_abscised_mean, group=sp, colour=Lifeform)) +
	geom_point(alpha= 0.8, size=2) +
	facet_wrap(~Lifeform)+
	scale_y_continuous(expand= c(0.05, 0.05)) 
ggsave("../results/CoFruit_Lifeform.png")

ggplot(data= traitDat.m, aes(x=Endocarp_investment, y=proportion_abscised_mean, group=sp)) +
	geom_point(alpha= 0.8, size=2) +
	scale_y_continuous(expand= c(0.05, 0.05)) 
ggsave("../results/CoFruit_allSp.png")

#---------------------------

seedDat <- subset(TraitDat,LIFEFORM== "LIANA"|LIFEFORM== "MIDSTORY"|LIFEFORM== "SHRUB"|LIFEFORM== "TREE"|LIFEFORM=="UNDERSTORY")

seedDat <- subset(seedDat,total_seeds>500)

ggplot(data= seedDat, aes(x=sp, y=proportion_abscised)) +
	geom_boxplot(alpha= 0.8, width=1) +
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
	scale_y_continuous(expand= c(0.05, 0.05), limits = c(0,1))
ggsave("../results/prop_abscised_greaterThan500.png")

library(randomcoloR)
pal <- distinctColorPalette(32)
pie(rep(1, 32), col=pal)

ggplot(data= seedDat, aes(x=FAMILY, y=proportion_abscised, group=sp,fill=as.factor(FAMILY))) +
	geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single")) +
	scale_fill_manual(values=pal)+
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position = "none")
ggsave("../results/prop_abscised_greaterThan500_sp.png")

ggplot(data= subset(seedDat, total_seeds>500), aes(x=total_seeds, y=proportion_abscised, colour= as.factor(sp))) +
	geom_point(position= position_dodge(1), alpha=0.8, size=1) +
	facet_wrap(~year) +
	theme(legend.position = "none")
ggsave("../results/propAbscised_totalSeeds_greaterThan500.png")

ggplot(data= subset(traitDat, total_seeds>500), aes(x=BCIReproductive, y=proportion_abscised, colour= as.factor(sp))) +
	geom_point(position= position_dodge(1), alpha=0.8, size=1) +
	facet_wrap(~year) +
	theme(legend.position = "none")
ggsave("../results/propAbscised_BCIrepro_greaterThan500.png")

give.yr <- function(x){
   return(c(y = -0.1, label = length(x)))
}

ggplot(data= seedDat, aes(x=FAMILY, y=proportion_abscised, group=sp,fill=as.factor(FAMILY))) +
	geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single")) +
	scale_fill_manual(values=pal)+
	stat_summary(fun.data = give.yr, geom = "text", size=3, position=position_nudge(x = 2, y = 0))+
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position = "none")





