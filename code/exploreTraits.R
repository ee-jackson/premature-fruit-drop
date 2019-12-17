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


seedTrait <- read.csv("../data/20120227_seedsMassForTraits.csv", header=TRUE, stringsAsFactors = FALSE)
tidyTraits <- read.csv("../data/TidyTrait.csv", header=TRUE, stringsAsFactors = FALSE)
seedRain<-read.table("../data/BCI_TRAP200_20190215_spcorrected.txt", header=TRUE, stringsAsFactors = FALSE)
propDat <- read.csv("../output/tables/proportionAbscised.csv", header=TRUE, stringsAsFactors = FALSE)
sumDat <- read.csv("../output/tables/summarizeSeedRain.csv", header=TRUE, stringsAsFactors = FALSE)
disDat <- read.csv("../output/tables/dispersalMode.csv", header=TRUE, stringsAsFactors = FALSE)

#merge datasets

traitDat <- dplyr::left_join(sumDat, tidyTraits, by = c("sp" = "Codigo"), all.x=TRUE)

traitSum <- dplyr::left_join(disDat, seedTrait, by = c("sp" = "SP4"), all.x=TRUE)

traitDat <- dplyr::left_join(disDat, tidyTraits, by = c("sp" = "Codigo"), all.x=TRUE)

tidyProp <- dplyr::left_join(propDat, tidyTraits, by = c("sp" = "Codigo"), all.x=TRUE)

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

#seedDat <- subset(seedDat,total_seeds>500)

ggplot(data= subset(seedDat,total_seeds>500), aes(x=sp, y=proportion_abscised)) +
	geom_boxplot(alpha= 0.8, width=1) +
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
	scale_y_continuous(expand= c(0.05, 0.05), limits = c(0,1))
ggsave("../output/plots/prop_abscised_greaterThan500.png")



ggplot(data= sumDat, aes(x=FAMILY, y=proportion_abscised, fill=LIFEFORM)) +
	geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single")) +
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position = "none") +
	geom_hline(yintercept=0.5, linetype="dashed", color = "red", size=0.5)
ggsave("../../notable/attachments/20191126/prop_abscised_fam.png")



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

ggplot(data=traitSum, aes(x=log(BCIReproductive), y=proportion_abscised)) +
	geom_point(alpha=0.8, size=1) +
	theme(legend.position = "none")
ggsave("../output/plots/propAbscised_BCIrepro.png")


ggplot(data=subset(tidyProp, year==2010), aes(x=log(BCIReproductive), y=proportion_abscised)) +
	geom_point(alpha=0.8, size=1) +
	theme(legend.position = "none")
ggsave("../../notable/attachments/20191126/propAbscised_BCIrepro_2010.png")



p1 <- ggplot(data=traitSum, aes(x=log(SEED_DRY), y=proportion_abscised)) +
			geom_point(alpha=0.8, size=1) +
			theme(legend.position = "none")

p2 <- ggplot(data=traitSum, aes(x=log(SEED_FRESH), y=proportion_abscised)) +
			geom_point(alpha=0.8, size=1) +
			theme(legend.position = "none")

p3 <- ggplot(data=traitSum, aes(x=log(FRUIT_DRY), y=proportion_abscised)) +
			geom_point(alpha=0.8, size=1) +
			theme(legend.position = "none")

p4 <- ggplot(data=(traitSum), aes(x=log(FRUIT_FRSH), y=proportion_abscised)) +
			geom_point(alpha=0.8, size=1) +
			theme(legend.position = "none")

ggarrange(p1,p2,p3,p4)
ggsave("../../notable/attachments/20191126/propAbscised_seedmass.png")

p5 <- ggplot(data=traitSum, aes(x=log(DSPR_DRY), y=proportion_abscised)) +
			geom_point(alpha=0.8, size=1) +
			theme(legend.position = "none")

p6 <- ggplot(data=(traitSum), aes(x=log(DSPR_FRESH), y=proportion_abscised)) +
			geom_point(alpha=0.8, size=1) +
			theme(legend.position = "none")
ggarrange(p5,p6)
ggsave("../../notable/attachments/20191126/propAbscised_diasporemass.png")

ggplot(data= seedDat, aes(x=FAMILY, y=proportion_abscised, group=sp,fill=as.factor(FAMILY))) +
	geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single")) +
	scale_fill_manual(values=pal)+
	stat_summary(fun.data = give.yr, geom = "text", size=3, position=position_nudge(x = 2, y = 0))+
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position = "none")

ggplot(data= sumDat, aes(x=proportion_abscised)) +
	geom_histogram(stat = "bin",
  position = "stack", binwidth = 0.1, bins = NULL,
  na.rm = TRUE, show.legend = NA, inherit.aes = TRUE) +
	scale_x_continuous(expand= c(0, 0),limits=c(0,1)) 

# Histogram overlaid with kernel density curve
ggplot(subset(sumDat, total_seeds>100), aes(x=proportion_abscised)) + 
    geom_histogram(aes(y=..density..), binwidth=.05, colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(proportion_abscised, na.rm=TRUE)), color="red", linetype="dashed", size=1)
ggsave("../../notable/attachments/20191126/propAbscised_histo_density_greaterthan100.png")

ggplot(subset(sumDat, total_seeds>100), aes(x=proportion_abscised, fill=LIFEFORM)) + 
	geom_density(alpha=.3)
ggsave("../../notable/attachments/20191126/propAbscised_density_greaterthan100.png")

p2<-ggplot(subset(sumDat, total_seeds>10), aes(x=proportion_abscised)) + 
		geom_histogram(binwidth=.05, colour="black", fill="white") + 
	    facet_wrap(LIFEFORM ~ .)
ggsave("../../notable/attachments/20191126/propAbscised_histo_lifeform.png")

# subset the data
p3<-ggplot(subset(sumDat, total_seeds>10), aes(x=proportion_abscised)) + 
	    geom_histogram(aes(y=..density..), binwidth=.05, colour="black", fill="white") +
	    geom_density(alpha=.2, fill="#FF6666") +
    facet_wrap(LIFEFORM ~ .)
ggsave("../../notable/attachments/20191126/propAbscised_histo_density_lifeform_greaterthan100.png")

ggplot(subset(sumDat, total_seeds>100), aes(x=proportion_abscised)) + 
    geom_histogram(aes(y=..density..), binwidth=.05, colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    facet_wrap(LIFEFORM ~ .)
ggsave("../../notable/attachments/20191126/propAbscised_histo_density_lifeform_greaterthan100.png")


sumDat$LIFEFORM <- factor(sumDat$LIFEFORM, levels = c("LIANA", "SHRUB", "UNDERSTORY", "MIDSTORY", "TREE"))

p1<-ggplot(subset(sumDat, total_seeds>10), aes(y=proportion_abscised, x=LIFEFORM)) + 
		geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single"))

ggarrange(p1,p2,p3)
ggsave("../output/plots/20191203/lifeform_abscision_greaterthan50seeds.png")

############# DISDAT

ggplot(disDat, aes(x=proportion_abscised)) + 
    geom_histogram(aes(y=..density..), binwidth=.05, colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    facet_wrap(dispersal_mode ~ .)

ggplot(disDat, aes(x=proportion_abscised, fill=dispersal_mode)) + 
	geom_density(alpha=.3)

ggplot(disDat, aes(x=proportion_abscised)) + 
	geom_histogram(binwidth=.05, colour="black", fill="white") + 
    facet_wrap(dispersal_mode ~ .)
ggsave("../output/plots/dispersal_mode_histogram.png")

ggplot(subset(disDat, total_seeds>100), aes(y=proportion_abscised, x=bio)) + 
		geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single"))

ggplot(subset(disDat, total_seeds>100), aes(y=proportion_abscised, x=dispersal_mode, fill=bio)) + 
		geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single"))

ggplot(subset(disDat, total_seeds>50), aes(y=proportion_abscised, x=bio)) + 
		geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single"), width=0.4) +
		geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.5) 
ggsave("../output/plots/20191203/dispersalbio_abscision_greaterthan50seeds.png")

ggplot(subset(disDat, total_seeds>50), aes(x=proportion_abscised, fill=bio)) +
	geom_density(alpha=.3)
ggsave("../output/plots/20191203/dispersalbio_abscision_density_greaterthan50seeds.png")

######################## TRAITDAT

traitDat %>%
	subset(total_seeds>50) %>%
	select(c("sp", "Coleo_pres", "Hymeno_pres", "Lepid_pres", "proportion_abscised")) %>%
	melt(id.vars=c("sp", "proportion_abscised"), measure.vars=c("Coleo_pres", "Hymeno_pres", "Lepid_pres")) %>%
	rename(predator=variable, presence=value)-> presDat

ggplot(na.omit(presDat), aes(y=proportion_abscised, group=presence, x=presence)) + 
		geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single"), width=0.4)+
		facet_wrap(~predator) +
		geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.5) +
		scale_x_continuous(breaks=c(0,1), labels=c(0,1), limits=c(-0.5,1.5))
ggsave("../output/plots/20191203/pres_abscision_greaterthan50seeds.png")
