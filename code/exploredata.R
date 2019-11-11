#!/usr/bin/env Rscript
rm(list=ls())

#load packages
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(ggpubr)
theme_set(theme_bw())

seedTrapDat<-read.csv("../data/20140218_Lauracea_200trap.csv", header=TRUE, stringsAsFactors=FALSE)
seedTraitDat<-read.csv("../data/20120227_seedsMassForTraits.csv", header=TRUE, stringsAsFactors=FALSE)

#convert fetcha to class Date
seedTrapDat$fecha <- as.character(seedTrapDat$fecha)
seedTrapDat$fecha <- as.Date(seedTrapDat$fecha, "%Y %m %d")

seeddat <- left_join(seedTrapDat, seedTraitDat, by = c("sp" = "SP4"))

tbl_df(seeddat)


#############################################

# look at the proportion of fruit that is abscised

#############################################

# create a df that sums quantity by year, sp and part
absdat <- seeddat %>% 
	group_by(year,part,sp, N_SEEDFULL) %>%
	summarise(quantity_sum= sum(quantity))

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
propdat<- left_join(absdat.a, absdat.v, by= c("year", "sp"))

propdat$proportion_abscised <- propdat$abscised_seeds/(propdat$viable_seeds+propdat$abscised_seeds)

##########################################
# plotting

# plot abscised against viable
ggplot(data= propdat, aes(x=abscised_seeds, y=viable_seeds, colour= year)) +
	geom_point(position= position_dodge(3)) +
	facet_wrap(~sp) +
	scale_y_continuous(expand= c(0.05, 0.05))
#	geom_path()
ggsave("../results/abscised_vs_viable.png")

ggplot(data= propdat, aes(x=year, y=proportion_abscised, colour= sp)) +
	geom_point(position= position_dodge(1), alpha= 0.5) +
	facet_wrap(~sp) +
	scale_y_continuous(expand= c(0.01, 0.01))
ggsave("../results/prop_abscised_byyear.png")

ggplot(data= propdat, aes(x=viable_seeds, y=proportion_abscised, colour= sp)) +
	geom_point(position= position_dodge(3)) +
	scale_y_continuous(expand= c(0.05, 0.05))
ggsave("../results/abscised_vs_viable.png")

ggplot(data= propTraits, aes(x=reorder(sp, proportion_abscised), y=proportion_abscised)) +
	geom_boxplot(position= position_dodge(1), alpha= 0.5, outlier.colour = NULL, width=0.1) +
	scale_y_continuous(expand= c(0.05, 0.05))
ggsave("../results/prop_abscised.png")	

#############################################

# look at other traits

#############################################
colnames(propTraits)
colnames(seedTraitDat)

propTraits <- dplyr::inner_join(propdat, seedTraitDat, by = c("sp" = "SP4"), all.x=TRUE)

# DSPR_DRY – mean entire diaspore dry mass after drying at 60 C (g)
# hypothesise that smaller seeds can be produced in greater quantities

p1 <- ggplot(data= propTraits, aes(x=FRUIT_FRSH, y=proportion_abscised, colour= sp)) +
		geom_boxplot(position= position_dodge(1), alpha= 0.8, outlier.colour = NULL, width=0.5) +
		scale_y_continuous(expand= c(0.05, 0.05))

p2 <- ggplot(data= propTraits, aes(x=FRUIT_DRY, y=proportion_abscised, colour= sp)) +
		geom_boxplot(position= position_dodge(1), alpha= 0.8, outlier.colour = NULL, width=0.3) +
		scale_y_continuous(expand= c(0.05, 0.05))

p3 <- ggplot(data= propTraits, aes(x=SEED_FRESH, y=proportion_abscised, colour= sp)) +
		geom_boxplot(position= position_dodge(1), alpha= 0.8, outlier.colour = NULL, width=0.3) +
		scale_y_continuous(expand= c(0.05, 0.05))

p4 <- ggplot(data= propTraits, aes(x=SEED_DRY, y=proportion_abscised, colour= sp)) +
		geom_boxplot(position= position_dodge(1), alpha= 0.8, outlier.colour = NULL, width=0.2) +
		scale_y_continuous(expand= c(0.05, 0.05))


ggarrange(p1,p2,p3,p4, common.legend=TRUE) 
ggsave("../results/prop_traits1.png")	


p7 <- ggplot(data= propTraits, aes(x=DSPR_DRY, y=proportion_abscised, colour= sp)) +
		geom_boxplot(position= position_dodge(1), alpha= 0.8, outlier.colour = NULL, width=0.3) +
		scale_y_continuous(expand= c(0.05, 0.05))

p8 <- ggplot(data= propTraits, aes(x=DSPR_FRESH, y=proportion_abscised, colour= sp)) +
		geom_boxplot(position= position_dodge(1), alpha= 0.8, outlier.colour = NULL, width=0.3) +
		scale_y_continuous(expand= c(0.05, 0.05))

ggarrange(p7,p8, common.legend=TRUE)
ggsave("../results/prop_traits2.png")	 

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

