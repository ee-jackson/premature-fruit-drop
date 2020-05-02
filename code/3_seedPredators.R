#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: seedPredators.R
## Desc: exploring relationship between predation and abscission
## Date: March 2020

rm(list = ls())
require(tidyverse)
require(reshape2)
require(ggExtra)
require(ggpubr)
theme_set(theme_pubr(base_size = 40))

# read dataset

fruitDat <- read.csv("../output/tables/cleanData.csv")

# Create datasets where presence/absence data are melted #

# for different predator orders


fruitDat %>%
	select(c("sp", "hymeno_pres", "lepid_pres", "coleo_pres", "proportion_abscised")) %>%
	rename(coleoptera=coleo_pres, hymenoptera = hymeno_pres, lepidoptera = lepid_pres) %>%
	melt(id.vars=c("sp", "proportion_abscised"), measure.vars=c("coleoptera", "hymenoptera", "lepidoptera")) %>%
	rename(predator=variable, presence=value) -> presDatO
	#drop_na() %>%
	#mutate(presence = factor(ifelse(0, "No", "Yes"))) 
	
# for presence/absence of any predator
fruitDat %>%
	select(c("sp", "seedpred_pres", "proportion_abscised")) %>%
	melt(id.vars=c("sp", "proportion_abscised"), measure.vars=c("seedpred_pres")) %>%
	rename(predator=variable, presence=value) -> presDatA


# number of predators

fruitDat %>%
	select(c("sp", "seedpred_n", "proportion_abscised")) %>%
	melt(id.vars=c("sp", "proportion_abscised"), measure.vars=c("seedpred_n")) %>%
	rename(predator=variable, predator_n=value) -> presDatN

# plot

Pal <- c("#E69F00", "#56B4E9", "#009E73",  "#D55E00", "#CC79A7")

ggplot(na.omit(presDatO), aes(y=proportion_abscised, group=presence, x=presence)) + 
	geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single"), width=0.4, size=1)+
	facet_wrap(~predator) +
	geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.5, size=4) +
	scale_x_continuous(breaks=c(0,1), labels=c(0,1), limits=c(-0.5,1.5)) +
	theme(legend.position= "none") +
	xlab("Presence of seed predator") +
	ylab("Proportion of seeds abscised") ->p2

ggplot(na.omit(presDatA), aes(y=proportion_abscised, group=presence, x=presence)) + 
	geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single"), width=0.4, size=1)+
	geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.5, size=4) +
	scale_x_continuous(breaks=c(0,1), labels=c(0,1), limits=c(-0.5,1.5)) +
	ylab("Proportion of seeds abscised") +
	xlab("Presence of seed predator") -> p1

ggarrange(p1,p2,labels = c("A", "B"), font.label= c(size=50))
ggsave("../output/plots/prematureFruitDrop/predPres.png", dpi="retina")

# Life forms # 
Pal <- c("Tree"="#E69F00", "Shrub" = "#56B4E9", "Liana" = "#009E73", "Understory" = "#D55E00", "Midstory" = "#CC79A7")

fruitDat %>%
	select(c("sp", "seedpred_pres", "proportion_abscised", "lifeform")) %>%
	melt(id.vars=c("sp", "proportion_abscised", "lifeform"), measure.vars=c("seedpred_pres")) %>%
	rename(predator=variable, presence=value) -> presDatL

ggplot(na.omit(presDatL), aes(y=proportion_abscised, group=presence, x=presence)) + 
	geom_boxplot(alpha= 0.2, position = position_dodge2(preserve = "single"), width=0.2, aes(fill = lifeform))+
	facet_grid(.~lifeform) +
	scale_fill_manual(values=Pal) +
	geom_jitter(shape=16, size=4, position=position_jitter(0.1),aes(colour=lifeform)) +
	scale_fill_manual(values=Pal) +
	scale_colour_manual(values=Pal) +
	scale_x_continuous(breaks=c(0,1), labels=c(0,1), limits=c(-0.5,1.5))+
	ylab("Proportion of seeds abscised") +
	xlab("Presence of seed predator") +
	theme(legend.position = "none")
ggsave("../output/plots/prematureFruitDrop/predPresLifeform.png", dpi="retina")

### are they sig different?
presDatA$presence <-as.factor(presDatA$presence)

na.omit(presDatA) %>%
	select(c("presence", "proportion_abscised","sp")) %>%
	melt(id.vars=c("proportion_abscised","sp"), measure.vars=c("presence")) ->presDatA2

group_by(presDatA, presence) %>%
  summarise(
    count = n(),
    mean = mean(proportion_abscised, na.rm = TRUE),
    sd = sd(proportion_abscised, na.rm = TRUE)
	)

presDatA %>%
	select(c("proportion_abscised", "presence", "sp")) -> presDatA2

subset(presDatA2, presence==1) -> yes
subset(presDatA2, presence==0) -> no

t.test(yes$proportion_abscised, no$proportion_abscised, alternative = "two.sided", var.equal = TRUE)

wilcox.test(yes$proportion_abscised, no$proportion_abscised)

wilcox.test(proportion_abscised ~ presence, data = presDatA,
        exact = FALSE, alternative = "less")

## GLM
predlm <- glm(cbind(abscised_seeds,viable_seeds) ~ seedpredationrate, family = binomial(logit), data = fruitDat)

summary(predlm)

with(summary(predlm), 1 - deviance/null.deviance) # calculate R squared

# plot

rsq_label <- paste('italic(R)^2 == ', round(with(summary(predlm), 1 - deviance/null.deviance), 2))

ggplot() +
	geom_point(aes(x = na.omit(fruitDat$seedpredationrate), y = fitted(predlm), colour="red"), size=4) +
	geom_point(aes(x = fruitDat$seedpredationrate, y=fruitDat$proportion_abscised), size=4)+
	scale_y_continuous(expand= c(0, 0)) +
	scale_x_continuous(expand= c(0, 0)) +
	ylab("Proportion of seeds abscised") +
	xlab("Rate of seed predation") +
	theme(legend.position="none") +
	annotate("text", x=0.4,y=0.85,label=rsq_label, parse=TRUE, size=15) +
	annotate("text", x=0.4,y=0.9,label="p = < 0.0001", size=15)

ggsave("../output/plots/prematureFruitDrop/glm.png", dpi="retina")

"R^2 == ", round(summary(predlm), 1 - (deviance/null.deviance), 2)

"italic(R) ^ 2 == 0.07"

ggplot(fruitDat, aes(x=seedpredationrate, y=proportion_abscised)) +
	geom_point(size=5) +
	geom_smooth(method="glm", se=FALSE)+
	scale_y_continuous(expand= c(0, 0)) +
	scale_x_continuous(expand= c(0, 0)) 
ggsave("../output/plots/prematureFruitDrop/predRate.png", dpi="retina")

ggplot(fruitDat, aes(x=seedpredationrate, y=proportion_abscised, colour=lifeform)) +
	geom_point(size=5, alpha=0.7) +
	geom_smooth(method="glm", se=FALSE, size=2)+
	scale_colour_manual(values=Pal) +
	scale_y_continuous(expand= c(0, 0)) +
	scale_x_continuous(expand= c(0, 0))
ggsave("../output/plots/prematureFruitDrop/predRateLifeform.png", dpi="retina") 







