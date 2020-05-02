#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: prematureFruitDrop.R
## Desc: descriptive plots of premature fruit drop at BCI
## Date: March 2020

rm(list = ls())
require(tidyverse)
require(reshape2)
require(ggpubr)
theme_set(theme_pubr(base_size = 60))

# read dataset

fruitDat <- read.csv("../output/tables/cleanData.csv")

#histogram of proportion abscised for all species


ggplot(fruitDat, aes(x=proportion_abscised)) + 
    geom_histogram(aes(y=..density..), binwidth=.05, colour="black", fill="white", size=1, boundary=0) +
    geom_density(alpha=.2, fill="#FF6666", bw=.05) +
	scale_y_continuous(expand= c(0, 0)) +
	scale_x_continuous(expand= c(0, 0)) +
	xlab("Proportion of seeds abscised")+
	theme_bw(base_size = 60)
ggsave("../output/plots/prematureFruitDrop/overallDensity.png", dpi="print")

ggplot(fruitDat, aes(x=proportion_abscised)) + 
    geom_histogram(aes(y=..count..), binwidth=.05, colour="black", fill="#FF6666", alpha=.2, size=1, boundary=0) +
	scale_y_continuous(expand= c(0, 0),minor_breaks=seq(1,50,1), breaks=seq(5,50,5)) +
	scale_x_continuous(expand= c(0, 0)) +
	xlab("Proportion of seeds abscised") +
	ylab("Count of plant species") +
	theme_bw(base_size = 60)
ggsave("../output/plots/prematureFruitDrop/overallHist.png", dpi="print")


# by lifeform

Pal <- c("Tree"="#E69F00", "Shrub" = "#56B4E9", "Liana" = "#009E73", "Understory" = "#D55E00", "Midstory" = "#CC79A7")

p1 <- ggplot(fruitDat, aes(x=proportion_abscised)) + 
		geom_histogram(aes(y=..density..), binwidth=.05, colour="black", fill="white", size=1, boundary=0) +
		geom_density(alpha=.2, aes(fill=lifeform), bw=.05) +
		facet_wrap(lifeform ~ .) +
		scale_fill_manual(values=Pal) +
		scale_y_continuous(expand= c(0, 0)) +
		scale_x_continuous(expand= c(0, 0)) +
		theme(legend.position = "none") +
		xlab("Proportion of seeds abscised")

p2 <- ggplot(fruitDat, aes(y=proportion_abscised, x=reorder(lifeform, proportion_abscised))) + 
		geom_boxplot(position = position_dodge2(preserve = "single"),alpha=0.2,aes(fill=lifeform)) +
		geom_jitter(shape=16, size=4, position=position_jitter(0.2),aes(colour=lifeform)) +
		scale_fill_manual(values=Pal) +
		scale_colour_manual(values=Pal) +
		theme(legend.position = "none") +
		xlab("") +
		ylab("Proportion of seeds abscised")

p3 <- ggplot(fruitDat, aes(x=proportion_abscised)) + 
		geom_histogram(aes(y=..count..,fill=lifeform), alpha=0.2, binwidth=.05, colour="black", size=1, boundary=0) +
		facet_wrap(lifeform ~ .) +
		scale_fill_manual(values=Pal) +
		scale_y_continuous(expand= c(0, 0), minor_breaks=seq(1,20,1), breaks=seq(2,20,2)) +
		scale_x_continuous(expand= c(0, 0)) +
		theme(legend.position = "none") +
		xlab("Proportion of seeds abscised")


ggarrange(p1,p2, labels = c("A", "B"), font.label= c(size=50))

ggarrange(p3,p2, labels = c("A", "B"), font.label= c(size=50))
ggsave("../output/plots/prematureFruitDrop/lifeformHist.png")

### TRAITS
# cvseed, cofruit, bcireproductive, endocarp_investment, height_avg, seed_dry

#remove outliers

fruitDat$bcireproductive <- ifelse(fruitDat$bcireproductive > 500, NA, fruitDat$bcireproductive)

fruitDat$cofruit <- ifelse(fruitDat$cofruit < 100, NA, fruitDat$cofruit)

#log these two 

fruitDat$seed_dry_log <- log10(fruitDat$seed_dry)
fruitDat$cofruit_log <- log10(fruitDat$cofruit)

fruitTraits <- melt(fruitDat, id.vars = c("sp", "proportion_abscised"), measure.vars = c( "height_avg","cvseed", "cofruit_log", "bcireproductive", "endocarp_investment",  "seed_dry_log"))

labs <- c(height_avg = "Tree height (m)", cvseed = "Interannual crop size variation", cofruit_log = "Overlap in fruit production (log10)", bcireproductive = "Local Abundance", endocarp_investment = "Endocarp investment", seed_dry_log = "Seed dry mass (log10, g)")

#fruitTraits <- melt(fruitTraits, id.vars = c("sp", "Proportion of seeds abscised"), measure.vars = c("Tree height (m)" , "Interannual crop size variation", "Overlap in fruit production (log10)", "Local Abundance", "Endocarp investment", "Seed dry mass (log10, g)"))

ggscatter(fruitTraits, x = "value", y = "proportion_abscised", size = 3,
          facet.by = "variable", scales = "free_x",
          add = "reg.line", conf.int = TRUE, 
          ggtheme = theme_pubr(base_size=45),
          add.params = list(colour = "black", fill = "lightgray"),
          panel.labs = list(variable=labs),
          cor.coef = TRUE,
          cor.coef.size = 15,
          cor.method = "spearman",
          use = "complete.obs",
          cor.coef.name = "rho"
        ) + ylab("Proportion of seeds abscised") + xlab("")
ggsave("../output/plots/prematureFruitDrop/traitsScatter.png", dpi = "retina")

### CORRELATION MATRIX

#different fuction to check
cor.test(x= corTraits$"Tree height", y=corTraits$"Proportion of\nseeds abscised", use = "complete.obs", method="spearman", exact=FALSE)

corTraits <- subset(fruitDat, select = c("proportion_abscised", "cvseed", "cofruit", "bcireproductive", "endocarp_investment", "height_avg", "seed_dry"))

corTraits <- corTraits %>% rename("Tree height" = height_avg, "Interannual crop\nsize variation" = cvseed, "Overlap in\nfruit production" = cofruit, "Local\nAbundance" = bcireproductive, "Endocarp\ninvestment" = endocarp_investment, "Seed dry mass" = seed_dry, "Proportion of\nseeds abscised" = proportion_abscised)

library(ggcorrplot)

ggcorrplot::ggcorrplot(cor(corTraits, use = "pairwise.complete.obs", method="spearman"), p.mat = cor_pmat(corTraits, use = "pairwise.complete.obs", method="spearman"), hc.order=TRUE, type="lower",lab=TRUE, sig.level=0.05,insig="blank", lab_size=15, tl.cex=35, digits=2) +
theme(axis.text.x=element_text(angle=0, hjust=0.5, vjust=0.5), legend.position="none") 

ggsave("../output/plots/prematureFruitDrop/traitsCorr.png", dpi = "retina")

