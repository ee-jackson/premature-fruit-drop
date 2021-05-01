#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 01_plot-proportion-abscised.R
## Desc: plot the histogram and trait correlation matrix
## Date: March 2020

# Load packages ---------------------------

library("groundhog")
groundhog_day = "2021-04-29"
groundhog.library("tidyverse", groundhog_day)
groundhog.library("reshape2", groundhog_day)
groundhog.library("ggcorrplot", groundhog_day)
groundhog.library("here", groundhog_day)

# Load data ---------------------------

load(here::here("data", "clean", "fruit_traits.RData"))

# only include sp x year points if there were at least 10 parts found
fruit_traits <- subset(fruit_traits, sum_parts >= 10)

# Plot histogram ---------------------------

fruit_traits %>%
	select("sp4","year", "proportion_abscised") %>%
	group_by(sp4) %>%
	summarise(proportion_abscised_m = mean(proportion_abscised)) %>%
	ggplot(aes(x=proportion_abscised_m)) +
	    geom_histogram(aes(y=..count..), binwidth=.05, colour="black", fill="grey", alpha=.2, size=0.25, boundary=0) +
		scale_y_continuous(expand= c(0, 0), lim =c(0,45)) +
		scale_x_continuous(expand= c(0, 0)) +
		xlab("Mean proportion of seeds prematurely abscised") +
		ylab("Count of plant species") +
		theme_classic(base_size = 8) +
		theme(plot.margin = margin(2, 3, 2, 2, "mm") )

ggsave(here::here("output", "figures", "01_histogram.tiff"),
    device = "tiff", dpi = 350, width = 80, height = 60, units = "mm")

# Plot correlation matrix ---------------------------

# log these two as that is how we model them
fruit_traits$seed_dry_log <- log(fruit_traits$seed_dry)
fruit_traits$bcireproductive_log <- log(fruit_traits$bcireproductive)

# reformat data for ggcorrplot
fruit_traits %>%
	group_by(sp4,height_avg,cvseed,cofruit,endocarp_investment,seed_dry,bcireproductive) %>%
	summarise(proportion_abscised_m = mean(proportion_abscised),
	 .groups ="keep")%>%
	ungroup() %>%
	select(-sp4) %>%
	rename("Tree height" = height_avg, "Interannual crop\nsize variation" = cvseed, "Overlap in\nfruit production" = cofruit, "Local\nAbundance" = bcireproductive, "Endocarp\ninvestment" = endocarp_investment, "Seed dry mass" = seed_dry, "mean Proportion of\nseeds abscised" = proportion_abscised_m) -> cor_traits

# plot
ggcorrplot(cor(cor_traits, use = "pairwise.complete.obs", method="spearman"),
	p.mat = cor_pmat(cor_traits, use = "pairwise.complete.obs",
	method = "spearman"), hc.order = TRUE, type = "lower",lab = TRUE,
	sig.level = 0.05, insig = "pch", pch.cex = 10, lab_size = 3,
	tl.cex = 7, digits = 2, colors = c("#ca0020", "#f7f7f7", "#0571b0")) +
	theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5), legend.position="none")

ggsave(here::here("output", "figures", "s1_correlations.tiff"),
    device = "tiff", dpi = 350, width = 80, height = 80, units = "mm")
