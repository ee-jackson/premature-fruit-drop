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

# Load data ---------------------------

fruit_traits <- readRDS(here::here("data", "clean", "fruit_traits.rds"))

# Plot histogram ---------------------------

fruit_traits %>%
	select("sp4","year", "proportion_abscised") %>%
	group_by(sp4) %>%
	summarise(proportion_abscised_m = mean(proportion_abscised)) %>%
	ggplot(aes(x=proportion_abscised_m)) +
	    geom_histogram(aes(y=..count..), binwidth=.05, colour="black", fill="grey", alpha=.2, size=0.25, boundary=0) +
		scale_y_continuous(expand= c(0, 0), lim =c(0,45)) +
		scale_x_continuous(expand= c(0, 0)) +
		xlab("mean proportion of seeds prematurely abscised") +
		ylab("count of plant species") +
		theme_classic(base_size = 8) +
		theme(plot.margin = margin(2, 3, 2, 2, "mm") )

ggsave(here::here("output", "figures", "01_histogram.pdf"),
    device = "pdf", dpi = 600, width = 80, height = 60, units = "mm")

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
	rename("Tree height" = height_avg,
	       "Temporal crop\nsize variation" = cvseed,
	       "Temporal overlap in\nfruit production" = cofruit,
	       "Local Abundance\nof conspecifics" = bcireproductive,
	       "Investment in mechanical\nseed defences" = endocarp_investment,
	       "Seed mass" = seed_dry,
	       "Mean proportion of\nseeds abscised" = proportion_abscised_m) -> cor_traits

# plot
ggcorrplot(cor(cor_traits, use = "pairwise.complete.obs", method="spearman"),
	p.mat = cor_pmat(cor_traits, use = "pairwise.complete.obs",
	method = "spearman"), hc.order = TRUE, type = "lower",lab = TRUE,
	sig.level = 0.05, insig = "pch", pch.cex = 8, lab_size = 2,
	tl.cex = 7, digits = 2, colors = c("#0571b0", "#f7f7f7", "#ca0020")) +
	theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5), legend.position="none")

ggsave(here::here("output", "figures", "s1_correlations.pdf"),
       device = "pdf", dpi = 600, width = 80, height = 80, units = "mm")
