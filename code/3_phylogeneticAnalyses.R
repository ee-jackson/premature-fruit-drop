#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: phylogeny.R
## Desc: phylogenetic analysis
## Date: March 2020

rm(list = ls())
library("tidyverse")
library("phytools")
library("ape")

# load tree
PhyloExtraSpec <- read.tree('../data/raw/PhyloExtraSpec.tree')
# load dataset
load("../data/clean/fruit_traits.RData")

fruit_traits <- subset(fruit_traits, sum_parts >= 10)
# mean proportion_abscised across yrs, 1 value per sp
fruit_traits %>%
    group_by(sp4) %>% 
    summarise(proportion_abscised = mean(proportion_abscised), 
        abscised_seeds = sum(abscised_seeds), 
        viable_seeds = sum(viable_seeds), 
        total_seeds = sum(total_seeds),
        n_years = length(unique(year))) %>%
    ungroup() -> fruit_traits_m

fruit_traits %>%
    dplyr::select(c(sp4,
                plant_species19, genus, species)) %>%
    distinct() %>%
    left_join(fruit_traits_m) -> model_data

# add taxa name for those that are missing
model_data$plant_species19 <- ifelse(is.na(model_data$plant_species19), paste(model_data$genus, model_data$species, sep="_"), paste(model_data$plant_species19))

model_data$plant_species19 <- as.factor(model_data$plant_species19)
model_data <- rename(model_data, taxa = plant_species19)

## get tree in order

check <- geiger::name.check(phy = PhyloExtraSpec, data = model_data, 
    data.names = model_data$taxa)
#check

phylo <- drop.tip(PhyloExtraSpec, check$tree_not_data)

# remove polytomies
phylo <- multi2di(phylo, random = TRUE)

#label nodes
phylo$node.label <- 1:length(phylo$node.label)

# Make sure edge length isn't zero
phylo$edge.length  <- phylo$edge.length + 0.001

# Put them in the right order:
model_data <- model_data[   match(phylo$tip.label, model_data$taxa),   ] 


p_a <- model_data$proportion_abscised
names(p_a) <- model_data$taxa

# estimate Blomberg’s K
phytools::phylosig(x=p_a,tree=phylo, test=TRUE, nsim = 1000, method="K")

# estimate Pagel’s lambda 
phytools::phylosig(x=p_a,tree=phylo, test=TRUE, nsim = 1000, method="lambda")

# plot map of mean proportion abscised across the tree
obj <- phytools::contMap(phylo, p_a, plot=F,legend=1.5) ->obj
obj <- setMap(obj,colors=c("#FDE725FF", "#21908CFF", "#440154FF"))
plot(obj, fsize=c(0.5,1), lwd=3, sig=1, type="fan")

tiff(file = "../output/figures/contMap4.tiff",  
    width = 180, height = 180, units = "mm", res = 300)
plot(obj, fsize=c(0.5,1), lwd=1.5,legend=65, type="fan", sig=1, lwd=1.5,res = 300,ftype="bi")
dev.off()