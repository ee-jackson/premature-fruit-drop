#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 03_do-phylogenetic-analysis.R
## Desc: Test for phylogenetic signal in proportion abscised
## Date: March 2020

# Load packages ---------------------------

library("groundhog")
groundhog_day = "2021-04-29"
groundhog.library("tidyverse", groundhog_day)
groundhog.library("phytools", groundhog_day)
groundhog.library("ape", groundhog_day)

# Load data ---------------------------

# load tree
PhyloExtraSpec <- read.tree(here::here("data", "raw", "PhyloExtraSpec.tree"))

# load dataset
load(here::here("data", "clean", "fruit_traits.RData"))

# Clean up data ---------------------------

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
model_data$plant_species19 <- ifelse(is.na(model_data$plant_species19),
    paste(model_data$genus, model_data$species, sep="_"),
    paste(model_data$plant_species19))

model_data$plant_species19 <- as.factor(model_data$plant_species19)
model_data <- rename(model_data, taxa = plant_species19)

# drop species that are in the tree but not in the data
check <- geiger::name.check(phy = PhyloExtraSpec, data = model_data,
    data.names = model_data$taxa)
check

phylo <- drop.tip(PhyloExtraSpec, check$tree_not_data)

# remove polytomies
phylo <- multi2di(phylo, random = TRUE)

# label nodes
phylo$node.label <- 1:length(phylo$node.label)

# make sure edge length isn't zero
phylo$edge.length  <- phylo$edge.length + 0.001

# put them in the right order
model_data <- model_data[   match(phylo$tip.label, model_data$taxa),   ]

p_a <- model_data$proportion_abscised

names(p_a) <- model_data$taxa

# Test for phylo signal ---------------------------

# estimate Blomberg’s K
phytools::phylosig(x = p_a, tree=phylo,
    test=TRUE, nsim = 1000, method="K")

# estimate Pagel’s lambda
phytools::phylosig(x = p_a, tree=phylo,
    test=TRUE, nsim = 1000, method="lambda")

# Plot tree ---------------------------

# plot map of mean proportion abscised across the tree
obj <- phytools::contMap(phylo, p_a, plot=F)

obj <- setMap(obj,colors=c("#F0F921FF", "#CC4678FF", "#0D0887FF"))

# save as tiff
tiff(file = here::here("output", "figures", "s2_phylotree.tiff"),
    width = 180, height = 180, units = "mm", res = 600)
plot(obj, legend=FALSE, fsize=c(0.5,1), lwd=2,
     type="fan", sig=1, res = 1000,ftype="bi")
add.color.bar(90,obj$cols,title="premature seed\nabscission\n",
              lims = obj$lims, digits = 1, prompt = FALSE, x = -220, y = -200,
              lwd = 2, fsize=0.8, subtitle="")
dev.off()

# as pdf
pdf(file = here::here("output", "figures", "s2_phylotree3.pdf"),
    width = 7.09, height = 7.09)
plot(obj, legend=FALSE, fsize=c(0.5,1), lwd=2,
     type="fan", sig=1, res = 1000)
add.color.bar(90,obj$cols,title="premature seed\nabscission\n",
              lims = obj$lims, digits = 1, prompt = FALSE, x = -220, y = -200,
              lwd = 2, fsize=0.8, subtitle="")
dev.off()
