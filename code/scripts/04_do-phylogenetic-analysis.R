#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 04_do-phylogenetic-analysis.R
## Desc: Test for phylogenetic signal in proportion abscised
## Date: March 2020

# Load packages -----------------------------------------------------------

library("tidyverse") # v1.3.1
library("phytools") # v0.7-70
library("ape") # v5.5
library("geiger") # v2.0.7
library("here") # v1.0.1

# Load data ---------------------------------------------------------------

# load tree - available here https://doi.org/10.5061/dryad.230j5ch
PhyloExtraSpec <- read.tree(here::here("data", "raw", "PhyloExtraSpec.tree"))

# load clean dataset
fruit_traits <- readRDS(here::here("data", "clean", "fruit_traits.rds"))

# Ready data --------------------------------------------------------------

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
    dplyr::select(c(sp4, plant_species19, genus, species)) %>%
    distinct() %>%
    left_join(fruit_traits_m, by = "sp4") -> model_data

# add taxa name for those that are missing
model_data$plant_species19 <- ifelse(is.na(model_data$plant_species19),
    paste(model_data$genus, sep = "_", model_data$species),
    paste(model_data$plant_species19))

model_data$plant_species19 <- as.factor(model_data$plant_species19)
model_data <- rename(model_data, taxa = plant_species19)

# check if names in the tree and names in the data match
check <- geiger::name.check(phy = PhyloExtraSpec, data = model_data,
    data.names = model_data$taxa)
check

# remove species from the tree which are not in the data
phylo <- drop.tip(PhyloExtraSpec, check$tree_not_data)

# remove species from the data which are not in the tree
matches <- match(model_data$taxa, check$data_not_tree, nomatch = 0)
model_data <- subset(model_data, matches == 0)

# OK?
geiger::name.check(phy = phylo, data.names = model_data$taxa)

# remove polytomies
phylo <- multi2di(phylo, random = TRUE)

# label nodes
phylo$node.label <- 1:length(phylo$node.label)

# make sure edge length isn't zero
phylo$edge.length <- phylo$edge.length + 0.001

# put them in the right order
model_data <- model_data[  match(phylo$tip.label, model_data$taxa),  ]

p_a <- model_data$proportion_abscised

names(p_a) <- model_data$taxa

# Test for phylo signal ---------------------------------------------------

# estimate Blomberg’s K
phytools::phylosig(x = p_a, tree = phylo,
    test=TRUE, nsim = 1000, method = "K")

# estimate Pagel’s lambda
phytools::phylosig(x = p_a, tree = phylo,
    test=TRUE, nsim = 1000, method = "lambda")

# Create figure 3 ---------------------------------------------------------

# plot map of mean proportion abscised across the tree
obj <- phytools::contMap(tree = phylo, x = p_a, plot = F)

# colours from viridis colour palette
obj <- setMap(obj, colors = c("#F0F921FF", "#CC4678FF", "#0D0887FF"))

# as pdf
pdf(file = here::here("output", "figures", "03_phylotree.pdf"),
    width = 7.09, height = 7.09)
plot(obj, legend = FALSE, fsize = c(0.5,1), lwd = 2,
     type="fan", sig = 1, res = 1000)
add.color.bar(90,obj$cols, title = "premature seed\nabscission\n",
              lims = obj$lims, digits = 1, prompt = FALSE, x = -220, y = -200,
              lwd = 2, fsize = 0.8, subtitle = "")
dev.off()
