#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: phylogeny.R
## Desc: pjhylogenetic analysis
## Date: March 2020

rm(list = ls())
require(tidyverse)
require(ggpubr)
require(phytools)
require(caper)
require(picante)
require(geiger)
theme_set(theme_pubr(base_size = 40))

# read dataset
fruitDat <- read.csv("../output/tables/cleanData.csv")
PhyloExtraSpec<-read.tree('../data/PhyloExtraSpec.tree')

# remove outliers
fruitDat$bcireproductive <- ifelse(fruitDat$bcireproductive > 500, NA, fruitDat$bcireproductive)

fruitDat$cofruit <- ifelse(fruitDat$cofruit < 100, NA, fruitDat$cofruit)

# add taxa name for those that are missing

fruitDat$plant_species19 <- ifelse(is.na(fruitDat$plant_species19), paste(fruitDat$genus, fruitDat$species, sep="_"), paste(fruitDat$plant_species19))

fruitDat$plant_species19 <- as.factor(fruitDat$plant_species19)

# get tree in order

fruitDat <- rename(fruitDat, taxa=plant_species19)

check <- geiger::name.check(phy = PhyloExtraSpec, data = fruitDat, data.names = fruitDat$taxa)
#check

phylo <- drop.tip(PhyloExtraSpec, check$tree_not_data)

# remove polytomies
phylo <- multi2di(phylo, random = TRUE)

#label nodes
phylo$node.label <- 1:length(phylo$node.label)

# Make sure edge length isn't zero
phylo$edge.length  <- phylo$edge.length + 0.001

# Put them in the right order:
fruitDat <- fruitDat[   match(phylo$tip.label, fruitDat$taxa),   ] 

###### PGLS ############

# To perform PGLS models in R, caper requires you to first combine the phylogeny and data into one object using the function comparative.data.

# make comparative data object and check to see what it dropped
compDat <- comparative.data(phy = phylo, data =as.data.frame(fruitDat), 
                            names.col = taxa, vcv = TRUE, 
                            na.omit = FALSE, warn.dropped = TRUE)
compDat$dropped$tips
compDat$dropped$unmatched.rows

##pgls model, Pagel’s Lambda, using the Maximum Likelihood (ML) estimate of λ

par(mfrow = c(2, 3)) 

# LOCAL ABUNDANCE
lambdaBR <- caper::pgls(proportion_abscised ~ bcireproductive, data = compDat, lambda = "ML")
summary(lambdaBR) # lambda is 0.294, p is 4.909e-05 high R of 0.1111
#OLS model to compare
lmodel <- lm(proportion_abscised ~ bcireproductive, data = fruitDat)
# plot the model
plot(proportion_abscised ~ bcireproductive, data = fruitDat, cex=3, cex.lab=4) + abline(lambdaBR, col="red") + abline(lmodel) +title(main="Local Abundance", cex.main=5)

#check the likelihood profile
lambda.profile <- pgls.profile(lambdaBR, "lambda")
plot(lambda.profile) # flat likelihood surface
#print the confidence intervals
pgls.confint(model.pgls, "lambda")$ci.val

# SEED DRY MASS
lambdaSD <- pgls(proportion_abscised ~ log10(seed_dry), data = compDat, lambda = "ML")
summary(lambdaSD) # lambda is 0.242, p=0.0007574

lambda.profile <- pgls.profile(lambdaSD, "lambda")
plot(lambda.profile) # also flat

lmodel <- lm(proportion_abscised ~ log10(seed_dry), data = fruitDat)
plot(proportion_abscised ~ log10(seed_dry), data = fruitDat, cex=3,cex.lab=4) + abline(lambdaSD, col="red") + abline(lmodel) +title(main="Seed dry mass", cex.main=5)

# TREE HEIGHT
lambdaHA <- pgls(proportion_abscised ~ height_avg, data = compDat, lambda = "ML")
summary(lambdaHA) # 0.220 p=8.646e-05 this has high R value of 0.1009?

lambda.profile <- pgls.profile(lambdaHA, "lambda")
plot(lambda.profile) # flat

lmodel <- lm(proportion_abscised ~ height_avg, data = fruitDat)
plot(proportion_abscised ~ height_avg, data = fruitDat, cex=3,cex.lab=4) + abline(lambdaHA, col="red") + abline(lmodel) +title(main="Tree height", cex.main=5)


#  CROP SIZE VARIATION
lambdaCV <- pgls(proportion_abscised ~ cvseed, data = compDat, lambda = "ML")
summary(lambdaCV) 

lambda.profile <- pgls.profile(lambdaCV, "lambda")
plot(lambda.profile) # flat!

lmodel <- lm(proportion_abscised ~ cvseed, data = fruitDat)
plot(proportion_abscised ~ cvseed, data = fruitDat, cex=3,cex.lab=4) + abline(lambdaCV, col="red") + abline(lmodel) +title(main="Crop size variation", cex.main=5)

# OVERLAP IN FRUIT PRODUCTION
lambdaCF <- pgls(proportion_abscised ~ log10(cofruit), data = compDat, lambda = "ML")
summary(lambdaCF) 

lambda.profile <- pgls.profile(lambdaCF, "lambda")
plot(lambda.profile)

lmodel <- lm(proportion_abscised ~ log10(cofruit), data = fruitDat)
plot(proportion_abscised ~ log10(cofruit), data = fruitDat, cex=3,cex.lab=4) + abline(lambdaCF, col="red") + abline(lmodel) +title(main="Overlap in fruit production", cex.main=5)

# ENDOCARP INVESTMENT
lambdaEI <- pgls(proportion_abscised ~ endocarp_investment, data = compDat, lambda = "ML")
summary(lambdaEI) # 0.286

lambda.profile <- pgls.profile(lambdaEI, "lambda")
plot(lambda.profile) #FLAT!

lmodel <- lm(proportion_abscised ~ endocarp_investment, data = fruitDat)
plot(proportion_abscised ~ endocarp_investment, data = fruitDat, cex=3,cex.lab=4) + abline(lambdaEI, col="red") + abline(lmodel) +title(main="Endocarp investment", cex.main=5) 

par(mfrow = c(1, 1))

# SEED PRED
lambdaSP <- pgls(proportion_abscised ~ seedpredationrate, data = compDat, lambda = "ML")
summary(lambdaSP)

lmodel <- lm(proportion_abscised ~ seedpredationrate, data = fruitDat)
plot(proportion_abscised ~ seedpredationrate, data = fruitDat, cex=3,cex.lab=4) + abline(lambdaSP, col="red") + abline(lmodel) 

###############################################################
####### Estimating phylogenetic signal for one variable

# Pagel’s  λ
model.pgls <- pgls(proportion_abscised ~ 1, data = compDat, lambda = "ML")
summary(model.pgls)

#can look at the likelihood profiles for branch length transformations in PGLS models using pgls.profile

# Ideally you want a line with an obvious peak/optimum like this, rather than a flat line which would suggest Lambda could be anything. You can see that the optimum (the peak of the curve) should be the same as estimated in our PGLS model. 
lambda.profile <- pgls.profile(model.pgls, "lambda")
plot(lambda.profile)

# The dotted red lines are the 95% confidence intervals on Lambda for our model. pgls.confint prints out these numbers in $ci.val
pgls.confint(model.pgls, "lambda")$ci.val

# Blomberg’s K
p_a <- fruitDat$proportion_abscised
names(p_a) <- fruitDat$taxa
Kcalc(p_a[phylo$tip.label], phylo)
picante::phylosignal(p_a[phylo$tip.label], phylo, reps = 1000)

phytools::phylosig(x=p_a,tree=phylo, test=TRUE, nsim = 1000)

phytools::plotTree(newtree,p_a, showTipLabel=FALSE, showNodeLabel=FALSE, fsize=0.5)

png(file = "../output/plots/prematureFruitDrop/contMap.png",  
    width = 2560, # The width of the plot in inches
    height = 1440)
contMap(phylo, p_a, fsize=1.5, type="fan", lwd=6)
dev.off()
