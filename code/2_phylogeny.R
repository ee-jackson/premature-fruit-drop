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

fruitDat %>%
	mutate(seedpred_pres = factor(ifelse(seedpred_pres == 0, "no", "yes"))) -> fruitDat

fruitDat$seed_dry_log <- log10(fruitDat$seed_dry)
fruitDat$cofruit_log <- log10(fruitDat$cofruit)
fruitDat$bcireproductive_log <- log10(fruitDat$bcireproductive)

#round these to integers ?
fruitDat$abscised_seeds <- round(fruitDat$abscised_seeds)
fruitDat$viable_seeds <- round(fruitDat$viable_seeds)
fruitDat$total_seeds <- fruitDat$viable_seeds + fruitDat$abscised_seeds
fruitDat$proportion_abscised <- fruitDat$abscised_seeds/fruitDat$total_seeds

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


# function to loop through and fit pgls models

vars <- names(fruitDat)[c(13,14,17,21,25:28)]

pglsmodels <- lapply(setNames(vars, vars), function(var) {
	formula = paste("proportion_abscised ~", var)
	caper::pgls(formula=eval(parse(text=formula)), data=compDat, lambda = "ML")
	}
)


# plot in loop

pglsmodels$seedpred_pres <- NULL
pglsmodels$dispersal_mode <- NULL

labs <- c(
            height_avg = "Tree height",
            cvseed = "Interannual crop size variation",
            cofruit_log = "Overlap in fruit production (log)",
            bcireproductive_log = "Local abundance (log)",
            endocarp_investment = "Endocarp investment",
            seed_dry_log ="Seed dry mass (log)",
            proportion_abscised = "Proportion of seeds abscised")

eq <- function(x) predict(pglsmodels$height_avg, newdata=data.frame(height_avg=x))

plot_data_var <- function (model, modelname) {
    ggplot(data= fruitDat, aes_string(x = modelname, y = "proportion_abscised")) +
        geom_point(size=1) +
		stat_function(geom="line", colour="blue", fun = eq) +
        xlab(modelname) +
        ylab("Proportion of seeds abscised") +
        xlab(labs[[modelname]]) +
        theme_bw(base_size=40)
}

myplots <- imap(pglsmodels, plot_data_var)

ggpubr::ggarrange(plotlist = myplots)
#ggsave("../output/plots/pgls.png", dpi="screen")

#Calculate phylogenetic signal for traits
row.names(fruitDat) <- fruitDat$taxa
fd2 = fruitDat[c(3,13,14,17,25:27)]
sigk<-picante::multiPhylosignal(fd2, phylo)
write.csv(sig,"../output/tables/multiPhylosignal.csv")


###############################################################
####### Estimating phylogenetic signal for one variable

# Pagel’s  λ
model.pgls <- caper::pgls(proportion_abscised ~ 1, data = compDat, lambda = "ML")
summary(model.pgls)

m2 <- phylolm::phyloglm(formula = (abscised_seeds/total_seeds) ~ 1, data = fruitDat, phy = phylo)
pez::communityPGLMM(formula=proportion_abscised ~ 1, data=fruitDat, family="binomial",sp = fruitDat$taxa)

gls(proportion_abscised ~,data=data,correlation=corPagel(value=1,phy=tree,fixed=FALSE),method="ML")

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

# lambda
picante::phylosignal(p_a[phylo$tip.label], phylo, reps = 1000)

# k
phytools::phylosig(x=p_a,tree=phylo, test=TRUE, nsim = 1000, method="K")
phytools::phylosig(x=p_a,tree=phylo, test=TRUE, nsim = 1000, method="lambda")

phytools::plotTree(newtree,p_a, showTipLabel=FALSE, showNodeLabel=FALSE, fsize=0.5)

png(file = "../output/plots/prematureFruitDrop/contMap_t.png",  
    width = 2560, # The width of the plot in inches
    height = 1440)
contMap(phylo, p_a, fsize=2, type="fan", lwd=6)
dev.off()

ggsave("../output/plots/contMap_t.png", dpi="screen")

##########

p_a <- fruitDat$proportion_abscised
names(p_a) <- fruitDat$taxa

bci <- fruitDat$bcireproductive_log
names(bci) <- fruitDat$taxa

phytools::phylosig(x=bci,tree=phylo, test=TRUE, nsim = 1000, method="K")
phytools::phylosig(x=bci,tree=phylo, test=TRUE, nsim = 1000, method="lambda")
