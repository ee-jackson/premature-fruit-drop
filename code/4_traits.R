#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: traits.R
## Desc: GLMMs for proportion abscised and plant traits
## Date: June 2020

rm(list = ls())

### load packages #############################################################
require(tidyverse)
require(lme4)
require(DHARMa)
require(broom)
require(ggpubr)
require(reshape2)
require(patchwork)

require(MuMIn)
require(MASS)

require(glmmTMB)

### read in and format data ###################################################

fruitDat <- read.csv("../output/tables/fullCleanData.csv")

fruitDat$seed_dry_log <- log10(fruitDat$seed_dry)
fruitDat$bcireproductive_log <- log10(fruitDat$bcireproductive)

# round these to integers so that DHARMa will take them
fruitDat$abscised_seeds <- round(fruitDat$abscised_seeds)
fruitDat$viable_seeds <- round(fruitDat$viable_seeds)

fruitDat$year <- as.factor(fruitDat$year)
fruitDat$sp <- as.factor(fruitDat$sp)

set.seed(123)

### fit models ################################################################

fruitDat <- transform(fruitDat, 
    cofruit.SC = scale(cofruit)
    )

# select variables to loop over
vars = c("height_avg","cvseed","cofruit.SC","endocarp_investment",
		"seedpred_pres","seed_dry_log","bcireproductive_log")

# a loop to run univariate binomial models in lme4
models <- lapply(setNames(vars, vars), function(var) {
	formula <- paste("cbind(abscised_seeds,viable_seeds)~", var, 
					"+ (1|year) + (1|sp) + (1|year:sp)")
	lme4::glmer(formula, family = binomial(logit), data=fruitDat)
	}
)

# look at the output
lapply(models, summary)

# height_avg does not converge so we will try different optimisers
modelfit.all <- lme4::allFit(models$height_avg)
ss <- summary(modelfit.all)
ss # bobyqa and nlminbwrap optimisers converge

# refit with optimizer = "bobyqa" and replace old fit
models$height_avg <- lme4::glmer(cbind(abscised_seeds,viable_seeds) ~ 
	height_avg + (1|year) + (1|sp) + (1|year:sp), family = binomial(logit), 
	data=fruitDat, control = glmerControl(optimizer = "bobyqa"))

# compute per-model statistics
stats <- purrr::map_dfr(models, broom::glance, conf.int = TRUE, .id = "vars") 

write.csv(stats, "../output/tables/allGLMMstats.csv")

# compute statistics about each of the coefficients for each model
res_anova <- purrr::map_dfr(models2, broom::tidy, 
							conf.int = TRUE, .id = "vars")

# remove rows with intercept so we only keep coefs for the variables
results <- res_anova[!grepl("(Intercept)", res_anova$term),]
results
write.csv(res_anova, "../output/tables/allGLMMcoef.csv")

### model diagnostics #########################################################

# a function to create simulated residuals for each model fit
resid_plots <- function(model, modelname) {
     output <- DHARMa::simulateResiduals(fittedModel = model)
     
     plot(output, sub=modelname) 
}

# look at a residual plot for one variable to check if it's working
resid_plots(model = models[[2]], modelname = names(models)[2])

# loop through all model fits and print residual plots in a pdf doc
pdf("../output/plots/all_residualplots_GLMM_spxyear_binom.pdf",width=10, height=7)
imap(models, resid_plots)
dev.off()

# loop through to get simulated residuals for all model fits
modelssim <-lapply(models, simulateResiduals)

# test simulated residuals for zero inflation
# a value > 1 means that it has more zeros than expected
lapply(modelssim, testZeroInflation) 

# test for over/undersdispersion
lapply(modelssim, testDispersion) 

### plot models ###############################################################

# remove categorical variable
models$seedpred_pres <- NULL

# unscale cofruit for plotting
fruitDat$cofruit.SC <- cofruit.SC * attr(cofruit.SC, 'scaled:scale') 
						+ attr(cofruit.SC, 'scaled:center')

# create labels
labs <- c(
            height_avg = "Tree height",
            cvseed = "Interannual crop size variation",
            cofruit.SC = "Overlap in fruit production",
            bcireproductive_log = "Local abundance (log)",
            endocarp_investment = "Endocarp investment",
            seed_dry_log ="Seed dry mass (log)",
            proportion_abscised = "Proportion of seeds abscised")

# function to plot all model fits
plot_model_fit <- function (model, modelname, labs) {
    output <- as.data.frame(effects::effect(term= modelname, 
    										mod= model, xlevels=30))
    ggplot() +
        geom_point(data= fruitDat, aes_string(x = modelname, 
        					y = "proportion_abscised"), size=0.5,alpha= 0.5) +
        geom_line(data= output, aes_string(x = modelname, y = "fit"), 
        					colour= "blue") +
        geom_ribbon(data= output, aes_string(x= modelname, ymin="lower", 
        					ymax="upper"), alpha= 0.1, fill="blue") +
        xlab(labs[modelname]) +
        ylab("Proportion of seeds abscised") +
        theme_bw(base_size=20)
}

# loop through model fits with plotting function
myplots <- imap(models, plot_model_fit, labs)

# need to backtransform scale() for cofruit

# get model predictions
output <- as.data.frame(effects::effect(term= "cofruit.SC", 
    									mod= models$cofruit.SC, xlevels=30))

# function to backtransform
unscale <- function(x) {x * attr(fruitDat$cofruit.SC, 'scaled:scale') 
						+ attr(fruitDat$cofruit.SC, 'scaled:center')}

# apply function to output
output$cofruit.SC<-unscale(output$cofruit.SC)

# replot and replace
myplots$cofruit.SC <- ggplot() +
	geom_point(data= fruitDat, aes(x = cofruit, 
						y = proportion_abscised), size=0.5,alpha= 0.5) +
	geom_line(data= output, aes(x = cofruit.SC, y = fit), 
						colour= "blue") +
	geom_ribbon(data= output, aes(x= cofruit.SC, ymin=lower, 
						ymax=upper), alpha= 0.1, fill="blue") +
	ylab("Proportion of seeds abscised") +
	xlab("Overlap in fruit production") +
	theme_bw(base_size=20)

# arrange plots in a grid
ggpubr::ggarrange(plotlist = myplots)

# and save!
ggsave("../output/plots/glmms.png", dpi="retina")

### plot categorical variables ################################################

# read in averaged dataset
fruitDat.a <- read.csv("../output/tables/cleanData.csv")

fruitDat.a %>%
	mutate(seedpred_pres = factor(ifelse(seedpred_pres == 0, "no", "yes")))  -> fruitDat.a

fruitDat.a %>% 
    drop_na(seedpred_pres) %>%
    ggplot(., aes(y=proportion_abscised, group=as.factor(seedpred_pres), x=as.factor(seedpred_pres))) + 
        geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single"), width=0.3, size=1, outlier.shape = NA)+
        geom_jitter(shape=16, position=position_jitter(0.15), alpha=0.5, size=2.5) +
        theme(legend.position= "none") +
        xlab("Presence of seed predator") +
        ylab("Proportion of seeds abscised") +
        scale_y_continuous(limits=c(0.0,1.0)) +
        theme_pubr(base_size = 30) -> p1

fruitDat.a %>%
    select(c("sp", "hymeno_pres", "lepid_pres", "coleo_pres", "proportion_abscised")) %>%
    rename(coleoptera=coleo_pres, hymenoptera = hymeno_pres, lepidoptera = lepid_pres) %>%
    melt(id.vars=c("sp", "proportion_abscised"), measure.vars=c("coleoptera", "hymenoptera", "lepidoptera")) %>%
    rename(predator=variable, presence=value) %>% 
    drop_na(presence) %>%
    mutate(presence = factor(ifelse(presence == 0, "no", "yes"))) %>%
        ggplot(., aes(y=proportion_abscised, group=as.factor(presence), x=as.factor(presence))) + 
            facet_wrap(~predator) +
            geom_boxplot(alpha= 0.8,position = position_dodge2(preserve = "single"), width=0.4, size=1, outlier.shape = NA)+
            geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.5, size=2) +
            theme(legend.position= "none") +
            xlab("Presence of seed predator") +
            ylab("Proportion of seeds abscised") +
            scale_y_continuous(limits=c(0.0,1.0)) +
            theme_pubr(base_size = 30) -> p2

(p1+p2) + plot_annotation(tag_levels = 'A')

ggsave("../output/plots/predPresBoxplots.png", dpi="retina")