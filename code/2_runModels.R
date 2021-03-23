#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: traits.R
## Desc: GLMMs for proportion abscised and plant traits
## Date: June 2020

### load packages #############################################################

library("tidyverse")
library("lme4")
library("DHARMa")
library("broom.mixed")

### read in and format data ###################################################

rm(list = ls())

set.seed(123)

load("../data/clean/fruit_traits.RData")

# round number of seeds to integers
fruit_traits$abscised_seeds <- round(fruit_traits$abscised_seeds)
fruit_traits$viable_seeds <- round(fruit_traits$viable_seeds)

fruit_traits$year <- as.factor(fruit_traits$year)
fruit_traits$sp4 <- as.factor(fruit_traits$sp4)

# only include sp x year points if there were at least 10 parts found
fruit_traits <- subset(fruit_traits, sum_parts >= 10)

# transform
fruit_traits <- mutate(fruit_traits, 
    cofruit_cs = scale(cofruit),
    height_avg_cs = scale(height_avg),
    cvseed_cs = scale(cvseed),
    endocarp_investment_cs = scale(endocarp_investment),
    seed_dry_log = log(seed_dry),
    bcireproductive_log = log(bcireproductive)
    )

## fit models #################################################################

# select variables to loop over
vars <- c("height_avg_cs","cvseed_cs","cofruit_cs","endocarp_investment_cs",
        "seedpred_pres","seed_dry_log","bcireproductive_log")

# loop model fitting over all the traits
models <- lapply(setNames(vars, vars), function(var) {
	formula <- paste("cbind(abscised_seeds, viable_seeds)~", var, 
					"+ (1|year) + (1|sp4)")
	lme4::glmer(formula, family = binomial(logit), data = fruit_traits)
	}
)

# look at the output
lapply(models, summary)

# compute per-model statistics
stats <- purrr::map_dfr(models, broom::glance, conf.int = TRUE, .id = "vars") 

write.csv(stats, "../output/tables/allGLMMstats.csv")

# compute statistics about each of the coefficients for each model
res_anova <- purrr::map_dfr(models, broom::tidy, 
							conf.int = TRUE, .id = "vars")

write.csv(res_anova, "../output/tables/allGLMMcoef.csv")

# remove rows with intercept so we only keep coefs for the variables
results <- res_anova[!grepl("(Intercept)", res_anova$term),]
results # we will use this for plotting

### model diagnostics #########################################################

# a function to create simulated residuals for each model fit
resid_plots <- function(model, modelname) {
     output <- DHARMa::simulateResiduals(fittedModel = model)
     
     plot(output, sub=modelname) 
}

# look at a residual plot for one variable to check if it's working
resid_plots(model = models[[2]], modelname = names(models)[2])

# loop through all model fits and print residual plots in a pdf doc
pdf("../output/figures/all_residualplots.pdf",width=10, height=7)
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

# create labels
labs <- c(
            height_avg_cs = "Tree height (m)",
            cvseed_cs = "Interannual crop size variation",
            cofruit_cs = "Overlap in fruit production",
            bcireproductive_log = "log Local abundance",
            endocarp_investment_cs = "Endocarp investment (g)",
            seed_dry_log ="log Seed dry mass (g)",
            proportion_abscised = "Proportion of seeds abscised",
        	seedpred_pres = "Presence of seed predator")

# plot effect sizes and confidence intervals
ggplot(results, aes(x = vars, y= estimate)) +
	geom_pointrange(aes(ymin = conf.low, ymax = conf.high), 
        size = 0.25, fatten = 1)  +
	geom_hline(yintercept = 0, linetype = 2, size = 0.25)  +
    labs(y = "Estimate ± CI [95%]", x = "Plant trait") +
    scale_x_discrete(labels = labs) +
    scale_y_continuous(limits = c(-0.75, 2)) +
    coord_flip() +
    theme_bw(base_size = 7)

ggsave("../output/figures/effectsizes.tiff", 
    device = "tiff", dpi = 350, width = 80, height = 80, units = "mm") 