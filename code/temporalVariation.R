#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: temporalVariation.R
## Desc: generate descriptive plots of temporal variation in fruit abscission
## Date: May 2020

rm(list = ls())

#load packages
require(tidyverse)
require(lme4)
require(ggpubr)
require(reshape2)

fruitDatFull <- read.csv("../output/tables/fullCleanData.csv")

fruitDatFull %>%
	mutate(seedpred_pres = factor(ifelse(seedpred_pres == 0, "No", "Yes"))) %>%
	filter(total_seeds>50) -> fruitDatFull 

fruitDatFull$plant_species19 <- ifelse(is.na(fruitDatFull$plant_species19), paste(fruitDatFull$genus, fruitDatFull$species, sep="_"), paste(fruitDatFull$plant_species19))

fruitDatFull$abscised_seeds <- round(fruitDatFull$abscised_seeds)
fruitDatFull$viable_seeds <- round(fruitDatFull$viable_seeds)

fruitDatFull %>%
	group_by(year) %>%
	mutate(total_seeds_per_yr= sum(viable_seeds + abscised_seeds)) %>%
	mutate(total_seeds_per_yr_log = log(total_seeds_per_yr)) %>%
	ungroup() -> fd2

fd2 %>% 
	group_by(sp) %>%
	filter(length(unique(year))>=29) %>%
	ungroup() -> sub

#######################

ggplot(sub) +
	geom_boxplot(aes(x = reorder(plant_species19, proportion_abscised), y = proportion_abscised)) +
	scale_y_continuous(expand= c(0, 0)) +
	xlab("Species") +
	ylab("Proportion of seeds abscised") +
	theme_bw(base_size=20) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(fruitDatFull) +
	geom_boxplot(aes(x = year, y = proportion_abscised, group=year), fill="blue", alpha=0.2) +
	scale_y_continuous(expand= c(0, 0)) +
	xlab("Year") +
	ylab("Proportion of seeds abscised") +
	theme_bw(base_size=50)



ggplot(sub) +
	geom_point(aes(x = year, y = proportion_abscised), size=2) +
	geom_smooth(se=FALSE, aes(x = year, y = proportion_abscised)) +
	facet_wrap(~plant_species19) +
	scale_y_continuous(limits=c(0,1), expand= c(0.01, 0.01)) +
	xlab("Year") +
	ylab("Proportion of seeds abscised") +
	theme_bw(base_size=30) +
	theme(axis.title=element_text(size=45))

ggsave("../output/plots/yearBySp.png", dpi="screen")


ggplot(sub) +
	geom_point(aes(x = total_seeds_per_yr, y = proportion_abscised), size=2) +
	geom_smooth(se=FALSE, aes(x = total_seeds_per_yr, y = proportion_abscised)) +
	facet_wrap(~plant_species19) +
	scale_y_continuous(limits=c(0,1), expand= c(0.01, 0.01)) +
	xlab("Total seeds") +
	ylab("Proportion of seeds abscised") +
	theme_bw(base_size=30) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title=element_text(size=45))

ggsave("../output/plots/totalFruits.png", dpi="screen")

######################### MODELS

m1 <- lme4::glmer(cbind(abscised_seeds, viable_seeds)~ total_seeds_per_yr_log + (1|sp), family= binomial(logit), data= sub2)

fd2 %>% 
	group_by(sp) %>%
	filter(length(unique(year))>=10) %>%
	ungroup() -> sub2

vars <- unique(sub2$plant_species19)

models <- lapply(setNames(vars, vars), function(var) {
	glm(cbind(abscised_seeds, viable_seeds)~ total_seeds_per_yr, family= binomial(logit), data= subset(sub2, plant_species19== var))
	}
)

# compute statistics about each of the coefficients for each model
res_anova <- purrr::map_dfr(models, broom::tidy, conf.int = TRUE, .id = "vars")

# we are interested in rows where term = the variable
res_anova[!grepl("(Intercept)", res_anova$term),] %>%
	rename(species = vars) -> results

# get degrees of freedom and add to results table
melt(as.data.frame(lapply(models, df.residual))) -> degfree
colnames(degfree) <- c("species", "df")
left_join(results, degfree, by = "species") -> results2

#add overdispersion ratio
melt(sapply(models, overdisp_fun)) %>%
	subset(Var1=="ratio") %>%
	dplyr::select(species = Var2, od_ratio = value) -> od

left_join(results2, od, by = "species") -> results3

write.csv(results3,"../output/tables/totalSeedsPerYearGlms_nolog.csv")

######## try quasi

models2 <- lapply(setNames(vars, vars), function(var) {
	glm(cbind(abscised_seeds, viable_seeds)~ total_seeds_per_yr, family= quasibinomial(logit), data= subset(sub2, plant_species19== var))
	}
)

summary_tables <- function (model, modelname) {
    coef(summary(models2[[modelname]]))
}

melt(imap(models2, summary_tables)) %>%
	subset(Var1 == "total_seeds_per_yr") %>%
	dplyr::select(-Var1) ->ah

quasi_summary <- dcast(ah, L1 ~ Var2, value.var="value")

write.csv(quasi_summary,"../output/tables/totalSeedsPerYearGlms_quasi_nolog.csv")

### GAMs for temporal data
mod_gam1 <- mgcv::gam(cbind(abscised_seeds, viable_seeds) ~ s(year, k=31) + s(sp, bs="re"), data=sub2, family=binomial(logit))
summary(mod_gam1)
gam.check(mod_gam1)
anova(mod_gam1)
plot(mod_gam1)

# plot for 1 sp
testdata <- data.frame(year = seq(1988, 2018, 1),
                      sp = "ALCC")
fits <- predict(mod_gam1, newdata=testdata, type='response', se=T)
predicts <- data.frame(testdata, fits) %>% 
  mutate(lower = fit - 1.96*se.fit,
         upper = fit + 1.96*se.fit)

plot_mod_gam2_response <- ggplot() +
  geom_point(data=subset(sub2,sp=="ALCC"), aes(x=year, y=proportion_abscised)) +
  geom_ribbon(data=predicts,aes(x=year,y=fit,ymin = lower, ymax=upper), fill='gray90') +
  geom_line(data=predicts,aes(x=year,y=fit), color='#00aaff') +
  theme_bw()

# fit a GAM for each sp in our subset
vars <- unique(sub$plant_species19)

gams <- lapply(setNames(vars, vars), function(var) {
	mgcv::gam(cbind(abscised_seeds, viable_seeds) ~ s(year), data= subset(sub, plant_species19== var), family=binomial(logit))
	}
)

res_anova <- purrr::map_dfr(gams, broom::tidy, conf.int = TRUE, .id = "vars")
res_anova
write.csv(res_anova, "../output/tables/GAMcoef.csv")

plot_data_var <- function (model, modelname) {
    testdata <- data.frame(year = seq(1988, 2018, 1),
                      sp = modelname)
	fits <- predict(model, newdata=testdata, type='response', se=TRUE)
	predicts <- data.frame(testdata, fits) %>% 
		mutate(lower = fit - 1.96*se.fit,
         upper = fit + 1.96*se.fit)
    ggplot() +
        geom_point(data= subset(sub, plant_species19== modelname), aes_string(x = "year", y = "proportion_abscised"), size=0.4) +
        geom_line(data= predicts, aes_string(x = "year", y = "fit"), colour= "blue", size=0.3) +
        geom_ribbon(data= predicts, aes_string(x= "year", y="fit", ymin="lower", ymax="upper"), alpha= 0.1, fill="blue") +
        ylab("Proportion of\nseeds abscised") +
        scale_y_continuous(limits=c(0.0,1.0)) +
        ggtitle(modelname) +
        theme_bw(base_size=10)
}

mygamplots <- imap(gams, plot_data_var)

ggpubr::ggarrange(plotlist = mygamplots)

ggsave("../output/plots/GAMs.png", dpi="retina")

### accounting for autocorrelation

vars <- unique(sub$plant_species19)

gamms <- lapply(setNames(vars, vars), function(var) {
	mgcv::gamm(cbind(abscised_seeds, viable_seeds) ~ s(year), data= subset(sub, plant_species19== var), family=binomial(logit), correlation=corAR1(form = ~year))
	}
)

# subset to just gams
gamms <- lapply(gamms, function(g) { g$gam })

# get tidy table of coefficents
res_anova <- purrr::map_dfr(gamms, broom::tidy, conf.int = TRUE, .id = "vars")
res_anova
write.csv(res_anova, "../output/tables/GAMMcoef.csv")



# 95 percentile range se
# but check this https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/
plot_data_var <- function (model, modelname) {
    testdata <- data.frame(year = seq(1988, 2018, 1),
                      sp = modelname)
	fits <- predict(model, newdata=testdata, type='response', se=T)
	predicts <- data.frame(testdata, fits) %>% 
		mutate(lower = fit - 1.96*se.fit,
         upper = fit + 1.96*se.fit)
    ggplot() +
        geom_point(data= subset(sub, plant_species19== modelname), aes_string(x = "year", y = "proportion_abscised"), size=0.4) +
        geom_line(data= predicts, aes_string(x = "year", y = "fit"), colour= "blue", size=0.3) +
        geom_ribbon(data= predicts, aes_string(x= "year", y="fit", ymin="lower", ymax="upper"), alpha= 0.1, fill="blue") +
        ylab("Proportion of\nseeds abscised") +
        scale_y_continuous(limits=c(0.0,1.0)) +
        ggtitle(modelname) +
        theme_bw(base_size=10)
}

mygammplots <- imap(gamms, plot_data_var)

ggpubr::ggarrange(plotlist = mygammplots)

ggsave("../output/plots/GAMMs.png", dpi="retina")
