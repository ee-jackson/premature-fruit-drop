#!/usr/bin/env Rscript
rm(list = ls()) 

#load packages
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
library(data.table)
library(ggExtra)
library(purrr)
library(Hmisc)
library(raster)
theme_set(theme_bw())

#########
# load and combine datasets

sumDat <- read.csv("../output/tables/summarizeSeedRain.csv", header=TRUE, stringsAsFactors = FALSE)
seedTrait <- read.csv("../data/20120227_seedsMassForTraits.csv", header=TRUE, stringsAsFactors = FALSE)
tidyTraits <- read.csv("../data/TidyTrait.csv", header=TRUE, stringsAsFactors = FALSE)

seedTrait <- rename(seedTrait, sp = SP4) 
tidyTraits <- rename(tidyTraits, sp = Codigo) 

allTraits <- list(sumDat, seedTrait, tidyTraits) %>% 
	reduce(left_join, by = c("sp"="sp")) %>%
	select_if(is.numeric) %>%
	dplyr::select(-c(X,Coleo_pres, Hymeno_pres, Lepid_pres, SeedPred_pres))

########

res<-rcorr(as.matrix(allTraits), type ="spearman")

res$P
res$r

# function to flatten the matrix into 2 cols
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
    )
}

# flatten matrix to table
cormat<-flattenCorrMatrix(res$r, res$P)

# look at only what correlates with proportion abscised
cormat <-subset(cormat,cormat$row=="proportion_abscised")

## graphical plot ##

library(PerformanceAnalytics)

# look at just seed traits beacuse full dataset is too big
seedTraits <- dplyr::select(allTraits, c(FRUIT_FRSH,FRUIT_DRY, N_SEEDFULL, N_SEEDEMPTY,DSPR_FRESH, DSPR_DRY,SEED_FRESH,SEED_DRY))

#Spearman is a non-parametric, thus it is not possible to get CIs. There is a error message because R cannot compute exact p values (the test is based on ranks, we have few cars with the same hp or wt).
#We can get rid off the warning letting R know that approximate values are fine
chart.Correlation(seedTraits, histogram=TRUE, pch=20, method="spearman", exact=FALSE)
ggplot2::ggsave("../output/plots/20191203/chart.corr_seedtraits_spearman.png")

chart.Correlation(seedTraits, histogram=TRUE, pch=20, method="pearson")
ggplot2::ggsave("../output/plots/20191203/chart.corr_seedtraits_pearson.png")

## correlation plot ##

library(corrplot)
corrplot(res$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

##add p values to above plot

# Insignificant correlation are crossed
corrplot(res$r, type="upper", order="hclust", 
         p.mat = res$P, sig.level = 0.05, insig = "label_sig")
# Insignificant correlations are left blank
corrplot(res$r, type="upper", order="hclust", 
         p.mat = res$P, sig.level = 0.05, insig = "blank")
ggsave("../output/plots/20191203/corrplot_alltraits_spearman.png")

library(ggcorrplot)

ggcorrplot::ggcorrplot(cor(seedTraits, use = "complete.obs"), p.mat = cor_pmat(seedTraits, use = "complete.obs"), hc.order=TRUE, type='lower',lab=FALSE, sig.level=0.05,insig='blank')

ggcorrplot::ggcorrplot(cor(dplyr::select(allTraits, -Hymeno_n), use = "complete.obs"), p.mat = cor_pmat(allTraits, use = "complete.obs"), insig='blank',hc.order=TRUE, type='lower',lab=FALSE, sig.level=0.05)

#####
library(GGally)
GGally::ggpairs(seedTraits, alpha=0.8)
ggsave("../output/plots/20191203/pairs_seedtraits.png")

