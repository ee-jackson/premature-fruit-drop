#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: prematureFruitDrop.R
## Desc: descriptive plots of premature fruit drop at BCI
## Date: March 2020

rm(list = ls())
require(tidyverse)
require(reshape2)
require(ggExtra)
theme_set(theme_bw())

# read dataset

fruitDat <- read.csv("../output/tables/cleanData.csv")
