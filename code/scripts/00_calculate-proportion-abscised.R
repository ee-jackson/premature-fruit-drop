#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 00_calculate-proportion-abscised.R
## Desc: create a clean dataset for downstream analyses: calculate proportion of
##       prematurely abscised seeds, per year, per sp and join with plant and
##       seed trait datasets
## Date: November 2019

# Load packages ---------------------------

library("groundhog")
groundhog_day = "2021-05-01"
groundhog.library("tidyverse", groundhog_day)

# Load data ---------------------------

# seed rain data - from Joe Wright
seed_rain <- read.table(here::here("data", "raw", "BCI_TRAP200_20190215_spcorrected.txt"),
                        header=TRUE, stringsAsFactors = FALSE)

# seed traits - from Joe Wright
seed_trait <- read.csv(here::here("data", "raw","20120227_seedsMassForTraits.csv"),
                       header=TRUE, stringsAsFactors = FALSE)

# plant traits - available here https://doi.org/10.5061/dryad.230j5ch
plant_trait <- read.csv(here::here("data", "raw","TidyTrait.csv"),
                       header=TRUE, stringsAsFactors = FALSE)

# Clean up and join seed rain with seed trait data ---------------------------

# convert fetcha to class Date
seed_rain$fecha <- as.character(seed_rain$fecha)
seed_rain$fecha <- as.Date(seed_rain$fecha, "%Y-%m-%d")
seed_rain$year <- format(as.Date(seed_rain$fecha), "%Y")

# date is from 1987-01-12 to 2019-01-29, we only want complete years so remove year=2019 and year=1987
seed_rain <- subset(seed_rain, seed_rain$year != "1987" & seed_rain$year != "2019")

# rename sp as SP4
seed_rain <- rename(seed_rain, SP4 = sp)

# select columns we want from the seed trait dataset
seed_trait <- seed_trait %>%
   select(LIFEFORM, N_SEEDFULL, SP4, GENUS, SPECIES)

# join seed trait and seed rain by 4 letter species code
seed_dat <- left_join(seed_rain, seed_trait, by = "SP4")

# subset data to only include woody plants and parts 1 (mature fruits), 2 (single diaspores), and 5 (immature fruit)
seed_dat <- subset(seed_dat, part==1|part==2|part==5)
seed_dat <- subset(seed_dat,LIFEFORM== "LIANA"|LIFEFORM== "MIDSTORY"
	|LIFEFORM== "SHRUB"|LIFEFORM== "TREE"|LIFEFORM=="UNDERSTORY")

# remove sp where there is no data for N_SEEDFULL (mean # of seeds per fruit)
seed_dat <- seed_dat %>%
	drop_na("N_SEEDFULL")

glimpse(seed_dat)

# Calculate proportion of seeds absicsed ---------------------------

# create a df that sums quantity of parts by year, sp and part
abs_dat <- seed_dat %>%
	group_by(part, SP4, year, N_SEEDFULL, GENUS, SPECIES, LIFEFORM) %>%
	summarise(quantity_sum= sum(quantity, na.rm = TRUE)) %>%
	ungroup()

# calculate sum of viable seeds

abs_dat_v <- subset(abs_dat, part== 1 | part== 2) %>%
	replace(., is.na(.), 0) %>%
	rowwise() %>%
	mutate(viable_seeds = ifelse(part==1, quantity_sum*N_SEEDFULL,
		quantity_sum)) %>%
	ungroup() %>%
	group_by(SP4, year, GENUS, SPECIES, LIFEFORM) %>%
	summarise(viable_seeds= sum(viable_seeds, na.rm = TRUE)) %>%
	ungroup()

# calculate sum of abscised seeds

abs_dat_a <- subset(abs_dat, part== 5) %>%
	replace(., is.na(.), 0) %>%
	rowwise() %>%
	mutate(abscised_seeds = quantity_sum*N_SEEDFULL) %>%
	ungroup() %>%
	group_by(SP4, year, GENUS, SPECIES, LIFEFORM) %>%
	summarise(abscised_seeds= sum(abscised_seeds, na.rm = TRUE)) %>%
	ungroup()

# join those 2 dfs
prop_dat <- full_join(abs_dat_a, abs_dat_v,
	by= c("SP4", "year", "GENUS", "SPECIES", "LIFEFORM"))

# calculate proportion abscised
prop_dat <- prop_dat %>%
	replace(., is.na(.), 0) %>%
	rowwise() %>%
	mutate(total_seeds = sum(abscised_seeds, viable_seeds, na.rm = TRUE),
	       proportion_abscised = abscised_seeds / total_seeds) %>%
	ungroup()

# create and add a column for the sum of parts found
sum_parts_dat <- abs_dat %>%
	group_by(SP4, year) %>%
	summarise(sum_parts= sum(quantity_sum, na.rm = TRUE)) %>%
	ungroup()

fruit_dat <- left_join(prop_dat, sum_parts_dat, by = c("SP4", "year"))

# Add plant traits to the dataset ---------------------------

# select columns and only keep if n=>200
plant_trait %>%
	subset(TotUnits_collected >= 200) %>%
	rename(SP4 = Codigo, SP6 = Code6) %>%
	select(SP4, SP6, Plant_species19, Family, HEIGHT_AVG, seed_dry,
		cvseed,  CoFruit, BCIReproductive, Endocarp_investment,
		SeedPred_pres) -> plant_trait

# join
fruit_traits <- left_join(fruit_dat, plant_trait, by = "SP4", all.x = TRUE)

# Tidy and save ---------------------------

# make colnames lowercase
colnames(fruit_traits) <- tolower(colnames(fruit_traits))

# change lifeform from caps to title case
fruit_traits$lifeform <- str_to_title(fruit_traits$lifeform, locale = "en")

# need factors when fitting models later
fruit_traits$year <- as.factor(fruit_traits$year)
fruit_traits$sp4 <- as.factor(fruit_traits$sp4)

# only include sp x year data points if there were at least 10 parts found
fruit_traits <- subset(fruit_traits, sum_parts >= 10)

# save as rds
saveRDS(fruit_traits, file = here::here("data", "clean", "fruit_traits.rds"))

# save as csv
write.csv(fruit_traits, row.names = FALSE,
          file = here::here("data", "clean", "fruit_traits.csv"))
