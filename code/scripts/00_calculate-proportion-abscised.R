#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 00_calculate-proportion-abscised.R
## Desc: Calculate proportion of prematurely abscised seeds,
##       per year, per sp
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

# Clean up and join seed rain with seed trait data ---------------------------

## clean seed rain
# convert fetcha to class Date
seed_rain$fecha <- as.character(seed_rain$fecha)
seed_rain$fecha <- as.Date(seed_rain$fecha, "%Y-%m-%d")
seed_rain$year <- format(as.Date(seed_rain$fecha), "%Y")

# date is from 1987-01-12 to 2019-01-29, we only want complete years so remove year=2019 and year=1987
seed_rain <- subset(seed_rain, seed_rain$year != "1987" & seed_rain$year != "2019")

# rename sp as SP4
seed_rain <- rename(seed_rain, sp4 = sp)

## clean seed trait
# make colnames lowercase
colnames(seed_trait) <- tolower(colnames(seed_trait))

## join
# join seed trait and seed rain by 4 letter species code
seed_dat <- left_join(seed_rain, seed_trait, by = "sp4")

# subset data to only include woody plants and parts 1 (mature fruits), 2 (single diaspores), and 5 (immature fruit)
# remove sp where there is no data for n_seedfull (mean # of seeds per fruit)
seed_dat %>%
  filter(part == 1|part == 2|part == 5) %>%
  filter(lifeform == "LIANA"|lifeform == "MIDSTORY"|
           lifeform == "SHRUB"|lifeform == "TREE"|lifeform =="UNDERSTORY") %>%
  drop_na("n_seedfull") %>%
  select(sp4, genus, species, year, n_seedfull, part, quantity) -> seed_dat

# check for any NAs
sapply(seed_dat, function(x) sum(is.na(x)))

# Calculate proportion of seeds absicsed ---------------------------

# create a df that sums quantity of parts by year, sp and part
abs_dat <- seed_dat %>%
	group_by(part, sp4, year, n_seedfull, genus, species) %>%
	summarise(quantity_sum = sum(quantity, na.rm = TRUE)) %>%
	ungroup()

# calculate sum of viable seeds
abs_dat_v <- subset(abs_dat, part == 1 | part == 2) %>%
	rowwise() %>%
	mutate(viable_seeds = ifelse(part ==1, quantity_sum*n_seedfull,
		quantity_sum)) %>%
	ungroup() %>%
	group_by(sp4, year, genus, species) %>%
	summarise(viable_seeds = sum(viable_seeds, na.rm = TRUE)) %>%
	ungroup()

# calculate sum of abscised seeds
abs_dat_a <- subset(abs_dat, part == 5) %>%
	rowwise() %>%
	mutate(abscised_seeds = quantity_sum*n_seedfull) %>%
	ungroup() %>%
	group_by(sp4, year, genus, species) %>%
	summarise(abscised_seeds= sum(abscised_seeds, na.rm = TRUE)) %>%
	ungroup()

# join those 2 dfs
prop_dat <- full_join(abs_dat_a, abs_dat_v,
	by= c("sp4", "year", "genus", "species"))

# calculate proportion abscised
prop_dat <- prop_dat %>%
	replace(., is.na(.), 0) %>%
	rowwise() %>%
	mutate(total_seeds = sum(abscised_seeds, viable_seeds, na.rm = TRUE),
	       proportion_abscised = abscised_seeds / total_seeds) %>%
	ungroup()

# create and add a column for the sum of parts found
sum_parts_dat <- abs_dat %>%
	group_by(sp4, year) %>%
	summarise(sum_parts = sum(quantity_sum, na.rm = TRUE)) %>%
	ungroup()

fruit_dat <- left_join(prop_dat, sum_parts_dat, by = c("sp4", "year"))

# Check and save ---------------------------

glimpse(fruit_dat)

# save as csv
write.csv(fruit_dat, row.names = FALSE,
          file = here::here("data", "clean", "fruit_drop.csv"))
