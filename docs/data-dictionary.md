# data-dictionary

## fruit_drop.csv
This file contains species-specific yearly seed abscission rates which were calculated using seed rain and seed trait datasets provided by Joe Wright. It was created using the script `00_calculate-proportion-abscised.R`.

| variable | description |
|:---------|:------------|
| **Identifier variables** | |
| `year` | Calendar year for which the data was collected |
| `sp4` | 4-letter species code of plant species  |
| `species` | Plant species |
| `genus` | Plant genus |
| **Seed trap data variables** | |
| `abscised_seeds` | Count of prematurely abscised seeds (endosperm of seeds is not filled) summed across seed traps |
| `viable_seeds` | Count of viable seeds (endosperm of seeds is filled) summed across seed traps |
| `total_seeds` | abscised_seeds + viable_seeds |
| `proportion_abscised` | abscised_seeds / total_seeds |
| `sum_parts` | Count of reproductive organs or parts (fruits + single diaspores) |

## fruit_traits.rds
This is the clean data used in the analyses. It is a combination of `fruit_drop.csv` and plant trait data from [Gripenberg _et al._ 2019](https://doi.org/10.1111/ele.13359). It was created using the script `01_combine-data.R`.

| variable | description |
|:---------|:------------|
| **Identifier variables** | |
| `year` | Calendar year for which the data was collected |
| `sp4` | 4-letter species code of plant species  |
| `sp6` | 6-letter species code of plant species  |
| `species` | Plant species |
| `genus` | Plant genus |
| `plant_species19` | Plant *genus_species* (valid in January 2019) |
| **Seed trap data variables** | |
| `abscised_seeds` | Count of prematurely abscised seeds (endosperm of seeds is not filled) summed across seed traps |
| `viable_seeds` | Count of viable seeds (endosperm of seeds is filled) summed across seed traps |
| `total_seeds` | abscised_seeds + viable_seeds |
| `proportion_abscised` | abscised_seeds / total_seeds |
| `sum_parts` | Count of reproductive organs or parts (fruits + single diaspores) |
| **Plant trait data variables** | |
| `height_avg` | Average height of the 5 tallest tree individuals in the 50 ha forest dynamics plot (for free-standing species only) |
| `seed_dry` | Dry seed mass (g) where a ‘seed’ is defined to include the endosperm and embryo only |
| `cvseed` | Temporal variation in seed crop sizes. Variable analogous to CVyear in [Wright et al., 2005](http://doi.wiley.com/10.1890/03-0750) |
| `cofruit` | Temporal overlap in fruit production by other species. The total number of other species observed to fruit in the same week as a given species |
| `bcireproductive` | Local abundance of conspecifics. Number of reproductive-sized individuals in the 50ha plot (for free-standing species only), estimated using data from the 2010 census of the ForestGEO plot |
| `endocarp_investment` | Investment in mechanical seed defences. The proportion of diaspore mass made up by protective tissue (e.g. endocarps and seed coats) rather than seed mass|
| `seedpred_pres` | Seed predator species reared from this plant species (yes = 1, no = 0) |
