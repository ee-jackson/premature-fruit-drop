# data-dictionary

## fruit_traits.rds
This is the clean data used in the analyses. It is a combination of seed trap data provided by Joe Wright and plant trait data from [Gripenberg _et al._ 2019](https://doi.org/10.1111/ele.13359).

| variable | description |
|:---------|:------------|
| **Identifier variables** | |
| `year` | Calendar year for which the data was collected |
| `sp4` | 4-letter species code of plant species  |
| `sp6` | 6-letter species code of plant species  |
| `species` | Plant species |
| `genus` | Plant genus |
| `family` | Plant family |
| `lifeform` | Lifeform of plant species (Liana, Midstory, Shrub, Tree or Understory) |
| **Seed trap data variables** | |
| `abscised_seeds` | Count of prematurely abscised seeds (endosperm of seeds is not filled) summed across seed traps |
| `viable_seeds` | Count of viable seeds summed across seed traps |
| `total_seeds` | abscised_seeds + viable_seeds |
| `proportion_abscised` | abscised_seeds / total_seeds |
| `sum_parts` | Count of reproductive organs or parts |
| **Plant trait data variables** | |
| `plant_species19` | Plant *genus_species* (valid in January 2019) |
| `height_avg` | Average height of the 5 tallest tree individuals in the 50 ha forest dynamics plot (for free-standing species only) |
| `seed_dry` | Dry seed mass (g) |
| `cvseed` | Variable reflecting interannual variation in seed crop sizes |
| `cofruit` | Temporal overlap in fruit production by other species |
| `bcireproductive` | Number of reproductive-sized individuals in the 50ha plot (for free-standing species only) |
| `endocarp_investment` | Protective tissue content (a measure of the investment in mechanical seed defences) |
| `seedpred_pres` | Seed predator species reared from this plant species (yes=1, no=0) |