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
| `lifeform` | Lifeform of plant species (Liana, Midstory, Shrub, Tree or Understory) |
| **Seed trap data variables** | |
| `abscised_seeds` | Count of prematurely abscised seeds (endosperm of seeds is not filled) summed across seed traps |
| `viable_seeds` | Count of viable seeds (endosperm of seeds is filled) summed across seed traps |
| `total_seeds` | abscised_seeds + viable_seeds |
| `proportion_abscised` | abscised_seeds / total_seeds |
| `sum_parts` | Count of reproductive organs or parts (fruits + single diaspores) |
