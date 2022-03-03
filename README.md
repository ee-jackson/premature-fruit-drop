# premature-fruit-drop

[ <img src="https://raw.githubusercontent.com/ee-jackson/premature-fruit-drop/main/docs/graphical-abstract.png" width=300 align = right>](https://github.com/ee-jackson/premature-fruit-drop/blob/main/docs/graphical-abstract.png)

[![DOI](https://zenodo.org/badge/218278065.svg)](https://zenodo.org/badge/latestdoi/218278065)

This repository contains code associated with the article: 

__Jackson E. E., Wright S. J., Calderón O., Bullock J. M., Oliver T. H., & Gripenberg S. (2022). Pre-dispersal seed predation could help explain premature fruit drop in a tropical forest. *Journal of Ecology.* DOI:[ 10.1111/1365-2745.13867](https://doi.org/10.1111/1365-2745.13867)__

Contact: eleanor.elizabeth.j@gmail.com  

<br>

## Abstract
1.  Pre-dispersal seed mortality caused by premature fruit drop is a potentially important source of plant mortality, but one which has rarely been studied in the context of tropical forest plants. Of particular interest is premature fruit drop triggered by enemies, which – if density-dependent – could contribute to species co-existence in tropical forest plant communities.
2.  We used a long-term (31 year) dataset on seed and fruit fall obtained through weekly collections from a network of seed traps in a lowland tropical forest (Barro Colorado Island, Panama) to estimate the proportion of seeds prematurely abscised for 201 woody plant species. To determine whether enemy attack might contribute to premature fruit drop we tested whether plant species abscise more of their fruit prematurely if they: (1) have attributes hypothesised to be associated with high levels of enemy attack, and (2) are known to be attacked by one enemy-group (insect seed predators). We also tested (3) whether mean rates of premature fruit drop for plant species are phylogenetically conserved.
3.  Overall rates of premature fruit drop were high in the plant community. Across all species, 39% of seeds were abscised before completing their development. Rates of premature seed abscission varied considerably among species and could not be explained by phylogeny. Premature seed abscission rates were higher in species which are known to host pre-dispersal insect seed predators and species with attributes that were hypothesised to make them more susceptible to attack by pre-dispersal enemies, namely species which (1) have larger seeds, (2) have a greater average height, (3) have temporally predictable fruiting patterns, and (4) are more abundant at the study site.
4.  _Synthesis._ Premature fruit drop is likely to be a major source of seed mortality for many plant species on Barro Colorado Island. It is plausible that pre-dispersal seed enemies, such as insect seed predators, contribute to community-level patterns of premature fruit drop and have the potential to mediate species co-existence through stabilising negative density dependence. Our study suggests that the role of pre-dispersal enemies in structuring tropical plant communities should be considered alongside the more commonly studied post-dispersal seed and seedling enemies.

## Datasets used in this project

The yearly seed abscission dataset, `fruit_drop.csv`, is archived in the Dryad Digital Repository at [doi.org/10.5061/dryad.4mw6m909j](https://doi.org/10.5061/dryad.4mw6m909j).

The plant trait dataset, `TidyTrait.csv`, and phylogenetic tree, `PhyloExtraSpec.tree`, are in the Dryad Digital Repository at [doi.org/10.5061/dryad.230j5ch](https://doi.org/10.5061/dryad.230j5ch) and are associated with [Gripenberg _et al._ 2019](https://doi.org/10.1111/ele.13359).

## Contents:

### [`code/`](code/)
The [`code/`](code/) directory contains two subdirectories, [`exploration/`](code/exploration/) which contains R Markdown exploratory analyses, and [`scripts/`](code/scripts/) which contains all the code for cleaning, combining, and analysing the data. All paths in the scripts are relative to the root directory (where the `.Rproj` file lives).

Each `.R` script has a summary at the top of what it does. The scripts are numbered in the order in which they would typically be run. All `.Rmd` files in [`exploration/`](code/exploration/) are knitted to `github_documents` to make the GitHub repo browsable.

### `data/`
The original data is stored in the `data/raw/` subdirectory. Any data that is produced using code is stored in `data/clean/`.

### [`output/`](output/)
The [`output/`](output/) directory contains the subdirectories [`figures/`](output/figures/) and [`results/`](output/results/), which contain the figures used in the paper and other output from analyses, respectively.

### [`docs/`](docs/)
The `docs/` directory contains the data dictionary and any other relevant documents.

## Usage
To reproduce results and figures from the article, run the `.R` scripts in [`code/scripts/`](code/scripts/) in the order in which they are labelled. The first script will download the required data from Dryad.

## Requirements
See session info in [`docs/session-info.txt`](docs/session-info.txt)

## License
Code is under a [MIT license](LICENSE.md)
