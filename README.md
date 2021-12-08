# premature-fruit-drop

This repository contains code associated with the article: __Does pre-dispersal seed predation explain premature fruit drop in a tropical forest?__ _Jackson E. E., Wright S. J., Calderón O., Bullock J. M., Oliver T. H., & Gripenberg S._

Currently under peer review at the Journal of Ecology

Contact: eleanor.elizabeth.j@gmail.com

## Datasets used in this project

The plant trait dataset, `TidyTrait.csv`, and phylogenetic tree, `PhyloExtraSpec.tree`, are in the [Dryad Digital Repository](https://doi.org/10.5061/dryad.230j5ch) and are associated with [Gripenberg _et al._ 2019](https://doi.org/10.1111/ele.13359).

The yearly seed abscission dataset, `fruit_drop.csv`, is archived in the [Dryad Digital Repository](https://doi.org/10.5061/dryad.4mw6m909j).

## Contents:

### code/
The `code/` directory contains two subdirectories, `exploration/` which contains R Markdown exploratory analyses, and `scripts/` which contains all the code for cleaning, combining, and analysing the data. All paths in the scripts are relative to the root directory (where the .Rproj file lives).

Each `.R` script has a summary at the top of what it does. The scripts are numbered in the order in which they would typically be run. All `.Rmd` files in `exploration/` are knitted to `github_documents` to make the GitHub repo browsable.

### data/
The original data is stored in the `data/raw/` subdirectory. Any data that is produced using code is stored in `data/clean/`.

### output/
The `output/` directory contains the subdirectories `figures/` and `results/`, which contain the figures used in the paper and other output from analyses, respectively.

### docs/
The `docs/` directory contains the data dictionary and any other relevant documents.

## Usage
To reproduce results and figures from the article, run the scripts in `code/scripts/` in the order in which they are labelled. The first script will download the required data from Dryad.

## Requirements
See session info in [`docs/session-info.txt`](https://github.com/ee-jackson/premature-fruit-drop/blob/main/docs/session-info.txt)

## License
Code is under a [MIT license](https://github.com/ee-jackson/premature-fruit-drop/blob/main/LICENSE.md).
