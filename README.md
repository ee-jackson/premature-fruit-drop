# premature-fruit-drop

This repository contains code and data associated with the article: __Premature fruit drop in a tropical forest plant community__ and upon publication, will be archived in the Dryad Digital Repository.

## Datasets used in this project

The seed rain dataset, `BCI_TRAP200_20190215_spcorrected.txt`, and seed trait dataset, `20120227_seedsMassForTraits.csv`, were provided by [Joe Wright](https://stri.si.edu/scientist/s-joseph-wright).

The plant trait dataset, `TidyTrait.csv`, and phylogenetic tree, `PhyloExtraSpec.tree`, are in the [Dryad Digital Repository](https://doi.org/10.5061/dryad.230j5ch) and are associated with [Gripenberg _et al._ 2019](https://doi.org/10.1111/ele.13359).

Upon publication, the clean dataset, `fruit_traits.csv`, will be made available. This data was created using the script `00_calculate-proportion-abscised.R`, which you can find within this repository. It contains species-specific yearly seed abscission rates and selected plant population attributes, collated from the sources listed above.

## Contents:

### data/
The original data is stored in the `data/raw/` subdirectory. Any data that is produced using code is stored in `data/clean/`.

### code/
The `code/` directory contains two subdirectories, `exploration/` which contains R Markdown exploratory analyses, and `scripts/` which contains all the code for cleaning, combining, and analysing the data. All paths in the scripts are relative to the root directory (where the .Rproj file lives).

Each .R script has a summary at the top of what it does. The scripts are numbered in the order in which they would typically be run. All R Markdown files in `exploration/` are knitted to `github_documents` to make the GitHub repo browsable.

### output/
The `output/` directory contains the subdirectories `figures/` and `results/`, which contain the figures used in the paper and other output from analyses, respectively.

### docs/
The `docs/` directory contains the data dictionary and any other relevant documents.
