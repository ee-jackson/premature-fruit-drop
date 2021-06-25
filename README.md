# premature-fruit-drop

This repository contains code and data for the project: __Premature fruit drop in a tropical forest plant community__ and is mirrored in the Dryad Digital Repository.

## Data sets used in this project

The seed rain data set, `BCI_TRAP200_20190215_spcorrected.txt`, and seed trait data set, `20120227_seedsMassForTraits.csv`, were provided by [Joe Wright](https://stri.si.edu/scientist/s-joseph-wright).

The plant trait data set, `TidyTrait.csv`, and phylogenetic tree, `PhyloExtraSpec.tree`, are in the [Dryad Digital Repository](https://doi.org/10.5061/dryad.230j5ch) and are associated with [Gripenberg _et al._ 2019](https://doi.org/10.1111/ele.13359).

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