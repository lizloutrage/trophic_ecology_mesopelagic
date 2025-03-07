
# :fish: :fork_and_knife: Niche partitioning among the deep pelagic fish community in the Bay of Biscay, Northeast Atlantic

<!-- badges: start -->

[![License:
GPL-2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
<!-- badges: end -->

The aim of the `trophic_ecology_mesopelagic` project is to reproduce
analysis performed is the paper by Loutrage et al., 2024
(<https://doi.org/10.1016/j.dsr.2024.104347>)

## Data:

All raw data are available in the online directory data.InDoRES platform
<https://data.indores.fr/> (any use must include the reference and DOI):

- DOI of the trawling data set: <https://doi.org/10.48579/PRO/AIKOEB>
- DOI of the isotopic data set: <https://doi.org/10.48579/PRO/D7MBHB>

## Overview

Here is an overview of `trophic_ecology_mesopelagic` content:

- [`data/`](https://github.com/lizloutrage/trophic_ecology_mesopelagic/tree/main/data):
  contains all raw data required to perform analyses

- [`R/`](https://github.com/lizloutrage/trophic_ecology_mesopelagic/tree/main/R):
  contains the functions to run the analysis

- [`_targets`](https://github.com/lizloutrage/trophic_ecology_mesopelagic/tree/main/index.qmd):
  contains the workflow of the analyses

- [`index.qmd`](https://github.com/lizloutrage/trophic_ecology_mesopelagic/tree/main/index.qmd):
  contains the final report to be knitted

- [`figures/`](https://github.com/lizloutrage/trophic_ecology_mesopelagic/tree/main/figures):
  contains the figures in high resolution

## Workflow

- calculation of the isotopic niches of species at community level
  (i.e. with all depths grouped together) and calculation of niche
  overlaps between species
- Determining trophic guilds using a clustering method, then comparing
  the depth distribution of species within trophic guilds to determine
  whether species use depth to segregate.
- calculation of isotopic diversity indices (Cucherousset & Villéger,
  2015)
- comparison of the niche size values for each species with the values
  obtained under a null model (at random)
- comparison of the sum of niche overlap values within each depth layer
  with the values obtained under a null model (at random)

## Comment

- We have used the **targets workflow** to run the analysis
  \[`_targets`\]. To run the analysis run “tar_make()” and to view the
  workflow run “tar_visnetwork()”.
- The default parameters applied are 10,000 bivariate random samples
  with a replacement of n = 10, therefore the analysis takes some time
  to run, you can change these parameters to reduce the analysis time.
- the si_div function comes from the paper by Cucherousset & Villéger
  2015 (DOI: 10.1016/j.ecolind.2015.03.032)
- the functions for comparing the size of the isotopic niche of species
  and their overlap with null models were inspired by the approach of
  Suchomel et al. (2022) (DOI: 10.3390/d14080689) and Brind’Amour &
  Dubois (2013) (DOI: 10.1371/journal.pone.0084198)

## Code of Conduct

Please note that the `trophic_ecology_mesopelagic` project is released
with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
