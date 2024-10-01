# Denunciations at the Inquisitorial trial in Giaveno, Italy, 1335

[![R language](https://img.shields.io/badge/language-R-blue)](https://www.r-project.org/)
[![ERC funding](https://img.shields.io/badge/funding-ERC-green)](https://cordis.europa.eu/project/id/101000442)

This repository contains the data and code to replicate the results of the article:
- Estévez, J. L., Salihović, D., & Sgourev. S. V. (2024). 'Endogenous Dynamics of Denunciation: Evidence from an Inquisitorial Trial'. _PNAS Nexus_, _3_(9), pgae340. https://doi.org/10.1093/pnasnexus/pgae340

## Purpose of the repository

This repository aims to provide transparency and reproducibility for the research conducted on the Inquisitorial trial in Giaveno, Italy, in 1335. 
It includes all data and code used in the analysis presented in the article.
For an overview of the data, see [here](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/blob/main/data/README.md).

## Software requirements

Both the data and code are provided in R format. The results were obtained using R version 4.3.2 in RStudio 2023.09.1. 
The analysis relies on several R packages, listed below with their respective versions:
- [data.table](https://rdatatable.gitlab.io/data.table/) (1.15.4)
- [ggplot2](https://ggplot2.tidyverse.org/) (3.5.1)
- [goldfish](https://stocnet.github.io/goldfish/index.html) (1.6.8)
- [igraph](https://r.igraph.org/) (2.0.3)
- [sna](https://cran.r-project.org/web/packages/sna/index.html) (2.7-2)
- [network](https://cran.r-project.org/web/packages/network/index.html) (1.18.2)
- [netseg](https://mbojan.github.io/netseg/) (1.0-2)
- [survival](https://github.com/therneau/survival) (3.6-4)
- [psych](https://cran.r-project.org/web/packages/psych/) (2.4.3)
- [broom](https://broom.tidymodels.org/articles/broom.html) (1.0.6)
- [pixiedust](https://github.com/nutterb/pixiedust) (0.9.4)
- [MKinfer](https://github.com/stamats/MKinfer) (1.2)
- [ggpubr](https://rpkgs.datanovia.com/ggpubr/) (0.6.0)
- [patchwork](https://patchwork.data-imaginist.com/) (1.2.0)

## File list

The repository includes seven R scripts and a data folder:
- [1_Castellario_data_exploration.R](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/blob/main/1_Castellario_data_exploration.R)
- [2_Castellario_kinship_ties_and_congregations.R](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/blob/main/2_Castellario_kinship_ties_and_congregations.R)
- [3_Castellario_preliminary_analyses.R](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/blob/main/3_Castellario_preliminary_analyses.R)
- [4_Castellario_DyNAM_REM_analyses.R](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/blob/main/4_Castellario_DyNAM_REM_analyses.R)
- [4_Castellario_DyNAM_REM_analyses (time in days).R](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/blob/main/4_Castellario_DyNAM_REM_analyses%20(time%20in%20days).R)
- [5_Castellario_descriptive_table.R](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/blob/main/5_Castellario_descriptive_table.R)
- [6_Saturation_graph.R](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/blob/main/6_Saturation_graph.R)
- [data](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/tree/main/data)

## Instructions for use

- Ensure you have the required version of R and RStudio installed.
- Install the necessary packages using the specified versions.
- Load the data from the data/data.RData file.
- Run the R scripts in the order listed to replicate the analysis and results.
- The scripts [4_Castellario_DyNAM_REM_analyses.R](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/blob/main/4_Castellario_DyNAM_REM_analyses.R) and [4_Castellario_DyNAM_REM_analyses (time in days).R](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/blob/main/4_Castellario_DyNAM_REM_analyses%20(time%20in%20days).R) produce the same results, differing only in the scale of time and the intercept of eventual results, which are expressed either in seconds or days. In the article, we present the results expressed in days.

## Source

The data was collected from Merlo, Grado G. 1977. _Eretici e inquisitori nella società piemontese del Trecento: con l’edizione dei processi tenuti a Giaveno dall’inquisitore Alberto De Castellario (1335) e nelle Valli di Lanzo dall’inquisitore Tommaso Di Casasco (1373)_. Turin: Claudiana Editrice. 
The original Latin manuscript is held at the Archives of the Order of Preachers in Rome (Archivio generale dell’Ordine dei Predicatori, Rome, MS II.64, ff. 1r-111v).

## Funding

This research received support from the European Research Council (ERC), under the European Union’s Horizon 2020 research and innovation program (grant agreement No. 101000442, project [“Networks of Dissent: Computational Modelling of Dissident and Inquisitorial Cultures in Medieval Europe”](https://cordis.europa.eu/project/id/101000442)). 

Open access funded by Helsinki University Library.

## Citation

- Estévez, J. L., Salihović, D., & Sgourev. S. V. (2024). 'Endogenous Dynamics of Denunciation: Evidence from an Inquisitorial Trial'. _PNAS Nexus_, _3_(9), pgae340. https://doi.org/10.1093/pnasnexus/pgae340

## Contact information

For any questions, please contact:
- José Luis Estévez (joseluisesna@gmail.com)
- Davor Salihović (davor.salihovic@gmail.com)
