# Denunciations at the Inquisitorial trial in Giaveno, Italy, 1335 (data overview)

![R/CSV format](https://img.shields.io/badge/data_format-R/CSV-blue)
[![ERC funding](https://img.shields.io/badge/funding-ERC-green)](https://cordis.europa.eu/project/id/101000442)

## Description

The dataset is stored in an RData file named data.RData. This file contains a list called `castellario` (named after the surname of the inquisitor overseeing the trial) with six data frame objects: `nodes`, `edges`, `congregations`, `groups`, `kinship`, and `keyevents`. Below is a detailed description of each data frame:

`nodes`: A 351 x 7 data frame providing person-level information. The variables include:
- `id`: Unique identifier.
- `label`: Name of the individual.
- `sex`: Gender of the individual ("m" for males, "f" for females).
- `dead`: Dichotomous indicator of whether the individual was deceased at the time of the trial.
- `occupation_type`: Type of profession.
- `origin_or_residence`: Place of origin or residence.
- `waldensian_master`: Dichotomous indicator of whether the individual was identified as a Waldensian master from another region.

`edges`: A 1,074 x 4 data frame documenting the denunciations reported to the inquisitor in an enriched edge list format. The columns include:
- `source`: ID of the denouncer.
- `target`: ID of the person or group being denounced. If nobody, the value reads `NA` (not applicable).
- `time`: Date when the denunciation was reported.
- `summoned`: Dichotomous indicator of whether the denouncer was summoned to appear before the inquisitor.

`congregations`: A 226 x 5 data frame containing information about the congregations reported. The columns are:
- `id`: ID of the congregation.
- `label`: Description of the congregation, including details about the household where it was held.
- `denouncer`: ID of the person who reported the congregation.
- `participants`: IDs of the individuals participating in the congregation, separated by a hashtag (#) symbol.
- `ministers`: IDs of the individual(s) acting as minister(s) of the congregation, separated by a hashtag (#) symbol.

`groups`: A 68 x 3 data frame with details about the collective entities reported. The columns are:
- `id:` ID of the group.
- `label`: Phrase (in Latin) as captured in the register.
- `members`: List of identifiable individuals who were part of the group.

`kinship`: A 191 x 3 data frame containing an edge list of individuals with some form of kinship affiliation. The columns include:
- `id1`: ID of person i.
- `id2`: ID of person j.
- `type`: Type of affiliation between i and j (e.g., son, wife, brother, sister).

`keyevents`: A 166 x 3 data frame with details about key events during the trial. The columns are:
- `id`: ID of the person concerned.
- `time`: Date of the event.
- `type`: Type of event (public summons, torture, or deposition).

## File list

- [data.RData](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/blob/main/data/data.RData)
- To ensure interoperability with other software, the data were also stored as six separate CSV files, which can be retrieved from [data (csv format)](https://github.com/joseluisesna/Denunciations_in_Giaveno_1335/tree/main/data/data%20(csv%20format)).

## Source

The data was collected from Merlo, Grado G. 1977. _Eretici e inquisitori nella società piemontese del Trecento: con l’edizione dei processi tenuti a Giaveno dall’inquisitore Alberto De Castellario (1335) e nelle Valli di Lanzo dall’inquisitore Tommaso Di Casasco (1373)_. Turin: Claudiana Editrice. 
The original Latin manuscript is held at the Archives of the Order of Preachers in Rome (Archivio generale dell’Ordine dei Predicatori, Rome, MS II.64, ff. 1r-111v).

## Contact information

For any questions, please contact:
- Davor Salihović (davor.salihovic@gmail.com)
