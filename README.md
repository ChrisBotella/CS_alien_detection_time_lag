# CS_alien_detection_time_lag

## File descriptions

input data:
- **official_first_rec.csv**: Year of first official record per species and European country between 2010 and 2021. This table is based on an extraction of Seebens et al. (2017), enriched by a group of researchers of the COST Action (CA17122) network.
- **CS_1st_rec.csv**: Year of first Citizen Science (CS) record per species and European country between 1990 and 2021, if any record is available among all source CS databases. 4 columns: firstRec (year of first record among all Citizen science databases); country (european country concerned); species (species scientific name following GBIF taxonomy); LifeForm.
- **count_per_lifeForm_and_country.csv**: Count of CS occurrences per life form and European country.
- **species_variables.csv**: Provides species variables including habitat, invasiveness status at various levels, google and scopus hits, etc.

scripts:
- **make_time_lags_csv.R**: Produces the CSV table of time lags between the first year of observation in CS databases and official records along with explanatory covariables.
- **analyse_time_lags.R**: Produces various Figure for analysis.

## Generate Time Lags table

Requires R installed (tested under R 3.5.1) and the following packages: rgbif, data.table, maptools, rgeos, raster.
- Download all scripts and csv files.
- **make_time_lags_csv.R** and change directory paths in the header of the script to your local paths.  
- Run R script **make_time_lags_csv.R**.

References:
Seebens, H., Blackburn, T. M., Dyer, E. E., Genovesi, P., Hulme, P. E., Jeschke, J. M., ... & Essl, F. (2017). No saturation in the accumulation of alien species worldwide. Nature communications, 8(1), 1-9.
