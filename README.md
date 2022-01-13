# CS_alien_detection_time_lag

## File descriptions

Input data:
- **official_first_rec.csv**: Year of first official record per species and European country between 2010 and 2021. This table is based on an extraction of Seebens et al. (2017), enriched by a group of researchers of the COST Action (CA17122) network.
- **CS_1st_rec.csv**: Year of first Citizen Science (CS) record per species and European country between 1990 and 2021, if any record is available among all source CS databases. 4 columns: firstRec (year of first record among all Citizen science databases); country (european country concerned); species (species scientific name following GBIF taxonomy); LifeForm.
- **count_per_lifeForm_and_country.csv**: Count of CS occurrences per life form and European country.
- **species_variables.csv**: Provides species variables including habitat, invasiveness status at various levels, google and scopus hits, etc.

Scripts:
- **make_time_lags_csv.R**: Produces the CSV table of time lags between the first year of observation in CS databases and official records along with explanatory covariables.
- **analyse_time_lags.R**: Produces various Figure for analysis.

References:
Seebens, H., Blackburn, T. M., Dyer, E. E., Genovesi, P., Hulme, P. E., Jeschke, J. M., ... & Essl, F. (2017). No saturation in the accumulation of alien species worldwide. Nature communications, 8(1), 1-9.

## Generate Time Lags table

Requires R installed (tested under R 3.5.1) and the following packages: rgbif, data.table, maptools, rgeos, raster.
- Download all scripts and csv files.
- **make_time_lags_csv.R** and change directory paths in the header of the script to your local paths.  
- Run R script **make_time_lags_csv.R**.

## Variable descriptions

Find below a brief description and the origin of the variables associated with the time lags provided in the output table. 

LifeForm and country level variables:
- obsEffort_LF: Number of CS occurrences in the country for all species of the LifeForm of the species.

Country level variables:
- Region: Country name.
- ResearchEffort: Number of alien species official first records for the country in Seebens database.

Species level variables:
- phylum: Taxonomic phylum.
- class: Taxonomic class.
- LifeForm: The life form, namely one of "Vascular plants", "Birds", "Mammals", "Reptiles", "Insects", "Arthropods p.p. (Myriapods, Diplopods etc.)", "Molluscs", "Invertebrates (excl. Arthropods, Molluscs)", "Algae", "Crustaceans", "Fishes", "Amphibians", "Bryozoa", "Fungi" or "Bacteria and protozoans". See the code for their taxonomic description.
- nOccTotSp: Total number of records in all CS databases since 1990.
- scopus: 
- google: ...Number of Google hits...
- EASIN_Id:
- Status: 
- eu_status: ... is accepted as EU concern or under consideration for it... 
- mentioned_in_EU_quarantine_species:
- habitat:
- TER:
- FRW:
- MAR:
- EST:
- worst_100_world_ISSG: 
- worst_100_Europe_DAISIE:
- partly_native: 

Species AND country level variables:
- count: Number of CS records for the species in the country since 1990.
- obsInNeigborCountryBefore: Is there a CS record of the species in a neighborhing country that is earlier than its official first record year in the current country ? This variable is computed using the Country Borders data from: https://www.geodatasource.com 
