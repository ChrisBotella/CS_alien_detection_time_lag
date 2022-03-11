
# Script to obtain variables at species level
# scopus records, google hits searches and EU concern

# Author: Pablo González-Moreno. January 2022

# packages
library(dplyr)

# Get species list
datos <- read.csv(file = "timeLags_all_variables_clean.csv",sep=";")
species <- unique(datos[,c("species","LifeForm")])


# EU concern ####
# https://github.com/trias-project/indicators/blob/master/data/input/eu_concern_species_under_consideration.tsv

concernuncer <- read.csv("species_variables/eu_concern_species_under_consideration.csv",sep=";")
concernuncer$eu_status <- "consideration"
concernuncer$species <- concernuncer$verbatimScientificName
concern <- read.csv("species_variables/eu_concern_species.csv",sep=";")
concern$eu_status <- "accepted"
concern$species <- concern$checklist_scientificName

concern <- rbind(concern[,c("species","eu_status")],concernuncer[,c("species","eu_status")])

species_1 <- merge(species,concern,by.x="species",by.y = "species",all.x = T )
table(species_1$eu_status)

# Google hits ####
# Get species list
datos <- read.csv(file = "timeLags_all_variables_clean.csv",sep=";")

lags <- datos %>%
  filter(!is.na(timeLag)) # we keep data only with both dates and with official first record later than 2000


species <- unique(lags[,c("species","LifeForm")])


library(gtrendsR)
species$google_sum <- NA
species$google_mean <- NA

# unique(searchgoo[[2]]$location)
# Download all info
for(i in 2:nrow(species)){
  searchterm <- species$species[i]
  searchgoo <- gtrends(c(searchterm,"Gingko biloba"),gprop = "web",time="2010-01-01 2021-12-31",low_search_volume = T)
  save(searchgoo,file=paste("species_variables/google/",searchterm," .RData",sep = ""))
  print(paste("Species ", i, " ", searchterm))
}

# Process hits
for(i in 1:nrow(species)){
  searchterm <- species[i,"species"]
  load(file=paste("species_variables/google/",searchterm," .RData",sep=""))
  
  hits_time <- searchgoo[[1]]
  hits_time[hits_time$hits=="" & !is.na(hits_time$hits),"hits"] <- 0
  hits_time[is.na(hits_time$hits),"hits"] <- 0
  hits_time[hits_time$hits=="<1" & !is.na(hits_time$hits),"hits"] <- 1
  hits_time$hits <- as.numeric(hits_time$hits)
  
  species[i,"google_sum"] <- sum(hits_time[hits_time$keyword==searchterm,"hits"] )
  species[i,"google_mean"] <- mean(hits_time[hits_time$keyword==searchterm,"hits"] ,na.rm = T)
  print(paste("Species ", i, " ", searchterm))
}

write.csv(species,"species_variables/species_google.csv")

# Google hits per country ####
  # we loop throught he previous saved files


google_country <- data.frame(location=NA,hits=NA,keyword=NA,geo=NA,gprop=NA)
for(i in 1:nrow(species)){
  searchterm <- species[i,"species"]
  load(file=paste("species_variables/google/",searchterm," .RData",sep=""))
  hits_country <- searchgoo[[2]] %>%
    filter(keyword==searchterm)
  google_country <- rbind(google_country,hits_country)
  print(paste("Species ", i, " ", searchterm))
}

google_country <- google_country %>%
  filter(!is.na(keyword))

# The hits are in % of the total. Change in to numeric
google_country[google_country$hits=="" & !is.na(google_country$hits),"hits"] <- 0
google_country[google_country$hits=="<1" & !is.na(google_country$hits),"hits"] <- 1
google_country[is.na(google_country$hits),"hits"] <- 0
google_country$hits <- as.numeric(google_country$hits)

google_country[google_country$location=="Czechia","location"] <- "Czech Republic"
google_country[google_country$location=="Bosnia & Herzegovina","location"] <- "Bosnia and Herzegovina"	
google_country[google_country$location=="North Macedonia","location"] <- "Macedonia"


write.csv(google_country,"species_variables/species_country_google2.csv")




# scopus ####
library(rscopus)
rscopus::set_api_key("a01743b5b0dda99aee2ac374f2d48e55")
species_1$scopus <- NA
for(i in 1:nrow(species_1)){
  searchterm <- species_1$species[i]
  res = scopus_search(query = searchterm,count = 1,max_count = 1)
  species_1[i,"scopus"] <- res$total_results
  print(paste("Species ", i, " ", searchterm))
}


species_dicc <- read.csv("species_dicc.csv")
species_dicc <- merge(species_dicc,species_1[,c(1,4)],by.x="species",by.y = "species",all.x = T )
write.csv(species_dicc,"species_dicc.csv")




# EASIN ####
library(xlsx)
easin <- read.xlsx("species_variables/EASIN data extraction.xlsx",sheetIndex = 2)
species_dicc_easin <- merge(datos,easin,by.x="species",by.y = "Name",all.x = T)
species_dicc_easin[!is.na(species_dicc_easin$EASIN_Id),"partly_native"] <- "NO"
write.csv(species_dicc_easin,"easin_nz.csv")

