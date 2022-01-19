
# Script to obtain variables at species level
# scopus records, google hits searches and EU concern

# Author: Pablo González-Moreno. January 2022

# packages
library(dplyr)

# Get species list
datos <- read.csv(file = "timeLags_21_10_13.csv",sep=";")

species <- unique(datos[,c("species","SeebName")])

# EU concern ####
# https://github.com/trias-project/indicators/blob/master/data/input/eu_concern_species_under_consideration.tsv

concernuncer <- read.csv("eu_concern_species_under_consideration.csv",sep=";")
concernuncer$eu_status <- "consideration"
concernuncer$species <- concernuncer$verbatimScientificName
concern <- read.csv("eu_concern_species.csv",sep=";")
concern$eu_status <- "accepted"
concern$species <- concern$checklist_scientificName

concern <- rbind(concern[,c("species","eu_status")],concernuncer[,c("species","eu_status")])

species_1 <- merge(species,concern,by.x="species",by.y = "species",all.x = T )
table(species_1$eu_status)


# Google hits ####
library(gtrendsR)
species_1$google <- NA
for(i in 1:nrow(species_1)){
  searchterm <- species_1$species[i]
  searchgoo <- gtrends(searchterm,gprop = "web",time="today+5-y")
  save(searchgoo,file=paste("google/",searchterm,".RData"))
  species_1[i,"google"] <- sum(searchgoo[[1]]$hits)
  print(paste("Species ", i, " ", searchterm))
}
write.csv(species_1,"species_google.csv")


# Google hits per country ####
  # we loop throught he previous saved files

datos <- read.csv(file = "timeLags_22_01_11_all_variables_clean.csv",sep=";")
species_country <- unique(datos[,c("species","Region")])
species <- unique(datos[,c("species")])
gogole_country <- data.frame(location=NA,hits=NA,keyword=NA,geo=NA,gprop=NA)
for(i in 1:length(species)){
  searchterm <- species[i]
  load(file=paste("google/",searchterm,".RData",sep=" "))
  gogole_country <- rbind(gogole_country,searchgoo[[2]])
  print(paste("Species ", i, " ", searchterm))
}

gogole_country <- gogole_country %>%
  filter(!is.na(keyword))

# The hits are in % of the total. Change in to numeric
gogole_country[gogole_country$hits=="","hits"] <- 0
gogole_country[gogole_country$hits=="<1","hits"] <- 1
gogole_country$hits <- as.numeric(gogole_country$hits)

gogole_country[gogole_country$location=="Czechia","location"] <- "Czech Republic"
gogole_country[gogole_country$location=="Bosnia & Herzegovina","location"] <- "Bosnia and Herzegovina"	
gogole_country[gogole_country$location=="North Macedonia","location"] <- "Macedonia"


write.csv(gogole_country,"species_country_google.csv")



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

write.csv(species_1,"species_scopus.csv")

species_dicc <- read.csv("species_dicc.csv")
species_dicc <- merge(species_dicc,species_1[,c(1,4)],by.x="species",by.y = "species",all.x = T )
write.csv(species_dicc,"species_dicc.csv")

datos <- merge(datos, species_dicc,by.x="species",by.y = "species",all.x = T )
write.csv(datos,"timeLags_21_10_13_variables.csv")


# EASIN ####
datos <- read.csv(file = "timeLags_21_12_18_add_variables_JD.csv",sep=",")
library(xlsx)
easin <- read.xlsx("EASIN data extraction.xlsx",sheetIndex = 2)
species_dicc_easin <- merge(datos,easin,by.x="species",by.y = "Name",all.x = T)
species_dicc_easin[!is.na(species_dicc_easin$EASIN_Id),"partly_native"] <- "NO"
write.csv(species_dicc_easin,"timeLags_21_12_18_add_variables_JD_PGM.csv")

