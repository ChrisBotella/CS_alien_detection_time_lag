library(dplyr)
belgium <- read.csv(file = "belgium_time_lag_analysis.txt",sep="\t")
belgium$Region <- "Belgium"

firstRecDf=read.csv('official_first_rec.csv',sep=";",header=T,stringsAsFactors = F)
belgiumfirstoff <- belgium %>%
  filter(!is.na(griis_first_observed)) %>%
  select(-first_observed)

firstRecDf <- merge(firstRecDf,belgiumfirstoff,by.y=c("Region","species"),by.x=c("Region","TaxonName"),all = T)
write.table(firstRecDf,file="official_first_rec_belgium.csv",sep=";",row.names = F)

