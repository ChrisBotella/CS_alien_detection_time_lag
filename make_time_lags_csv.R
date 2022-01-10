# Dependencies
require(rgbif)
require(data.table)
require(maptools)
require(rgeos)
require(raster)

### Directory of official first record  
#firDir = "C:/Users/user/pCloud local/boulot/data/Invasions SA et FR/InvasionsFirstRecord/"
firDir = "C:/Users/cbotella/pCloud local/boulot/data/Invasions SA et FR/InvasionsFirstRecord/"

### Directory of CS occurrences (all sources) 
occDir = "C:/Users/cbotella/Documents/work local/data/CS time lag/extraction gbif/"

#mainDir = "C:/Users/cbotella/pCloud local/boulot/data/CS time lag/"
mainDir = "C:/Users/user/pCloud local/boulot/data/CS time lag/"

### Directories of outputs
saveDir = paste(mainDir,'21_11_30 analysis with Pablo/',sep="")

### Directory of country neighborhood data
bordersDir = paste(mainDir,'neigbor countries/country-borders-master/',sep="")

#####
# Functions
#####

regions =c('Belgium','Italy','Norway',
           'Greece','Germany','Portugal',
           'France','Spain','Albania',
           'Iceland','Netherlands','United Kingdom',
           'Sweden','Czech','Turkey',
           'Slovakia','Denmark','Ireland',
           'Poland','Hungary','Switzerland',
           'Austria','Slovenia','Croatia',
           'Cyprus','Ukraine','Latvia',
           'Bulgaria','Estonia','Bosnia',
           'Lithuania','Malta','Estonia',
           'Romania','Serbia','San Marino',
           'Moldova','Luxembourg','Liechtenstein',
           'Belarus','Montenegro','Finland',
           'Andorra','Luxembourg','Macedonia','Sea')

short.EU.regions.from.Region_seeb = function(vec){
  corVec = vec
  cd = rep(F,length(vec))
  for(i in 1:length(regions)){
    cdTmp = regexpr(regions[i],corVec)>0
    corVec[cdTmp] = regions[i]
    cd = cd | cdTmp
  }
  corVec[!cd]=NA
  return(corVec) 
}


df_as_model = function(filename,sep = ';'){
  names = colnames(fread( filename, sep = sep,nrows= 1 , header = T ))
  names=gsub(" ",".",names)
  names=gsub("'",".",names)
  expr = 'data.frame('
  for (i in 1:length(names)){
    if (i<length(names)){
      expr = paste(expr,names[i],'=NA,',sep="")
    }else{
      expr = paste(expr,names[i],'=NA)',sep="")
    }
  }
  return( eval(parse(text= expr)) )
}

LFs = data.frame(LifeForm=c('Vascular plants','Birds',
                            'Mammals','Reptiles',
                            'Insects','Arthropods p.p. (Myriapods, Diplopods etc.)',                                     
                            'Molluscs','Invertebrates (excl. Arthropods, Molluscs)',
                            'Algae','Crustaceans','Fishes',
                            'Amphibians','Bryozoa','Fungi',"Bacteria and protozoans"),
                 lifeForm=c('Vascular plants','Birds','Mammals',
                            'Reptiles',
                            'Insects','Other arthropods',
                            'Molluscs','Other invertebrates',
                            'Algae','Crustaceans','Fishes',
                            'Amphibians','Bryozoa','Fungi',"Bacter/proto"))

lifeforms= c("Fungi",
             "Algae",
             "Vascular plants",
             "Amphibians",
             "Mammals",                                    
             "Birds",
             "Reptiles",
             "Fishes",
             "Insects",
             "Crustaceans",
             "Arthropods p.p. (Myriapods, Diplopods etc.)",
             "Molluscs",
             "Invertebrates (excl. Arthropods, Molluscs)",
             "Bacteria and protozoans",                    
             "Bryozoa")
LF = lapply(lifeforms,function(lf){
  if(lf==lifeforms[1]){
    "!is.na(L$kingdom) & L$kingdom=='Fungi'"
  }else if(lf==lifeforms[2]){
    "!is.na(L$phylum) & L$phylum%in%c('Marchantiophyta','Charophyta','Chlorophyta','Anthocerotophyta','Rhodophyta','Glaucophyta')"
  }else if(lf==lifeforms[3]){
    "!is.na(L$phylum) & L$phylum=='Tracheophyta'"
  }else if(lf==lifeforms[4]){
    "!is.na(L$class) & L$class=='Amphibia'"
  }else if(lf==lifeforms[5]){
    "!is.na(L$class) & L$class=='Mammalia'"
  }else if(lf==lifeforms[6]){
    "!is.na(L$class) & L$class=='Aves'"
  }else if(lf==lifeforms[7]){
    "!is.na(L$class) & L$class=='Reptilia'"
  }else if(lf==lifeforms[8]){
    "!is.na(L$class) & L$class%in%c('Actinopterygii','Elasmobranchii')"
  }else if(lf==lifeforms[9]){
    "!is.na(L$class) & L$class=='Insecta'"
  }else if(lf==lifeforms[10]){
    "!is.na(L$phylum) & L$phylum=='Arthropoda' & L$class%in%c('Malacostraca','Maxillopoda','Branchiopoda')"
  }else if(lf==lifeforms[11]){
    "!is.na(L$phylum) & L$phylum=='Arthropoda' & L$class%in%c('','Collembola','Trilobita','Chilopoda','Diplopoda','Arachnida','Pycnogonida')"
  }else if(lf==lifeforms[12]){
    "!is.na(L$phylum) & L$phylum=='Mollusca'"
  }else if(lf==lifeforms[13]){
    '!is.na(L$phylum) & L$phylum%in%c("Annelida","Echinodermata","Cnidaria","Porifera","Ctenophora","Platyhelminthes","Nematoda")'
  }else if(lf==lifeforms[14]){
    "!is.na(L$kingdom) & L$kingdom%in%c('Protozoa','Bacteria')"
  }else if(lf==lifeforms[15]){
    "!is.na(L$phylum) & L$phylum=='Bryozoa'"
  }
})
names(LF) = lifeforms

get.LifeForm = function(L){
  # tab must contain columns kingdom, phylum and class
  L$LifeForm = NA
  for(i in 1:length(LF)){
    eval(parse(text=paste('cd =',LF[[i]])))
    if(sum(cd)>0){
      L$LifeForm[cd] = names(LF)[i]
    }
  }
  return(L$LifeForm)
}

multiplot <- function(plots=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  numPlots = length(plots)
  print(numPlots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#####
# EU countries 
#####

data(wrld_simpl)
EU_simpl = wrld_simpl[wrld_simpl@data$REGION=="150",]
EU_simpl = EU_simpl[EU_simpl@data$NAME!="Russia",]

countries = EU_simpl@data[,c("ISO2",'UN','NAME',"AREA")]
countries = rbind(countries,data.frame(ISO2='WA',UN=1,NAME="Sea or Ocean",AREA=NA))
EU_rast = raster(ncol=5000, nrow=5000)
EU_rast <- rasterize(EU_simpl, EU_rast, 
                     field = EU_simpl@data$UN, 
                     update = TRUE, 
                     updateValue = "NA")
EU_rast=raster::crop(EU_rast,extent(-25, 40, 25, 90))
EU_rast[is.na(EU_rast)] = 1

setwd(firDir)
firstObs = read.csv('firstObs.csv',sep=";",header=T)
countries$Region_seeb = NA
for(i in 1:length(regions)){
  countries$Region_seeb[regexpr(regions[i],countries$NAME)>0]= as.character(firstObs$Region[regexpr(regions[i],firstObs$Region)>0][1])
}
countries$Region_seeb[countries$NAME=="Svalbard"]="Norway"
countries$Region_seeb[countries$NAME=="Jersey"]="United Kingdom"
countries$Region_seeb[countries$NAME=="Guernsey"]="United Kingdom"
countries$Region_seeb[countries$NAME=="Holy See (Vatican City)"]="Italy"
countries$Region_seeb[countries$NAME=="Gibraltar"]="United Kingdom"
countries$Region_seeb[countries$NAME=="Monaco"]="France"
countries$Region_seeb[countries$NAME=="Aaland Islands"]="Finland"
countries$Region_seeb[countries$NAME=="Isle of Man"]="United Kingdom"
countries$Region_seeb[countries$NAME=='Luxembourg']='Luxembourg'
countries$Region_seeb[countries$NAME=='The former Yugoslav Republic of Macedonia']='Macedonia'
countries$Region_seeb[countries$NAME=="Sea or Ocean"]='Sea'

setwd(saveDir)
save(countries,EU_simpl,EU_rast,file = 'Europe_light')


#####
# UPDATE Species list under GBIF taxo 
#####

setwd(paste(mainDir,'preliminary updated/',sep=""))
firstObs = read.csv('official_first_rec_updated_21_10_05_CSV.csv',sep=";",header=T,stringsAsFactors = F)

# I removed the rows where first rec year < 2010
# I kept the earliest first rec year when there was several
# I removed rows fully highlighted in red

# firstObs$PresentStatus%in%c('casual',"Invasive",'invasive','established','eradicated','alien','In captivity/culture') & 
spTable = data.frame(unique(firstObs[,c('TaxonName',
                                        'scientificName',
                                        'OrigName')]))
colnames(spTable)[2] = 'SeebName'
# Manually fix some names that don't match
spTable$SeebName[spTable$TaxonName=="Perovskia x superba"]="Perovskia abrotanoides"
spTable$SeebName[spTable$TaxonName=="Callopistromyia annulipes"]="Callopistromyia annulipes"
spTable$SeebName[spTable$TaxonName=="Spiranthes cernua x S. odorata"]="Spiranthes cernua (L.) Richard"
spTable$SeebName[spTable$TaxonName=="Hermetia illucens"]="Hermetia illucens"
spTable$SeebName[spTable$TaxonName=="Cistus ×purpureus"]="Cistus ×purpureus"
spTable$SeebName[spTable$TaxonName=="Erica herbacea"]="Erica herbacea"
spTable$SeebName[spTable$TaxonName=="Iris orientalis"]="Iris orientalis"
spTable$SeebName[spTable$SeebName=="Allium porrum"]="Allium ampeloprasum"
spTable$SeebName[spTable$SeebName=="Mimulus  cupreus"]="Erythranthe cuprea"
spTable$SeebName[spTable$SeebName=='Candidatus Liberibacter solanacearum']='Liberibacter solanacearum'

# Get GBIF scientificName
cols = c("usageKey","rank","scientificName",'phylum','class','kingdom',
         "matchType","confidence","synonym",
         "status","canonicalName","species","speciesKey")

toAdd=as.data.frame(matrix(NA,dim(spTable)[1],length(cols)))
colnames(toAdd)=cols
for(i in 1:dim(spTable)[1]){
  print(i)
  tente = as.data.frame(rgbif::name_backbone(as.character(spTable$SeebName[i])))
  if(sum(!cols%in%colnames(tente))>0){
    for(col in cols[!cols%in%colnames(tente)]){
      eval(parse(text=paste('tente$',col,'=NA',sep="")))
    }
  }
  tente = tente[,cols]
  if(!is.null(dim(tente))){
    toAdd[i,]= tente[1,]
  }
}
spTableFinal = cbind(spTable,toAdd)
#spTableFinal[toAdd$matchType=="HIGHERRANK",c('SeebName','rank','species','synonym')][10:27,]
spTableFinal$matchType[spTableFinal$matchType=="HIGHERRANK" & spTableFinal$rank=="SPECIES"]="EXACT"
spTableFinal = spTableFinal[!spTableFinal$matchType%in%c('HIGHERRANK','NONE'),]

spTableFinal$LifeForm = get.LifeForm(spTableFinal)

setwd(saveDir)
write.table(spTableFinal,'matching_names_Seeb_with_GBIF.csv',sep=";",row.names=F,col.names=T)

sps = aggregate(list(nSeebNames=rep(1,dim(spTableFinal)[1])),
                by=list(species=spTableFinal$species,
                        scientificName=spTableFinal$scientificName,
                        LifeForm=spTableFinal$LifeForm),sum)

setwd(saveDir)
write.table(sps,'speciesList_Seeb_sup2010_EU_GBIFtaxo.csv',sep=";",row.names=F,col.names=T)

#####
# Official first rec DF
#####

setwd(saveDir)
load(file = 'Europe_light')
matchin=read.csv('matching_names_Seeb_with_GBIF.csv',sep=";",header=T,stringsAsFactors = F)
firstObs = read.csv('official_first_rec_updated_21_10_05_CSV.csv',sep=";",header=T)

firstObs = firstObs[firstObs$TaxonName%in%matchin$TaxonName | firstObs$scientificName%in%matchin$SeebName,colnames(firstObs)!='LifeForm']
firstObs = merge(firstObs,matchin[,c('TaxonName','species','LifeForm')],by='TaxonName',all.x=T)
firstObs = firstObs[!is.na(firstObs$species) & !is.na(firstObs$LifeForm),]
firstObs$Region=short.EU.regions.from.Region_seeb(firstObs$Region)
firstObs = firstObs[!is.na(firstObs$Region),]
firstRecDf = firstObs[,c('species','scientificName','LifeForm',
                         'Region','FirstRecord')]
colnames(firstRecDf) = c("species",'seebName','LifeForm',
                         'Region','off_FirstRec')
setwd(saveDir)
write.table(firstRecDf,'official_firstRec_clean_21_10_13.csv',sep=";",col.names=T,row.names=F)

#####
# Extract species FirstObs GBIF
#####

setwd(saveDir)
spTable=read.csv('speciesList_Seeb_sup2010_EU_GBIFtaxo.csv',sep=";",header=T,stringsAsFactors = F)

load(file = 'Europe_light')

spToKeep = unique(spTable$species[!is.na(spTable$species)])

keptCols = c("kingdom","phylum","class",'species',
             'taxonRank',
             'decimalLatitude','decimalLongitude',
             'year')

files= c('full_iNat.csv',
         'Waarnemingen_be.csv',
         'algae.csv',
         'seasearch.csv',
         'other_vertebrates.csv','non_plant_or_animal.csv',
         'tracheophyta.csv','invertebrates.csv','observ_org_before_2010.csv',
         'artportalen_non_birds.csv','artportalen_passeriformes.csv','artportalen_other_birds.csv',
         'inat_ukbms.csv','naturgucker.csv','norwegiansos.csv',
         'other.csv','plantnet.csv',
         #'waarnemingen.csv',
         'ebird.csv','dof.csv')

#setwd(occDir)
#tmp=read.csv('full_iNat.csv',sep='\t',header=T)
#tmp = tmp[,c("kingdom","phylum","class","species","taxonRank",       
#             "decimalLatitude", "decimalLongitude","year","speciesKey" )  ]
#write.table(tmp,'full_iNat.csv',sep='\t',row.names=F,col.names=T)

taille_serie = 1000000
saveName ="CS_1st_rec.csv"
saveNameGrp = "count_per_lifeForm_and_country.csv"
#perSpecies = read.csv(paste(saveDir,'perSpecies.csv',sep=""),sep=";",header=T,stringsAsFactors = F)
#perGroup = read.csv(paste(saveDir,'perGroup.csv',sep=""),sep=";",header=T,stringsAsFactors = F)
perGroup = as.data.frame(matrix(NA,0,3));colnames(perGroup)=c('country','LifeForm','count');
perSpecies = as.data.frame(matrix(NA,0,5));colnames(perSpecies)=c('country','species','LifeForm','count','firstRec');
for(file in files){
  print(file)
  setwd(occDir)
  tmp=df_as_model(file,sep="\t")
  selec.ind = which(colnames(tmp)%in%keptCols)
  depasse_pas = T
  toSkip = 0
  while (depasse_pas){
    #perSpecies = read.csv(saveName,sep=";",header=T,stringsAsFactors = F)
    setwd(occDir)
    L = data.frame( fread(file, 
                          sep="\t", 
                          nrows=taille_serie,
                          header=FALSE,
                          skip= 1 + toSkip,
                          col.names = keptCols,
                          select=selec.ind,
                          na.strings = c("NA")) )
    if (dim(L)[1]<(taille_serie-1)){ depasse_pas=FALSE }
    
    # occurrence is identified at the species level and year available
    L = L[L$taxonRank=='SPECIES' & !is.na(L$year),,drop=F]
    if(dim(L)[1]>0){
      L$count = 1
      ### Get LifeForm
      L$LifeForm = get.LifeForm(L[,c('kingdom','phylum','class')])
      ### Get country
      L$countryId= raster::extract(EU_rast,L[,c("decimalLongitude","decimalLatitude")])
      L =merge(L,countries[,c('UN','NAME')],by.x='countryId',by.y='UN',all.x=T)
      colnames(L)[colnames(L)=="NAME"]="country"
      ### count total occurrences per taxonomic group and country
      perGroup = rbind(perGroup,L[,c('country','LifeForm','count')])
      perGroup = aggregate(list(count=perGroup$count),
                           by=list(country=perGroup$country,
                                   LifeForm=perGroup$LifeForm),
                           FUN=sum)
      
      setwd(saveDir)
      write.table(perGroup,saveNameGrp,sep=";",row.names=F,col.names=T)
      
      L = L [L$species%in%spToKeep,,drop=F]
      if(dim(L)[1]>0){
        L = L[,c("decimalLatitude","decimalLongitude",
                 "kingdom","phylum","class",
                 'species',
                 'year','country','LifeForm','count')]
        colnames(L)[c(1:2,7)]= c("Latitude","Longitude","firstRec")
        perSpecies = rbind(perSpecies,L[,c('country','species','LifeForm','count','firstRec')])
        
        # count occurrence per species and country 
        updated = aggregate(list(count=perSpecies$count),
                            by=list(country=perSpecies$country,
                                    species=perSpecies$species,
                                    LifeForm=perSpecies$LifeForm),
                            FUN=sum) 
        # first year of obs per species and country
        updated2 = aggregate(list(firstRec=perSpecies$firstRec),
                             by=list(country=perSpecies$country,
                                     species=perSpecies$species,
                                     LifeForm=perSpecies$LifeForm),
                             FUN=min) 
        updated$firstRec = updated2$firstRec
        perSpecies = updated
        
        ### Update files
        setwd(saveDir)
        write.table(perSpecies,saveName,sep=";",row.names=F,col.names=T)
      }
    }
    
    ### Number of rows done
    toSkip = toSkip + taille_serie
    print(paste('Number done:',toSkip))
    gc(reset=T)
  }
}


#####
# Time lag table
#####

setwd(saveDir)
firstRecDf=read.csv('official_firstRec_clean_21_10_13.csv',sep=";",header=T,stringsAsFactors = F)
matchin=read.csv('matching_names_Seeb_with_GBIF.csv',sep=";",header=T,stringsAsFactors = F)[,c('species','phylum','class',"LifeForm","SeebName")]

load(file = 'Europe_light')
perSpecies=read.csv('CS_1st_rec.csv',sep=";",header=T,stringsAsFactors = F)
perSpecies=merge(perSpecies,countries[,c('NAME','Region_seeb')],by.x='country',by.y='NAME',all.x=T)
perSpecies = perSpecies[!is.na(perSpecies$Region_seeb),colnames(perSpecies)!="country"]
perSpecies$Region_seeb=short.EU.regions.from.Region_seeb(perSpecies$Region_seeb)
colnames(perSpecies)[5] = 'Region'
perGroup=read.csv('count_per_lifeForm_and_year.csv',sep=";",header=T,stringsAsFactors = F)
colnames(perGroup)[3]= 'obsEffort_LF'
perGroup=merge(perGroup,countries[,c('NAME','Region_seeb')],by.x='country',by.y='NAME',all.x=T)
perGroup = perGroup[!is.na(perGroup$Region_seeb),colnames(perGroup)!="country"]
perGroup$Region_seeb=short.EU.regions.from.Region_seeb(perGroup$Region_seeb)
colnames(perGroup)[3] = 'Region'
#length(unique(firstRecDf$species[firstRecDf$LifeForm=='Reptiles']))
#length(unique(firstRecDf$species[firstRecDf$LifeForm=='Arthropods p.p. (Myriapods, Diplopods etc.)']))
TL = merge(firstRecDf[,c("species",
                         'Region',
                         'off_FirstRec')],
           perSpecies[,c('species','Region',
                         'count','firstRec')],by=c("species",'Region'),
           all.x=T,all.y=T)
TL = merge(TL,matchin,by="species",all.x=T)
TL$timeLag = TL$firstRec - TL$off_FirstRec
TL = unique(TL)
cat('Number of time lags:',sum(!is.na(TL$timeLag)))
TL = TL[is.na(TL$off_FirstRec) | TL$off_FirstRec>=2010,]
TL = merge(TL,perGroup,by=c('LifeForm','Region'),all.x=T)
TL$obsEffort_LF[is.na(TL$obsEffort_LF)]=0
tab = table(TL$Region[!is.na(TL$off_FirstRec)])
rEff = data.frame(ResearchEffort= as.numeric(tab),Region=names(tab))
TL = merge(TL,rEff,by=c('Region'),all.x=T)

setwd(saveDir)
write.table(TL,'timeLags_21_10_13.csv',sep=";",row.names=T,col.names=T)

#####
# Add nOccTot, detecInNeigborCountryBefore
#####

setwd(saveDir)
load(file = 'Europe_light')
setwd(bordersDir)
bord = read.csv('GEODATASOURCE-COUNTRY-BORDERS.csv',sep=",",header=T,stringsAsFactors = F)
bord = bord[complete.cases(bord),]
change.country=function(tab,
                        prevName,prevCode,
                        newName,newCode){
  tab$country_code[tab$country_code==prevCode]=newCode
  tab$country_name[tab$country_name==prevName]=newName
  tab$country_border_code[tab$country_border_code==prevCode]=newCode
  tab$country_border_name[tab$country_border_name==prevName]=newName
  return(tab)
}

countries$ISO2=as.character(countries$ISO2)
bord2 = bord
for(i in 1:dim(countries)[1]){
  if(countries$ISO2[i]%in%bord$country_code | countries$ISO2[i]%in%bord$country_border_code){
    prevN = unique(bord$country_name[bord$country_code==countries$ISO2[i]]) 
    prevN = unique(c(prevN,
                     bord$country_border_name[bord$country_border_code==countries$ISO2[i]]))
    prevC = countries$ISO2[i]
    
    if(length(prevN)>0){
      newN = countries$Region_seeb[i]
      newC = unique(countries$ISO2[countries$NAME==newN])
      if(length(newC)==0){newC=prevC}
      bord2 = change.country(bord2,
                             prevN,
                             prevC,
                             newN,
                             newC) 
    }
  }
}

setwd(saveDir)
tl = read.csv('timeLags_21_10_13_variables.csv',sep=",",header=T)
tl$nOccTotSp = NA
tl$obsInNeigborCountryBefore = NA
for(i in 1:dim(tl)[1]){
  tl$nOccTotSp[i] = sum(tl$count[tl$species==tl$species[i]])
  if(!is.na(tl$off_FirstRec[i])){
    adjReg = bord2$country_border_name[bord2$country_name==as.character(tl$Region[i])] 
    adjReg = c(adjReg , bord2$country_name[bord2$country_border_name==as.character(tl$Region[i])] ) 
    adjReg = unique(adjReg[!is.na(adjReg)])
    adjReg = adjReg[adjReg!=""]
    # Among official first rec
    tmp = tl$off_FirstRec[tl$species==tl$species[i] & tl$Region%in%adjReg]
    tmp = tmp[!is.na(tmp)]
    # Among CS first rec
    tmp2 = tl$firstRec[tl$species==tl$species[i] & tl$Region%in%adjReg]
    tmp2 = tmp2[!is.na(tmp2)]
    earliestRecInAdj = min(tmp2,tmp,na.rm=T)
    if(!is.na(earliestRecInAdj)){
      # Is earliest off/CS first rec in adjacent country before current official rec  
      tl$obsInNeigborCountryBefore[i] = earliestRecInAdj<tl$off_FirstRec[i]
    }
  }
  if(i/20==round(i/20)){
    flush.console()
    cat('\r Processed ',round(1000*i/dim(tl)[1])/10,'%')
  }
}

setwd(saveDir)
write.table(tl,'timeLags_21_12_04_add_variables_CB.csv',sep=";",row.names=F,col.names=T)

