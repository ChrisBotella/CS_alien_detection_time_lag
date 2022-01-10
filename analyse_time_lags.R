require(ggplot2)

#mainDir = "C:/Users/cbotella/pCloud local/boulot/data/CS time lag/"
mainDir = "C:/Users/user/pCloud local/boulot/data/CS time lag/"

### Directories of outputs
saveDir = paste(mainDir,'21_11_30 analysis with Pablo/',sep="")

#####
# Figures
#####

setwd(saveDir)
TL = read.csv('timeLags_21_10_13.csv',sep=";",header=T)

tmp = TL[!is.na(TL$timeLag),]

# time lags per LifeForm
tmp = merge(tmp,LFs,by="LifeForm",all.x=T)
tmp$lifeForm=factor(tmp$lifeForm,levels=LFs$lifeForm)
tmp2 = unique(tmp[,c('lifeForm','species')])
p0=ggplot(tmp2,aes(x=lifeForm))+geom_bar()+theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank())+ylab('Number of species having time lags')
p1=ggplot(tmp,aes(x=lifeForm))+geom_bar()+theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank())+ylab('Number of time lags')
p2=ggplot()+geom_boxplot(data=tmp,aes(x=lifeForm,y=timeLag))+geom_point(data=tmp,aes(x=lifeForm,y=timeLag))+theme(axis.text.x = element_text(angle=25))+ylab('Time lag (CS - official)')

setwd(saveDir)
png('timeLags_LifeForms.png',height=900,width=500)
multiplot(list(p2,p1,p0),cols=1)
dev.off()

# time lags per country
regis = table(TL$Region)
regis=regis[order(regis,decreasing=T)]
regNames = names(regis)

TL$Region = factor(TL$Region,levels=regNames)

tmp = TL[!is.na(TL$timeLag),]
#tab=tab[tab>7]
tmp2 = tmp[tmp$Region%in%regNames,c('Region','timeLag')]
noTL = data.frame(Region=factor(regNames[!regNames%in%as.character(tmp2$Region)],levels=regNames),timeLag=NA)

tmp2 = rbind(tmp2,noTL)

TL$caseType=NA
TL$caseType[is.na(TL$firstRec) & !is.na(TL$off_FirstRec)] = "Official:Yes ; CS:No"
TL$caseType[!is.na(TL$firstRec) & is.na(TL$off_FirstRec)] ="Official:No ; CS:Yes" 
TL$caseType[!is.na(TL$firstRec) & !is.na(TL$off_FirstRec)] = "Both"

txtSize=13

p1=ggplot(tmp2,aes(x=Region))+geom_bar()+
  theme(text = element_text(size=txtSize),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())+ylab('Number of time lags')
p2=ggplot()+geom_boxplot(data=tmp2,aes(x=Region,y=timeLag))+
  geom_point(data=tmp2,aes(x=Region,y=timeLag))+
  theme(text = element_text(size=txtSize),
        axis.text.x = element_text(angle=25))+ylab('Time lag (CS - official)')+xlab('Country')

p3 = ggplot(TL,aes(x=Region,fill=caseType))+geom_bar()+xlab('country')+
  theme(legend.position = 'None',
        text = element_text(size=txtSize),
        axis.text.x = element_text(angle=25))

setwd(saveDir)
png('timeLags_Countries.png',height=1100,width=800)
multiplot(list(p3,p2),cols=1)
dev.off()

# In Seeb but not CS
types = c("Official:Yes ; CS:No","Official:No ; CS:Yes","Both")
for (type in types){
  if(type=="Official:Yes ; CS:No"){
    cd = is.na(TL$firstRec) & !is.na(TL$off_FirstRec)
  }else if(type=="Official:No ; CS:Yes"){
    cd = !is.na(TL$firstRec) & is.na(TL$off_FirstRec)
  }else{
    cd = !is.na(TL$firstRec) & !is.na(TL$off_FirstRec)
  }
  tmp=unique(TL[cd,c('species','Region','LifeForm')])
  tmp = merge(tmp,LFs,by="LifeForm",all.x=T)
  tab=table(tmp$lifeForm)
  toAdd=data.frame(lifeForm=names(tab),nCases=as.numeric(tab))
  toAdd=merge(toAdd,LFs,by="lifeForm",all.x=T)
  toAdd$lifeForm=factor(toAdd$lifeForm,levels=LFs$lifeForm)
  toAdd$propCases=NA
  for(i in 1:dim(toAdd)[1]){
    totCases = sum(as.character(TL$LifeForm)==as.character(toAdd$LifeForm[i]))
    toAdd$propCases[i]=toAdd$nCases[i]/totCases
  }
  toAdd$type = type
  if(type==types[1]){toPlot=toAdd}else{toPlot=rbind(toPlot,toAdd)}
}
toPlot$type=factor(toPlot$type,levels=types)
toPlot$legFake="Official:Yes ; CS:No"

p1=ggplot()+geom_col(data=toPlot,aes(x=lifeForm,y=propCases,fill=type))+ylab('Proportion of cases')+theme_bw()+theme(text=element_text(size=17),axis.text.x = element_text(angle=25),axis.title.x = element_blank(),axis.ticks.x = element_blank())
p2=ggplot()+geom_col(data=toPlot,aes(x=lifeForm,y=nCases,fill=legFake))+ylab('Number of cases')+theme_bw()+scale_fill_manual(values ='black')+theme(text=element_text(size=17),axis.text.x = element_text(angle=25))

setwd(saveDir)
png('casesTypes_per_lifeForm.png',height=1000,width=700)
multiplot(list(p1,p2),cols=1)
dev.off()

# rec per year
tmp1 = TL
tmp1$type = NA
tmp1$type[!is.na(TL$firstRec) & is.na(TL$off_FirstRec)]="Official:No ; CS:Yes"
tmp1$type[is.na(TL$firstRec) & !is.na(TL$off_FirstRec)]="Official:Yes ; CS:No"
tmp1$type[!is.na(TL$firstRec) & !is.na(TL$off_FirstRec)]="Both"
tmp1$type=factor(tmp1$type,levels=c("Official:Yes ; CS:No","Official:No ; CS:Yes","Both"))
tmp1$firstRec_all = sapply(1:dim(tmp1)[1],function(i)min(tmp1$off_FirstRec[i],tmp1$firstRec[i],na.rm=T))
p2=ggplot()+geom_bar(data=tmp1,aes(x=firstRec_all,fill=type))+xlab("First record (any)")+theme(text=element_text(size=17))

setwd(saveDir)
png('1stRec_per_year.png',height=700,width=900)
print(p2)
dev.off()

# time lag vs sampling effort, research effort 
tmp = TL[!is.na(TL$timeLag),]
tmp$logEffort = log10(tmp$obsEffort_LF)
p=ggplot(tmp,aes(x=logEffort,y=timeLag))+
  geom_point()+
  geom_smooth(method='lm',formula= y ~ x)+
  xlab("log10-Sampling effort (#obs per LifeForm in country)")+
  theme(text=element_text(size=17))

setwd(saveDir)
png('tl_vs_sampling_effort.png',height=700,width=900)
print(p)
dev.off()