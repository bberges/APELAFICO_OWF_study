library(ggplot2)
library(tidyverse)
library(lubridate)
library(icesTAF)
library(suncalc)

rm(list=ls())

sourceDir(file.path('.','function'))

#setwd('D:/PAM_RABO/finless_neonate_HK')
setwd('D:/git/APELAFICO_OWF_study/')
#setwd('G:/git/WBAT_APELAFICO')

figurePath    <- file.path('.','figures')
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

WBAT.tab <- read.csv(file.path(dataPath,'survey_db.csv'))
WBAT.tab <- WBAT.tab[,c(1:5,8,9)]
WBAT.tab$dataSet_station <- paste0(WBAT.tab$dataSet,'_',WBAT.tab$station)

############################################################################
## Read data and plot
############################################################################

load(file.path(dataPath,'CPOD_all.RData'))

CPOD.all$station[CPOD.all$station == "267878"] <- "267838"

CPOD.all$dataSet_station <- paste0(CPOD.all$dataSet,'_',CPOD.all$station)

WBAT.tab <- WBAT.tab[WBAT.tab$SA_exported == 1,]

flagFirst.all <- T
for(stationSet in unique(WBAT.tab$dataSet_station)){
  print(stationSet)
  tab.filt <- WBAT.tab[WBAT.tab$dataSet_station == stationSet,]
  
  # Read all data in the set of station+data set
  flagFirst <- T
  for(idxDataSet in tab.filt$surveName){
    load(file.path(dataPath,paste0('WBAT_',idxDataSet,'.RData')))
    WBAT.all <- subset(WBAT.all,depth == 0)
    
    if(flagFirst){
      WBAT.join <- WBAT.all
      flagFirst <- F
    }else{
      WBAT.join <- rbind(WBAT.join,WBAT.all)
    }
  }
  
  # summarize WBAT data per intervals
  WBAT.summary <- WBAT.join %>% group_by(treshold,IDinter,frequency,chunk) %>% summarize(dataSet_station=stationSet,
                                                                                          station=unique(station),
                                                                                         dataSet=unique(dataSet),
                                                                                          SA=mean(SA,na.rm=T),
                                                                                         datetime=first(datetime),
                                                                                         depthIntegration=first(depthIntegration))
  
  
  
  mySunlightTimes <- getSunlightTimes(date = as.Date(WBAT.summary$datetime),
                                      lat = 51.65512058,
                                      lon = 2.793530261, tz = "UTC") # hack, lat/lon needs to be inputed for each station
  WBAT.summary$hourSunset    <- hour(mySunlightTimes$sunset)+minute(mySunlightTimes$sunset)/60+second(mySunlightTimes$sunset)/60/60
  WBAT.summary$hourSunrise   <- hour(mySunlightTimes$sunrise)+minute(mySunlightTimes$sunrise)/60+second(mySunlightTimes$sunrise)/60/60
  WBAT.summary$sunset <- mySunlightTimes$sunset
  WBAT.summary$sunrise <- mySunlightTimes$sunrise
  
  WBAT.summary$dayNight <- 'night'
  WBAT.summary$dayNight[(WBAT.summary$sunrise <= WBAT.summary$datetime) & (WBAT.summary$datetime <= WBAT.summary$sunset)] <- 'day'
  
  if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
    CPOD.stationSet <- strsplit(stationSet,' ')[[1]][1]
    # summarize CPOD
    CPOD.current <- subset(CPOD.all,dataSet_station == CPOD.stationSet)
    CPOD.current$stationName  <- CPOD.current$station
    CPOD.current$lat <- NA
    CPOD.current$lon <- NA
    
    mySunlightTimes <- getSunlightTimes(date = as.Date(CPOD.current$timeIci_str),
                                        lat = 51.65512058,
                                        lon = 2.793530261, tz = "UTC") # hack, lat/lon needs to be inputed for each station
    CPOD.current$hourSunset    <- hour(mySunlightTimes$sunset)+minute(mySunlightTimes$sunset)/60+second(mySunlightTimes$sunset)/60/60
    CPOD.current$hourSunrise   <- hour(mySunlightTimes$sunrise)+minute(mySunlightTimes$sunrise)/60+second(mySunlightTimes$sunrise)/60/60
    CPOD.current$sunset <- mySunlightTimes$sunset
    CPOD.current$sunrise <- mySunlightTimes$sunrise
    
    CPOD.current$dayNight <- 'night'
    CPOD.current$dayNight[(CPOD.current$sunrise <= CPOD.current$timeIci_str) & (CPOD.current$timeIci_str <= CPOD.current$sunset)] <- 'day'
    
    res.CPOD <- format_CPOD(CPOD.current)
  }

  if(flagFirst.all){
    if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
      CPOD.all.summary <- res.CPOD$CPOD.day
    }
    WBAT.all.summary <- WBAT.summary
    flagFirst.all <- F
  }else{
    if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
      CPOD.all.summary <- rbind(CPOD.all.summary,res.CPOD$CPOD.day)
    }
    WBAT.all.summary <- rbind(WBAT.all.summary,WBAT.summary)
  }

  # # make individual plot
  # print(ggplot(subset(WBAT.summary,treshold %in% c(-60,-55,-50)),aes(x=datetime,y=log10(SA),col=as.factor(treshold)))+
  #         geom_line()+
  #         geom_smooth(col='black')+
  #         ggtitle(paste0('WBAT time series - ',stationSet))+
  #         facet_grid(treshold~frequency))
  # 
  # if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
  #   print(ggplot(res.CPOD$CPOD.day,aes(x=time_day,y=buzz_ratio))+
  #             geom_line()+
  #             geom_smooth(col='black')+
  #             ggtitle(paste0('buzz click ratio - ',stationSet)))
  # }
  # 
  # print(ggplot(subset(WBAT.summary,treshold %in% c(-60,-55,-50)),aes(x=dayNight,y=log10(SA),fill=as.factor(treshold)))+
  #       geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  #       facet_grid(treshold~frequency)+
  #       ggtitle(paste0('WBAT day/night - ',stationSet)))
  # 
  # if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
  #   print(ggplot(subset(res.CPOD$CPOD.hour,buzz_pos_minutes!=0),aes(x=dayNight,y=buzz_pos_minutes))+
  #         geom_boxplot()+
  #           ggtitle(paste0('buzz pos min - ',stationSet)))
  # }
}

windows()
print(ggplot(WBAT.all.summary,aes(x=station,y=log10(SA)))+
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  facet_grid(frequency~dataSet,scales = 'free_x')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle('SA all stations'))

print(ggplot(CPOD.all.summary,aes(x=stationName,y=pos_minutes))+
  geom_boxplot()+
  facet_wrap(~dataSet,scales = 'free')+
  ggtitle('CPOD positive minutes per station')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

print(ggplot(CPOD.all.summary,aes(x=stationName,y=buzz_ratio))+
  geom_boxplot()+
  facet_wrap(~dataSet,scales = 'free_x')+
  ggtitle('CPOD buzz click ratio per station')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))


WBAT.all.summary$time_day <- as.POSIXct(cut(WBAT.all.summary$datetime, breaks = "1 day"),tz='UTC')
CPOD.all.summary$station <- CPOD.all.summary$stationName
CPOD.all.summary$dataSet_station <- paste0(CPOD.all.summary$dataSet,'_',CPOD.all.summary$station)

# combine data frames
flagFirst <- T
for(stationSet in unique(WBAT.all.summary$dataSet_station)){
  idx.WBAT <- WBAT.all.summary$dataSet_station == stationSet
  
  if(!(unique(WBAT.all.summary$dataSet[idx.WBAT]) %in% c('2022-HKZ','2023-HKN'))){
    CPOD.stationSet <- strsplit(stationSet,' ')[[1]][1]
    CPOD.current <- subset(CPOD.all.summary,dataSet_station == CPOD.stationSet)
    
    WBAT.all.day <- WBAT.all.summary[idx.WBAT,] %>% group_by(time_day,frequency,treshold) %>% summarize(SA=mean(SA,na.rm=T))
    
    df.join <- left_join(WBAT.all.day,CPOD.current,by=c('time_day'))
    df.join$station <- unique(WBAT.all.summary$station[idx.WBAT])
    df.join$dataSet <- unique(WBAT.all.summary$dataSet[idx.WBAT])
    
    if(flagFirst){
      df.join.all <- df.join
      flagFirst <- F
    }else{
      df.join.all <- rbind(df.join.all,df.join)
    }
  }
}

for(idxDataSet in unique(df.join.all$dataSet)){
  if(dim(subset(df.join.all,dataSet == idxDataSet & all_buzz!=0 & frequency == 70 & treshold == -50))[1] != 0){
    print(ggplot(subset(df.join.all,dataSet == idxDataSet & all_buzz!=0 & frequency == 70 & treshold == -50),aes(x=buzz_ratio,y=log10(SA)))+
      geom_point()+
      geom_smooth(method='lm')+
      facet_wrap(~station)+
      ggtitle(paste0('CPOD/WBAT(70) relation - ',idxDataSet)))
  }
  
  if(dim(subset(df.join.all,dataSet == idxDataSet & all_buzz!=0 & frequency == 200 & treshold == -50))[1] != 0){
    print(ggplot(subset(df.join.all,dataSet == idxDataSet & all_buzz!=0 & frequency == 200 & treshold == -50),aes(x=buzz_ratio,y=log10(SA)))+
            geom_point()+
            geom_smooth(method='lm')+
            facet_wrap(~station)+
            ggtitle(paste0('CPOD/WBAT(200) relation - ',idxDataSet)))
  }
}



# 
# fileList <- list.files(file.path(dataPath),pattern="WBAT",full.names = T)
# fileList <- fileList[!file.info(fileList)$isdir]
# 
# for(myFile in fileList){
#   load(myFile)
#   
#   name.split  <- strsplit(myFile,'/')[[1]][3]
#   name.split  <- strsplit(name.split,'.RData')[[1]]
#   name.split  <- strsplit(name.split,'')
#   
#   WBAT.all <- subset(WBAT.all,depth == 0)
#   
#   WBAT.summary <- WBAT.all %>% group_by(treshold,IDinter,frequency,chunk) %>% summarize(SA=mean(SA,na.rm=T),
#                                                                                          datetime=first(datetime,na.rm=T),
#                                                                                          depthIntegration=first(depthIntegration,na.rm=T))
#   
#   ggplot(WBAT.summary,aes(x=datetime,y=log10(SA),col=as.factor(treshold)))+
#     geom_line()+
#     ggtitle()
# }
# 
# load(file.path(resultPath,'WBAT_2023-BSW_278093 BSW2_70khz.RData'))
# WBAT.join <- WBAT.all
# 
# load(file.path(resultPath,'WBAT_2021-BE_P1_grafton_200khz.RData'))
# WBAT.join <- rbind(WBAT.join,WBAT.all)
# 
# WBAT.join <- subset(WBAT.join,depth == 0)
# 
# WBAT.summary <- WBAT.join %>% group_by(treshold,IDinter,frequency,chunk) %>% summarize(SA=mean(SA,na.rm=T),
#                                                                               datetime=first(datetime,na.rm=T),
#                                                                               depthIntegration=first(depthIntegration,na.rm=T))
# 
# windows()
# ggplot(subset(WBAT.summary,treshold==-50),aes(x=datetime,y=log10(SA)))+
#   geom_line()
# 
# windows()
# WBAT.join$IDinterUnique <- paste0(WBAT.join$IDinter,'_',WBAT.join$chunk)
# df.plot <- subset(WBAT.join,treshold %in% c(-50))
# ggplot(df.plot,aes(x=datetime,group=IDinterUnique))+
#   stat_summary(aes(y =log10(SA)),fun=mean, geom = "line")
