library(ggplot2)
library(tidyverse)
library(lubridate)
library(icesTAF)
library(timetk)

rm(list=ls())

#setwd('D:/PAM_RABO/finless_neonate_HK')
setwd('D:/git/APELAFICO_OWF_study/')
#setwd('G:/git/WBAT_APELAFICO')

sourceDir(file.path('.','function'))

figurePath    <- file.path('.','figures')
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

WBAT.tab <- read.csv(file.path(dataPath,'survey_db.csv'))
WBAT.tab <- WBAT.tab[,c(1:5,8,9)]
WBAT.tab$dataSet_station <- paste0(WBAT.tab$dataSet,'_',WBAT.tab$station)

overview.tab <- read.csv(file.path(dataPath,'data_overview.csv'))
overview.tab$stationSet <- paste0(overview.tab$dataSet,'_',overview.tab$station)
overview.tab$pairingName <- paste0(overview.tab$dataSet,'_',overview.tab$pairing)
overview.tab <- overview.tab %>% select(-c('year','dataSet','station'))


load(file = file.path(resultPath,'CPOD_WBAT_workspace.RData'))

# adjust CPOD data frames (should be done at data generation...)
CPOD.all.day$station <- CPOD.all.day$stationName
CPOD.all.day$stationSet <- paste0(CPOD.all.day$dataSet,'_',CPOD.all.day$station)
CPOD.all.day$stationSet[CPOD.all.day$stationSet == "2023-BSW_274174"] <- "2023-BSW_274174 BSW1"
CPOD.all.day$stationSet[CPOD.all.day$stationSet == "2023-BSW_278093"] <- "2023-BSW_278093 BSW2"
CPOD.all.day$stationSet[CPOD.all.day$stationSet == "2023-BSW_267814"] <- "2023-BSW_267814 BSW3"
CPOD.all.day$stationSet[CPOD.all.day$stationSet == "2023-BSW_267838"] <- "2023-BSW_267838 BSW4"
CPOD.all.day <- CPOD.all.day %>% select(-c('lat','lon'))
CPOD.all.day <- left_join(CPOD.all.day,overview.tab,by=c('stationSet'))
CPOD.all.day <- subset(CPOD.all.day,pairing != -1)

CPOD.all.hour$station <- CPOD.all.hour$stationName
CPOD.all.hour$stationSet <- paste0(CPOD.all.hour$dataSet,'_',CPOD.all.hour$station)
CPOD.all.hour$stationSet[CPOD.all.hour$stationSet == "2023-BSW_274174"] <- "2023-BSW_274174 BSW1"
CPOD.all.hour$stationSet[CPOD.all.hour$stationSet == "2023-BSW_278093"] <- "2023-BSW_278093 BSW2"
CPOD.all.hour$stationSet[CPOD.all.hour$stationSet == "2023-BSW_267814"] <- "2023-BSW_267814 BSW3"
CPOD.all.hour$stationSet[CPOD.all.hour$stationSet == "2023-BSW_267838"] <- "2023-BSW_267838 BSW4"
CPOD.all.hour <- CPOD.all.hour %>% select(-c('lat','lon'))
CPOD.all.hour <- left_join(CPOD.all.hour,overview.tab,by=c('stationSet'))
CPOD.all.hour <- subset(CPOD.all.hour,pairing != -1)

CPOD.all.min$station <- CPOD.all.min$stationName
CPOD.all.min$stationSet <- paste0(CPOD.all.min$dataSet,'_',CPOD.all.min$station)
CPOD.all.min$stationSet[CPOD.all.min$stationSet == "2023-BSW_274174"] <- "2023-BSW_274174 BSW1"
CPOD.all.min$stationSet[CPOD.all.min$stationSet == "2023-BSW_278093"] <- "2023-BSW_278093 BSW2"
CPOD.all.min$stationSet[CPOD.all.min$stationSet == "2023-BSW_267814"] <- "2023-BSW_267814 BSW3"
CPOD.all.min$stationSet[CPOD.all.min$stationSet == "2023-BSW_267838"] <- "2023-BSW_267838 BSW4"
CPOD.all.min <- CPOD.all.min %>% select(-c('lat','lon'))
CPOD.all.min <- left_join(CPOD.all.min,overview.tab,by=c('stationSet'))
CPOD.all.min <- subset(CPOD.all.min,pairing != -1)

WBAT.pairwise <- subset(WBAT.all.summary,pairing != -1 & n >= 9 & treshold == -50 & frequency == 70 & depthIntegration > 5)
WBAT.pairwise$type <- factor(WBAT.pairwise$type,levels = c("OWF","out","wreck"))
WBAT.pairwise$time_hour <- as.POSIXct(cut(WBAT.pairwise$datetime, breaks = "1 hour"),tz='UTC')

CPOD.all.min$type <- factor(CPOD.all.min$type,levels = c("OWF","out","wreck"))
CPOD.all.hour$type <- factor(CPOD.all.hour$type,levels = c("OWF","out","wreck"))
CPOD.all.day$type <- factor(CPOD.all.day$type,levels = c("OWF","out","wreck"))

CPOD.all.hour$time_hour <- as.POSIXct(cut(CPOD.all.hour$time_hour, breaks = "1 hour"),tz='UTC')

############################################################################
# pairwise comparison
############################################################################
#unique(WBAT.pairwise$type)

print(ggplot(WBAT.pairwise,aes(as.factor(pairing),log10(SA),col=as.factor(type)))+
        geom_boxplot(position="dodge")+
        theme_bw()+
        ylim(0.5,3)+
        xlab('Pairing')+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        facet_grid(dayNight~dataSet)+
        ggtitle('WBAT-all pairing'))

for(idxDataSet in unique(WBAT.pairwise$dataSet)){
  # WBAT
  df.plot <- subset(WBAT.pairwise,dataSet == idxDataSet)
  
  print(ggplot(df.plot,aes(as.factor(pairing),log10(SA),fill=as.factor(type)))+
          geom_boxplot(position="dodge")+
          theme_bw()+
          ylim(0.5,2.5)+
          xlab('Pairing')+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          ggtitle(paste0(idxDataSet)))
  
  # CPOD
  df.plot <- subset(CPOD.all.hour,dataSet == idxDataSet)
  
  ggplot(df.plot,aes(time_hour,pos_minutes,col=type))+
    geom_line()
  
  print(ggplot(df.plot,aes(as.factor(pairing),pos_minutes,fill=as.factor(type)))+
          geom_boxplot(position="dodge")+
          theme_bw()+
          ylim(0,20)+
          xlab('Pairing')+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          ggtitle(paste0(idxDataSet)))
  
  
  for(idxPairing in unique(WBAT.pairwise$pairing)){
    df.plot <- subset(WBAT.pairwise,dataSet == idxDataSet & pairing == idxPairing)
    df.plot <- df.plot[order(df.plot$type),]
    
    print(df.plot %>%
            group_by(type) %>%
            plot_seasonal_diagnostics(datetime,
                                      log10(SA),
                                      .interactive = F,
                                      .feature_set=c('year','hour','week'))+
            theme_bw()+
            ylim(0.5,2.5)+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
            ggtitle(paste0('WBAT-',idxDataSet,' pairing-',idxPairing)))
    
    df.plot <- subset(CPOD.all.hour,dataSet == idxDataSet & pairing == idxPairing)
    df.plot <- df.plot[order(df.plot$type),]
    
    print(df.plot %>%
            group_by(type) %>%
            plot_seasonal_diagnostics(time_hour,
                                      pos_minutes,
                                      .interactive = F,
                                      .feature_set=c('year','hour','week'))+
            theme_bw()+
            ylim(0,10)+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
            ggtitle(paste0('CPOD-',idxDataSet,' pairing-',idxPairing)))
  }
}

############################################################################
# CPOD/SA correlation
############################################################################
df.all <- left_join(WBAT.pairwise,CPOD.all.hour,by=c('stationSet',
                                                     'type',
                                                     'dataSet',
                                                     'pairing',
                                                     'time_hour'))

temp <- subset(df.all,dayNight.x == 'day')


ggplot(df.all,aes(x=pos_minutes,y=log10(SA),col=type,shape=dataSet,group=stationSet))+
  stat_summary(fun.data=mean_se, geom="pointrange", na.rm = TRUE)+
  facet_wrap(~dayNight)


############################################################################
# code dump
############################################################################

windows()
ggplot(subset(WBAT.all.summary,pairing != -1 & treshold == -50 & frequency == 70),aes(x=datetime,y=log(depthIntegration),col=station))+
  geom_line()+
  facet_wrap(~dataSet,scales = 'free_x')

WBAT.all.summary$time_hour <- as.POSIXct(cut(WBAT.all.summary$datetime, breaks = "1 hour"),tz='UTC')
CPOD.all.hour$time_day <- as.POSIXct(cut(CPOD.all.hour$time_hour, breaks = "1 day"),tz='UTC')
windows()
ggplot(subset(CPOD.all.hour,dataSet == "2023-BE"),aes(x=time_day,y=pos_minutes,fill=dayNight))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~stationName)

ggplot(subset(CPOD.all.hour,dataSet == "2023-BE"),aes(x=stationName,y=pos_minutes))+
  geom_boxplot()

windows()
print(ggplot(WBAT.all.summary,aes(x=station,y=log10(SA)))+
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
        facet_grid(frequency~dataSet,scales = 'free_x')+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle('SA all stations'))

