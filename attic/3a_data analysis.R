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

load(file = file.path(resultPath,'CPOD_WBAT_workspace.RData'))

WBAT.all.summary$time_hour <- as.POSIXct(cut(WBAT.all.summary$datetime, breaks = "1 hour"),tz='UTC')

CPOD.all.hour$time_day <- as.POSIXct(cut(CPOD.all.hour$time_hour, breaks = "1 day"),tz='UTC')

windows()
print(ggplot(WBAT.all.summary,aes(x=station,y=log10(SA)))+
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
        facet_grid(frequency~dataSet,scales = 'free_x')+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle('SA all stations'))

############################################################################
# inflation of CPOD data
############################################################################

ggplot(subset(CPOD.all.hour,dataSet == "2023-BE"),aes(x=time_day,y=pos_minutes,fill=dayNight))+
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~stationName)
CPOD.all.hour$dayNight
# 
# windows()
# ggplot()+
#   geom_line(data=subset(WBAT.all.summary,dataSet == idxDataSet),
#                aes(x=datetime,y=log10(SA)))+
#   facet_wrap(~station)

############################################################################
# SA over different intervals
############################################################################

# windows()
# ggplot(subset(WBAT.sample.all,treshold == -50),aes(x=as.factor(n),y=ratio,fill=as.factor(frequency)))+
#   geom_boxplot()+
#   ylim(0,1.5)+
#   facet_wrap(~dataSet,scales = 'free_x')

############################################################################
# 70khz vs 200khz
############################################################################

############################################################################
# temporal trends
############################################################################
# https://business-science.github.io/timetk/articles/TK05_Plotting_Seasonality_and_Correlation.html
# stl package

p <- subset(subset(WBAT.all.summary,treshold == -50),dataSet == "2021-BE") %>%
  group_by(station) %>%
  plot_seasonal_diagnostics(datetime, log10(SA),.interactive = F,.feature_set=c('hour','week'))+#,.geom = c("boxplot", "violin")
  ylim(0,3)

ggsave(file.path(figurePath,'Fig_ts_2021-BE.png'),
       p,
       width = 300,
       height = 300,
       units = c("mm"),
       dpi = 300)

p <- subset(subset(WBAT.all.summary,treshold == -50),dataSet == "2022-cpower") %>%
  group_by(station) %>%
  plot_seasonal_diagnostics(datetime, log10(SA),.interactive = F,.feature_set=c('hour','week'))+#,.geom = c("boxplot", "violin")
  ylim(0,3)

ggsave(file.path(figurePath,'Fig_ts_2022-cpower.png'),
       p,
       width = 300,
       height = 300,
       units = c("mm"),
       dpi = 300)

p <- subset(subset(WBAT.all.summary,treshold == -50),dataSet == "2022-HKZ") %>%
  group_by(station) %>%
  plot_seasonal_diagnostics(datetime, log10(SA),.interactive = F,.feature_set=c('hour','week'))+#,.geom = c("boxplot", "violin")
  ylim(0,3)

ggsave(file.path(figurePath,'Fig_ts_2022-HKZ.png'),
       p,
       width = 300,
       height = 300,
       units = c("mm"),
       dpi = 300)

p <- subset(subset(WBAT.all.summary,treshold == -50),dataSet == "2023-BSW") %>%
  group_by(station) %>%
  plot_seasonal_diagnostics(datetime, log10(SA),.interactive = F,.feature_set=c('hour','week'))+#,.geom = c("boxplot", "violin")
  ylim(0,3)

ggsave(file.path(figurePath,'Fig_ts_2023-BSW.png'),
       p,
       width = 300,
       height = 300,
       units = c("mm"),
       dpi = 300)

p <- subset(subset(WBAT.all.summary,treshold == -50),dataSet == "2023-BE") %>%
      group_by(station) %>%
      plot_seasonal_diagnostics(datetime, log10(SA),.interactive = F,.feature_set=c('hour','week'))+#,.geom = c("boxplot", "violin")
      ylim(0,3)

ggsave(file.path(figurePath,'Fig_ts_2023-BE.png'),
       p,
       width = 300,
       height = 300,
       units = c("mm"),
       dpi = 300)

p <- subset(subset(WBAT.all.summary,treshold == -50),dataSet == "2023-HKN") %>%
  group_by(station) %>%
  plot_seasonal_diagnostics(datetime, log10(SA),.interactive = F,.feature_set=c('hour','week'))+#,.geom = c("boxplot", "violin")
  ylim(0,3)

ggsave(file.path(figurePath,'Fig_ts_2023-HKN.png'),
       p,
       width = 300,
       height = 300,
       units = c("mm"),
       dpi = 300)

############################################################################
# spatial trends
############################################################################
