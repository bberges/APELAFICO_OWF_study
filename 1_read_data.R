################################################################################
## Script functionality: Pre-processing the RAW data gained from the CPOD's and Echosounders
################################################################################

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, data.table)

rm(list=ls())

#setwd('D:/PAM_RABO/finless_neonate_HK')
setwd('Z:/APELAFICO/')
#setwd('G:/git/WBAT_APELAFICO')

figurePath    <- file.path('.','figures')
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

processCPOD <- F
processWBAT <- T

############################################################################
## Read CPOD files
############################################################################

if(processCPOD){
  fileList <- list.files(file.path(dataPath,'CPOD'),pattern="*.csv")
  
  flagFirst <- T
  
  for(file in fileList){
    CPOD.current              <- fread(file.path(dataPath,'CPOD',file))
    temp.list <- strsplit(strsplit(file,'.csv')[[1]],'_')[[1]]
    CPOD.current$dataSet <- temp.list[2]
    CPOD.current$station <- temp.list[3]
    
    if(flagFirst){
      CPOD.all <- CPOD.current
      flagFirst <- F
    }else{
      CPOD.all <- rbind(CPOD.all,CPOD.current)
    }
  }
  
  save('CPOD.all',
       file = file.path(dataPath,'CPOD_all.RData')) 
}

############################################################################
## Read WBAT exports
############################################################################

if(processWBAT){
  dirList <- list.files(file.path(dataPath,'WBAT'))
  
  for(myDir in dirList[c(44,46)]){
    print(myDir)
    flagFirst <- T
    fileList <- list.files(file.path(dataPath,'WBAT',myDir),pattern="*.txt")
    
    # dir.string <- strsplit(myDir,'_')[[1]]
    # dir.dataSet <- dir.string[1]
    # dir.dataSet <- gsub("-", "", dir.dataSet)
    # dir.phase <- dir.string[2]
    # dir.station <- dir.string[3]
    # dir.frequency   <- dir.string[4]
    # options(warn=1)
    # for(file in fileList){
    #   print(file)
    #   temp.list   <- strsplit(strsplit(file,'.txt')[[1]],'_')[[1]]
    #   if(length(temp.list) == 6){
    #     dataSet <- temp.list[1]
    #     phase   <- 'P1'
    #     station <- temp.list[2]
    #     treshold    <- as.numeric(strsplit(temp.list[4],'=')[[1]][2])
    #     frequency   <- as.numeric(strsplit(temp.list[3],'khz')[[1]][1])
    #     chunk       <- as.numeric(strsplit(temp.list[5],'=')[[1]][2]) 
    #   }else if(length(temp.list) == 7){
    #     dataSet <- temp.list[1]
    #     phase   <- temp.list[2]
    #     station <- temp.list[3]
    #     treshold    <- as.numeric(strsplit(temp.list[5],'=')[[1]][2])
    #     frequency   <- as.numeric(strsplit(temp.list[4],'khz')[[1]][1])
    #     chunk       <- as.numeric(strsplit(temp.list[6],'=')[[1]][2]) 
    #   }
    #   WBAT.current              <- fread(file.path(dataPath,'WBAT',myDir,file),skip=12,fill=TRUE)
    # }
    
    for(file in fileList){
      print(file)
      temp.list   <- strsplit(strsplit(file,'.txt')[[1]],'_')[[1]]
      if(length(temp.list) == 6){
        dataSet <- temp.list[1]
        phase   <- 'P1'
        station <- temp.list[2]
        treshold    <- as.numeric(strsplit(temp.list[4],'=')[[1]][2])
        frequency   <- as.numeric(strsplit(temp.list[3],'khz')[[1]][1])
        chunk       <- as.numeric(strsplit(temp.list[5],'=')[[1]][2])
      }else if(length(temp.list) == 7){
        dataSet <- temp.list[1]
        phase   <- temp.list[2]
        station <- temp.list[3]
        treshold    <- as.numeric(strsplit(temp.list[5],'=')[[1]][2])
        frequency   <- as.numeric(strsplit(temp.list[4],'khz')[[1]][1])
        chunk       <- as.numeric(strsplit(temp.list[6],'=')[[1]][2])
      }
      WBAT.current              <- fread(file.path(dataPath,'WBAT',myDir,file),skip=12,fill=TRUE)

      # fix of time strings
      n.char <- apply(WBAT.current[,'TIME'],1,nchar)
      WBAT.current$TIME[n.char == 7] <- paste0('0',WBAT.current$TIME[n.char == 7])
      WBAT.current$TIME[n.char == 5] <- paste0('000',WBAT.current$TIME[n.char == 5])
      WBAT.current$TIME[n.char == 4] <- paste0('0000',WBAT.current$TIME[n.char == 4])

      WBAT.current$datetime <- as.POSIXct(paste0(WBAT.current$DATE,'T',WBAT.current$TIME),format="%Y%m%dT%H%M%S",tz="UTC")

      # break into intervals
      thresh <- 60*5
      WBAT.current <- WBAT.current %>%
        arrange(datetime) %>%
        mutate(timediff = difftime(datetime, lag(datetime), units = 'secs'))

      WBAT.current <- WBAT.current %>%
        mutate(timediff = ifelse(is.na(timediff), 1, timediff)) %>%
        mutate(thresh_pass = timediff > thresh,
               thresh_pass = as.numeric(thresh_pass)) %>%  # true = 1
        mutate(thresh_pass = ifelse(timediff==1, 1, thresh_pass))

      WBAT.current$IDinter <- c(cumsum(WBAT.current$thresh_pass))

      # tidy up data frame
      WBAT.current <- WBAT.current[WBAT.current$NoPelagic > 0,]
      WBAT.current <- WBAT.current %>% dplyr::select(-thresh_pass,-timediff,-DATE,-TIME,-Latitude,-Longitude,-START_LOG,-Species,-Frequency,-MinDepth,-MaxDepth,-Transcei.,-Threshold,-BottomUpper,-B_CH0)
      idxColDepth <- str_detect(colnames(WBAT.current), 'P_CH')
      colnames(WBAT.current)[idxColDepth] <- as.numeric(sapply(strsplit(colnames(WBAT.current)[idxColDepth],'P_CH'), "[[", 2))
      WBAT.current <- WBAT.current %>% pivot_longer(!datetime & !IDinter & !STOP_LOG & !ScatterPCT & !NoPelagic,names_to = 'depth',values_to = 'SA')
      WBAT.current$depth <- as.numeric(WBAT.current$depth)*WBAT.current$ScatterPCT
      WBAT.current$depthIntegration <- WBAT.current$NoPelagic*WBAT.current$ScatterPCT
      WBAT.current <- WBAT.current[WBAT.current$SA != -1,]
      
      WBAT.current$dataSet  <- dataSet
      WBAT.current$phase    <- phase
      WBAT.current$station  <- station
      WBAT.current$frequency  <- frequency
      WBAT.current$treshold   <- treshold
      WBAT.current$chunk      <- chunk

      if(flagFirst){
        WBAT.all <- WBAT.current
        flagFirst <- F
      }else{
        WBAT.all <- rbind(WBAT.all,WBAT.current)
      }
    }

     save('WBAT.all',
         file = file.path(dataPath,paste0('WBAT_',myDir,'.RData')))
  } 
}

WBAT.subset <- subset(WBAT.all,treshold == -50)
# 
# write.csv(WBAT.subset,file = paste0(myDir,'_file.csv'))
# 
hist(WBAT.subset$SA)

################################################################################
# Some controles and first plots 
################################################################################

# load(file.path(resultPath,'WBAT_2023-BSW_278093 BSW2_70khz.RData'))
# WBAT.join <- WBAT.all
# 
# load(file.path(resultPath,'WBAT_2021-BE_P1_grafton_200khz.RData'))
# WBAT.join <- rbind(WBAT.join,WBAT.all)
# 
# WBAT.join <- subset(WBAT.join,depth == 0)
# 
# WBAT.summary <- WBAT.join %>% 
#   group_by(treshold,IDinter,frequency,chunk) %>% 
#   summarize(SA=mean(SA,na.rm=T),
#             datetime=first(datetime,na.rm=T),
#             depthIntegration=first(depthIntegration,na.rm=T))
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
