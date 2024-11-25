if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, data.table, suncalc)

rm(list=ls())

#setwd('D:/PAM_RABO/finless_neonate_HK')
setwd("C:/Users/joost/Documents/05_WUR/Year_3/Internship/Offshore_Windparks/OWF_Project_folder/bberges_APELAFICO_OWF_study/APELAFICO_OWF_study/")
#setwd('G:/git/WBAT_APELAFICO')

getwd()

figurePath    <- file.path('.','figures')
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

############################################################################
## reading annotated data sets
############################################################################

processFiles <- F
if(processFiles){
  fileList <- list.files(file.path(dataPath,'2022-cpower_annot'),pattern = "\\.txt$")     # Making a list of all the file names              
  
  flagFirst <- T
  for(file in fileList){                                                                  # Iterating over the files in the file list
    print(file)
    temp.list   <- strsplit(strsplit(file,'.txt')[[1]],'_')[[1]]                          # First removing .txt and then splitting on _ to get lose string parts
    if(length(temp.list) == 6){                                                           # This checks of name is split in 6 parts > NO? then skip code
      dataSet <- temp.list[1]
      phase   <- 'P1'
      station <- temp.list[3]
      treshold    <- as.numeric(strsplit(temp.list[5],'=')[[1]][2])                       # Does some data wrangling on the names of the stations
      frequency   <- as.numeric(strsplit(temp.list[4],'khz')[[1]][1])
      chunk       <- as.numeric(strsplit(temp.list[6],'=')[[1]][2]) 
    }
    
    WBAT.current              <- fread(file.path(dataPath, '2022-cpower_annot', file), skip="DATE") # Read data, and skip until DATE
    
    # fix of time strings  
    n.char <- apply(WBAT.current[,'TIME'],1,nchar)                                        # n.char calculates the number of characters in the string 
    WBAT.current$TIME[n.char == 7] <- paste0('0',WBAT.current$TIME[n.char == 7])          # Not all time strings have the same length 
    WBAT.current$TIME[n.char == 5] <- paste0('000',WBAT.current$TIME[n.char == 5])        # Based on n.char it adds x amount of zero's to fix this issue
    WBAT.current$TIME[n.char == 4] <- paste0('0000',WBAT.current$TIME[n.char == 4])
    
    WBAT.current$datetime <- as.POSIXct(paste0(WBAT.current$DATE, 'T', WBAT.current$TIME),# Combine DATE and TIME into a single datetime object
                                        format="%Y%m%dT%H%M%S", tz="UTC")                 # 'T' is a seperation between date and time
    
    # break into intervals
    thresh <- 60*5                                                                        # Sets threshold of time intervals to 5 minutes (60 sec * 5 = 300 sec)
    WBAT.current <- WBAT.current %>% 
      arrange(datetime) %>%
      mutate(timediff = difftime(datetime, lag(datetime), units = 'secs'))                # Time gap between the rows
    
    WBAT.current <- WBAT.current %>%
      mutate(timediff = ifelse(is.na(timediff), 1, timediff)) %>%                         # Replaces all NA with a 1 (first time diff as this is non existent)
      mutate(thresh_pass = timediff > thresh,                                             # Looks if time diff is more then 5 minutes ### time diff is bigger then threshold of 5 min?? Then return TRUE otherwise return False 
             thresh_pass = as.numeric(thresh_pass)) %>%  # true = 1                       # Makes the true = 1 and false = 0
      mutate(thresh_pass = ifelse(timediff==1, 1, thresh_pass))                           # All time diff = 1 get a 1 in the thres_pass column
    
    WBAT.current$IDinter <- c(cumsum(WBAT.current$thresh_pass))                           # Gives a interval ID, if time diff is bigger than add 1 to IDinter, all separate time intervals get a unique ID this way
    
    # tidy up data frame
    WBAT.current <- WBAT.current[WBAT.current$NoPelagic > 0,]                             # Remove rows where NoPelagic is smaller than 0
    WBAT.current <- WBAT.current %>%                                                      # Removes unnecessary columns 
      dplyr::select(-thresh_pass,-timediff,-DATE,
                    -TIME,-Latitude,-Longitude,
                    -START_LOG,-Species,-Frequency,
                    -MinDepth,-MaxDepth,-Transcei.,
                    -Threshold,-BottomUpper,-B_CH0)
    
    idxColDepth <- str_detect(colnames(WBAT.current), 'P_CH')                             # Does detect columns with P_CH
    colnames(WBAT.current)[idxColDepth] <- as.numeric(sapply(strsplit(colnames(WBAT.current)[idxColDepth],'P_CH'), "[[", 2)) # Extracts information an mutates a column
    
    WBAT.current <- WBAT.current %>%                                                      # from wide to long format, except the specific column and depth get the colnames and SA get the values
      pivot_longer(!datetime & !IDinter & !STOP_LOG & !ScatterPCT & !NoPelagic, names_to = 'depth', values_to = 'SA')
    
    WBAT.current$depth <- as.numeric(WBAT.current$depth)*WBAT.current$ScatterPCT          # depth * ScatterPCT correcting depth 
    WBAT.current$depthIntegration <- WBAT.current$NoPelagic*WBAT.current$ScatterPCT       # NoPelagic * ScatterPCt making columns depthintegreation
    WBAT.current <- WBAT.current[WBAT.current$SA != -1,]                                  # Filtering out -1 SA values as these are incorrect
    
    WBAT.current$dataSet  <- dataSet                                                      # Adds metadata in 6 Metadata columns 
    WBAT.current$phase    <- phase
    WBAT.current$station  <- station
    WBAT.current$frequency  <- frequency
    WBAT.current$treshold   <- treshold
    WBAT.current$chunk      <- chunk
    
    if(flagFirst){
      WBAT.annot <- WBAT.current                                                          # If first iteration: current = annot 
      flagFirst <- F
    }else{
      WBAT.annot <- rbind(WBAT.annot,WBAT.current)                                        # If not first iteration: merge current in annot
    }
  }
  
  save('WBAT.annot',
       file = file.path(dataPath,'2022-cpower_annot',paste0('WBAT_2022-cpower-annot_-60db.RData'))) # Save WBAT.annot
  
  WBAT.annot$dataSet_station <- paste0(WBAT.annot$dataSet,'_',WBAT.annot$station)         # New column dataSet_station
  WBAT.annot$surveName <- paste0(WBAT.annot$dataSet_station,'_',WBAT.annot$frequency,'khz')   # New column surveName

  # combine data sets
  flagFirst <- T
  for(stationSet in unique(WBAT.annot$dataSet_station)){                                  # Get every unique dataSet_station in WBAT.annot and name stationSet
    print(stationSet)
    surveNameUnique <- unique(WBAT.annot[WBAT.annot$dataSet_station == stationSet,]$surveName) # Extract unique surveName associated with current stationSet and name surveNameUnique
    
    for(idxDataSet in surveNameUnique){                                                   # get every unique surveName from surveNameUnique and name idxDataSet
      load(file.path(dataPath,paste0('WBAT_',idxDataSet,'.RData')))                       # Load file with matching to idxDataSet
      
      WBAT.all$dataSet_station <- paste0(WBAT.all$dataSet,'_',WBAT.all$station)           # Add dataSet_station column to WBAT.all
      WBAT.all$surveName <- paste0(WBAT.all$dataSet_station,'_',WBAT.all$frequency,'khz') # Add surveName column to WBAT.all
      WBAT.all <- subset(WBAT.all,treshold == -60)                                        # Subset on -60 dB threshold ###################################
      
      if(flagFirst){
        WBAT.comp <- rbind(WBAT.annot,WBAT.all)                                           # If first iter combine annot with current .all
        flagFirst <- F
      }else{
        WBAT.comp <- rbind(WBAT.comp,WBAT.all)                                            # If second iter combine comp with current .all
      }
    }
  }
  
  save('WBAT.comp',
       file = file.path(dataPath,'2022-cpower_annot',paste0('WBAT_2022-cpower-comp_-60db.RData')))  # Save as .comp
}else{
  load(file = file.path(dataPath,'2022-cpower_annot',paste0('WBAT_2022-cpower-comp_-60db.RData')))  # Load .comp
}

WBAT.comp$filt <- NA                                                                      # Creates column $filt with NA's 
WBAT.comp$filt[WBAT.comp$treshold == 1] <- 1                                              # Rows with treshold 1 are filt 1
WBAT.comp$comp <- NA                                                                      # Creates column $comp with NA's 
WBAT.comp$comp[WBAT.comp$treshold == 1] <- 'ref'                                          # Rows with treshold 1 are "ref"

# compare results with filtering
for(idxDataSet in unique(WBAT.comp$surveName)){                                           # Every unique surveName in .comp to idxDataSet
  print(idxDataSet)
  idxFilt <- WBAT.comp$surveName == idxDataSet & WBAT.comp$treshold == -60                # If surveName matches idxDataSet & treshold = -60
  WBAT.current <- WBAT.comp[idxFilt,]                                                     # Make subset of .comp in .current
  
  idxAnnot <- WBAT.comp$surveName == idxDataSet & WBAT.comp$treshold == 1                 # If surveName matches idxDataSet & threshold = 1
  minDate <- min(WBAT.comp[idxAnnot,]$datetime)                                           # Calculate earliest datetime matching idxAnnot
  maxDate <- max(WBAT.comp[idxAnnot,]$datetime)                                           # Calculate latest datetime matching idxAnnot

  upper_bound <- quantile(log10(WBAT.current$SA),0.0)    #median(log10(WBAT.current$SA)) + 2 * mad(log10(WBAT.current$SA), constant = 1)
                                                                                          # Determines the 75% threshold
  idxIn <- log10(WBAT.current$SA) >= upper_bound                                           # Filters out everything smaller then 75% treshold 
  
  idxFiltDate <- WBAT.current$datetime > minDate & WBAT.current$datetime < maxDate        # Removes everything outside the set time range
  
  WBAT.comp$filt[which(idxFilt)[!idxIn]] <- -1                                            # Assigns -1 to the filt column for rows in idxFilt where idxIn is FALSE
  WBAT.comp$filt[which(idxFilt)[idxIn]] <- 1                                              # Assigns 1 to the filt column for rows in idxFilt where idxIn is TRUE
  
  WBAT.comp$comp[which(idxFilt)[!idxFiltDate]] <- 'out'                                   # Assigns 'out' to the comp column for rows in idxFilt where idxFiltDate is FALSE
  WBAT.comp$comp[which(idxFilt)[idxFiltDate]] <- 'in'                                     # Assigns 'in' to the comp column for rows in idxFilt where idxFiltDate is TRUE
}

idxOrder <- order(WBAT.comp$datetime)                                                     # Sort in ascending order
thresh <- 60*6
WBAT.comp <- WBAT.comp[idxOrder,] %>%                                                     # Ordering and calculating time diff
  mutate(timediff = difftime(datetime, lag(datetime), units = 'secs'))

WBAT.comp <- WBAT.comp %>%
  mutate(timediff = ifelse(is.na(timediff), 1, timediff)) %>%                             # Replaces all NA with a 1 (first time diff as this is non existent)
  mutate(thresh_pass = timediff > thresh,                                                 # Looks if time diff is more then 5 minutes ### time diff is bigger then threshold of 5 min?? Then return TRUE otherwise return False 
         thresh_pass = as.numeric(thresh_pass)) %>%  # true = 1                           # Makes the true = 1 and false = 0
  mutate(thresh_pass = ifelse(timediff==1, 1, thresh_pass))                               # All time diff = 1 get a 1 in the thresh_pass column

WBAT.comp$IDinter <- c(cumsum(WBAT.comp$thresh_pass))                                     # Creating IDinter again
#WBAT.comp$IDinterTag <- paste0(WBAT.comp$dataSet_station,'_',WBAT.comp$treshold,'_',WBAT.comp$frequency,'_',WBAT.current$IDinter)

windows()
ggplot(subset(WBAT.comp,comp %in% c('in','ref') & filt == 1 & depth == 0),aes(x=as.factor(treshold),y=log10(SA)))+# & log10(SA) > 0
  geom_boxplot()+
  labs(x = "Automatic annotation (-60) vs Manual annotation (1)", title = "log10(SA) of automatic annotation compared to manual annotation at -60dB") +
  facet_grid(station~frequency)

summarized_data <- subset(WBAT.comp,comp %in% c('in','ref') & filt == 1 & depth == 0 & SA != 0) %>% 
  group_by(surveName,comp) %>% 
  summarize(SA=mean(log10(SA)))

windows()
ggplot(summarized_data, aes(x = surveName, y = SA, fill = comp)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(x = "Survey Name", y = "Mean log10(SA)", title = "Mean log10(SA) by Survey Name and Composition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("blue", "red")) 

# Testing and taking the sum instead of the mean
test <- subset(WBAT.comp, frequency == 70 & comp == "ref" & station == "267814" & depth != 0)
test_raw <- subset(WBAT.comp, frequency == 70 & comp == "in" & station == "267814" & depth != 0)

test <- subset(WBAT.comp, frequency == 200 & comp == "ref" & depth != 0)
test_raw <- subset(WBAT.comp, frequency == 200 & comp == "in" & depth != 0)

test_combined <- subset(WBAT.comp, frequency == 70 & comp %in% c('in','ref') & depth != 0)

summary(test$SA)
summary(test_raw$SA)
summary(test_raw_filtered$SA)

sum(test$SA)
sum(test_raw$SA)
sum(test_raw_filtered$SA)

mean(test$SA)
mean(test_raw$SA)
mean(test_raw_filtered$SA)


Lela <- quantile((test_raw$SA), 0.99)    
test_raw_filtered <-  subset(test_raw, (test_raw$SA) >= Lela) 



ggplot(test_raw_filtered, aes(x = surveName, y = SA))+
  geom_col() 

ggplot(test, aes(x = surveName, y = SA))+
  geom_col() 


plot(test_combined_filtered$SA)
plot(test$SA)

# Set the percentage to drop (e.g., drop 25%)
drop_percentage <- 0.75

# Filter the data for 'ref' and 'in', then drop the bottom x% for each
test_combined_filtered <- test_combined %>%
  group_by(comp) %>%
  filter(comp == "ref" | comp == "in") %>%
  # Apply the drop logic to each group
  group_by(comp) %>%
  filter(if_else(comp == "in", 
                 SA > quantile(SA, probs = drop_percentage), 
                 SA > quantile(SA, probs = 0.0))) %>%
  ungroup()

ggplot(test_combined_filtered, aes(x = surveName, y = log10(SA), fill = comp))+
  geom_boxplot() 


# Create a summarise
total_sum <- test_combined_filtered %>% 
  group_by(surveName, comp) %>% 
  summarise(sum = sum(SA))

ggplot(total_sum, aes(x = surveName, y = log10(sum), fill = comp))+
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("blue", "red"))

ggplot(total_sum, aes(x = surveName, y = sum, fill = comp))+
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("blue", "red"))













  