### Spatial Working Memory Task - preprocessing from data from log-files ####
#Task by Klingberg, T., Forssberg, H. & Westerberg, H. Increased Brain Activity in Frontal and Parietal Cortex Underlies the Development of Visuospatial Working Memory Capacity during Childhood. J. Cogn. Neurosci. 14, 1-10 (2002).
#Code by Sophia-Helen Sass

#please additionally run "SWM_corrupt_logfix.R" to gather the two data sets that had been split into 4 parts due to technical issues 

###### preparation ############################################
#### legend ####
  # SWM     = spatial location memory (= 1st response)
  # SER     = serial order memory (= 2nd response)
  # _4      = 4 Items to memorize and recall in one trial 
  # _7      = 7 Items to memorize and recall in one trial  
  
  
#clear workspace
rm(list = ls())
  
  #install packages to read json files
  #install.packages("rjson")
  #install.packages("plyr")
  
  #load packages
  library(rjson)
  library(plyr)
  library(dplyr)
  
  #set working and data directory
  setwd("your_directory")
  datadir = ("your_directory")
  
  ##### fetching data from folders ########################################
  #structure: one folder per participant contains json-data files of every task in experiment. Name of folder: Record-ID, e.g. 01528
  #Algorithm goes through all participant folders to fetch respective json-files, convert them to data frames and save a new data set containing mean data of all participants which is ready to be analyzed.
  
  folders = dir(datadir) 
  nid = max(c(length(folders)))
  SWM = array(NA, c(nid, 24)) 
  
  for (f in 1:nid){ 
    tryCatch({
      subj    = folders[f]
      txtpath = paste0(datadir, "/", subj, sep="")
      setwd(txtpath)
      
  data_raw <- fromJSON(file = "spatial_working_memory_task-results.json", method = "C", unexpected.escape = "error", simplify = T)
  
  #extract first entry of the list
  data_first_entry <- data_raw[1]
  #save list as character string (fromJSON function needs json string)
  data_as_chr <-as.character(data_first_entry)
  # convert to list
  data_list <- fromJSON(json_str = data_as_chr)
  
  # json file contains empty values for skipped 2nd questions. Replace them by NA in order
  # to transform the list to a data frame later
  for(i in 1:length(data_list)){
    if(data_list[[i]][["ended_on"]]=="skipped")
      data_list[[i]][["duration"]]=NA}
  
  # fix the space in question2 for algorithm to work
  data_list[[95]][["sender"]]="question2"
  
  # extract training trials
  Training <- data_list[c(31,32,43,44,62,63,80,81)]
  Training=ldply(Training,data.frame) # side note: 1 correct response is missing 
  # in training trial 6_15, question 1 
  
  # remove them from data_list, so they won't be fetched in the following
  data_list <- data_list[-c(1:85)]
  
  
  # extract responses for question 1 and question 2 and turn it into data frame
  spatial_mem<-c() 
  serial_mem <- c()
  new_element <-c()
  for (j in 1:length(data_list)) {
    if (data_list[[j]][["sender"]]=="question" | data_list[[j]][["sender"]]=="question1" |data_list[[j]][["sender"]]=="question2") 
      spatial_mem[[length(spatial_mem)+1]]<- (data_list[[j]]) }
  
  spatial_mem=ldply(spatial_mem,data.frame)
  
  #in trial 65_15 (data_list entry 826) there is no "correctResponse" and "correct" given --> cannot be analyzed or restored and has to be excluded here  
  spatial_mem<- spatial_mem[-c(107), ]
  
  #change data type for further processing
  spatial_mem$sender<- as.character(spatial_mem$sender) 
  spatial_mem$sender_type<- as.character(spatial_mem$sender_type)
  spatial_mem$sender_id<- as.character(spatial_mem$sender_id)
  spatial_mem$ended_on<- as.character(spatial_mem$ended_on)
  spatial_mem$timestamp<- as.character(spatial_mem$timestamp)
  
  #grouping in 4 step and 7 step conditions (including all trials SHOWN in the task)
  item4_tmp_num <- subset(spatial_mem[1:48, ]) 
  item4_tmp2_num <- subset(spatial_mem[144:191, ])
  item4_tmp_num<- rbind(item4_tmp_num,item4_tmp2_num)
  item7_tmp_num <- subset(spatial_mem[49:143, ])
  
  #grouping in spatial memory(q1) and serial memory(q2) conditions (including all trials SHOWN in the task)
  SWM_q1_num <- subset(spatial_mem, sender == "question"|sender == "question1") 
  SWM_q2_num <- subset(spatial_mem, sender == "question2") 
  
  #exclude RTs <150 ms (participant probably clicked randomly)
  #so these variables will be used to count the number of trials USED in the analyses
  #Info: there are some negative RTs in the data and I don't know how this is possible
  RToutlierQ1 <- subset(SWM_q1_num, duration < 150 & is.na(duration)==FALSE)
  RToutlier <- rbind(RToutlierQ1,subset(SWM_q2_num, duration < 150 & is.na(duration) == FALSE))
  item4_tmp <- subset(item4_tmp_num, duration >= 150 | is.na(duration)==TRUE)
  item7_tmp <- subset(item7_tmp_num, duration >= 150 | is.na(duration)==TRUE)
  
  SWM_q1 <- subset(SWM_q1_num, duration >= 150 | is.na(duration)==TRUE)
  SWM_q2 <- subset(SWM_q2_num, duration >= 150 | is.na(duration)==TRUE)
  
  #compute basic measures
  # spatial mem nb corr 4 items
  SWM_nbCorr_4 = length(which(item4_tmp$correct==TRUE & item4_tmp$sender == "question" | 
                                item4_tmp$sender == "question1" & item4_tmp$correct==TRUE))
  # serial mem nb corr 4 items
  SER_nbCorr_4 = length(which(item4_tmp$correct==TRUE & item4_tmp$sender == "question2"))  
  
  # spatial mem nb corr 7 items
  SWM_nbCorr_7 = length(which(item7_tmp$correct==TRUE & item7_tmp$sender == "question" | 
                                item7_tmp$sender == "question1"& item7_tmp$correct==TRUE))  
  # serial mem nb corr 7 items
  SER_nbCorr_7 = length(which(item7_tmp$correct==TRUE & item7_tmp$sender == "question2")) 
  
  #### compute summarized SWM task measures #######
  SWM[f,1]  = subj
  # spatial memory rt correct 4 items
  SWM[f,2] = mean(item4_tmp$duration[(item4_tmp$sender == "question" | 
                                        item4_tmp$sender == "question1") & item4_tmp$correct==TRUE], na.rm = T)   
  # serial memory rt correct 4 items
  SWM[f,3] = mean(item4_tmp$duration[(item4_tmp$sender == "question2") &
                                       item4_tmp$correct==TRUE], na.rm = T) 
  # spatial memory rt correct 7 items
  SWM[f,4] = mean(item7_tmp$duration[(item7_tmp$sender == "question" | 
                                        item7_tmp$sender == "question1") & item7_tmp$correct==TRUE], na.rm = T)    
  # serial memory rt correct 7 items
  SWM[f,5] = mean(item7_tmp$duration[(item7_tmp$sender == "question2") & 
                                       item7_tmp$correct==TRUE], na.rm = T)            
  
  # number of RT below 150 ms that were excluded
  SWM[f,6] = nrow(RToutlier)                          
  
  # subset Spatial mem and Serial mem for 4- and 7-item trials
  numq1i4 <- subset(item4_tmp,sender=="question" | sender =="question1")
  numq2i4 <- subset(item4_tmp,sender=="question2")
  numq1i7 <- subset(item7_tmp,sender=="question" | sender =="question1")
  numq2i7 <- subset(item7_tmp,sender=="question2")
  
  # spatial mem sd(rt) correct 4 items
  SWM[f,7] = sd(item4_tmp$duration[(item4_tmp$sender == "question" | 
                                      item4_tmp$sender == "question1") & 
                                     item4_tmp$correct==TRUE ], na.rm = T) 
  # ser mem sd(rt) correct 4 items
  SWM[f,8] = sd(item4_tmp$duration[item4_tmp$sender=="question2" & 
                                     item4_tmp$correct==TRUE],na.rm=T) 
  # spatial mem sd(rt) correct 7 items
  SWM[f,9] = sd(item7_tmp$duration[(item7_tmp$sender == "question" | 
                                      item7_tmp$sender == "question1") & 
                                     item7_tmp$correct==TRUE], na.rm = T) 
  # ser mem sd(rt) correct 7 items
  SWM[f,10] = sd(item7_tmp$duration[item7_tmp$sender=="question2" & 
                                      item7_tmp$correct==TRUE],na.rm=T)  
  # spatial mem sd(rt) correct total
  SWM[f,11] = sd(SWM_q1$duration[SWM_q1$correct==TRUE],na.rm=T)
  # spatial mem mean rt correct total
  SWM[f,12] = mean(SWM_q1$duration[SWM_q1$correct==TRUE],na.rm=T) 
  # serial mem sd(rt) correct total
  SWM[f,13] = sd(SWM_q2$duration[SWM_q2$correct==TRUE],na.rm=T)  
  # serial mem mean rt correct total
  SWM[f,14] = mean(SWM_q2$duration[SWM_q2$correct==TRUE],na.rm=T) 
  
  
  # missing trials (timeouts)
  SWM_miss_4 <- subset(item4_tmp, sender=="question" & ended_on == "timeout" |
                         sender=="question1" & ended_on == "timeout")
  SER_miss_4 <- subset(item4_tmp, sender=="question2" & ended_on == "timeout")
  
  SWM_miss_7 <- subset(item7_tmp, sender=="question" & ended_on == "timeout"| 
                         sender=="question1" & ended_on == "timeout")
  
  SER_miss_7 = subset(item7_tmp, sender=="question2" & ended_on == "timeout")
  
  
  SWM[f,15] = nrow(SWM_miss_4)
  SWM[f,16] = nrow(SER_miss_4)
  SWM[f,17] = nrow(SWM_miss_7)
  SWM[f,18] = nrow(SER_miss_7)
  
  # accuracy scores
  SWM[f,19] = ((SWM_nbCorr_4)/(nrow(numq1i4)-nrow(SWM_miss_4)))*100
  SWM[f,20] = ((SER_nbCorr_4)/(nrow(numq2i4)-nrow(SWM_miss_4)))*100
  SWM[f,21] = ((SWM_nbCorr_7)/(nrow(numq1i7)-nrow(SWM_miss_7)))*100
  SWM[f,22] = ((SER_nbCorr_7)/(nrow(numq2i7)-nrow(SER_miss_7)))*100
  
  SWM[f,23] = ((SWM_nbCorr_4 + SWM_nbCorr_7)/(nrow(numq1i4) + nrow(numq1i7) - 
                                                nrow(SWM_miss_4) - nrow(SWM_miss_7)))*100
  SWM[f,24] = ((SER_nbCorr_4 + SER_nbCorr_7)/(nrow(numq2i4) + nrow(numq2i7) - 
                                                nrow(SER_miss_4) - nrow(SER_miss_7)))*100
  
  ## report subjects with problems reading the logfile
    }, error = function(e) {
      message("a problem occured - please check subject folder: ", folders[f])
    } )
  }
  
  
  #create data frame
  dimnames(SWM) = list(1:nid,c(
    "participant_ID", 
    "SWM_RT_corr_4",    
    "SER_RT_corr_4",    
    "SWM_RT_corr_7",    
    "SER_RT_corr_7",    
    
    "SWMtotal_RT1_below_150",     
    
    "SWM_SD_RT_corr_4",     
    "SER_SD_RT_corr_4",     
    "SWM_SD_RT_corr_7",     
    "SER_SD_RT_corr_7",     
    "SWM_SD_RT_corr_total",  
    "SWM_RT_corr_total",    
    "SER_SD_RT_corr_total", 
    "SER_RT_corr_total",    
    
    "SWM_misses_4",           
    "SER_misses_4",           
    "SWM_misses_7",           
    "SER_misses_7",           
    
    "SWM_acc_4",              
    "SER_acc_4",              
    "SWM_acc_7",              
    "SER_acc_7",              
    "SWM_acc_all",            
    "SER_acc_all"
  ))
  
  #create table for export
  B3_SWM <- SWM
  B3_SWM <- data.frame(B3_SWM)
  B3_SWM[ is.na(B3_SWM) ] <- NA
  
  #adjust data type
  B3_SWM[ ,c(1:24)]      <- lapply(B3_SWM[ ,c(1:24)], as.character)
  B3_SWM[ ,c(2:24)]      <- lapply(B3_SWM[ ,c(2:24)], as.numeric)
  
  #check empty values
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  B3_SWM[is.nan(B3_SWM)] <- NA
  

#add 2 corrupt logfile subjects
  setwd("P:/037/B3_BEHAVIORAL_STUDY/04_Experiment/Analysis_Scripts/SWM_Results")
  datadir = ("P:/037/B3_BEHAVIORAL_STUDY/04_Experiment/Analysis_Scripts/SWM_Results")
  subj01053 <- read.csv("subj01053_B3_SWM.csv")
  subj01087 <-read.csv("subj01087_B3_SWM.csv")
  add_two <- rbind(subj01087,subj01053)
  add_two <- subset(add_two, select=-c(Group, Age, Sex, Balancing))

  add_two <- dplyr::rename(add_two, participant_ID = Participant_ID)
  B3_SWM <- rbind(B3_SWM,add_two)
  #remove two empty rows
  B3_SWM <- B3_SWM[-c(16,23), ]

  
  #### save pre-processed data #########################################
  #set path for saving
  savedir = ("your_directory_to_save_data")

  #save data as csv
  path_C <- paste0(savedir, "/B3_SWM",".csv", sep="")
  write.csv(B3_SWM, file=path_C, row.names=F)
  
  ################### End #######################################################################
  
