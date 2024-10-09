################### pre-processing of spatial working memory (SWM) task data from log-files ####################################################
#Task by Klingberg, T., Forssberg, H. & Westerberg, H. Increased Brain Activity in Frontal and Parietal Cortex Underlies the Development of Visuospatial Working Memory Capacity during Childhood. J. Cogn. Neurosci. 14, 1-10 (2002).
#Code by Sophia-Helen Sass(2024)

# in this script, the SWM log files, that were split in 4 parts due to technical problems will be pre-processed and saved, so they can be included in the summarized data set with all other participants
##### legend ####
# SWM     = spatial location memory (= 1st response)
# SER     = serial order memory (= 2nd response)
# _4      = 4 Items to memorize and recall in one trial 
# _7      = 7 Items to memorize and recall in one trial  

#clear workspace
rm(list = ls())

#load packages 
library(rjson)
library(plyr)

#set working and data directory for the respective participant folder which were 01087 and 01053
# setwd("P:/037/B3_BEHAVIORAL_STUDY/04_Experiment/LOG_Files/full_datasets/01087")
# datadir = ("P:/037/B3_BEHAVIORAL_STUDY/04_Experiment/LOG_Files/full_datasets/01087")

setwd("P:/037/B3_BEHAVIORAL_STUDY/04_Experiment/LOG_Files/full_datasets/01053")
datadir = ("P:/037/B3_BEHAVIORAL_STUDY/04_Experiment/LOG_Files/full_datasets/01053")


#enter respective participant number here    
#subj    = 01087
subj    = 01053

##### fetching data from folders ########################################
#structure: one folder per participant contains json-data files of every task in experiment. Name of folder: Record-ID, e.g. 01528
#Algorithm goes through all participant folders to fetch respective json-files, convert them to data frames and save a new data set containing mean data of all participants which is ready to be analyzed.

folders = dir(datadir) 
nid = 1
SWM = array(NA, c(nid, 24)) 

    # please change the file name to ..._task_inv-results, if you want to catch balancing 2
    #file 1
    json1 <- fromJSON(file = "swm_part1_inv-results.json", method = "C", unexpected.escape = "error", simplify = T)
    data_raw1 <- json1[1]
    #save list as character string (fromJSON function needs json string)
    data_as_chr1 <-as.character(data_raw1)
    # convert to list
    data_list1 <- fromJSON(json_str = data_as_chr1)
    
    #file 2
    json2 <- fromJSON(file = "swm_part2_inv-results.json", method = "C", unexpected.escape = "error", simplify = T)
    data_raw2 <- json2[1]
    data_as_chr2 <-as.character(data_raw2)
    data_list2 <- fromJSON(json_str = data_as_chr2)
    
    #file 3
    json3 <- fromJSON(file = "swm_part3_inv-results.json", method = "C", unexpected.escape = "error", simplify = T)
    data_raw3 <- json3[1]
    data_as_chr3 <-as.character(data_raw3)
    data_list3 <- fromJSON(json_str = data_as_chr3)
    
    #file 4
    json4 <- fromJSON(file = "swm_part4_inv-results.json", method = "C", unexpected.escape = "error", simplify = T)
    data_raw4 <- json4[1]
    data_as_chr4 <-as.character(data_raw4)
    data_list4 <- fromJSON(json_str = data_as_chr4)
    
      #merge to one list
      data_list <- append(data_list1, data_list2)
      data_list <- append(data_list, data_list3)
      data_list <- append(data_list, data_list4)

      # #save as one json file if you want to
      # savedir = datadir
      # path_json <- paste0(savedir, "/spatial_working_memory_task-results.json")
      # exportJson <- toJSON(data_list)
      # write(exportJson, file=path_json)
    
    # json file contains empty values for skipped 2nd questions. Replace them by NA in order
    # to transform the list to a data frame later
      for(i in 1:length(data_list)){
        if(data_list[[i]][["ended_on"]]=="skipped")
          data_list[[i]][["duration"]]=NA}
      
      # fix the space in question2 for algorithm to work
      data_list[[95]][["sender"]]="question2"
      
      # extract training trials
      Training <- data_list[c(31,32,43,44,62,63,80,81)]
      Training=ldply(Training,data.frame) # site note: 1 correct response is missing 
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
      
      #change data type for further processing
      spatial_mem$sender<- as.character(spatial_mem$sender) 
      spatial_mem$sender_type<- as.character(spatial_mem$sender_type)
      spatial_mem$sender_id<- as.character(spatial_mem$sender_id)
      spatial_mem$ended_on<- as.character(spatial_mem$ended_on)
      spatial_mem$timestamp<- as.character(spatial_mem$timestamp)
      
      #grouping in 4 step and 7 step conditions
      item4_tmp_num <- subset(spatial_mem[1:48, ]) 
      item4_tmp2_num <- subset(spatial_mem[145:192, ])
      item4_tmp_num<- rbind(item4_tmp_num,item4_tmp2_num)
      item7_tmp_num <- subset(spatial_mem[49:144, ])
      
      #grouping in spatial memory(q1) and serial memory(q2) conditions
      SWM_q1_num <- subset(spatial_mem, sender == "question"|sender == "question1") 
      SWM_q2_num <- subset(spatial_mem, sender == "question2") 
      
      #exclude RTs <150 ms (participant probably clicked randomly)
      #there are some negative RTs in the data and I don't know how this is possible
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
      f=1
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
      
      #### save pre-processed data #########################################
      #set path for saving
      savedir = ("your_directory")
      
      #save data as csv for the respective participant
      # path_C <- paste0(savedir, "/subj01087_B3_SWM.csv", sep="")
      #  write.csv(B3_SWM, file=path_C, row.names=F)

      path_C <- paste0(savedir, "/subj01053_B3_SWM.csv", sep="")
      write.csv(B3_SWM, file=path_C, row.names=F)

       
       ################### End #######################################################################