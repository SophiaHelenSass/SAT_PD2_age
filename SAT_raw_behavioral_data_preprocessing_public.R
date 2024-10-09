################### pre-processing of raw behavioral data from the Space Adventure Task PD 2 json log-files ####################################################
#Task adapted from 	Steffen, J. et al. Shorter planning depth and higher response noise during sequential decision-making in old age. Sci. Rep. 13, 7692 (2023). 
#Code by Sophia-Helen Sass(2024)

###### preparation ############################################
#clear workspace
rm(list = ls())

#load packages
   library(plyr)
   library(tidyr)
   library(dplyr)
   library(reshape)
   
#set working directory
setwd("your_directory")

#the read-in algorithm does exactly the same for groups of young adults (YA) and older adults (OA) subsequently
### young adults ###############################################
#data read-in 
dataYA_full = read.csv('SAT_data_YA.csv')

#number of participants in young group 
nYA = 60
#number of trials
t = 140

#write index number of participant for processing in loops
for (i in (1:nYA)){
  subject<-rep(c(1:i),each=t)
}

#add subject number to long data set
dataYA_full <- cbind(subject,dataYA_full)

#calculate response time 2 (sum of response time for action 2 and 3)
for (i in 1:nYA){
  for (j in 1:t){
    dataYA_full$log_rt_2[dataYA_full$subject == i] = 
      dataYA_full$log_rt_sum[dataYA_full$subject == i & 
                               dataYA_full$log_rt_sum[j]] - 
      dataYA_full$log_rt_1[dataYA_full$subject == i & 
                             dataYA_full$log_rt_1[j]]
  }}

#subset for training trials (1:20) and for task (21:140)
dataYAtraining <- subset(dataYA_full, dataYA_full$block_number < 21)
dataYA <- subset(dataYA_full, dataYA_full$block_number > 20) 

#compute participant means summary for young adults
satYA = array(NA, c(nYA, 17)) 
for(j in 1:nYA){
  satYA[j,1]  = "young adults"
  satYA[j,2]  = mean(dataYA$Participant_ID[dataYA$subject ==j])
  satYA[j,3]  = sum(dataYA$gain[dataYA$subject ==j])
  satYA[j,4]  = length(dataYA$subject[dataYA$subject == j])
  satYA[j,5]  = sum(dataYA$gain[dataYA$subject ==j & dataYA$noise_level =='high'])
  satYA[j,6]  = sum(dataYA$gain[dataYA$subject ==j & dataYA$noise_level =='low'])
  satYA[j,7]  = mean(dataYA$log_rt_1[dataYA$subject ==j & dataYA$noise_level =='high'])
  satYA[j,8]  = mean(dataYA$log_rt_1[dataYA$subject ==j & dataYA$noise_level =='low'])
  satYA[j,9] = mean(dataYA$balance_condition [dataYA$subject == j])
  satYA[j,10] = mean(dataYA$log_rt_1[dataYA$subject ==j])
  satYA[j,11] = mean(dataYA$log_rt_2[dataYA$subject ==j])
  satYA[j,12] = mean(dataYA$log_rt_2[dataYA$subject ==j & dataYA$noise_level =='high'])
  satYA[j,13] = mean(dataYA$log_rt_2[dataYA$subject ==j & dataYA$noise_level =='low'])
  satYA[j,14] = mean(dataYA$log_rt_1[dataYA$subject ==j & dataYA$noise_level =='low' & dataYA$action1=="1"])
  satYA[j,15] = mean(dataYA$log_rt_1[dataYA$subject ==j & dataYA$noise_level =='low' & dataYA$action1=="2"])
  satYA[j,16] = mean(dataYA$log_rt_1[dataYA$subject ==j & dataYA$noise_level =='high' & dataYA$action1=="1"])
  satYA[j,17] = mean(dataYA$log_rt_1[dataYA$subject ==j & dataYA$noise_level =='high' & dataYA$action1=="2"])
}

### older adults #####################################
#data read-in older adults
dataOA_full = read.csv('SAT_data_OA.csv')

#number of participants in older group 
nOA = 57

#write index number of participant for processing in loops
for (i in (1:nOA)){
  subject<-rep(c(1:i),each=t)
}

#add subject number to long data set
dataOA_full <- cbind(subject,dataOA_full)

#calculate response time 2 (sum of response time for action 2 and 3)
for (i in 1:nOA){
  for (j in 1:t){
    dataOA_full$log_rt_2[dataOA_full$subject == i] = 
      dataOA_full$log_rt_sum[dataOA_full$subject == i & 
                               dataOA_full$log_rt_sum[j]] - 
      dataOA_full$log_rt_1[dataOA_full$subject == i & 
                             dataOA_full$log_rt_1[j]]
  }}

#subset for training trials (1:20) and for task (21:140)
dataOAtraining <- subset(dataOA_full, dataOA_full$block_number < 21)
dataOA <- subset(dataOA_full, dataOA_full$block_number > 20) 

#compute participant means for older adults
satOA = array(NA, c(nOA, 17)) 
for(i in 1:nOA){
  satOA[i,1]  = "older adults"
  satOA[i,2]  = mean(dataOA$Participant_ID[dataOA$subject==i])
  satOA[i,3]  = sum(dataOA$gain[dataOA$subject ==i])
  satOA[i,4]  = length(dataOA$subject[dataOA$subject == i])
  satOA[i,5]  = sum(dataOA$gain[dataOA$subject ==i & dataOA$noise_level =='high'])
  satOA[i,6]  = sum(dataOA$gain[dataOA$subject ==i & dataOA$noise_level =='low'])
  satOA[i,7]  = mean(dataOA$log_rt_1[dataOA$subject ==i & dataOA$noise_level =='high'])
  satOA[i,8]  = mean(dataOA$log_rt_1[dataOA$subject ==i & dataOA$noise_level =='low'])
  satOA[i,9] = mean(dataOA$balance_condition [dataOA$subject == i])
  satOA[i,10] = mean(dataOA$log_rt_1[dataOA$subject ==i])
  satOA[i,11] = mean(dataOA$log_rt_2[dataOA$subject ==i])
  satOA[i,12] = mean(dataOA$log_rt_2[dataOA$subject ==i & dataOA$noise_level =='high'])
  satOA[i,13] = mean(dataOA$log_rt_2[dataOA$subject ==i & dataOA$noise_level =='low'])
  satOA[i,14] = mean(dataOA$log_rt_1[dataOA$subject ==i & dataOA$noise_level =='low' & dataOA$action1=="1"])
  satOA[i,15] = mean(dataOA$log_rt_1[dataOA$subject ==i & dataOA$noise_level =='low' & dataOA$action1=="2"])
  satOA[i,16] = mean(dataOA$log_rt_1[dataOA$subject ==i & dataOA$noise_level =='high' & dataOA$action1=="1"])
  satOA[i,17] = mean(dataOA$log_rt_1[dataOA$subject ==i & dataOA$noise_level =='high' & dataOA$action1=="2"])
  }

#combine the data of young and older group in joint data frame
B3_SAT <- rbind(satOA,satYA)

#name columns
dimnames(B3_SAT) = list(1:sum(nOA,nYA),c(
  "group",
  "participant_ID",
  "overall_gain",
  "num_trials",
  "gain_high_noise",
  "gain_low_noise",
  "mean_RT1_high_noise",
  "mean_RT1_low_noise",
  "balancing",
  "mean_RT1_overall",
  "mean_RT2_overall",
  "mean_RT2_high_noise",
  "mean_RT2_low_noise",
  "mean_RT1_low_noise_jump",
  "mean_RT1_low_noise_move",
  "mean_RT1_high_noise_jump",
  "mean_RT1_high_noise_move"
))


#create table for export
B3_SAT <- data.frame(B3_SAT)
B3_SAT[is.na(B3_SAT)] <- NA

#adjust data type 
B3_SAT[ ,c(1:17)]           <- lapply(B3_SAT[ ,c(1:17)], as.character)
B3_SAT[ ,c(2:8,10:17)]       <- lapply(B3_SAT[ ,c(2:8,10:17)], as.numeric)
B3_SAT[ ,c(1)]              <- as.factor(B3_SAT[ , c(1)]) 
B3_SAT[ ,c(9)]              <- as.factor(B3_SAT[ , c(9)]) 
levels(B3_SAT$balancing)[0] <- "1"
levels(B3_SAT$balancing)[1] <- "2"

#### prepare long data sets ##########################################
#including training trials (1:20)
#add age group column
dataYA_full$group <- "young adults"
dataOA_full$group <- "older adults"

#combine long data sets (incl. training trials) for young and older adults in a one joint data frame
SAT_details_training <- rbind(dataOA_full, dataYA_full)

#combine long data sets (without training trials) for young and older adults in a one joint data frame
SAT_details <- rbind(dataOA, dataYA)


#### save pre-processed data #########################################
#set path for saving
savedir = ("your_directory")

#save data with current date
#summary without training
path_C <- paste0(savedir, "/SAT_sum_behavioral_data", ".csv", sep="")
write.csv(B3_SAT, file=path_C, row.names=F)

#long data set without training
path_C <- paste0(savedir, "/SAT_trial_data",".csv", sep="")
write.csv(SAT_details, file=path_C, row.names=F)

#long data set with training
path_C <- paste0(savedir, "/SAT_trial_data_incl_training",".csv", sep="")
write.csv(SAT_details_training, file=path_C, row.names=F)

################### End #######################################################################
