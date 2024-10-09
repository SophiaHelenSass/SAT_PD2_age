### Space Adventure Task - Pre-processing inferred planning depth (PD) from discounted low-probability pruning model (winning in model comparison)
#Task adapted from 	Steffen, J. et al. Shorter planning depth and higher response noise during sequential decision-making in old age. Sci. Rep. 13, 7692 (2023). 
#Code by Sophia-Helen Sass (2024)

###### preparation ############
#clear workspace
rm(list = ls())

#put required functions in library
library(plyr)
library(tidyr)
library(dplyr)
library(reshape)

#set data path for participant IDs and noise condition factor
setwd("your_directory")

condition <- read.csv('conditions.csv')
participant_ID <- read.csv('participant_ID.csv')

#please set data path of inference results from discounted low-probability pruning model here
setwd("your_directory")

####the read-in algorithm does the same for young and older adults group subsequently
#### young adults (YA) ####
PD_raw_YA <- read.csv('meanPD_1st_action_ya_single_lowprob_pruning_discount_hyperbolic_theta_kmax30.csv')

# subset for training trials (1:20) and for task (21:140)
PD_YAtraining <- subset(PD_raw_YA, PD_raw_YA$X < 20)
PD_raw_YA <- subset(PD_raw_YA, PD_raw_YA$X > 19) 
PD_YA <- melt(PD_raw_YA, id.vars= "X")
PD_YA <- PD_YA$value

#index of participant 
nYA = ncol(PD_raw_YA)-1

#index of trials
t = 120

#write index number of participant for processing in loops
for (i in (1:nYA)){
  subject<-rep(c(1:i),each=t)
}

#add noise condition factor
PD_YA <- cbind(subject,PD_YA, condition)

#compute participant means summary
mean_PD_YA = array(NA, c(nYA, 3)) 
for(j in 1:nYA){
  mean_PD_YA[j,1] = mean(PD_YA$PD_YA[PD_YA$subject ==j])
  mean_PD_YA[j,2] = mean(PD_YA$PD_YA[PD_YA$subject ==j & PD_YA$cond =='high'])
  mean_PD_YA[j,3] = mean(PD_YA$PD_YA[PD_YA$subject ==j & PD_YA$cond =='low'])
}

#### older adults (OA) ####
#please set data path of inference results from discounted low-probability pruning model here
setwd("your_directory")
PD_raw_OA <- read.csv('meanPD_1st_action_oa_single_lowprob_pruning_discount_hyperbolic_theta_kmax30.csv')

# subset for training trials (1:20) and for task (21:140)
PD_OAtraining <- subset(PD_raw_OA, PD_raw_OA$X < 20)
PD_raw_OA <- subset(PD_raw_OA, PD_raw_OA$X > 19) 

PD_OA <- melt(PD_raw_OA, id.vars= "X")
PD_OA <- PD_OA$value

#index of participants 
nOA = ncol(PD_raw_OA)-1

#index of trials
t = 120

#write index number of participants for processing in loops
for (i in (1:nOA)){
  subject<-rep(c(1:i),each=t)
}

#add noise condition factor
PD_OA <- cbind(subject,PD_OA, condition)

#compute participant means summary
mean_PD_OA = array(NA, c(nOA, 3)) 
for(j in 1:nOA){
  mean_PD_OA[j,1] = mean(PD_OA$PD_OA[PD_OA$subject ==j])
  mean_PD_OA[j,2] = mean(PD_OA$PD_OA[PD_OA$subject ==j & PD_OA$cond =='high'])
  mean_PD_OA[j,3] = mean(PD_OA$PD_OA[PD_OA$subject ==j & PD_OA$cond =='low'])
}

#put the data of OA and YA in one joint data frame
mean_PD <- rbind(mean_PD_OA,mean_PD_YA)
mean_PD <- cbind(participant_ID,mean_PD)

#name columns
dimnames(mean_PD) = list(1:sum(nOA,nYA),c(
  "participant_ID",
  "disc_lowprob_PD_overall",
  "disc_lowprob_PD_high_noise",
  "disc_lowprob_PD_low_noise"
))


####save pre-processed planning depth data####
savedir = ("your_directory")
path_C <- paste0(savedir, "/PD_disc_lowprob_prun", ".csv", sep="")
write.csv(mean_PD, file=path_C, row.names=F)


