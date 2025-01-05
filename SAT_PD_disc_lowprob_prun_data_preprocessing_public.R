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

#set your directory
setwd("/your directory")
condition <- read.csv('conditions.csv')

#the read-in algorithm does exactly the same for young adults (YA) and older adults (OA) subsequently
#### young adults ####
#load inferred planning depth data
PD_raw_YA <- read.csv('meanPD_1st_action_ya_with_IDs.csv')
PD_raw_YA$X <- seq(1:141)
#subset for training trials (1:20) and for task (21:140)
PD_YAtraining <- subset(PD_raw_YA, PD_raw_YA$X < 21)
PD_raw_YA <- subset(PD_raw_YA, PD_raw_YA$X > 20) 

#extract participant IDs
ids_ya <- PD_raw_YA[nrow(PD_raw_YA), ]
ids_ya <- t(ids_ya) 
ids_ya <- as.data.frame(ids_ya)
colnames(ids_ya) <- "participant_IDs"
PD_raw_YA<-PD_raw_YA[-c(nrow(PD_raw_YA)), ]

PD_YA <- melt(PD_raw_YA, id.vars= "X")
PD_YA <- PD_YA$value

#number of participants 
nYA = ncol(PD_raw_YA)-1

#number of mini-blocks
t = 120

#write index number of participants for processing in loops
for (i in (1:nYA)){
  subject<-rep(c(1:i),each=t)
}

#combine all in one table
PD_YA <- cbind(subject,PD_YA, condition)

####compute participant means summary for young adults ####
mean_PD_YA = array(NA, c(nYA, 3)) 
for(j in 1:nYA){
  mean_PD_YA[j,1] = mean(PD_YA$PD_YA[PD_YA$subject ==j])
  mean_PD_YA[j,2] = mean(PD_YA$PD_YA[PD_YA$subject ==j & PD_YA$cond =='high'])
  mean_PD_YA[j,3] = mean(PD_YA$PD_YA[PD_YA$subject ==j & PD_YA$cond =='low'])
}

mean_PD_YA <- data.frame(mean_PD_YA)
ids_ya <- data.frame(ids_ya)
ids<-ids_ya[-c(nrow(ids_ya)), ] 

mean_PD_YA<- cbind(mean_PD_YA,ids)


#### older adults ####
#load inferred planning depth data
PD_raw_OA <- read.csv('meanPD_1st_action_oa_with_IDs.csv')
PD_raw_OA$X <- seq(1:141)
#subset for training trials (1:20) and for task (21:140)
PD_OAtraining <- subset(PD_raw_OA, PD_raw_OA$X < 21)
PD_raw_OA <- subset(PD_raw_OA, PD_raw_OA$X > 20) 

#extract participant IDs
ids_OA <- PD_raw_OA[nrow(PD_raw_OA), ]
ids_OA <- t(ids_OA) 
ids_OA <- as.data.frame(ids_OA)
colnames(ids_OA) <- "participant_IDs"
PD_raw_OA<-PD_raw_OA[-c(nrow(PD_raw_OA)), ]


PD_OA <- melt(PD_raw_OA, id.vars= "X")
PD_OA <- PD_OA$value

#number of participants 
nOA = ncol(PD_raw_OA)-1

#number of mini-blocks
t = 120

#write index number of participants for processing in loops
for (i in (1:nOA)){
  subject<-rep(c(1:i),each=t)
}

#combine all in one table
PD_OA <- cbind(subject,PD_OA, condition)

####compute participant means summary for young adults ####
mean_PD_OA = array(NA, c(nOA, 3)) 
for(j in 1:nOA){
  mean_PD_OA[j,1] = mean(PD_OA$PD_OA[PD_OA$subject ==j])
  mean_PD_OA[j,2] = mean(PD_OA$PD_OA[PD_OA$subject ==j & PD_OA$cond =='high'])
  mean_PD_OA[j,3] = mean(PD_OA$PD_OA[PD_OA$subject ==j & PD_OA$cond =='low'])
}

mean_PD_OA <- data.frame(mean_PD_OA)
ids<-ids_OA[-c(nrow(ids_OA)), ] 

mean_PD_OA<- cbind(mean_PD_OA,ids)

#put the data of OA and YA in one joint set
mean_PD <- rbind(mean_PD_OA,mean_PD_YA)
mean_PD <- data.frame(mean_PD)

#name columns
dimnames(mean_PD) = list(1:sum(nOA,nYA),c(
  "disc_lowprob_PD_overall",
  "disc_lowprob_PD_high_noise",
  "disc_lowprob_PD_low_noise",
  "participant_ID"
))

mean_PD$subj<-NULL


####save pre-processed planning depth data####
savedir = ("your_directory")
path_C <- paste0(savedir, "/PD_disc_lowprob_prun", ".csv", sep="")
write.csv(mean_PD, file=path_C, row.names=F)


