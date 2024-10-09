################### pre-processing of action choice data from the Space Adventure Task PD 2 json log-files ####################################################
#Task adapted from 	Steffen, J. et al. Shorter planning depth and higher response noise during sequential decision-making in old age. Sci. Rep. 13, 7692 (2023). 
#Code by Sophia-Helen Sass(2024)

###### preparation ############################################
#clear workspace
rm(list = ls())

#load packages
library(dplyr)
library(plyr)

#set working directory
setwd("your_directory")

#data read-in  (YA = young adults, OA = older adults)
act_YA <- read.csv("transition_data_YA.csv", header = TRUE)
act_OA <- read.csv("transition_data_OA.csv", header = TRUE)

#connect data frames of young and older adults
SAT_actions <- rbind(act_OA,act_YA)

#remove incomplete cases
SAT_actions <- na.omit(SAT_actions)

#compute main measures#############################
#high prob jump: proportion of high probability transitions in all jumps (overall, low noise, high noise condition)
SAT_actions$jump_high_prob <- SAT_actions$jump_count_success/SAT_actions$jump_count_total
SAT_actions$jump_high_prob_LN <- SAT_actions$jump_count_loNoise_success/SAT_actions$jump_count_loNoise_total
SAT_actions$jump_high_prob_HN <- SAT_actions$jump_count_hiNoise_success/SAT_actions$jump_count_hiNoise_total

#proportions: proportion (prop) of moves (overall) and jumps in all actions (overall, low noise (LN), high noise (HN) condition)
#there are 60 mini-blocks in high noise and 60 in low noise condition and 3 actions per mini-block
SAT_actions$prop_m <- SAT_actions$move_count/360
SAT_actions$prop_j <- SAT_actions$jump_count_total/360
SAT_actions$prop_j_LN <- SAT_actions$jump_count_loNoise_total/180
SAT_actions$prop_j_HN <- SAT_actions$jump_count_hiNoise_total/180

#rename ID column for future data matching 
names(SAT_actions)[names(SAT_actions) == 'subject'] <- 'participant_ID'
#remove last "\" from ID 
SAT_actions$participant_ID= substr(SAT_actions$participant_ID,1,nchar(SAT_actions$participant_ID)-1)
#remove unnecessary columns
SAT_actions <- SAT_actions[ ,-c(1:10)]

#### save pre-processed data #########################################
#set path for saving
savedir = ("your_directory")

#save data as csv
path_C <- paste0(savedir, "/SAT_actions",".csv", sep="")
write.csv(SAT_actions, file=path_C, row.names=F)

################### End #######################################################################