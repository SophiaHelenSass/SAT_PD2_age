### read in all data (preprocessed behavioral and inference, questionnaires and neuro-psychological task battery data) ################################
#Code by Sophia-Helen Sass

###### preparation ############
#clear workspace
rm(list = ls())

#load packages
library(plyr)
library(tidyr)
library(dplyr)
library(reshape)

#load and format task data#####
#####SAT and related data####
#import inference data from winning model (discounted low-probability pruning)
#discounted low-probability pruning
setwd("your_directory")
SAT_params_disc_lowprob_prun=read.csv('SAT_params_disc_lowprob_prun.csv')
PD_disc_lowprob_prun=read.csv('PD_disc_lowprob_prun.csv')

#import SAT behavioral data
setwd("your_directory")
SAT_behavioral_data=read.csv('SAT_sum_behavioral_data.csv')

#import SAT debriefing
SAT_DB=read.csv('SAT_DB_all.csv')

####covariates####
######import IDP data#####
setwd("your_directory")
IDP=read.csv('B3_IDP.csv')

######import SAW data#####
setwd("your_directory")
SAW=read.csv('B3_SAW.csv')

######import SWM data#####
setwd("your_directory")
SWM=read.csv('B3_SWM.csv')


######import Raven data#####
setwd("your_directory")
Raven=read.csv('B3_Raven_detailed.csv')

####questionnaire data#####
setwd("your_directory")

######import NFC data####
NFC=read.csv('NFC_preprocessed.csv')

######import socio-demography data#####
socio=read.csv('socio_demo_preprocessed.csv')

#attention, the gender entry was falsely made by participant in questionnaire (they told us), needs to be corrected here!
#change gender of participant 01144
socio[socio$participant_ID==1144, "sex"] <- 1


#merge all data by Participants_ID to get one big data frame ######
SAT_all_vars <- merge(SAT_behavioral_data,SAT_DB,                  by = 'participant_ID')
SAT_all_vars <- merge(SAT_all_vars,PD_disc_lowprob_prun,           by = 'participant_ID')
SAT_all_vars <- merge(SAT_all_vars,SAT_params_disc_lowprob_prun,   by = 'participant_ID')
SAT_all_vars <- merge(SAT_all_vars,IDP,                            by = 'participant_ID')
SAT_all_vars <- merge(SAT_all_vars,SAW,                            by = 'participant_ID')
SAT_all_vars <- merge(SAT_all_vars,SWM,                            by = 'participant_ID') 
SAT_all_vars <- merge(SAT_all_vars,Raven,                          by = 'participant_ID')
SAT_all_vars <- merge(SAT_all_vars,NFC,                            by = 'participant_ID')
SAT_all_vars <- merge(SAT_all_vars,socio,                          by = 'participant_ID')


#outlier detection
outliers_table <- SAT_all_vars %>%
  identify_outliers(mean_RT1_overall)

#exclude extreme outliers
outliers_RT1 <- SAT_all_vars %>%
  filter((participant_ID %in% outliers_table$participant_ID[outliers_table$is.extreme]))

##### recalculating variables #####
#compute mutual score for experiences with computer/electronic devices
SAT_all_vars <- SAT_all_vars %>% mutate(electot=electronic_use+gaming)
SAT_all_vars <- SAT_all_vars %>% mutate(SEStot=income+education)

#compute actual NFC total score (positive/negative item values)
SAT_all_vars <- SAT_all_vars %>% 
  mutate(nfc_total = nfc_1 + nfc_2 + nfc_3 + (nfc_4 * -1) + nfc_5 + 
           (nfc_6 * -1) + (nfc_7 * -1) + (nfc_8 * -1) + (nfc_9 * -1) + 
           (nfc_10 * -1) + (nfc_11 * -1) + (nfc_12 * -1) + nfc_13 + 
           nfc_14 + (nfc_15 * -1) + (nfc_16 * -1))

#IDP adjustment for number of trials total
SAT_all_vars$IDP_ACC_rel <- SAT_all_vars$IDP_RESP_CORR/46 *100

##### data exclusion #####
#excluded
#SAT_all_vars_excl <- SAT_all_vars[SAT_all_vars$overall_gain <= 650, ]  

#SAT cleaned data set: gain lower than 650 fuel points 
SAT_excl <- SAT_all_vars[SAT_all_vars$overall_gain <= 650,  ]
SAT_all_vars <- SAT_all_vars[SAT_all_vars$overall_gain > 650,  ]

#SWM cleaned dataset: less than 15 correct responses to analyze (per sub task)
#4 items
SAT_all_vars <- subset(SAT_all_vars, SWM_misses_4<10)

#7 items
SAT_all_vars <- subset(SAT_all_vars, SWM_misses_7<10)

#IDP cleaned dataset: less than 15 correct responses to analyze 
excl <- subset(SAT_all_vars, IDP_RESP_TOT<10)
SAT_all_vars <- subset(SAT_all_vars, IDP_RESP_TOT>=10)

#SAW cleaned dataset: less than 15 correct responses to analyze 
SAT_all_vars <- subset(SAT_all_vars, SAW_RESP_TOT>=10)

#compute relative SAT performance####
# creating measure for relative Gain to test for task flaws regarding noise condition differences 
# 100 % performance: mean gain (gain as sum of all positive and negative rewards throughout the 
# task without training) of 1000 agents with PD3 and beta = 3, theta = 0 and alpha = 0.1
# relative gain measure: rel_gain_low = Participant's gain in low noise / ideal performance * 100

#set working and data directory
setwd("your_directory")
#load noise condition factor
cond<- read.csv("conditions.csv", header = TRUE)

#####optimal (rational) agent data with beta = 3, alpha = 0.1, theta = 0####
#set working and data directory
setwd("your_directory")
rational_agent<- read.csv("miniblock_gain_mean_std_rational_1000.csv", header = TRUE)
rational_agent <- rational_agent[ ,-c(4:7)]
names(rational_agent)[names(rational_agent) == 'Mean_gain_PD3'] <- 'Gain'
rational_agent$noise_condition<-cond

#gain of points sum 
rational_agentgainhigh<- sum(rational_agent$Gain[rational_agent$noise_condition == "high"])
rational_agentgainlow<- sum(rational_agent$Gain[rational_agent$noise_condition == "low"])
rational_agentgainsum <- sum(rational_agent$Gain)

#####random agent data with beta = 0, alpha = 0.1, theta = 0####
random_agent<- read.csv("miniblock_gain_mean_std_random_1000.csv", header = TRUE)
random_agent <- random_agent[ ,-c(4:7)]
names(random_agent)[names(random_agent) == 'Mean_gain_PD3'] <- 'Gain'
random_agent$noise_condition<-cond

#gain of points sum
random_agentgainhigh<- sum(random_agent$Gain[random_agent$noise_condition == "high"])
random_agentgainlow<- sum(random_agent$Gain[random_agent$noise_condition == "low"])
random_agentgainsum <- sum(random_agent$Gain)


#compute new gain scale
SAT_all_vars$rel_gain_high <- (SAT_all_vars$gain_high_noise - random_agentgainhigh)/
  (rational_agentgainhigh-random_agentgainhigh)*100
SAT_all_vars$rel_gain_low <- (SAT_all_vars$gain_low_noise - random_agentgainlow)/
  (rational_agentgainlow-random_agentgainlow)*100
SAT_all_vars$rel_gain_overall <- (SAT_all_vars$overall_gain - random_agentgainsum)/
  (rational_agentgainsum-random_agentgainsum)*100


#saving the data frame####
savedir = ("your_directory")
path_C <- paste0(savedir, "/SAT_neuro_quest_all_data",".csv", sep="")
write.csv(SAT_all_vars, file=path_C, row.names=F)

