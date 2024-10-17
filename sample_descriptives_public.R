#by Sophia-Helen Sass
### Descriptive Statistics and Group Comparison of neuro-psychological task outcomes and NFC questionnaire data

#clear workspace
rm(list = ls())

#load packages
library(car)
library(ggplot2)
library(psych)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)
library(reshape)
library(plyr)
library(ggsignif)
library(cowplot)
library(naniar)
library(Hmisc)

#set working and data directory
setwd("your directory")

#load most recent data version here
B3_SAT <- read.csv("SAT_neuro_quest_all_data.csv", header = TRUE)

subYA <- subset(B3_SAT, group == "young adults")
subOA <- subset(B3_SAT, group == "older adults")

############# sample descriptives ##################
#descr age
descr_age <- describeBy(B3_SAT$age_phone, B3_SAT$group)
descr_age <- ldply (descr_age, data.frame)
descr_age

#ftable sex
B3_SAT %>%
  group_by(group, sex) %>%
  dplyr::summarize(Freq=n())
sex.diff <- chisq.test(B3_SAT$group, B3_SAT$sex, correct=FALSE)

#ftable education
B3_SAT %>%
  group_by(group, education) %>%
  dplyr::summarize(Freq=n())

#transform education from ordinal to nominal scale (higher educational degree or no higher degree)
B3_SAT %>% add_column(educ_high = NA)
for(k in 1:107){
  if (B3_SAT$education[k] == 8 | B3_SAT$education[k] == 7){
    B3_SAT$educ_high[k] = 1}
  else {
    B3_SAT$educ_high[k] = 0}}

#ftable education
B3_SAT %>%
  group_by(group, educ_high) %>%
  dplyr::summarize(Freq=n())

educ.diff <- chisq.test(B3_SAT$group, B3_SAT$educ_high, correct=FALSE)


#ftable electronic use 
#transformed to nominal scale of regular use yes/no)
B3_elec <- subset(B3_SAT[B3_SAT$electronic_use=="5" |B3_SAT$electronic_use=="4",])
B3_elec %>%
  group_by(group, electronic_use_info) %>%
  dplyr::summarize(Freq=n())
elec.diff <- chisq.test(B3_SAT$group, B3_SAT$electronic_use_info, correct=FALSE)


B3_SAT %>% add_column(elec_high = NA)
for(k in 1:107){
  if (B3_SAT$electronic_use[k] == "5" | B3_SAT$electronic_use[k] == "4"){
    B3_SAT$elec_high[k] = 1}
  else {
    B3_SAT$elec_high[k] = 0}}


B3_SAT %>%
  group_by(group, elec_high) %>%
  dplyr::summarize(Freq=n())


B3_SAT$elec_high<- as.numeric(B3_SAT$elect_high)
B3_SAT$group<- as.factor(B3_SAT$group)

elec.diff <- chisq.test(B3_SAT$group, B3_SAT$elec_high, correct=FALSE)

#ftable gaming
#transformed to nominal scale of regular use yes/no)
B3_game <- subset(B3_SAT[B3_SAT$gaming=="4"|B3_SAT$gaming=="5",])
B3_game %>%
  group_by(group, gaming) %>%
  dplyr::summarize(Freq=n())


B3_SAT %>% add_column(game_high = NA)
for(k in 1:107){
  if (B3_SAT$gaming[k] == "5" | B3_SAT$gaming[k] == "4"){
    B3_SAT$game_high[k] = 1}
  else {
    B3_SAT$game_high[k] = 0}}

B3_SAT %>%
  group_by(group, game_high) %>%
  dplyr::summarize(Freq=n())


gaming.diff <- chisq.test(B3_SAT$group, B3_SAT$game_high, correct=FALSE)

############# descriptive statistics ###############
####SAW performance####
SAW_ACC <- describeBy(B3_SAT$SAW_ACC, B3_SAT$group)

#SAW_RESP normality 
shapiro.test(B3_SAT$SAW_ACC[B3_SAT$group=='young adults']) # given
shapiro.test(B3_SAT$SAW_ACC[B3_SAT$group=='older adults']) # given

#SAW_RESP homogeneity 
leveneTest(B3_SAT$SAW_ACC, B3_SAT$group)

#group comparison
t.test(B3_SAT$SAW_ACC~B3_SAT$group, var.equal = TRUE, alternative = "two.sided")


##SAW RT####
SAW.descr.RT <- describeBy(B3_SAT$SAW_RT_CORR/1000, B3_SAT$group)

#SAW_RT normality 
shapiro.test(B3_SAT$SAW_RT_CORR[B3_SAT$group == 'young adults']) 
shapiro.test(B3_SAT$SAW_RT_CORR[B3_SAT$group == 'older adults']) 

##SAW_RT homogeniety 
leveneTest(B3_SAT$SAW_RT_CORR, B3_SAT$group)

#group comparison
t.test(B3_SAT$SAW_RT_CORR~B3_SAT$group, var.equal = TRUE, alternative = "two.sided")


####SWM performance####
SWMall.descr.acc <- describeBy(B3_SAT$SWM_acc_all, B3_SAT$group)

#SWM_ACC normality 
shapiro.test(B3_SAT$SWM_acc_all[B3_SAT$group == 'young adults']) 
shapiro.test(B3_SAT$SWM_acc_all[B3_SAT$group == 'older adults']) 

##SWM_ACC homogeneity 
leveneTest(B3_SAT$SWM_acc_all, B3_SAT$group)

#group comparison
t.test(B3_SAT$SWM_acc_all~B3_SAT$group, var.equal = TRUE, alternative = "two.sided")

##SWM RT####
SWM.descr.RT <- describeBy(B3_SAT$SWM_RT_corr_total/1000, B3_SAT$group)

#SWM_RT normality 
shapiro.test(B3_SAT$SWM_RT_corr_total[B3_SAT$group == 'young adults']) 
shapiro.test(B3_SAT$SWM_RT_corr_total[B3_SAT$group == 'older adults']) 

##SWM_RT homogeniety 
leveneTest(B3_SAT$SWM_RT_corr_total, B3_SAT$group)

#group comparison
wilcox.test(SWM_RT_corr_total~group, data = B3_SAT, var.equal = FALSE,exact = FALSE, correct = FALSE, conf.int = FALSE)


####IDP performance####
B3_SAT$IDP_ACC<- B3_SAT$IDP_RESP_CORR/(B3_SAT$IDP_RESP_TOT+B3_SAT$IDP_trials_beyond_80s) * 100
B3_SAT$IDP_ACC<- as.numeric(B3_SAT$IDP_ACC)

IDP.descr.acc <- describeBy(B3_SAT$IDP_ACC, B3_SAT$group)

#IDP_ACC normality 
shapiro.test(B3_SAT$IDP_ACC[B3_SAT$group == 'young adults'])
shapiro.test(B3_SAT$IDP_ACC[B3_SAT$group == 'older adults'])

##IDP_ACC homogeniety 
leveneTest(B3_SAT$IDP_ACC, B3_SAT$group)

#group comparison
wilcox.test(IDP_ACC~group, data = B3_SAT, var.equal = FALSE,exact = FALSE, correct = FALSE, conf.int = FALSE)


###IDP RT####
IDP.descr.RT <- describeBy(B3_SAT$IDP_RT_CORR/1000, B3_SAT$group)

#IDP_RT normality 
shapiro.test(B3_SAT$IDP_RT_CORR[B3_SAT$group == 'young adults']) # not given
shapiro.test(B3_SAT$IDP_RT_CORR[B3_SAT$group == 'older adults']) # not given

##IDP_RT homogeniety 
leveneTest(B3_SAT$IDP_RT_CORR, B3_SAT$group)# given

#group comparison
wilcox.test(IDP_RT_CORR~group, data = B3_SAT, var.equal = FALSE,exact = FALSE, correct = FALSE, conf.int = FALSE)

####Raven####
Raven_ACC <- describeBy(B3_SAT$Raven_ACC, B3_SAT$group)
Raven_ACC

Raven_lowperformance<-count(subOA$Raven_RESP_CORR <=1)
Raven_quick_lowperfomance<-subset(subOA, Raven_RESP_CORR<=1 & Raven_time4task_min <5)

#Raven_RESP normality 
shapiro.test(B3_SAT$Raven_ACC[B3_SAT$group=='young adults']) 
shapiro.test(B3_SAT$Raven_ACC[B3_SAT$group=='older adults']) 

##Raven_RESP homogeniety 
leveneTest(B3_SAT$Raven_ACC, B3_SAT$group)# given

#group comparison
t.test(B3_SAT$Raven_RESP_CORR~B3_SAT$group, var.equal = TRUE, alternative = "two.sided")


##NFC
NFC.descr<- describeBy(B3_SAT$nfc_total, B3_SAT$group)

#NFC normality 
shapiro.test(B3_SAT$nfc_total[B3_SAT$group == 'young adults']) # given
shapiro.test(B3_SAT$nfc_total[B3_SAT$group == 'older adults']) # not given

##NFC homogeniety 
leveneTest(B3_SAT$nfc_total, B3_SAT$group)# given

t.test(B3_SAT$nfc_total~B3_SAT$group, var.equal = TRUE, alternative = "two.sided")




