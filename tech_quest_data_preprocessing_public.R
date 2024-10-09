################### pre-processing of technical issues questionnaire data from csv log-files ####################################################
#code and questionnaire by Sophia-Helen Sass(2024)

####preparation ####
#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('your_directory/Technical_Issues_questionnaire_data.csv')

####buiild data frame####
#Setting Labels
label(data$new_toke)="Musste ein weiterer Token vergeben werden?"
label(data$b9b3_toke_2)="Zweiter Token (da Problem bei erstem Durchgang des Experiments)"
label(data$b9b3_tech_err)="Hat die Versuchsperson technische Probleme berichtet?"
label(data$b9b3_tech_err2___0)="Frageboegen"
label(data$b9b3_tech_err2___1)="SAT Paradigma"
label(data$b9b3_tech_err2___2)="Spatial Working Memory (SWM)"
label(data$b9b3_tech_err2___3)="Identical Pictures (IDP)"
label(data$b9b3_tech_err2___4)="Spot-A-Word (SAW)"
label(data$b9b3_tech_err2___5)="Raven"
label(data$b9b3_tech_err3)="Beschreibe das aufgetretene Problem genauer (welches Device wurde verwendet [Laptop, Computer,...], welches OS [Windows, Mac,...], Problembeschreibung):"

#setting column names
colnames(data)[1] <- "participant_ID"
colnames(data)[2] <- "2nd_token_used"
colnames(data)[3] <- "2nd_token_name"
colnames(data)[4] <- "tech_issues"
colnames(data)[5] <- "issue_quest"
colnames(data)[6] <- "issue_SAT"
colnames(data)[7] <- "issue_SWM"
colnames(data)[8] <- "issue_IDP"
colnames(data)[9] <- "issue_SAW"
colnames(data)[10] <- "issue_Raven"
colnames(data)[11] <- "issue_note"

###### save preprocessed dataset  
#set path for saving
savedir = ("your_directory")

#csv
path_C <- paste0(savedir, "/tech_issues_preprocessed_data", ".csv", sep="")
write.csv(data, file=path_C, row.names=F)

