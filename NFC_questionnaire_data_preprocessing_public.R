#Need for Cognition - pre-processing of questionnaire data from csv log-files ####
#German version of the NFC questionnaire by Bless, H., Wänke, M., Bohner, G., Fellhauer, R. F., & et al. Need for Cognition: Eine Skala zur Erfassung von Engagement und Freude bei Denkaufgaben. [Need for cognition: A scale measuring engagement and happiness in cognitive tasks.]. Z. Für Sozialpsychologie 25, 147-154 (1994).
#Code by Sophia-Helen Sass (2024)

#clear workspace
rm(list = ls())

#load packages 
library(Hmisc)
library(dplyr)


##### fetching data ########################################
#read data from your data directory
data=read.csv("directory/NFC_questionnaire_data.csv")

#Setting Labels
label(data$nfc_1)="Die Aufgabe, neue Loesungen fuer Probleme zu finden, macht mir wirklich Spass."
label(data$nfc_2)="Ich wuerde lieber eine Aufgabe loesen, die Intelligenz erfordert, schwierig und bedeutend ist, als eine Aufgabe, die zwar irgendwie wichtig ist, aber nicht viel Nachdenken erfordert."
label(data$nfc_3)="Ich setze mir eher solche Ziele, die nur mit erheblicher geistiger Anstrengung erreicht werden koennen."
label(data$nfc_4)="Die Vorstellung, mich auf mein Denkvermoegen zu verlassen, um es zu etwas zu bringen, spricht mich nicht an."
label(data$nfc_5)="Ich finde es besonders befriedigend, eine bedeutende Aufgabe abzuschliessen, die viel Denken und geistige Anstrengung erfordert hat."
label(data$nfc_6)="Ich denke lieber ueber kleine, alltaegliche Vorhaben nach, als ueber langfristige."
label(data$nfc_7)="Ich wuerde lieber etwas tun, das wenig Denken erfordert, als etwas, das mit Sicherheit meine Denkfaehigkeit herausfordert."
label(data$nfc_8)="Ich finde wenig Befriedigung darin, angestrengt und stundenlang nachzudenken."
label(data$nfc_9)="In erster Linie denke ich, weil ich es muss."
label(data$nfc_10)="Ich trage nicht gern die Verantwortung fuer eine Situation, die sehr viel Denken erfordert."
label(data$nfc_11)="Denken entspricht nicht dem, was ich unter Spass verstehe."
label(data$nfc_12)="Ich versuche, Situationen vorauszuahnen und zu vermeiden, in denen die Wahrscheinlichkeit gross ist, dass ich intensiv ueber etwas nachdenken muss."
label(data$nfc_13)="Ich habe es gern, wenn mein Leben voller kniffliger Aufgaben ist, die ich lösen muss."
label(data$nfc_14)="Ich wuerde komplizierte Probleme einfachen Problemen vorziehen."
label(data$nfc_15)="Es genuegt mir, einfach die Antwort zu kennen, ohne die Gruende fuer die Antwort eines Problems zu verstehen."
label(data$nfc_16)="Es genuegt, dass etwas funktioniert, mir ist es egal, wie oder warum."


#Setting Factors(will create new variable for factors)
data$nfc_1 = factor(data$nfc_1,levels=c("-3","-2","-1","0","1","2","3"))
data$nfc_2 = factor(data$nfc_2,levels=c("-3","-2","-1","0","1","2","3"))
data$nfc_3 = factor(data$nfc_3,levels=c("-3","-2","-1","0","1","2","3"))
data$nfc_4 = factor(data$nfc_4,levels=c("-3","-2","-1","0","1","2","3"))#inverse
data$nfc_5 = factor(data$nfc_5,levels=c("-3","-2","-1","0","1","2","3"))
data$nfc_6 = factor(data$nfc_6,levels=c("-3","-2","-1","0","1","2","3"))#inverse
data$nfc_7 = factor(data$nfc_7,levels=c("-3","-2","-1","0","1","2","3"))#inverse
data$nfc_8 = factor(data$nfc_8,levels=c("-3","-2","-1","0","1","2","3"))#inverse
data$nfc_9 = factor(data$nfc_9,levels=c("-3","-2","-1","0","1","2","3"))#inverse
data$nfc_10 = factor(data$nfc_10,levels=c("-3","-2","-1","0","1","2","3"))#inverse
data$nfc_11 = factor(data$nfc_11,levels=c("-3","-2","-1","0","1","2","3"))#inverse
data$nfc_12 = factor(data$nfc_12,levels=c("-3","-2","-1","0","1","2","3"))#inverse
data$nfc_13 = factor(data$nfc_13,levels=c("-3","-2","-1","0","1","2","3"))
data$nfc_14 = factor(data$nfc_14,levels=c("-3","-2","-1","0","1","2","3"))
data$nfc_15 = factor(data$nfc_15,levels=c("-3","-2","-1","0","1","2","3"))#inverse
data$nfc_16 = factor(data$nfc_16,levels=c("-3","-2","-1","0","1","2","3"))#inverse

levels(data$nfc_1)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_2)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_3)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_4)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_5)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_6)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_7)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_8)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_9)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_10)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_11)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_12)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_13)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_14)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_15)=c("-3","-2","-1","0","1","2","3")
levels(data$nfc_16)=c("-3","-2","-1","0","1","2","3")

colnames(data)[1] <- "participant_ID"
data <- subset(data[ ,-c(2:4)])
NFC_data<- data.frame(data)


###### save preprocessed data set  
#set path for saving
savedir = ("your_saving_directory")

#csv
path_C <- paste0(savedir, "/NFC_preprocessed", ".csv", sep="")
write.csv(NFC_data, file=path_C, row.names=F)

