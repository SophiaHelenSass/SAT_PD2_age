################### pre-processing of socio-demographic and technical experience questionnaire data from csv log-files ####################################################
#Code and questionnaire by Sophia-Helen Sass(2024)

#clear workspace
rm(list = ls())

#load packages 
library(Hmisc)
library(dplyr)

#read data
data=read.csv("your_directory/Soziodemo_Computer.csv")

#Setting Labels
#socio-demgraphic data
label(data$b09_sozio_family)="Bitte nennen Sie uns Ihren Familienstand."
label(data$b09_sozio_child)="Haben Sie eigene Kinder?"
label(data$b09_sozio_migrat)="Migrationshintergrund: Sind Sie selbst, Ihre Eltern oder Ihre Grosseltern in einem anderen Land aufgewachsen als dem Land Ihres jetzigen Wohnorts?"
label(data$b09_sozio_popul)="Welche Grösse hat Ihr Wohnort (Einwohnerzahl)?"

label(data$b09_sozio_graduat)="Welchen hoechsten allgemeinbildenden Schulabschluss haben Sie?"
label(data$b09_sozio_job)="Waren Sie in den letzten drei Monaten berufstaetig?"
label(data$b09_sozio_jobmarket)="War dies auf dem 1. Arbeitsmarkt? Als erster Arbeitsmarkt wird der regulaere Arbeitsmarkt bezeichnet. Auf diesem Arbeitsmarkt bestehen Arbeits- und Beschaeftigungsverhaeltnisse ohne Zuschuesse oder sonstige Massnahmen der aktiven Arbeitsmarktpolitik auf Basis der freien Wirtschaft. Es werden keine staatlichen Leistungen seitens der Arbeitgeber oder Arbeitnehmer empfangen. Ausgeschlossen werden: Arbeitsbeschaffungsmassnahmen (ABM), die Errichtung von Beschaeftigungsgesellschaften, die sogenannten Ein-Euro-Jobs."
label(data$b9b3_job_state)="Was ist Ihr aktueller beruflicher Status?"
label(data$b09_sozio_income)="Wie hoch ist Ihr persoenliches durchschnittliches monatliches Nettoeinkommen?"
label(data$b09_sozio_livingtog___1)="lebt allein"
label(data$b09_sozio_livingtog___2)="lebt mit Kind/ern"
label(data$b09_sozio_livingtog___3)="lebt mit (Ehe-)Partner/in"
label(data$b09_sozio_livingtog___4)="lebt mit sonstigen Familienangehoerigen"
label(data$b09_sozio_livingtog___5)="lebt mit Wohngemeinschaft"
label(data$b09_sozio_livingtog___6)="lebt mit therapeutische Wohngemeinschaft/Wohnheim"
label(data$b09_sozio_livingtog___7)="lebt mit Eltern(-teil)"
label(data$b09_sozio_livingtog___8)="lebt obdachlos"

#technical experience
label(data$b9b3_use_electronics)="Wie haeufig nutzen Sie Computer/Laptop, Tablet, Smartphone oder aehnliche Geraete?"
label(data$b9b3_games)="Wie haeufig spielen Sie Computer-/Video-/Onlinespiele?"
label(data$b9b3_games_which)="Welche Computer-/Video-/Onlinespiele haben Sie in den letzten 3 Monaten gespielt? "
label(data$b9b3_device)="Welches Geraet benutzen Sie gerade zur Durchfuehrung dieses Experiments?"
label(data$b9b3_elec_os)="Welches Betriebssystem hat der Computer/Laptop oder das Tablet, an dem Sie die Aufgaben dieser Studie durchfuehren?"
label(data$browser)="Welchen Internet-Browser verwenden Sie gerade?"
label(data$browser_other)="Welchen anderen Browser verwenden Sie?"

#Setting Factors(will create new variable for factors)
data$b09_sozio_family.factor = factor(data$b09_sozio_family,levels=c("1","2","3","4","5"))
data$b09_sozio_child.factor = factor(data$b09_sozio_child,levels=c("0","1"))
data$b09_sozio_migrat.factor = factor(data$b09_sozio_migrat,levels=c("0","1"))
data$b09_sozio_popul.factor = factor(data$b09_sozio_popul,levels=c("1","2","3","4","5","6"))

data$b09_sozio_graduat.factor = factor(data$b09_sozio_graduat,levels=c("1","2","3","4","5","6","7","8","9"))
data$b09_sozio_job.factor = factor(data$b09_sozio_job,levels=c("0","1"))
data$b09_sozio_jobmarket.factor = factor(data$b09_sozio_jobmarket,levels=c("0","1"))
data$b9b3_job_state.factor = factor(data$b9b3_job_state,levels=c("0","1","3","4","5","6","7","8","9"))
data$b09_sozio_income.factor = factor(data$b09_sozio_income,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$b09_sozio_livingtog___1.factor = factor(data$b09_sozio_livingtog___1,levels=c("0","1"))
data$b09_sozio_livingtog___2.factor = factor(data$b09_sozio_livingtog___2,levels=c("0","1"))
data$b09_sozio_livingtog___3.factor = factor(data$b09_sozio_livingtog___3,levels=c("0","1"))
data$b09_sozio_livingtog___4.factor = factor(data$b09_sozio_livingtog___4,levels=c("0","1"))
data$b09_sozio_livingtog___5.factor = factor(data$b09_sozio_livingtog___5,levels=c("0","1"))
data$b09_sozio_livingtog___6.factor = factor(data$b09_sozio_livingtog___6,levels=c("0","1"))
data$b09_sozio_livingtog___7.factor = factor(data$b09_sozio_livingtog___7,levels=c("0","1"))
data$b09_sozio_livingtog___8.factor = factor(data$b09_sozio_livingtog___8,levels=c("0","1"))


data$b9b3_use_electronics.factor = factor(data$b9b3_use_electronics,levels=c("1","2","3","4","5"))
data$b9b3_games.factor = factor(data$b9b3_games,levels=c("1","2","3","4","5"))
data$b9b3_device.factor = factor(data$b9b3_device,levels=c("1","2","3"))
data$b9b3_elec_os.factor = factor(data$b9b3_elec_os,levels=c("0","1","2","3"))
data$browser.factor = factor(data$browser,levels=c("1","2","3","4","5","6","7"))


levels(data$b09_sozio_family.factor)=c("ledig","in Ehe/ Partnerschaft lebend","getrennt lebend","geschieden","verwitwet", na.rm = TRUE)
levels(data$b09_sozio_child.factor)=c("nein","ja")
levels(data$b09_sozio_migrat.factor)=c("nein","ja", na.rm = TRUE)
levels(data$b09_sozio_popul.factor)=c("bis 5.000","bis 20.000","bis 50.000","bis 100.000","bis 500.000","Ã¼ber 500.000")

levels(data$b09_sozio_graduat.factor)=c("Schueler/in, besuche eine allgemeinbildende Vollzeitschule","Schueler/in, besuche eine berufsorientierte Aufbau-, Fachschule o.ae.","Von der Schule abgegangen ohne Hauptschulabschluss (Volksschulabschluss)","Hauptschulabschluss (Volksschulabschluss)","Realschulabschluss (Mittlere Reife)","Abschluss der Polytechnischen Oberschule 10. Klasse (vor 1965: 8. Klasse)","Fachhochschulreife, Abschluss Fachoberschule","Allgemeine oder fachgebundene Hochschulreife/ Abitur (Gymnasium bzw. EOS, auch EOS mit Lehre)","Einen anderen Schulabschluss und zwar:", na.rm = TRUE)
levels(data$b09_sozio_job.factor)=c("nein","ja", na.rm = TRUE)
levels(data$b09_sozio_jobmarket.factor)=c("nein","ja", na.rm = TRUE)
levels(data$b9b3_job_state.factor)=c("Angestellt","Selbststaendig","Beamtet","Student/Studentin","Schueler/Schuelerin","Berentet","Berufsunfaehig","Arbeitssuchend","Hausmann/ Hausfrau", na.rm = TRUE)
levels(data$b09_sozio_income.factor)=c("unter 500 ","500 - 1.000 ","1.000 - 1.500 ","1.500 - 2.000 ","2.000 - 2.500 ","2.500 - 3.500 ","3.500 - 4.000 ","4.000 - 4.500 ","4.500 - 5.000 ","mehr als 5.000 ", na.rm = TRUE)

levels(data$b9b3_use_electronics.factor)=c("(fast) nie","weniger als ein Mal im Monat","ein- bis mehrmals im Monat","ein- bis mehrmals in der Woche","taeglich", na.rm = TRUE)
levels(data$b9b3_games.factor)=c("(fast) nie","weniger als ein Mal im Monat","ein- bis mehrmals im Monat","ein- bis mehrmals in der Woche","taeglich", na.rm = TRUE)
levels(data$b9b3_device.factor)=c("Laptop","Standrechner","Tablet", na.rm = TRUE)
levels(data$b9b3_elec_os.factor)=c("Windows","MacOS (Apple)","Linux","Android", na.rm = TRUE)
levels(data$browser.factor)=c("Mozilla Firefox","Microsoft Edge","Google Chrome","Safari (Apple)","Tor","Opera","andere", na.rm = TRUE)

#remove piping ID column
data <- subset(data, select = -c(b9b3_pipe_id))


#setting column names
colnames(data)[1]  <- "participant_ID"
colnames(data)[2]  <- "age_prescreen"
colnames(data)[3]  <- "age_phone"
colnames(data)[4]  <- "sex"
colnames(data)[5]  <- "marital_status"
colnames(data)[29] <- "marital_status_info"
colnames(data)[6]  <- "children"
colnames(data)[30] <- "children_info"
colnames(data)[7]  <- "migration"
colnames(data)[31] <- "migration_info"
colnames(data)[8]  <- "population"
colnames(data)[32] <- "population_info"

colnames(data)[9]  <- "education"
colnames(data)[33] <- "education_info"
colnames(data)[10] <- "job status_3_months"
colnames(data)[34] <- "job status_3_months_info"
colnames(data)[11] <- "job_status_specific"
colnames(data)[35] <- "job_status_specific_info"
colnames(data)[12] <- "job_status_current"
colnames(data)[36] <- "job_status_current_info"
colnames(data)[13] <- "income"
colnames(data)[37] <- "income_info"

colnames(data)[14] <- "live_alone"
colnames(data)[38] <- "live_alone_info"
colnames(data)[15] <- "live_child"
colnames(data)[39] <- "live_child_info"
colnames(data)[16] <- "live_partner"
colnames(data)[40] <- "live_partner_info"
colnames(data)[17] <- "live_fam"
colnames(data)[41] <- "live_fam_info"
colnames(data)[18] <- "live_flatshare"
colnames(data)[42] <- "live_flatshare_info"
colnames(data)[19] <- "live_therap_flatshare"
colnames(data)[43] <- "live_therap_flatshare_info"
colnames(data)[20] <- "live_parents"
colnames(data)[44] <- "live_parents_info"
colnames(data)[21] <- "homeless"
colnames(data)[45] <- "homeless_info"

colnames(data)[22] <- "electronic_use"
colnames(data)[46] <- "electronic_use_info"
colnames(data)[23] <- "gaming"
colnames(data)[47] <- "gaming_info"
colnames(data)[24] <- "gaming_specific"
colnames(data)[25] <- "device_exp"
colnames(data)[48] <- "device_exp_info"
colnames(data)[26] <- "operating_system"
colnames(data)[49] <- "operating_system_info"
colnames(data)[27] <- "browser"
colnames(data)[50] <- "browser_info"




#correct eductaion
data$education[data$participant_ID == 1192]=8
data$education_info[data$participant_ID == 1192]="Allgemeine oder fachgebundene Hochschulreife/ Abitur (Gymnasium bzw. EOS, auch EOS mit Lehre)"

data$education[data$participant_ID == 1322]=8
data$education_info[data$participant_ID == 1322]="Allgemeine oder fachgebundene Hochschulreife/ Abitur (Gymnasium bzw. EOS, auch EOS mit Lehre)"

data$education[data$participant_ID == 1457]=8
data$education_info[data$participant_ID == 1457]="Allgemeine oder fachgebundene Hochschulreife/ Abitur (Gymnasium bzw. EOS, auch EOS mit Lehre)"

data$education[data$participant_ID == 1623]=8
data$education_info[data$participant_ID == 1623]="Allgemeine oder fachgebundene Hochschulreife/ Abitur (Gymnasium bzw. EOS, auch EOS mit Lehre)"

#build data frame
socio_demo_data_complete<- data.frame(data)

#create data frame with relevant variables
socio_demo_data<- socio_demo_data_complete[,-c(29:50)]


###### save pre-processed data    
#set path for saving
savedir = ("your_directory")

#save reduced data frame as csv
path_C <- paste0(savedir, "/socio_demo_preprocessed", ".csv", sep="")
write.csv(socio_demo_data, file=path_C, row.names=F)


