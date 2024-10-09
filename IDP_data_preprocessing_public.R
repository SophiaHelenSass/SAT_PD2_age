### Identical Pictures Task - preprocessing from data from log-files 
#Code by Sophia-Helen Sass (2024)
#Task by Lindenberger, U., Mayr, U. & Kliegl, R. Speed and intelligence in old age. Psychol. Aging 8, 207-220 (1993).

###### preparation ############################################
#clear workspace
  rm(list = ls())

#load packages
  library(rjson)
  library(plyr)

#set working and data directory
  setwd("your_data_directory")
  datadir = ("your_data_directory")

##### fetching data from folders ########################################
#structure: one folder per participant contains json-data files of every task in experiment. Name of folder: Record-ID, e.g. 01528
#Algorithm goes through all participant folders to fetch respective json-files, 
#convert them to data frames and save a new data set containing mean data of all participants which is ready to be analyzed.
  
folders = dir(datadir)
nid = max(c(length(folders)))
idpic = array(NA, c(nid, 11)) 

for (f in 1:nid){ 
  tryCatch({
    subj    = folders[f]
    txtpath = paste0(datadir, "/", subj, sep="")
    setwd(txtpath)

 
file_raw <- fromJSON(file = "identical_pictures_task-results.json", method = "C", unexpected.escape = "error", simplify = T)

#extract first entry of the list
data_raw <- file_raw[1]

#save list as character string (fromJSON function needs json string)
data_as_chr <-as.character(data_raw)
# convert to list
data_list <- fromJSON(json_str = data_as_chr)
# remove first 9 and last 3 entries (irrelevant, no task data, you end up with same variable count per element, which refers to one trial description 
data_list_reduced <- data_list[- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 56, 57, 58)]
# convert list to data frame
dataframe_IDP <- ldply(data_list_reduced, data.frame)
#remove column 1 to 9 (no behavioral data)
dataframe_IDP <- dataframe_IDP[- c(1:9)] 

#exclude RTs <150 ms (participant probably clicked randomly) and set time limit of 80 seconds for whole task (speed component)
start = min(dataframe_IDP$time_show,na.rm=T)
end = start + 80000
beyond_80s = subset(dataframe_IDP, time_switch > end,na.rm=T)
tmp <- subset(dataframe_IDP, duration > 150 & time_switch <= end,na.rm=T) 

# count how many have been excluded
RToutlier <- subset (dataframe_IDP, duration < 150 & time_switch <= end,na.rm=T) 

#filter correct responses
cor <- subset(dataframe_IDP, correct == 'TRUE',na.rm=T)

#fill new data frame with summarized value per variable per participant
idpic[f,1] = subj
idpic[f,2] = nrow(tmp) + nrow(RToutlier)                            # total number of responses
idpic[f,3] = nrow(RToutlier)                                        # number of trials below 150 ms
idpic[f,4] = sum(tmp$correct)                                       # number of correct responses
idpic[f,5] = nrow(tmp) - sum(tmp$correct)                           # number of errors
idpic[f,6] = (sum(tmp$correct)/nrow(tmp))*100                      # accuracy as percentage correct
idpic[f,7] = mean(tmp$duration,na.rm=T)                            # mean(rt) ms
idpic[f,8] = sd(tmp$duration,na.rm=T)                              # std(rt)
idpic[f,9] = mean(cor$duration,na.rm=T)                            # mean(rt) ms correct
idpic[f,10] = sd(cor$duration,na.rm=T)                              # std(rt) correct
idpic[f,11] = nrow(beyond_80s)                                      # number of trials beyond 80 ms time cut-off

#report subjects with problems reading the log file
}, error = function(e) {
  message("a problem occured - please check subject folder: ", folders[f])
  })
 setwd(datadir)
}

#create data frame 
dimnames(idpic) = list(1:nid,c(
  "participant_ID",
  "IDP_RESP_TOT",
  "IDP_RT_OUTLIER",
  "IDP_RESP_CORR",
  "IDP_RESP_ERR",
  "IDP_ACC",
  "IDP_RT",
  "IDP_RT_SD",
  "IDP_RT_CORR",
  "IDP_RT_SD_CORR",
  "IDP_trials_beyond_80s"
))

#create table for export
B3_IDP <- idpic
B3_IDP <- data.frame(B3_IDP)
B3_IDP[ is.na(B3_IDP) ] <- NA

#adjust data type 
B3_IDP[ ,c(1:11)]         <- lapply(B3_IDP[ ,c(1:11)], as.character)
B3_IDP[ ,c(2:11)]         <- lapply(B3_IDP[ ,c(2:11)], as.numeric)

#### save pre-processed data ########################################
#set path for saving
savedir = ("your_directory_to_save_data")

#save data as csv
path_C <- paste0(savedir, "/B3_IDP",".csv", sep="")
write.csv(B3_IDP, file=path_C, row.names=F)

################### End #######################################################################


