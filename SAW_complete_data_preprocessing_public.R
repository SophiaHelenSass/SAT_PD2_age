################### spot-a-word task - pre-processing of data from log-files ####################################################
#Task by Lindenberger, U., Mayr, U. & Kliegl, R. Speed and intelligence in old age. Psychol. Aging 8, 207-220 (1993).
#Code by Sophia-Helen Sass(2024)

#clear workspace
rm(list = ls())

#load packages 
library(rjson)
library(plyr)

#set working and data directory
setwd("your_directory")
datadir = ("your_directory")

##### fetching data from folders ########################################
#structure: one folder per participant contains json-data files of every task in experiment. Name of folder: Record-ID, e.g. 01528
#Algorithm goes through all participant folders to fetch respective json-files, convert them to data frames and save a new data set containing mean data of all participants which is ready to be analyzed.

folders = dir(datadir) 
nid = max(c(length(folders)))


saw = array(NA, c(nid,10)) 
for (f in 1:nid){ 
  tryCatch({
    subj    = folders[f]
    txtpath = paste0(datadir, "/", subj, sep="")
    setwd(txtpath)
    
    file_raw <- fromJSON(file = "spot_a_word_task-results.json", method = "C", unexpected.escape = "error", simplify = T)
    
    #extract first entry of the list
    data_raw <- file_raw[1]
    #save list as character string (fromJSON function needs json string)
    data_as_chr <-as.character(data_raw)
    #convert to list
    data_list <- fromJSON(json_str = data_as_chr)
    #remove first 9 and last 3 entries (irrelevant, no task data, you end up with same variable count per element, which refers to one trial description 
    data_list_reduced <- data_list[- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 45, 46, 47)]
    #in some trials there was no detection of RT/negative RT, hence we have a list of different length,which cannot be processed --> traisl need to be excluded
    data_list_reduced_new <- list()
    index <- 1
    #writes all trials of complete length (22) in a new list
    for (i in 1:length(data_list_reduced)){
      if (length(data_list_reduced[[i]])!= 22) next
      data_list_reduced_new[[index]] <- data_list_reduced[[i]]
      index <- index + 1
    }
    # convert new list to data frame
    dataframe_SAW <- ldply(data_list_reduced_new, data.frame)
    #remove column 1 to 9 (no behavioral data)
    dataframe_SAW <- dataframe_SAW[- c(1:3)] 
    
    #exclude RTs <150 ms (participant probably clicked randomly)
    tmp <- subset(dataframe_SAW, duration > 150) 
    # count how many have been excluded
    RToutlier <- subset (dataframe_SAW, duration <= 150) 
    
    #filter correct responses
    cor <- subset(dataframe_SAW, correct == 'TRUE')
    
    #fill new data frame with one value per variable per participant  
    saw[f,1]  = subj
    saw[f,2]  = nrow(dataframe_SAW)                                  # total number of responses
    saw[f,3]  = nrow(RToutlier)                                      # number of trials below 150 ms
    saw[f,4]  = sum(tmp$correct)                                     # number of correct responses
    saw[f,5]  = nrow(tmp) - sum(tmp$correct)                         # number of errors
    saw[f,6] = (sum(tmp$correct)/nrow(tmp))*100                      # accuracy as percentage correct
    saw[f,7] = mean(tmp$duration,na.rm=T)                            # mean(rt) ms
    saw[f,8] = sd(tmp$duration,na.rm=T)                              # std(rt)
    saw[f,9] = mean(cor$duration,na.rm=T)                            # mean(rt) ms correct
    saw[f,10] = sd(cor$duration,na.rm=T)                             # std(rt) correct
  
  #report subjects with problems reading the log file
  }, error = function(e) {
    message("a problem occured - please check subject folder: ", folders[f])
  })
  setwd(datadir)
}

#create data frame 
dimnames(saw) = list(1:nid,c(
  "participant_ID",
  "SAW_RESP_TOT",
  "SAW_RT_OUTLIER",
  "SAW_RESP_CORR",
  "SAW_RESP_ERR",
  "SAW_ACC",
  "SAW_RT",
  "SAW_RT_SD",
  "SAW_RT_CORR",
  "SAW_RT_SD_CORR"
))

#create table for export
B3_SAW <- saw
B3_SAW <- data.frame(B3_SAW)
B3_SAW[ is.na(B3_SAW) ] <- NA

#adjust data type
B3_SAW[ ,c(1:10)]         <- lapply(B3_SAW[ ,c(1:10)], as.character)
B3_SAW[ ,c(2:10)]         <- lapply(B3_SAW[ ,c(2:10)], as.numeric)

#### save pre-processed data #########################################
#set path for saving
savedir = ("your_directory_to_save_data")

#save all data sets as csv
path_C <- paste0(savedir, "/B3_SAW",".csv", sep="")
write.csv(B3_SAW, file=path_C, row.names=F)

################### End #######################################################################
