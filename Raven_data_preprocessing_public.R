#### preprocessing of data from 12-item form of Raven's Advanced Progressive Matrices ####
#Task by 	Bors, D. A. & Stokes, T. L. Raven's Advanced Progressive Matrices: Norms for First-Year University Students and the Development of a Short Form. Educ. Psychol. Meas. 58, 382-398 (1998).
#Code by Sophia-Helen Sass

##### preprataion #####
#clear workspace
rm(list = ls())

#put packages in library
library(rjson)
library(plyr)

#set working directory
setwd("your_directory")
datadir = ("your_directory")


#fetch all data by iterating through folders in directory
folders = dir(datadir) 
nid = max(c(length(folders)))
raven = array(NA, c(nid, 42)) 

for (f in 1:nid){ 
  tryCatch({
    subj    = folders[f]
    txtpath = paste0(datadir, "/", subj, sep="")
    setwd(txtpath)
    
    data_raw <- fromJSON(file = "raven_task-results.json", method = "C", unexpected.escape = "error", simplify = T)
    
    #extract first entry of the list
    data_first_entry <- data_raw[1]
    
    #save list as character string (fromJSON function needs json string)
    data_as_chr <-as.character(data_first_entry)
    
    # convert to list
    data_list <- fromJSON(json_str = data_as_chr)
    
    # remove first 9 and last 3 entries (irrelevant, no task data, you end up 
    # with same variable count per element, which refers to one trial description 
    data_list_reduced <- data_list[- c(1,2,4,6,7,20:22)]
    #data_list_response <- data_list_reduced[- c(20:22)]
    
    # convert list to data frame
    dataframe_Raven <- ldply(data_list_reduced, data.frame)
    
    #filter example trials and test trials, as well as aborted trials due to 15 min timeout
    examples <- subset(dataframe_Raven, sender == 'example1' | sender == 'example2')
    test_data <- subset (dataframe_Raven,sender != 'example1' & sender != 'example2' )
    test_data <- subset (test_data,ended_on == 'response'& duration > 1000)
    
    # exclude RTs <150 ms, participant clicked randomly
    tmp <- test_data
 
    # if you want to exclude the trials and calculate the task 
    # measures with the rest of the data, choose this option, otherwise, keep the tmp variable
    # count how many (would) have been excluded
    RToutlier <- subset (test_data, duration <= 150) 
    
    #filter correct responses
    cor <- subset(tmp, correct == 'TRUE')
    
    #fill new data frame with one value per variable per participant
    raven[f,1] = subj
    raven[f,2] = nrow(tmp)                                             # total number of responses
    raven[f,3] = nrow(RToutlier)                                       # number of trials below 1000 ms
    raven[f,4] = sum(tmp$correct)                                      # number of correct responses
    raven[f,5] = nrow(tmp) - sum(tmp$correct)                          # number of errors
    raven[f,6] = (sum(tmp$correct)/nrow(tmp))*100                      # accuracy as percentage correct
    raven[f,7] = mean(tmp$duration,na.rm=T)                            # mean(rt) ms
    raven[f,8] = sd(tmp$duration,na.rm=T)                              # std(rt)
    raven[f,9] = sqrt(sd(tmp$duration,na.rm=T))                        # sqrt(std(rt))
    raven[f,10] = mean(cor$duration,na.rm=T)                           # mean(rt) ms correct
    raven[f,11] = sd(cor$duration,na.rm=T)                             # std(rt) correct
    raven[f,12] = sqrt(sd(cor$duration,na.rm=T))                       # sqrt(std(rt)) correct
    raven[f,13] = ((max(tmp$time_switch)-min(tmp$time_show, na.rm=T ))/1000)/60  # full time for task in minutes (first trial to end)
    raven[f,14] = sum(examples$correct)                                # num of correct example trials

    raven[f,29] = examples$duration[examples$sender == 'example1']
    raven[f,30] = examples$duration[examples$sender == 'example2'] 
    
    raven[f,31] = tmp$duration[1]
    raven[f,32] = tmp$duration[2]
    raven[f,33] = tmp$duration[3]
    raven[f,34] = tmp$duration[4]
    raven[f,35] = tmp$duration[5]
    raven[f,36] = tmp$duration[6]
    raven[f,37] = tmp$duration[7]
    raven[f,38] = tmp$duration[8]
    raven[f,39] = tmp$duration[9]
    raven[f,40] = tmp$duration[10]
    raven[f,41] = tmp$duration[11]
    raven[f,42] = tmp$duration[12]
    
    
    raven[f,15] = examples$correct[examples$sender == 'example1']
    raven[f,16] = examples$correct[examples$sender == 'example2']
    
    raven[f,17] = tmp$correct[1]
    raven[f,18] = tmp$correct[2] 
    raven[f,19] = tmp$correct[3]
    raven[f,20] = tmp$correct[4] 
    raven[f,21] = tmp$correct[5]
    raven[f,22] = tmp$correct[6]
    raven[f,23] = tmp$correct[7]
    raven[f,24] = tmp$correct[8]
    raven[f,25] = tmp$correct[9]
    raven[f,26] = tmp$correct[10]
    raven[f,27] = tmp$correct[11]  
    raven[f,28] = tmp$correct[12]
   
#report subjects with problems reading the log file
  }, error = function(e) {
    message("a problem occured - please check subject folder: ", folders[f])
  })
  setwd(datadir)
  
}

###################### create data frame ###############################################

dimnames(raven) = list(1:nid,c(
  "participant_ID",
  "Raven_RESP_TOT",     
  "Raven_RT_OUTLIER",   
  "Raven_RESP_CORR",    
  "Raven_RESP_ERR",     
  "Raven_ACC",          
  "Raven_RT",           
  "Raven_RT_SD",        
  "Raven_RT_SQRT",      
  "Raven_RT_CORR",      
  "Raven_RT_SD_CORR",   
  "Raven_RT_SQRT_CORR", 
  "Raven_time4task_min",
  "Raven_Example_trials_num_corr",
  "Raven_Example1",
  "Raven_Example2",
  "Raven_Trial1",
  "Raven_Trial2",
  "Raven_Trial3",
  "Raven_Trial4",
  "Raven_Trial5",
  "Raven_Trial6",
  "Raven_Trial7",
  "Raven_Trial8",
  "Raven_Trial9",
  "Raven_Trial10",
  "Raven_Trial11",
  "Raven_Trial12",
  "Raven_RT_Exmpl1",
  "Raven_RT_Exmpl2",
  "Raven_RT_T1",
  "Raven_RT_T2",
  "Raven_RT_T3",
  "Raven_RT_T4",
  "Raven_RT_T5",
  "Raven_RT_T6",
  "Raven_RT_T7",
  "Raven_RT_T8",
  "Raven_RT_T9",
  "Raven_RT_T10",
  "Raven_RT_T11",
  "Raven_RT_T12"

))

B3_Raven <- raven
B3_Raven <- data.frame(B3_Raven)
B3_Raven[ is.na(B3_Raven) ] <- NA


B3_Raven[ ,c(1:42)]         <- lapply(B3_Raven[ ,c(1:42)], as.character)
B3_Raven[ ,c(2:14,29:42)]         <- lapply(B3_Raven[ ,c(2:14,29:42)], as.numeric)
B3_Raven[ ,c(15:28)]        <- lapply(B3_Raven[ ,c(15:28)], as.factor)

########################### save data #####################################################
#set path for saving
savedir = ("your_directory_to_save_data")

#csv
path_C <- paste0(savedir, "/B3_Raven_detailed", ".csv", sep="")
write.csv(B3_Raven, file=path_C, row.names=F)

###############
### THE END ###
###############


