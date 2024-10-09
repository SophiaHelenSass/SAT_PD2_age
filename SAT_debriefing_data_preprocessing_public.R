################### pre-processing of debriefing questionnaire data from the Space Adventure Task PD 2 debriefing json log-files ####################################################
#Task adapted from 	Steffen, J. et al. Shorter planning depth and higher response noise during sequential decision-making in old age. Sci. Rep. 13, 7692 (2023). 
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
#Algorithm goes through all participant folders to fetch respective json-files, 
#convert them to data frames and save a new data set containing mean data of all participants which is ready to be analyzed.

folders = dir(datadir) 
nid = max(c(length(folders)))
DB = array(NA, c(nid, 17)) 

for (f in 1:nid){ 
  tryCatch({
    subj    = folders[f]
    txtpath = paste0(datadir, "/", subj, sep="")
    setwd(txtpath)
    
    data_raw <- fromJSON(file = "space_adventure_debrief-results.json", method = "C", unexpected.escape = "error", simplify = T)
    
    #extract first entry of the list
    data_first_entry <- data_raw[1]
    #save list as character string (fromJSON function needs json string)
    data_as_chr <-as.character(data_first_entry)
    # convert to list
    data_list <- fromJSON(json_str = data_as_chr)
    
    # write data frame
    DB[f,1]  = subj
    DB[f,2] = data_list$move
    DB[f,3] = data_list$jump
    
    #set various response options (because participants' responses were free text) 
    if (data_list[["points_p1"]] == "-20" & data_list[["points_p2"]] == "-10" & data_list[["points_p3"]] == "0" & data_list[["points_p4"]] == "10" & data_list[["points_p5"]] == "20" |
        data_list[["points_p1"]] == "-20" & data_list[["points_p2"]] == "-10" & data_list[["points_p3"]] == "0" & data_list[["points_p4"]] == "+10" & data_list[["points_p5"]] == "+20"|
        data_list[["points_p1"]] == "minus 20" & data_list[["points_p2"]] == "minus 10" & data_list[["points_p3"]] == "0" & data_list[["points_p4"]] == "plus 10" & data_list[["points_p5"]] == "plus 20"|
        data_list[["points_p1"]] == "- 20" & data_list[["points_p2"]] == "- 10" & data_list[["points_p3"]] == "0" & data_list[["points_p4"]] == "10" & data_list[["points_p5"]] == "20"){
      DB[f, 4] = "correct"
    } else {
      DB[f, 4] = "false"
    }
    
    #entries for fuel points for each planet
    DB[f,5] = data_list$points_p1
    DB[f,6] = data_list$points_p2
    DB[f,7] = data_list$points_p3
    DB[f,8] = data_list$points_p4
    DB[f,9] = data_list$points_p5
    
    #count number of correct responses for jumping pattern
    count = 0
    if (data_list[["target_p1"]] == "5"){
      count = count + 1} 
    if (data_list[["target_p2"]] == "4"){
      count = count + 1} 
    if (data_list[["target_p3"]] == "5"){
      count = count +1}
    if (data_list[["target_p4"]] == "6"){
      count = count + 1}
    if (data_list[["target_p5"]] == "2"){
      count = count + 1}
    if (data_list[["target_p6"]] == "2"){
      count = count + 1}
    #responses summarized as "correct" if  participants responded correctly for all of the 6 transitions
    if (count == 6){
      DB[f,10] ="correct"}
    if (count <6) { 
      DB[f,10] = "false"}
    
    #entries for jumping target for each planet
    DB[f,11] = data_list$target_p1
    DB[f,12] = data_list$target_p2
    DB[f,13] = data_list$target_p3
    DB[f,14] = data_list$target_p4
    DB[f,15] = data_list$target_p5
    DB[f,16] = data_list$target_p6
    
    #entries for responses to game rule questions
    #Question: Please indicate what applies to the space task (several answers may be correct).
    #response option 1: A lot of points were scored through clever planning.
    if (data_list[["mcq"]] == "optimality") {
      DB[f,17] ="correct"
    } else {
        DB[f,17] = "false"
        }
    
    # report subjects with problems reading the log-file
    }, error = function(e) {
    message("a problem occured - please check subject folder: ", folders[f])
  } )
}

    #name columns    
    dimnames(DB) = list(1:nid,c(
      "participant_ID",       #1
      "SAT_key_move",         #2
      "SAT_key_jump",         #3
      "SAT_points_planet",    #4
      "SAT_points_p1",        #5
      "SAT_points_p2",        #6
      "SAT_points_p3",        #7
      "SAT_points_p4",        #8
      "SAT_points_p5",        #9
      "SAT_jumping_pattern",  #10
      "SAT_target_p1",        #11
      "SAT_target_p2",        #12
      "SAT_target_p3",        #13
      "SAT_target_p4",        #14
      "SAT_target_p5",        #15
      "SAT_target_p6",        #16
      "SAT_Plan_optimality"  #17
    ))
  
  #create table for export  
  SAT_DB <- DB
  SAT_DB <- data.frame(SAT_DB)
  SAT_DB[ is.na(SAT_DB) ] <- NA
  
  #adjust data type    
  SAT_DB[ ,c(1:17)]         <- lapply(SAT_DB[ ,c(1:17)], as.character)
  SAT_DB[ ,c(2,3,4,10,17)]      <- lapply(SAT_DB[ ,c(2,3,4,10,17)], as.factor)

#check whether participants' responses were complete (some participants closed questionnaire without finishing)
    SAT_complete = SAT_DB[complete.cases(SAT_DB), ]
    SAT_incomplete = SAT_DB[complete.cases(SAT_DB)!= TRUE, ]

#### save pre-processed data #########################################
    #set path for saving
    savedir = (
      "your_directory")
    
    #save all data sets as csv
    path_C <- paste0(savedir, "/SAT_DB_all", ".csv", sep = "")
    write.csv(SAT_DB, file = path_C, row.names = F)

################### End #######################################################################
    
    