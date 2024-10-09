####pre-process raw data of model parameters (kappa, beta, theta) from SAT inference ####
#Parameters of winning model from model comparison are imported (discounted low-probability pruning model)
#Code by Sophia-Helen Sass

#### preparation ####
#clear workspace
rm(list = ls())

#read in parameters from inference
params <- read.csv("your_directory/pars_post_samples_lowprob_pruning_discount_hyperbolic_theta_kmax30.csv")
params$X <- NULL

####put parameters in order (THETA, KAPPA, BETA)####
row_len <- length(params$parameter[params$parameter=="$\\theta$"])
params_ordered <- data.frame(subject=rep(NA,row_len),ID=rep(NA,row_len),
                             group=rep(NA,row_len),theta=rep(NA,row_len),
                             beta=rep(NA,row_len), kappa=rep(NA,row_len))

params_ordered$subject <- params$subject[params$parameter=="$\\theta$"]
params_ordered$ID <- params$IDs[params$parameter=="$\\theta$"]
params_ordered$group <- params$group[params$parameter=="$\\theta$"]
params_ordered$theta <- params$value[params$parameter=="$\\theta$"]
params_ordered$beta <- params$value[params$parameter=="$\\beta$"]
params_ordered$kappa<- params$value[params$parameter=="$k$"]

#organize participant IDs 
#adapt participant index (avoid duplication in numbers)
nOA = 57

for (l in (1:nrow(params_ordered))){                                           
  if(params_ordered$group[l]=="YA"){                                          
    params_ordered$subject[l] <- params_ordered$subject[l]+nOA
  }}

####compute parameter means per participant####
row_len <- length(table(params_ordered$ID))
mean_params_ordered <- data.frame(participant_ID=rep(NA,row_len),
                                  disc_lowprob_prun_theta=rep(NA,row_len),
                                  disc_lowprob_prun_beta=rep(NA,row_len),disc_lowprob_prun_kappa=rep(NA,row_len))

for (i in seq(1:row_len)) {
  mean_params_ordered$participant_ID[i]       <- params_ordered$ID[params_ordered$subject==i][1]   
  mean_params_ordered$disc_lowprob_prun_theta[i] <- mean(params_ordered$theta[params_ordered$subject==i])
  mean_params_ordered$disc_lowprob_prun_beta[i]  <- mean(params_ordered$beta[params_ordered$subject==i])
  mean_params_ordered$disc_lowprob_prun_kappa[i] <- mean(params_ordered$kappa[params_ordered$subject==i])
  }

####save pre-processed data ####
savedir = ("your_directory")

#csv means
path_C <- paste0(savedir, "/SAT_params_disc_lowprob_prun",".csv", sep="")
write.csv(mean_params_ordered, file=path_C, row.names=F)
