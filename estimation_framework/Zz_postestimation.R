
# ------------------------------------------------------------------------------------------------------------#
#
# ------- THE RUN_ANALYSIS FUNCTION ------- 
#
# ------------------------------------------------------------------------------------------------------------#

# start the function here:

run_analysis <- function(model,dat,outputpath,modelname){ ### start run_analysis
  
  standardelement <- element_text(size = 20, colour = "black") # some GGPlot graph settings
  
  outputpath <<- outputpath # define customized outputpath
  
  cols <- model$model_spec$data_cols # get relevant columns
  model <- model # get model file
  
  lengthalt <<- length(alternatives) # number of alternatives in model
  
  # needs to be done again to compile Rcpp
  
  model_specification <- mixl::specify_model(model$model_spec$source, dat, model_name = modelname)
  
  # Probability for each observation and alternative
  
  baseprob <- suppressWarnings(mixl::probabilities(model = model,data = dat,availabilities = availabilities, num_threads = cores))
  
  # list with log-likelihood by ID
  
  prob_choice <- as.data.frame(baseprob[,c("ID","p_choice")])
  
  prob_choice <-
    prob_choice %>%
    group_by(ID) %>%
    mutate(loglike = sum(log(p_choice))) %>%
    ungroup
  
  prob_choice <-
    prob_choice %>%
    group_by(ID) %>%
    mutate(task = row_number()) %>%
    ungroup
  
  prob_choice <- prob_choice[prob_choice$task == 1,]
  
  prob_choice$p_choice <- NULL
  prob_choice$task <- NULL
  
  fwrite(x = data.frame(prob_choice),
         file = paste0(outputpath,modelname,"_loglike_by_ID.csv"),sep=";",row.names = F)
  
  # Plot choice frequencies
  
  dat$CHOICE <- as.integer(as.factor(dat$CHOICE))
  
  dat$choice_lab <- factor(dat$CHOICE, levels = c(1:lengthalt), labels = alternatives_names)
  
  p <- ggplot(dat, aes(x = choice_lab)) + geom_bar(aes(y = (..count..)/sum(..count..)*100)) +
    xlab("Alternative") + theme_minimal() + ylab("Relative frequency [%]") + theme(text = standardelement) +
    theme(axis.text.x = element_text(angle = 45))
  
  ggsave(filename = paste0(outputpath,"000_alt_choice_frequencies.png"), device = "png",plot = p,width = 20,height = 15,units = "cm")
  
  # Number of choices per ID
  
  tab <- table(dat[,"ID"])
  tab <- as.data.frame(tab[tab<100])
  summary(tab$Freq)
  
  p <- ggplot(tab, aes( x = Freq, y = (..count..)/sum(..count..)*100 ))+ 
    xlab("Number of choices per ID") + theme_minimal() + 
    ylab("Density [%]") + theme(text = standardelement) + 
    stat_bin(binwidth = 2) + geom_bar()
  
  ggsave(filename = paste0(outputpath,"000_choices_per_ID.png"), device = "png",plot = p,width = 20,height = 15,units = "cm")
  
  # 1% of IDs with lowes panel choice probabilities (outliers)
  
  probperID <- aggregate(baseprob[,"p_choice"],by=list(baseprob[,"ID"]),
                         FUN= function(x) c(Avg_P_Choice = mean(x), NrObs = length(x) ))
  
  probperID <- do.call(data.frame,probperID)
  
  colnames(probperID) <- c("ID","Avg_P_Choice","NrObs")
  
  probperID$ID <- probperID$ID + 1 # since after estimation, the ID is starting from zero
  
  #probperID_thresh <- probperID[probperID$NrObs >= 1,]
  
  generateOutput(probperID_input = probperID,dat = dat,dataname = "worst_probs_total")
  
  
  # prediction accuarcy and 95% confidence interval
  
  library(foreach)
  
  fn<-function(i) {
    set.seed(i)
    
    est_new <- mvrnorm(n = 1, est, robvarcov, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
    model$estimate <- est_new
    baseprob <- suppressWarnings(mixl::probabilities(model = model,data = dat,availabilities = availabilities, num_threads = cores))
    
    baseprob <- cbind(baseprob,Predict=apply(baseprob[,c(5:(lengthalt+4))],1,
                                             FUN=function(x)sample(c(1:lengthalt),size=1,prob=x)))
    
    y <- as.vector(round(sum(as.numeric(baseprob[,"choice_index"]==baseprob[,"Predict"]))/nrow(baseprob)*100,2))
    
    return(y)
  }
  
  x <- foreach(i=1:bootn) %do% fn(i)
  x <- as.data.frame(do.call(rbind.data.frame, x))
  
  names(x)[1] <- c("m11")
  
  h <- mean(x$m11)
  
  lh <- quantile(x$m11, c(0.025))
  uh <- quantile(x$m11, c(0.975))
  
  boots <- c(lh,h,uh)
  boots <- round(boots, digits = 2)
  names(boots)[c(1:3)] <- c("lh","h","uh")
  
  boots <- as.data.frame(boots)
  #print(boots)
  
  fwrite(x = data.frame(boots),file = paste0(outputpath,
                                             modelname,"_95ci.csv"),sep=";",row.names = T)
  

  # Calculation of MPEs and elasticities
  
  print("Take a break. This may take a while...")
  
  generateMPE(model = model,dat = dat,availabilities = availabilities,
              outputpath = outputpath,baseprob = baseprob)
  
  # tryCatch-function for insignificant parameters

  tryCatch({
    insignificant_params(model = model,outputpath = outputpath)
  }, warning = function(w) {
  }, error = function(e) {
  }, finally = {
  })
 
  # Confusion matrices and hitrates
  
  randprob <- jitter(baseprob[,c(5:(lengthalt+4))], factor = 0.00001, amount = NULL) # add very very small amount of noise
  
  Predict_highest <- apply(X = randprob, MARGIN = 1,FUN = function(x) which( x[1:lengthalt]==max(x) ) )
  
  Predict_highest <- ldply(Predict_highest, data.frame)
  names(Predict_highest) <- "Predict_highest"
  
  baseprob <- cbind(baseprob,Predict_highest)
  baseprob <- cbind(baseprob,Predict=apply(baseprob[,c(5:(lengthalt+4))],1,
                                           FUN=function(x)sample(c(1:lengthalt),size=1,prob=x))) # for economist method
  
  confusionmatrix_base <- round(table(baseprob[,"choice_index"],
                                      baseprob[,"Predict"])/nrow(baseprob)*100,2)
  hitrate_econ <- sum(diag(confusionmatrix_base))
  conf_econ <- generateConfusion(confusionmatrix_base = confusionmatrix_base,
                                 version = "econ",outputpath = outputpath)
  
  confusionmatrix_base <- round(table(baseprob[,"choice_index"],
                                      baseprob[,"Predict_highest"])/nrow(baseprob)*100,2)
  hitrate_opt <- sum(diag(confusionmatrix_base))
  conf_opt<-generateConfusion(confusionmatrix_base = confusionmatrix_base,
                              version = "opt",outputpath = outputpath)
  
  hitrate <- as.data.frame(cbind(hitrate_econ,hitrate_opt))
  fwrite(x = data.frame(hitrate),file = paste0(outputpath,
                                               modelname,"_hitrates.csv"),sep=";",row.names = F)
  
  print( paste0( "Hitrate (probabilistic) [%]: ", round( h, digits=2) ) )
  
  # store data for susequent functions
  
  dat <- as.data.frame(dat)
  cleannames <- gsub( '\\$', '', names(model$model_spec$data_cols ))
  subdat <- dat[cleannames]
  
  dat <- setDF(cbind(subdat,P_Choice=baseprob[,"p_choice"]))
  
  # partworth function
  
  partworth_analysis(dat = dat,outputpath = outputpath,standardelement = standardelement)
  
  print("The calculation is over.")
  
} ### end run_analysis


# ------------------------------------------------------------------------------------------------------------#
#
# ------- INPUT FUNCTIONS -------
#
# ------------------------------------------------------------------------------------------------------------#


###################
# GENERATE OUTPUT #
###################

generateOutput <- function(dat,probperID_input,dataname){
  
  probperID <- probperID_input
  probperID <- probperID[order(probperID$Avg_P_Choice,decreasing = F),]
  probperID <- probperID[1:10,] # 10 IDs with lowest choice probabilities
  probperID <- probperID[,c("ID","Avg_P_Choice","NrObs")]
  probperID$Avg_P_Choice <- round(probperID$Avg_P_Choice,3)
  
  fwrite(x = probperID,file =  paste0(outputpath,modelname,"_",
                                      dataname,".csv"),sep=";")
}


##############
# MPEs ELAST #
##############

increase_factor <- 1.00001 # increase factor to obtain arc-elasticities

generateMPE <- function(model,dat,availabilities,outputpath,baseprob){
  
  res_perc <- matrix(ncol=model$model_spec$num_utility_functions)
  res_bin <- matrix(ncol=model$model_spec$num_utility_functions)
  
  cols <- model$model_spec$data_cols

  for(col in cols){
    
    #col <- "tt_b_rp" # test variable
    
    tempdat <- dat
    tempbaseprob <- baseprob
    
    # Identifies discrete (incl. weighted effect coded) variables with 3 or less levels
    
    bin_cond <- length( unique( tempdat[[col]][ tempdat[[col]] > -97 & tempdat[[col]] < 9999 ] ) ) <= 3 & max(tempdat[[col]]) == 1
    
    if(col == "dini_mueter"){ # special cases to be defined here
      bin_cond <- TRUE
    }
    
    if(bin_cond){
      
      if(length( unique( tempdat[[col]][ tempdat[[col]] > -97 & tempdat[[col]] < 9999 ] ) ) <= 2){ # recognize if (zero-centered) dummy coded
        
      tempdat[[col]] <- min(dat[[col]][dat[[col]]> -97 & dat[[col]] < 9999])
      minvalue <- min(dat[[col]][dat[[col]]> -97 & dat[[col]] < 9999])
      
      tempbaseprob <- mixl::probabilities(model = model,
                                          data = tempdat,
                                          availabilities = availabilities,num_threads = cores)
      
      tempdat[[col]] <- max(dat[[col]][dat[[col]]> -97 & dat[[col]] < 9999])
      maxvalue <- max(dat[[col]][dat[[col]]> -97 & dat[[col]] < 9999])
      
      change_x <- 1
      change_x_rel <- change_x / ((0 + 1)/2)
      
        }else{ # recognize if weighted effect coded with 3 or more categories; change in P relative to average (and not base category)
        
        tempdat[[col]] <- 0
        minvalue <- 0
        
        tempbaseprob <- mixl::probabilities(model = model,
                                            data = tempdat,
                                            availabilities = availabilities,num_threads = cores)
        
        tempdat[[col]] <- max(dat[[col]][dat[[col]]> -97 & dat[[col]] < 9999])
        maxvalue <- max(dat[[col]][dat[[col]]> -97 & dat[[col]] < 9999])
        
        change_x <- 1
        change_x_rel <- change_x / ((0 + 1)/2)
        
        }
      
      }else{ # for continuous variables
      
      minvalue <- mean(tempdat[[col]])
      
      tempdat[[col]] <- tempdat[[col]]*increase_factor
      
      maxvalue <- mean(tempdat[[col]])
      
      change_x <- maxvalue - minvalue
      change_x_rel <- change_x / ((maxvalue + minvalue)/2)
      
    }
    
    tempprob <- mixl::probabilities(model = model,data = tempdat,
                                    availabilities = availabilities,num_threads = cores)
    
    before <- apply( ( tempbaseprob[,c(5:(4+lengthalt))] ), 2, mean ) # before the change
    
    after <- apply( ( tempprob[,c(5:(4+lengthalt))] ), 2, mean ) # after the change
    
    change_mpe <- (after - before)*100

    change <- after - before
    change_rel <- change / ((after + before)/2)
    
    change_elast <- change_rel / change_x_rel
    
    if(bin_cond){
      res_bin <- rbind(res_bin,change_mpe)
      row.names(res_bin)[nrow(res_bin)] <- col
    }else{
      res_perc <- rbind(res_perc,change_elast)
      row.names(res_perc)[nrow(res_perc)] <- col
    }
  }
  
  res_bin_dis <- res_bin
  res_perc_dis <- res_perc
  
  # Nicer presentation
  
  adjres <- function(res){
    res <- round(res,2)
    colnames(res) <- paste0(alternatives)
    res <- res[order(row.names(res)),]
  }
  
  if(nrow(res_perc_dis)>1){
    res_perc_dis <- adjres(res_perc_dis)
    fwrite(x = data.frame(res_perc_dis),
           file = paste0(outputpath,modelname,"_change_perc_ELAST.csv"),sep=";",row.names = T)
    }
  
    if(nrow(res_bin_dis)>1){
      res_bin_dis <- adjres(res_bin_dis)
      fwrite(x = data.frame(res_bin_dis),
             file = paste0(outputpath,modelname,"_change_bin_MPE.csv"),sep=";",row.names = T)
    }
    
}


##############
# INSIG PARS #
##############

insignificant_params <- function(model, outputpath){
  
  varcov=vcov(model)
  
  est=model$estimate
  meat1=meat(model)
  bread1=bread(model)
  meat1[is.na(meat1)]=0
  bread1[is.na(bread1)]=0
  robvarcov=sandwich(model,bread1,meat1)
  robse=sqrt(diag(robvarcov))
  robtrat_0=est/robse
  
  insign <- data.frame(robtrat_0[abs(robtrat_0)<1.96])
  names(insign) <- "rob_tvalue"
  
  fwrite(x = data.frame(insign),file = paste0(outputpath,modelname,"_insig_pars.csv"),sep=";",row.names = T)
  
}


####################
# CONFUSION MATRIX #
####################

generateConfusion <- function(confusionmatrix_base,version,outputpath){
  confusionmatrix_dis <- as.data.frame(matrix(confusionmatrix_base,
                                              nrow=lengthalt,
                                              dimnames = list(alternatives_names,alternatives_names)))
  row.names(confusionmatrix_dis) <- paste0(alternatives_names)
  colnames(confusionmatrix_dis) <- paste0(alternatives_names)
  confusionmatrix_dis <- rbind(confusionmatrix_dis,as.numeric(c(apply(confusionmatrix_dis,2,sum))))
  confusionmatrix_dis <- cbind(confusionmatrix_dis,as.numeric(c(apply(confusionmatrix_dis,1,sum))))
  colnames(confusionmatrix_dis)[ncol(confusionmatrix_dis)] <- "Sum"
  row.names(confusionmatrix_dis)[nrow(confusionmatrix_dis)] <- "Sum"
  
  fwrite(x = data.frame(confusionmatrix_dis),file = 
           paste0(outputpath,modelname,"_confusion_",version,".csv"),row.names = T,sep=";")
  
  return(confusionmatrix_dis)
}


######################
# PARTWORTH ANALYSIS #
######################

partworth_analysis <- function(dat,outputpath,standardelement){
  
  pathw_dat <- setDF(dat)
  
  for (i in names(pathw_dat)) {
    pathw_dat[i][pathw_dat[i] <= -97]  <- NA
    pathw_dat[i][pathw_dat[i] >= 9999]  <- NA
    # recode effect coded variables into dummy variables (set min to zero)
    # be careful: beta's have to be retransformed into dummy effects as well, such that the partworth analysis works in its current form!
    # zero-centered dummy coding is elegant, no recoding necessary anyway
    pathw_dat[i][pathw_dat[i] > -97 & pathw_dat[i] < 0] <- pathw_dat[i][pathw_dat[i] >= -97 &
                                                                         pathw_dat[i] < 0] - min(pathw_dat[i])
  }
  
  pathw_dat <- pathw_dat[,!colnames(pathw_dat)%in%c("P_Choice")]
  
  meandat <- apply(pathw_dat,2,mean,na.rm=TRUE)
  
  
  # generate dataframe
  
  generate_data_frame <- function(x){
    res <- data.frame(names(x),x,row.names = NULL)
    res[,1] <- as.character(res[,1])
    return(res)
  }
  
  meandat <- generate_data_frame(meandat)
  colnames(meandat) <- c("name","mean")
  meandat <- meandat[order(meandat$mean,decreasing = T),]
  
  fwrite(x = meandat,file =  paste0(outputpath,modelname,"_mean_vars.csv"),sep=";",row.names = F)
  
}

