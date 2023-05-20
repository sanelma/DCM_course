# clean environment

rm(list = ls())

# ------------------------------------------------------------------------------------------------------------#
#
# ------- PACKAGES ------- 
#
# ------------------------------------------------------------------------------------------------------------#

list.of.packages <- c("foreign","plyr","stats","survival",#"devtools",
                      "reshape","reshape2","gtools","psych","tibble",
                      "xtable","texreg","miscTools","zoo","maxLik","mixl",
                      "data.table","plyr","numDeriv","sandwich","randtoolbox",
                      "Rcpp","dplyr","caret","car","Matrix","corrplot","gridExtra",
                      "ggplot2","fastDummies","randomForest","stringr","MASS","here")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages({lapply(list.of.packages, require, character.only = TRUE)})


# ------------------------------------------------------------------------------------------------------------#
#
# ------- CHOOSE YOUR SETTINGS -------
#
# ------------------------------------------------------------------------------------------------------------#

datafolder <-   "0_data/"
subfolder <-    "1_estimation/"
texfolder <-    "1_estimation/Xx_multitable/"
outputfolder <- "2_postestimation/"

parentpath <- paste0(here(),"/")

if(grepl("eu",Sys.info()["nodename"]) & Sys.info()["user"] =="schmidb"){ # Schmid's ETH Euler supercluster account
  #parentpath <- paste0("/cluster/home/schmidb/r-input/005_estimation/")
  cores <- 24
}else if(Sys.info()["nodename"]=="IVT-THKPD-30"){ # Schmid's ETH Laptop
  cores <- 1
}else if(Sys.info()["nodename"]=="IVT-ESP-27"){ # Schmid's ETH Desktop
  cores <- 1
}else if(grepl("staff-net-vpn",Sys.info()["nodename"]) & Sys.info()["user"] =="schmidb"){ # Schmid's personal Macbook
  cores <- 2
}else if(grepl("staff-net-vpn",Sys.info()["nodename"]) & Sys.info()["user"] =="bas"){ # Schmid's personal iMac
  cores <- 4
}else if(grepl("staff-net-vpn",Sys.info()["nodename"]) & Sys.info()["user"] =="bnair"){ # Other personal iMac
  cores <- 2
}else{
  cores <- 1
}


# ------------------------------------------------------------------------------------------------------------#
#
# ------- FURTHER PATH SETTINGS -------
#
# ------------------------------------------------------------------------------------------------------------#

getwd() # current working directory

parentpath <- gsub("1_estimation/","",parentpath) # because using here() function
parentpath <- gsub("estimation_framework/2_postestimation/","",parentpath) # because using here() function


inputpath <-  paste0(parentpath,subfolder)    # functions, starting values, etc.
datapath <-   paste0(parentpath,datafolder)   # data
outputpath <- paste0(parentpath,outputfolder) # postestimation
latexpath <-  paste0(parentpath,texfolder)     # latex output

#set seed: in order to get always the same (random) design

set.seed(3141592) # pi and it's first decimals, should bring you luck ;-)


# ------------------------------------------------------------------------------------------------------------#
#
# ------- USEFUL FUNCTIONS -------
#
# ------------------------------------------------------------------------------------------------------------#

# movement of columns function

move.col <- function(df, move_this, next_to_this, before = FALSE) {
  if (before==FALSE)
    df[,c(match(setdiff(names(df)[1:which(names(df)==next_to_this)],move_this),names(df)),
          match(move_this,names(df)),
          match(setdiff(names(df)[which(names(df)==next_to_this):ncol(df)],c(next_to_this,move_this)),names(df)))]
  else
    df[,c(match(setdiff(names(df)[1:(which(names(df)==next_to_this))],c(next_to_this,move_this)),names(df)),
          match(move_this,names(df)),
          match(setdiff(names(df)[(which(names(df)==next_to_this)):ncol(df)],move_this),names(df)))]
}

# parameter patterns for starting value detection

getparameters <- function(file){
  source(file)
  fast_mixl <- gsub("\n","",gsub("\\+|\\*|\\="," seperate ",fast_mixl))
  fast_mixl <- gsub("\\;"," seperate ",fast_mixl)
  fast_mixl <- gsub("\\,"," seperate ",fast_mixl)
  fast_mixl <- gsub("\\( 1 -","",fast_mixl)
  fast_mixl <- gsub("\\( 1 + ","",fast_mixl)
  fast_mixl <- gsub("\\(1 -","",fast_mixl)
  fast_mixl <- gsub("\\(1 + ","",fast_mixl)
  fast_mixl <- gsub("\\(1-","",fast_mixl)
  fast_mixl <- gsub("\\(1+ ","",fast_mixl)
  fast_mixl <- gsub("\\(","",fast_mixl)
  fast_mixl <- gsub("\\)","",fast_mixl)
  fast_mixl <- gsub("\\/","",fast_mixl)
  fast_mixl <- gsub("exp"," ",fast_mixl)
  fast_mixl <- gsub("pow"," ",fast_mixl)
  fast_mixl <- gsub("\\-"," seperate ",fast_mixl)
  fast_mixl <- unlist(strsplit(fast_mixl,split = c(" seperate ")))
  fast_mixl <- gsub(" ","",fast_mixl)
  parameters <- gsub(pattern = "@","",fast_mixl[grep(pattern = "@",fast_mixl)])
}

# wecdummy_cols function ----

# creates weighted effect coded dummies based on dummy_cols function from fastDummies package
# requires data.table and fastDummies package
# main idea from wec package using base contrasts function and frequency table: https://journal.r-project.org/archive/2017/RJ-2017-017/RJ-2017-017.pdf
# hess: http://www.stephanehess.me.uk/papers/journal%20papers/2016/Daly_Dekker_Hess_JOCM_2016.pdf

wecdummy_cols <- function (.data, select_columns = NULL, remove_first_dummy = TRUE, 
                           remove_most_frequent_dummy = FALSE, ignore_na = FALSE, split = NULL)
{
  stopifnot(is.null(select_columns) || is.character(select_columns), 
            select_columns != "", is.logical(remove_first_dummy), 
            length(remove_first_dummy) == 1)
  if (remove_first_dummy == TRUE & remove_most_frequent_dummy == TRUE) {
    stop("Select either 'remove_first_dummy' or 'remove_most_frequent_dummy'\nto proceed.")
  }
  # convert to data.table
  .data <- data.table::as.data.table(.data)
  if (!is.null(select_columns)) {
    char_cols <- select_columns
    cols_not_in_data <- char_cols[!char_cols %in% names(.data)]
    char_cols <- char_cols[!char_cols %in% cols_not_in_data]
    if (length(char_cols) == 0) {
      stop("select_columns is/are not in data. Please check data and spelling.")
    }
  }
  else if (ncol(.data) == 1) {
    char_cols <- names(.data)
  }
  else {
    char_cols <- sapply(.data, class)
    char_cols <- char_cols[char_cols %in% c("factor", "character")]
    char_cols <- names(char_cols)
  }
  if (length(char_cols) == 0 && is.null(select_columns)) {
    stop(paste0("No character or factor columns found. ", 
                "Please use select_columns to choose columns."))
  }
  if (!is.null(select_columns) && length(cols_not_in_data) > 0) {
    warning(paste0("NOTE: The following select_columns input(s) ", 
                   "is not a column in data.\n"), paste0(names(cols_not_in_data), "\t"))
  }
  # start main loop: get unique values for every variable aka dummy
  for (col_name in char_cols) {
    if (is.factor(.data[[col_name]])) {
      unique_vals <- levels(.data[[col_name]])
      if (any(is.na(.data[[col_name]]))) {
        unique_vals <- c(unique_vals, NA)
      }
    }
    else {
      unique_vals <- sort(unique(.data[[col_name]]))
    }
    unique_vals <- as.character(unique_vals)
    if (!is.null(split)) {
      unique_vals <- unique(trimws(unlist(strsplit(unique_vals, split = split))))
    }
    if (ignore_na) {
      unique_vals <- unique_vals[!is.na(unique_vals)]
    }
    if (remove_most_frequent_dummy) {
      vals <- as.character(.data[[col_name]])
      vals <- data.frame(sort(table(vals), decreasing = TRUE), stringsAsFactors = FALSE)
      if (vals$Freq[1] > vals$Freq[2]) {
        vals <- as.character(vals$vals[2:nrow(vals)])
        unique_vals <- unique_vals[which(unique_vals %in% vals)]
        unique_vals <- vals[order(match(vals, unique_vals))]
      }
      else {
        remove_first_dummy <- TRUE
      }
    }
    if (remove_first_dummy) {
      # works easiest with removing first dummy and factor! takes the first letter, otherwise relevel first
      base_unique_value <- unique_vals[1]
      unique_vals <- unique_vals[-1]
    }
    # create a frequency table and store names
    freqss <- as.vector(table(.data[[col_name]]))
    names(freqss) <- names(table(.data[[col_name]]))
    # does the weighting based base unique value
    freqss_w <- -1 * freqss[-1]/freqss[1]
    names(freqss_w) <- names(freqss)[-1]
    data.table::alloc.col(.data, ncol(.data) + length(unique_vals))
    data.table::set(.data, j = paste0(col_name, "_", unique_vals, "_wec"), value = 0)
    # second loop for every unique value in unique values
    for (unique_value in unique_vals) {
      # paste 1 where character level matches to the unique value
      data.table::set(.data,
                      i = which(data.table::chmatch(as.character(.data[[col_name]]), unique_value, nomatch = 0) == 1),
                      j = paste0(col_name, "_", unique_value, "_wec"),
                      value = 1)
      # paste weighted value where character level matches to reference level
      data.table::set(.data,
                      i = which(data.table::chmatch(as.character(.data[[col_name]]), base_unique_value, nomatch = 0) == 1),
                      j = paste0(col_name, "_", unique_value, "_wec"),
                      value = freqss_w[unique_value])
      if (!is.na(unique_value)) {
        data.table::set(.data,
                        i = which(is.na(.data[[col_name]])), 
                        j = paste0(col_name, "_", unique_value, "_wec"),
                        value = NA)
      }
      # dont know what this is doing right now, dont use it!
      if (!is.null(split)) {
        max_split_length <- max(sapply(strsplit(as.character(.data[[col_name]]), split = split), length))
        for (split_length in 1:max_split_length) {
          data.table::set(.data, i = which(data.table::chmatch(as.character(trimws(sapply(strsplit(as.character(.data[[col_name]]), 
                                                                                                   split = split), `[`, split_length))), unique_value, nomatch = 0) == 1),
                          j = paste0(col_name, "_", unique_value), value = 1)
        }
        if (is.na(unique_value)) {
          .data[[paste0(col_name, "_", unique_value)]][which(!is.na(.data[[col_name]]))] <- 0
        }
      }
    }
  }
  return(.data)
}


# ------------------------------------------------------------------------------------------------------------#
#
# ------- DELTA METHOD FOR STANDARD ERRORS -------
#
# ------------------------------------------------------------------------------------------------------------#

# STANDARD FUNCTIONS

deltamethod=function(par1,par2)
{
  #
  v1=est[par1]+est[par2]
  se1=sqrt(robvarcov[par1,par1]+robvarcov[par2,par2]+2*robvarcov[par1,par2])
  t1=round(v1/se1,2)
  #
  v2=est[par1]-est[par2]
  se2=sqrt(robvarcov[par1,par1]+robvarcov[par2,par2]-2*robvarcov[par1,par2])
  t2=round(v2/se2,2)
  #
  v3=est[par1]*est[par2]
  se3=sqrt(est[par2]^2*robvarcov[par1,par1]+est[par1]^2*robvarcov[par2,par2]+
             2*est[par1]*est[par2]*robvarcov[par1,par2])
  t3=round(v3/se3,2)
  #
  v4=est[par1]/est[par2]
  se4=sqrt(v4^2*(robvarcov[par1,par1]/(est[par1]^2)+robvarcov[par2,par2]/
                   (est[par2]^2)-2*robvarcov[par1,par2]/(est[par1]*est[par2])))
  t4=round(v4/se4,2)
  #
  function_value=round(c(v1,v2,v3,v4),2)
  function_se=round(c(se1,se2,se3,se4),2)
  function_t=c(t1,t2,t3,t4)
  #
  delta_output=cbind(function_value,function_se,function_t)
  rownames(delta_output)=c(paste(cbind(par1,"+",par2),collapse=""),
                           paste(cbind(par1,"-",par2),collapse=""),
                           paste(cbind(par1,"*",par2),collapse=""),
                           paste(cbind(par1,"/",par2),collapse=""))
  #
  return(delta_output)
}

# RATIO of SUM OF 2 in Numerator and 1 in Denominator

deltasumnum=function(par1,par2,par3)
{
  #
  value = (est[par1]+est[par2])/est[par3]
  
  phi1 =  1/est[par3]
  phi2 =  1/est[par3]
  phi3 = -(est[par1] + est[par2])/est[par3]^2
  
  se = sqrt(phi1^2*robvarcov[par1,par1] + phi2^2*robvarcov[par2,par2] + phi3^2*robvarcov[par3,par3] +
              2*phi1*phi2*robvarcov[par1,par2] + 2*phi1*phi3*robvarcov[par1,par3] + 2*phi2*phi3*robvarcov[par2,par3])
  
  t = round(value/se,2)
  
  function_value=round(c(value),2)
  function_se=round(c(se),2)
  function_t=c(t)
  #
  delta_output=cbind(function_value,function_se,function_t)
  rownames(delta_output)=c(paste(cbind("(",par1,"+",par2,")/",par3),collapse=""))
  #
  return(delta_output)
}

# RATIO of SUM OF 2 in Numerator and SUM of 2 in Denominator

interdelta=function(par1,par2,par3,par4)
{
  #
  value = (est[par1]+est[par2])/(est[par3]+est[par4])*60
  
  phi1 =  1/(est[par3]+est[par4])
  phi2 =  1/(est[par3]+est[par4])
  phi3 = -(est[par1] + est[par2])/(est[par3]+est[par4])^2
  phi4 = -(est[par1] + est[par2])/(est[par3]+est[par4])^2
  
  se = sqrt(phi1^2*robvarcov[par1,par1] + phi2^2*robvarcov[par2,par2] + phi3^2*robvarcov[par3,par3] + phi4^2*robvarcov[par4,par4] 
            + 2*phi1*phi2*robvarcov[par1,par2] + 2*phi1*phi3*robvarcov[par1,par3] + 2*phi1*phi4*robvarcov[par1,par4] 
            + 2*phi2*phi3*robvarcov[par2,par3] + 2*phi2*phi4*robvarcov[par2,par4] + 2*phi3*phi4*robvarcov[par3,par4] )*60
  
  t = round(value/se,2)
  
  function_value=round(c(value),2)
  function_se=round(c(se),2)
  function_t=c(t)
  #
  delta_output=cbind(function_value,function_se,function_t)
  rownames(delta_output)=c(paste(cbind("(",par1,"+",par2,")/(",par3,"+",par4,")"),collapse=""))
  #
  return(delta_output)
}

# MULTIPLICATION of first 2 and ADDING 1 (e.g. total effects in HCM)

deltatotal=function(par1,par2,par3)
{
  #
  value = est[par1]*est[par2] + est[par3]
  
  phi1 =  est[par2]
  phi2 =  est[par1]
  phi3 =  1
  
  se = sqrt(phi1^2*robvarcov[par1,par1] + phi2^2*robvarcov[par2,par2] + phi3^2*robvarcov[par3,par3] +
              2*phi1*phi2*robvarcov[par1,par2] + 2*phi1*phi3*robvarcov[par1,par3] + 2*phi2*phi3*robvarcov[par2,par3])
  
  t = round(value/se,2)
  
  function_value=round(c(value),2)
  function_se=round(c(se),2)
  function_t=c(t)
  #
  delta_output=cbind(function_value,function_se,function_t)
  rownames(delta_output)=c(paste(cbind(par1,"*",par2,"+",par3),collapse=""))
  #
  return(delta_output)
}

# 3 choice dimension total effects (Widmer et al., 2020)

delta3dimtotal =function(par1,par2,par3,par4,par5,par6)
{
  #
  value = est[par1] + est[par2]*est[par3] + est[par4]*est[par5]*est[par3] + est[par4]*est[par6]
  
  phi1 =  1
  phi2 =  est[par3]
  phi3 =  est[par2] + est[par4]*est[par5]
  phi4 =  est[par5]*est[par3] + est[par6]
  phi5 =  est[par4]*est[par3]
  phi6 =  est[par4]
  
  se = sqrt(phi1^2*robvarcov[par1,par1] + phi2^2*robvarcov[par2,par2] + phi3^2*robvarcov[par3,par3] +
              phi4^2*robvarcov[par4,par4] + phi5^2*robvarcov[par5,par5] + phi6^2*robvarcov[par6,par6] +
              
              2*phi1*phi2*robvarcov[par1,par2] + 2*phi1*phi3*robvarcov[par1,par3] + 2*phi1*phi4*robvarcov[par1,par4] +
              2*phi1*phi5*robvarcov[par1,par5] + 2*phi1*phi6*robvarcov[par1,par6] + 2*phi2*phi3*robvarcov[par2,par3] + 
              2*phi2*phi4*robvarcov[par2,par4] + 2*phi2*phi5*robvarcov[par2,par5] + 2*phi2*phi6*robvarcov[par2,par6] + 
              2*phi3*phi4*robvarcov[par3,par4] + 2*phi3*phi5*robvarcov[par3,par5] + 2*phi3*phi6*robvarcov[par3,par6] + 
              2*phi4*phi5*robvarcov[par4,par5] + 2*phi4*phi6*robvarcov[par4,par6] + 2*phi5*phi6*robvarcov[par5,par6] )
  
  t = round(value/se,2)
  
  function_value=round(c(value),2)
  function_se=round(c(se),2)
  function_t=c(t)
  #
  delta_output=cbind(function_value,function_se,function_t)
  rownames(delta_output)=c(paste(cbind(par1,"+",par2,"*",par3,"+",par4,"*",par5,"*",par3,"+",par4,"*",par6),collapse=""))
  #
  return(delta_output)
}

# COLESKY TIME-USE ERROR COMPONENTS

deltachole=function(par1,par2,par3,par4,par5,par6)
{
  #
  
  psi = est[par1]^2 + est[par3]^2 + est[par4]^2 + est[par5]^2 + est[par6]^2
  
  value = ( est[par1]*est[par2] ) / ( est[par2]^2 * psi )^(1/2)
  
  phi1 =  ( (est[par2]^2 * psi)^(1/2) * est[par2] - est[par1]*est[par2]*1/2*(est[par2]^2 * psi)^(-1/2) * 2*est[par2]^2 * est[par1] ) /
    ( est[par2]^2 * psi )
  
  phi2 =  ( (est[par2]^2 * psi)^(1/2) * est[par1] - est[par1]*est[par2]*1/2*(est[par2]^2 * psi)^(-1/2) * 2*est[par2]*psi ) /
    ( est[par2]^2 * psi )
  
  phi3 =  ( 0 - est[par1]*est[par2]*1/2*(est[par2]^2 * psi)^(-1/2) * 2*est[par2]^2 * est[par3] ) / 
    ( est[par2]^2 * psi )
  
  phi4 =  ( 0 - est[par1]*est[par2]*1/2*(est[par2]^2 * psi)^(-1/2) * 2*est[par2]^2 * est[par4] ) / 
    ( est[par2]^2 * psi )
  
  phi5 =  ( 0 - est[par1]*est[par2]*1/2*(est[par2]^2 * psi)^(-1/2) * 2*est[par2]^2 * est[par5] ) / 
    ( est[par2]^2 * psi )
  
  phi6 =  ( 0 - est[par1]*est[par2]*1/2*(est[par2]^2 * psi)^(-1/2) * 2*est[par2]^2 * est[par6] ) / 
    ( est[par2]^2 * psi )
  
  
  se = sqrt(phi1^2*robvarcov[par1,par1] + phi2^2*robvarcov[par2,par2] + phi3^2*robvarcov[par3,par3] +
              phi4^2*robvarcov[par4,par4] + phi5^2*robvarcov[par5,par5] + phi6^2*robvarcov[par6,par6] +
              
              2*phi1*phi2*robvarcov[par1,par2] + 2*phi1*phi3*robvarcov[par1,par3] + 2*phi1*phi4*robvarcov[par1,par4] +
              2*phi1*phi5*robvarcov[par1,par5] + 2*phi1*phi6*robvarcov[par1,par6] + 2*phi2*phi3*robvarcov[par2,par3] + 
              2*phi2*phi4*robvarcov[par2,par4] + 2*phi2*phi5*robvarcov[par2,par5] + 2*phi2*phi6*robvarcov[par2,par6] + 
              2*phi3*phi4*robvarcov[par3,par4] + 2*phi3*phi5*robvarcov[par3,par5] + 2*phi3*phi6*robvarcov[par3,par6] + 
              2*phi4*phi5*robvarcov[par4,par5] + 2*phi4*phi6*robvarcov[par4,par6] + 2*phi5*phi6*robvarcov[par5,par6] )
  
  t = round(value/se,2)
  
  function_value=round(c(value),2)
  function_se=round(c(se),2)
  function_t=c(t)
  #
  delta_output=cbind(function_value,function_se,function_t)
  rownames(delta_output)=c(paste(cbind("(",par1,"*",par2,")/(",par2,"^2 * VAR(PSI))^(1/2)"),collapse=""))
  #
  return(delta_output)
}

