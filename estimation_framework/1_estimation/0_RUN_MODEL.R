# define some basic things (paths, functions, settings, etc.) in the source file
# IMPORTANT: You have to install the R-package "rstudioapi" manually first!

if( (grepl("eu",Sys.info()["nodename"]) & Sys.info()["user"]=="schmidb") == FALSE ){ # Don't do the following on the Euler cluster
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set root in source file location
  getwd()
}


# load path settings, packages, functions and (roger) moore:
source("../Zz_source_code.R")


# ------------------------------------------------------------------------------------------------------------#
#
# ------- GLOBAL SETTINGS: DRAWS, CORES and MORE -------
#
# ------------------------------------------------------------------------------------------------------------#

Ndraws = 0      # 0 draws for mnl

startingkit = 1 # if 0: No starting values based on previous model estimates, if 1: with starting values
manual = 1      # if 1: Define starting values manually in Zz_start_values_manual_....R

modelname <- "1_mnl_pooled"

#modelname <- "2_mnl_pooled_sit_dc"
#modelname <- "2_mnl_pooled_sit_mecm"
#modelname <- "2_mnl_pooled_sit_mecb"

#modelname <- "2_mnl_pooled_sit_dc_pars"
#modelname <- "2_mnl_pooled_sit_mecb_pars"

#modelname <- "2_mnl_pooled_sit_pars_wtp_mult"

#modelname <- "3_mnl_pooled_soz"
#modelname <- "3_mnl_pooled_soz_mecb"
#modelname <- "3_mnl_pooled_soz_pars"

#modelname <- "4_mixl_pooled_normal"
#modelname <- "4_mixl_pooled_lognormal"
#modelname <- "4_mixl_pooled_johnson"

#modelname <- "5_gmnl_type2"

#modelname <- "6_necgmnl"

#modelname <- "7_wtp"
#modelname <- "8_wtp_hybrid"
#modelname <- "9_wtp_hybrid_mobitools"

modelfile = paste0(modelname,"_utilities.R")
source(modelfile)

# define your starting kits

if(startingkit==0){
  file_startingvalues <- "../Zz_start_values_zero.R"
}else{
  if(manual==0){
    file_startingvalues <- "1_mnl_pooled__est.Rdata"
    }else{
    file_startingvalues <- "../Zz_start_values_manual.R"
    #file_startingvalues <- "../Zz_start_values_manual_lognormal.R"
    #file_startingvalues <- "../Zz_start_values_manual_johnson.R"
    #file_startingvalues <- "../Zz_start_values_manual_wtp.R"
  }
}

# get the current time

start <- Sys.time()

# ------------------------------------------------------------------------------------------------------------#
#
# ------- LOAD DATA AND GET STARTED -------
#
# ------------------------------------------------------------------------------------------------------------#

#adjust datapath here
datapath = "~/Documents/ETH Sem 2/Discrete Choice Modeling/DCM_course/estimation_framework/0_data/"

#load alg design
d <- read.csv(paste0(datapath,"alg_design.csv"), header = TRUE, sep=",")

#load choice data
data1 <- read.csv(file = paste0(datapath, "qualtrics_pretest_download.csv"))

#need to get the choices from the choice data and connect to alg design

data1 = data1 %>%
  mutate_all(na_if,"") %>%
  filter(!is.na(gender)) %>%
  filter(!StartDate == "Start Date" & !StartDate == '{"ImportId":"startDate","timeZone":"Europe/Zurich"}') %>%
  mutate(user_id = row_number()) %>%
  dplyr::select(user_id, age, gender, budget, diet, country, Random.ID, starts_with("B"))


#have choices in column
data.long = data1 %>% 
  tidyr::pivot_longer(starts_with("B", ignore.case = FALSE), names_to = "scenario", values_to = "choice") %>%
  filter(!is.na(choice))

#need to make sure they get aligned in the right order when joining to ngene design

d = d %>%
  group_by(Block) %>%
  mutate(scenario = row_number()) %>%
  mutate(scenario = paste0("B", Block, "_Q", scenario))

choiceset = d %>%
  left_join(data.long, by = c("scenario"), multiple = "all")





#need to get the data into a format where the characteristics of the alternatives are a column each, and a column for the choice
#the characteristics for the scenarios can be found in the ngene design

#data$ID <- as.integer(as.factor(data$num_id)) # has to be numeric, starting from 1

choiceset$ID <- as.integer(as.factor(choiceset$user_id))
choiceset <- arrange(choiceset, ID)

choiceset$CHOICE <- as.integer(as.factor(choiceset$choice)) # has to be numeric, starting from 1
choiceset$DIET <- as.integer(as.factor(choiceset$diet)) 
choiceset$GENDER <- as.integer(as.factor(choiceset$gender)) 
choiceset$COUNTRY <- as.integer(as.factor(choiceset$country))  #need to aggregate here somehow
choiceset<- choiceset %>% mutate(
  BUDGET = case_when(budget == "1000-1500 CHF" ~ 1250, 
                     budget == "1500-2000 CHF" ~ 1750, 
                     budget == "2000-2500 CHF" ~ 2250, 
                     budget == "2500-3000 CHF" ~ 2750, 
                     is.na(budget) ~ -99, 
                     TRUE ~ -101389))    #need to check here that all the levels were defined

encodings.diet = choiceset %>% 
  ungroup() %>%
  dplyr::select(diet, DIET) %>%
  distinct(diet, .keep_all = TRUE)

encodings.gender = choiceset %>% 
  ungroup() %>%
  dplyr::select(gender, GENDER) %>%
  distinct(gender, .keep_all = TRUE)
encodings.country = choiceset %>% 
  ungroup() %>%
  dplyr::select(country, COUNTRY) %>%
  distinct(country, .keep_all = TRUE)



N <- length(unique(choiceset$ID))
N

choicetasks <- length(choiceset$ID)
choicetasks

choiceset <- choiceset %>% dplyr::rename(cooking_cost = cooking.cost, 
                         cooking_cooking_time = cooking.cooking_time,
                         cooking_dummy_unhealthy = cooking.dummy_unhealthy, 
                         cooking_dummy_healthy = cooking.dummy_healthy,         
                         cooking_dummy_local_organic = cooking.dummy_local_organic,
                         delivery_cost = delivery.cost, 
                         delivery_waiting_time_home = delivery.waiting_time_home,    
                         delivery_dummy_unhealthy = delivery.dummy_unhealthy,
                         delivery_dummy_healthy = delivery.dummy_healthy,
                         delivery_dummy_local_organic = delivery.dummy_local_organic,
                         restaurant_cost = restaurant.cost,  
                         restaurant_travel_time = restaurant.travel_time, 
                         restaurant_dummy_ts_restaurant = restaurant.dummy_ts_restaurant,
                         restaurant_dummy_unhealthy = restaurant.dummy_unhealthy, 
                         restaurant_dummy_healthy = restaurant.dummy_healthy, 
                         restaurant_dummy_local_organic = restaurant.dummy_local_organic)
choiceset = choiceset %>%
  rename(c = choice)
choiceset = choiceset %>%
  rename(AGE = age)

choiceset = choiceset %>%
  group_by(ID) %>%
  mutate(set = row_number())

#define availabilities: all alternatives always available
choiceset$av_1 <- 1
choiceset$av_2 <- 1
choiceset$av_3 <- 1


choiceset$setid <- c(choiceset$ID + choiceset$set*100000)

choiceset <- choiceset %>%
  dplyr::select(ID, set, setid, av_1, av_2, av_3, CHOICE, c, starts_with("cooking"), starts_with("delivery"), 
                starts_with("restaurant"), AGE, GENDER, BUDGET, DIET, COUNTRY)



# ------------------------------------------------------------------------------------------------------------#
#
# ------- DEFINE AVAILABILITIES -------
#
# ------------------------------------------------------------------------------------------------------------#


availabilities <- as.matrix(choiceset[,c(4:6)])



# ------------------------------------------------------------------------------------------------------------#
#
# ------- NEW: DEFINE WEIGHTS (AT THE OBSERVATION LEVEL) -------
#
# ------------------------------------------------------------------------------------------------------------#

# one has to specify a numeric vector, where the sum of weights in the dataset has to be equal to the number of 
# choice observations. The likelihood is then weighted accordingly (see e.g. Bierlaire, 2003)

weights <- NULL

# weights <- as.vector(ifelse(data$shop == 1, data$shop * 3, 0.3)) # this is just a stupid example!
# weights <- weights/mean(weights)
# 
# summary(weights)
# sum(weights)


# ------------------------------------------------------------------------------------------------------------#
#
# ------- EFFECT CODING and OTHER VARIABLES -------
#
# ------------------------------------------------------------------------------------------------------------#


# make all NAs to this value (important for postestimation)
#we run into 

choiceset$AGE = as.integer(choiceset$AGE)
choiceset[is.na(choiceset)] <- -99



# sex, age, inc, ..... Zero-centered (effect) coding -- WE MAY WANT TO DO THIS? 

c <- wecdummy_cols(choiceset$GENDER)
c$.data <- NULL
choiceset <- cbind(choiceset,c)
choiceset <- choiceset %>% 
  dplyr::rename(
    male_wec = .data_2_wec
  )

table(choiceset$male_wec)
summary(choiceset$male_wec)

# age: mean zero centered
# IF THERE IS A MISSING VALUE IN THE AGE, DOES THAT FACTOR INTO MEAN?

#scale choice set to have mean 1
mean.age = mean(choiceset[choiceset$AGE >0,]$AGE)
choiceset$AGE = choiceset$AGE/mean.age
summary(data$age)

#should add a none of the above to deal with this
#not sure how to handle missing values in these normalized encodings
c <- wecdummy_cols(choiceset$DIET)
c$.data <- NULL
data <- cbind(data,c)
data <- data %>% 
  dplyr::rename(
    urban_wec = .data_1_wec
  )

c <- NULL

#table(data$urban_wec)
#summary(data$urban_wec)

#should scale to mean 1








# ------------------------------------------------------------------------------------------------------------#
#
# ------- MODEL SPECIFICATION AND COMPILATION -------
#
# ------------------------------------------------------------------------------------------------------------#

model_specification <- mixl::specify_model(fast_mixl, choiceset, model_name = modelname)


# ------------------------------------------------------------------------------------------------------------#
#
# ------- CREATE DRAWS -------
#
# ------------------------------------------------------------------------------------------------------------#

if(Ndraws > 0) { # creates pseudo-random draws for MIXL models
  
  # automatically detect dimensions of integral
  
  dimensions <- model_specification$draw_dimensions
  
  # sobol draws (recommended)
  
  draws_matrix <- sobol(N*Ndraws, dim = dimensions, scrambling = 1, seed = 31415926)
  
  # halton draws
  
  # draws_matrix <- halton(N*Ndraws, dim = dimensions)
  
  draws_matrix <- data.table(qnorm(draws_matrix))
  colnames(draws_matrix)=c(paste0("draw_",rep(1:dimensions)))
  draws_matrix <- as.matrix(draws_matrix)
  
}


# ------------------------------------------------------------------------------------------------------------#
#
# ------- STARTINGVALUES -------
#
# ------------------------------------------------------------------------------------------------------------#

if(grepl("Rdata",file_startingvalues)){
  load(paste0(file_startingvalues))
  data_names <- est
}else{
  source(paste0(file_startingvalues))
}

parameters <- getparameters(modelfile)

if(length(setdiff(names(est),parameters))>0){
  est <- (est[!names(est)%in% setdiff(names(est),parameters)])
}

if(length(setdiff(parameters,names(est)))>0){
  newparams <- setdiff(parameters,names(est))
  add <- setNames(rnorm(length(newparams),0,.01),newparams)
  est <- c(est,add)
}


# ------------------------------------------------------------------------------------------------------------#
#
# ------- ESTIMATE MODEL -------
#
# ------------------------------------------------------------------------------------------------------------#

if(Ndraws==0) { # MNL
  model <- mixl::estimate(model_spec = model_specification, start_values = est, data = choiceset, availabilities = availabilities, 
                          weights = weights, nDraws = Ndraws, num_threads = cores, iterlim=10000) }

if(Ndraws>0) { # MIXL
  model <- mixl::estimate(model_spec = model_specification, start_values = est, data = data, availabilities = availabilities,
                          weights = weights, draws = draws_matrix, num_threads = cores, iterlim=10000) }

# estimation output tables and latex tables

mod <- summary(model)
est <- model$estimate

# exclude stuff you don't want to show:

mod$metrics$initLL <- NULL
mod$coefTable$se <- NULL
mod$coefTable$trat_0 <- NULL
mod$coefTable$trat_1 <- NULL
#mod$coefTable$rob_pval0 <- NULL
mod$coefTable$rob_pval1 <- NULL
mod$coefTable$robtrat_1 <- NULL
mod$coefTable$robtrat_0 <- NULL

mod

runlabel <- paste0(modelname,"_",format(Sys.time(),"%Y%m%d_%H%M%S"))
out <- capture.output(mod)
cat(out,file=paste0(runlabel,"__model.txt"),sep="\n",append=TRUE)

# latex files, estimates and (roger) more:

#texreg output is a helpful output file to compare tables in latex for report
invisible(mixl::summary_tex(model, output_file = paste0(latexpath,"ztex_",modelname,"__texmod.Rdata")))
save(est, file = paste0(modelname,"__est.Rdata"))

# save modelfile

save(model, file = paste0(outputpath,modelname,"__mod.Rdata"))


# ------------------------------------------------------------------------------------------------------------#
#
# ------- POSTERIORS (only if _RND is specified) -------
#
# ------------------------------------------------------------------------------------------------------------#

# posterior_data <- aggregate(data[, c('distie','leisure_wec','shop_wec','other_wec')], by=list(ID=data$ID), FUN=mean)

posteriors <- suppressWarnings(as.data.frame(mixl::posteriors(model, indiv_data = NULL))) # only for Parameterfunctions with _RND at the end
id <- c(1:model$Nindividuals)
posteriors <- cbind(id,posteriors)

write.table(posteriors,file=paste0(outputpath,modelname,"__posteriors.csv"),sep=";",row.names=FALSE,quote=FALSE,na=".")

