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
#starting values not an issue for MNL, but needed for more complex mixed models. 
#can set starting values as beta estimates from an MNL model. 


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
data1 <- read.csv(file = paste0(datapath, "qualtrics_download2023_19.14.csv"))

# precleaning 
data1 <- data1 %>% 
  mutate_all(na_if,"") %>%
  filter(!is.na(gender)) %>%
  filter(!StartDate == "Start Date" & !StartDate == '{"ImportId":"startDate","timeZone":"Europe/Zurich"}') %>%
  rename(cooking.frequency = Q31_1, 
                          delivery.frequency = Q31_2, 
                          eatout.frequency = Q31_3) %>%
  mutate(user_id = row_number()) %>%
  # add a dummy variable for the various dietary preferences
  mutate(diet.vegetarian = ifelse(grepl("Vegetarian", diet), 1, 0), 
         diet.vegan = ifelse(grepl("Vegan", diet), 1, 0), 
         diet.lowcal = ifelse(grepl("Low calorie", diet), 1, 0),
         diet.highcal = ifelse(grepl("High calorie", diet), 1, 0), 
         diet.allergies = ifelse(grepl("allergies", diet), 1, 0))

# get data on demographics for descriptive statistics on our sample
demographics = data1 %>% 
  dplyr::select(user_id, cooking.frequency, delivery.frequency, eatout.frequency, 
         age, gender, budget, starts_with("diet"))



# select data for models (choices and demographics): will connect to alg design 
# pivot longer to have one row per choice scenario
data.long = data1 %>%
  dplyr::select(user_id, age, gender, budget, 
                diet.vegetarian, diet.vegan, diet.lowcal, diet.highcal, diet.allergies,
                Random.ID, starts_with("B")) %>%
  tidyr::pivot_longer(starts_with("B", ignore.case = FALSE), 
                      names_to = "scenario", 
                      values_to = "choice") %>%
  filter(!is.na(choice))



#need to make sure they get aligned in the right order when joining to ngene design

# getting choiceset names from ngene design
d = d %>%
  group_by(Block) %>%
  mutate(scenario = row_number()) %>%
  mutate(scenario = paste0("B", Block, "_Q", scenario))

# joining ngene design to people's actual decisions. Call it "choiceset"
choiceset = d %>%
  left_join(data.long, by = c("scenario"), multiple = "all")





#need to get the data into a format where the characteristics of the alternatives are a column each, and a column for the choice
#the characteristics for the scenarios can be found in the ngene design

#data$ID <- as.integer(as.factor(data$num_id)) # has to be numeric, starting from 1

# creating numeric variables
choiceset$ID <- as.integer(as.factor(choiceset$user_id))
choiceset <- arrange(choiceset, ID)

choiceset$CHOICE <- as.integer(as.factor(choiceset$choice)) # has to be numeric, starting from 1
choiceset$GENDER <- as.integer(as.factor(choiceset$gender)) 
choiceset$AGE <- as.integer(as.factor(choiceset$age)) 
choiceset<- choiceset %>% mutate(
  BUDGET = case_when(budget == "< 1000 CHF" ~ 900,  # no midpoint here - not sure what is most reasonable budget to impute
                     budget == "1000-1500 CHF" ~ 1250, 
                     budget == "1500-2000 CHF" ~ 1750, 
                     budget == "2000-2500 CHF" ~ 2250, 
                     budget == "2500-3000 CHF" ~ 2750, 
                     budget == "> 3000 CHF" ~ 3500, #again, no midpoint to impute. 
                     is.na(budget) ~ -99, 
                     TRUE ~ -101389))    #all levels succesfully defined



encodings.gender = choiceset %>% 
  ungroup() %>%
  dplyr::select(gender, GENDER) %>%
  distinct(gender, .keep_all = TRUE)


choiceset = choiceset %>%
  dplyr::select(-age, -gender, - budget)

# number of participants
N <- length(unique(choiceset$ID))
N

# number of choice scenarios
choicetasks <- length(choiceset$ID)
choicetasks

# renaming to be mixl compatible
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
  group_by(ID) %>%
  mutate(set = row_number())

#define availabilities: all alternatives always available
choiceset$av_1 <- 1
choiceset$av_2 <- 1
choiceset$av_3 <- 1


choiceset$setid <- c(choiceset$ID + choiceset$set*100000)

choiceset <- choiceset %>%
  dplyr::select(ID, set, setid, av_1, av_2, av_3, CHOICE, c, starts_with("cooking"), starts_with("delivery"), 
                starts_with("restaurant"), AGE, GENDER, BUDGET, diet.vegetarian, diet.vegan, diet.lowcal, diet.highcal, diet.allergies)



# ------------------------------------------------------------------------------------------------------------#
#
# ------- DEFINE AVAILABILITIES -------
#
# ------------------------------------------------------------------------------------------------------------#

# define availabilities. Make sure you pull the right columns, if the df gets edited
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


# should think about best way to include variables
# for example, maybe age groups makes more sense than age as a continuous variable

# good to normalize by dividing interaction effects by the mean - helps with continuous interaction effects (then we multiply by one for something at the mean)


# make all NAs to this value (important for postestimation)
#we run into 

choiceset$AGE = as.integer(choiceset$AGE)
choiceset[is.na(choiceset)] <- -99



# dummy encodings of categorical variables - should figure out what to do with this


c <- NULL

c <- dummy_cols(data$purp_cat)
c$.data <- NULL
c$.data_1 <- NULL
data <- cbind(data,c)
data <- data %>%
  dplyr::rename(
    leisure_dc = .data_2,
    shop_dc = .data_3,
    other_dc = .data_4
  )



# normalize continuous variables to mean 1

# if we want to include interaction effect in a power function, should normalize by mean.
# but, if only want to include as an interaction on the constants or something w/out power function, then no need
# maybe best to just normalize by mean, then no risk

data[, age := age/48 ]
summary(data$age)






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
  
  draws_matrix <- data.table(qnorm(draws_matrix)) #qnorm generates normal distribution. can change to qunif() for uniform draws (should not be used for random constants, only )
  #so would need to define some draws normal and some uniform - normal for random ASC, uniform for _RND for taste heterogeneity
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
# we want to test all parameters against zero (also the scales, since they are in an exponent)

# run models first with all variables, and then exclude things that aren't significant are run again

# should balance measure of fit to choose model:
# can use AICC to compare models. The smaller the better
# BIC is more conservative than AIC, can also use (also smaller the better)
# LL (choicemodel): less negative the better
# maybe best for us to rely on the BIC


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
latexpath = "/Users/sanelmaheinonen/Documents/ETH Sem 2/Discrete Choice Modeling/DCM_course/estimation_framework/1_estimation/Xx_multitable/"
outputpath = "/Users/sanelmaheinonen/Documents/ETH Sem 2/Discrete Choice Modeling/DCM_course/estimation_framework/2_postestimation/"

#texreg output is a helpful output file to compare tables in latex for report
invisible(mixl::summary_tex(model, output_file = paste0(latexpath,"ztex_",modelname,"__texmod.Rdata")))
save(est, file = paste0(modelname,"__est.Rdata"))

# save modelfile

save(model, file = paste0(outputpath,modelname,"__mod.Rdata"))

cat(multitex)


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

