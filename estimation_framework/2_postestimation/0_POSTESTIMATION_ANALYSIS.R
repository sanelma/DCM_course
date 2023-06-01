
# define some basic things (paths, functions, settings, etc.) in Zz_source_code.R

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set root in source file location

source("../Zz_source_code.R")
source("../Zz_postestimation.R")

#outputpath = "C:/Users/Thibault Vignon/OneDrive/Desktop/ETHZ/Discrete Choice Models/DCM_course/estimation_framework/2_postestimation/"

outputpath = "/Users/sanelmaheinonen/Documents/ETH Sem 2/Discrete Choice Modeling/DCM_course/estimation_framework/2_postestimation/"

# ------------------------------------------------------------------------------------------------------------#
#
# ------- LOAD AND PREPARE HERE WHAT YOU NEED -------
#
# ------------------------------------------------------------------------------------------------------------#

# choose your model:


# modelname <- "1_mnl_pooled"

# modelname <- "2_mnl_pooled_sit_dc"
# modelname <- "2_mnl_pooled_sit_wec"
# modelname <- "2_mnl_pooled_sit_zcdc"

# modelname <- "2_mnl_pooled_sit_pars"
 modelname <- "3_mnl_pooled_soz_pars"

# modelname <- "4_mixl_pooled_normal"
 modelname <- "4_mixl_pooled_lognormal"
# modelname <- "4_mixl_pooled_johnson"

# modelname <- "5_gmnl_type2"
# modelname <- "6_necgmnl"

# modelname <- "7_wtp"

# modelname <- "8_wtp_hybrid"
# modelname <- "9_wtp_hybrid_mobitools"


# load the model
load(paste0(outputpath,modelname,"__mod.Rdata"))

# number of repetitions for calculation of prediction accuary confidence interval
bootn <<- 1

# define if working with test/training (= modeling) dataset
training <<- 1

if(training==1){
  
  dat <- model$data
  
  availabilities <- model$availabilities # choose appropriate columns for availabilities in training dataset
  
  head(availabilities) # check
  summary(availabilities)
  
} else {
  
  dat <- fread(paste0(datafolder,"testdata.csv"),sep = ";") # define your test dataset/holdout sample here
  #like machine learning test set: check if model predicts well on data not used to build model
  
  colnames(model$data)[!colnames(model$data)%in%colnames(dat)] # Check, if same columns in training- and testdata
  
  availabilities <- as.matrix(dat[,c(grep(pattern = "av_",colnames(dat)))]) # choose appropriate columns for availabilities in test dataset
  
  head(availabilities) # check
  summary(availabilities)
  
}

# Proper labeling of alternatives
#can only use letters or numbers - no special characters

#alternatives <<- c("WRP","BRP","CRP","PTRP", # RP mode choice 
#                   "WSP","BSP","CPSP","CSSP","PTSP", # SP mode choice 
#                  "RCC1","RCC2","RCC3", # SP route choice carsharing
#                   "RCPT1","RCPT2","RCPT3") # SP route choice public transport

alternatives <<- c("COOK", "DEL", "REST")

alternatives_names <<- alternatives

# Get estimates and robust covariance matrix

est <- model$estimate
robvarcov <- summary(model)$robvarcov

# ------------------------------------------------------------------------------------------------------------#
#
# ------- RUN ANALYSIS -------
#
# ------------------------------------------------------------------------------------------------------------#

# creates everything defined in Zz_postestimation.R

suppressWarnings(run_analysis(model=model,dat=dat,outputpath=outputpath,modelname=modelname))


# ------------------------------------------------------------------------------------------------------------#
#
# ------- STANDARD ERRORS of PARAMETER FUNCTIONS using DELTA METHOD -------
#
# ------------------------------------------------------------------------------------------------------------#
# note: More functions available in source code; add your personalized functions there

# Sanelma note: I think I figured this part out? 

names(est)

#shows functions of the parameters and their standard errors and t-values
# t value above magnitude ~2 is significant
deltamethod("B_D_TIME","B_COST")
deltamethod("B_R_TIME","B_COST")
deltamethod("B_C_TIME","B_COST")

deltamethod("B_HLTH","B_COST")  #people willing to pay on average 3.51 CHF for healthy? 
deltamethod("B_UNHLTH","B_COST")  #people (nonvegetarians) willing to pay on average 6.43 CHF to avoid unhealthy? 
deltamethod("B_LORG", "B_COST") #non (nonvegetarians) willing to pay on average 2.21 for local organic

deltamethod("B_SERV", "B_COST")

#can use deltasumnum when we have interaction effects 
# where B_TT_C is a reference category for rural areas, B_URBAN_TT_C is interaction w/ urban area
# ex: (B_TT_C + B_URBAN_TT_C)/B_COST  (sum gives us valuation for urban areas)
# can compare to delta method of B_TT_C/B_COST  (valuation for urban areas)
# can see that the rural value of travel time is much different from the urban value of travel time
# using standard error, can calculate confidence intervals for the two estimates and see that they are not overlapping - significantly different
# we would also see they are significantly different from a significant interaction effect
# don't need to add these to output tables, but if we find a significant effect then we could create its own output table


deltasumnum("B_D_TIME", "B_D_TIME_FEMALE", "B_COST")  
#computing value of delivery wait time for women 0.22, se 0.04 so range is 0.14 to 0.30
# for men, from above, it's 0.14 with se 0.03, so range 0.08 to 0.20. 
# 95% confidence intervals overlap, so we do not have a statistically significant difference in monetary valuation of wait time 

deltasumnum("B_UNHLTH", "B_UNHLTH_VEGETARIAN", "B_COST") 
#vegetarians willing to pay 9.66 to avoid unhealthy - confidence interval does not overlap with nonvegetarians

deltasumnum("B_LORG", "B_LORG_VEGETARIAN", "B_COST") 
# vegetarians willing to to pay 6.01 for local organic food : very different from nonvegetarians!



# ------------------------------------------------------------------------------------------------------------#
#
# ------- POSTERIORS ANALYSIS -------
#
# ------------------------------------------------------------------------------------------------------------#

# specify the model names for valuation inference - some models that have random components


modelnames=c("4_mixl_pooled_normal",
             "4_mixl_pooled_lognormal")
             "4_mixl_pooled_johnson",
         #    "5_gmnl_type2","6_necgmnl",
         #    "7_wtp","8_wtp_hybrid","9_wtp_hybrid_mobitools")


# define this as a WTP indicator, to detect if models were estimated in WTP or preference space
# if wtp_indicator > 0: Model estimated in WTP space

# will not be relevant for us - relevant for if we have multiple models
wtp_indicator <- "B_TT_C_RND" 

# create list for loop to calculate all valuation indicators
# delta method function is defined in source code - calculates standard errors of functions based on delta method rules

coeff_mat = list()

# for estimating posteriors - only for variables with RND at the end (random components? )
# important to specify correctly in mixl with _RND 

for (name in modelnames) {
  
  #name <- "2_mnl_pooled_sit_pars"

  data_check <- NULL
  
  posteriors <- fread(file=paste0(outputpath,name,"__posteriors.csv"),sep = ";",header = T)
  
  # IMPORTANT: Remove all _RND columns that are not included in all model files (e.g. LV_RND)!!!!!
  
  suppressWarnings(posteriors$LV_RND <- NULL)
  suppressWarnings(posteriors$LV_PT_RND <- NULL)
  
  data_check <- data.frame(posteriors)
 # data_check <- as.data.frame(data_check[grepl(wtp_indicator, names(data_check))])
  
  
  names(data_check) <- "check"
  
  if( median(data_check$check) < 0 ) {
  posteriors <- posteriors / posteriors$B_COST_RND
  }
  
  # why is the cost column removed? 
  posteriors[,grep(pattern = "COST",colnames(posteriors))] <- NULL
  
  medians <- apply(posteriors,2,median)
  iqr <- apply(posteriors,2,IQR)
  means <- apply(posteriors,2,mean)
  
  indicators <- rbind(medians) # include medians
  #indicators <- rbind(medians,iqr) # include medians and SDs
  #indicators <- rbind(medians,means,iqr) # include medians, means and SDs
  
  coeff_mat[[name]] <- assign(modelname, indicators) # write into list
  
  #print(coeff_mat)
  
}

#can look at distribution of posteriors - they should look roughly like the distribution that we specified
# the distributions are specified through the form in the mixl files - the default for draws is normal. for example, exp() in front is an exponential distribution

histogram(posteriors$B_C_TIME_RND, col = "grey",
          main = "Posterior distribution of b_cooking_time",
          xlab = "b_cooking_time")

#some people like to cook
summary(posteriors$B_C_TIME_RND) # can see that the max B_C_TIME is positive - indicates that some people like to cook

#histogram(posteriors$B_LORG_RND)  #no random component added here, so only two options on histogram
#summary(posteriors$B_LORG_RND) 


#mean(posteriors$B_TT_C_RND / posteriors$B_COST_RND) # mean value of travel time savings for car across all respondents
median(posteriors$B_C_TIME_RND / posteriors$B_COST_RND) # median is more robust to outliers - should use the median
# could also do simple t-tests to see if coefficients of different populations differ from each other

histogram(posteriors$B_SERV_RND, col = "grey", 
          main = "Posterior distribution of b_tableservice",
          xlab = "b_tableservice")
          

histogram(posteriors$B_D_TIME_RND, col = "grey", 
          main = "Posterior distribution of b_delivery_time",
          xlab = "b_deliverytime")

histogram(posteriors$B_UNHLTH_RND, col = "grey", 
          main = "Posterior distribution of b_unhealthy",
          xlab = "b_unhealthy")

histogram(posteriors$B_HLTH_RND, col = "grey", 
          main = "Posterior distribution of b_healthy",
          xlab = "b_healthy")

histogram(posteriors$B_COST_RND, col = "grey", 
          main = "Posterior distribution of cost",
          xlab = "b_cost")




coeffs <- as.data.frame(do.call(rbind,coeff_mat)) # use do.call(rbind,...) to put things together
coeffs <- round(coeffs, 2)

coeffs <- tibble::rownames_to_column(coeffs)
coeffs <- coeffs %>% rename(measure = rowname)

fwrite(x = coeffs,file =  paste0(outputpath,"Zz_all_models_value_indic.csv"),sep=";",row.names = F)


# ------------------------------------------------------------------------------------------------------------#
#
# ------- PARTWORTH ANALYSIS (EXAMPLE FOR MODE CHOICE SP) -------
#
# ------------------------------------------------------------------------------------------------------------#

modelname=paste0("1_mnl_pooled")

means <- fread(file=paste0(outputpath,modelname,"_mean_vars.csv"),sep = ";",header = T)

names(est)
means

minute_delivery <- abs(means$mean[means$name == "delivery_waiting_time_home"] * est["B_D_TIME"] )
minute_cooking <- abs(means$mean[means$name == "cooking_cooking_time"] * est["B_C_TIME"] )
minute_restaurant <- abs(means$mean[means$name == "restaurant_travel_time"] * est["B_R_TIME"] )

hour_delivery <- abs(means$mean[means$name == "delivery_waiting_time_home"] * est["B_D_TIME"] / 60 )
hour_cooking <- abs(means$mean[means$name == "cooking_cooking_time"] * est["B_C_TIME"] / 60 )
hour_restaurant <- abs(means$mean[means$name == "restaurant_travel_time"] * est["B_R_TIME"] / 60 )

chf_delivery <- abs(means$mean[means$name == "delivery_cost"] * est["B_COST"] )
chf_cooking <- abs(means$mean[means$name == "cooking_cost"] * est["B_COST"] )
chf_restaurant <- abs(means$mean[means$name == "restaurant_cost"] * est["B_COST"] )

lorg_delivery <- abs(means$mean[means$name == "delivery_dummy_local_organic"] * est["B_LORG"] )
lorg_cooking <- abs(means$mean[means$name == "cooking_dummy_local_organic"] * est["B_LORG"] )
lorg_restaurant <- abs(means$mean[means$name == "restaurant_dummy_local_organic"] * est["B_LORG"] )

unhlth_delivery <- abs(means$mean[means$name == "delivery_dummy_unhealthy"] * est["B_UNHLTH"] )
unhlth_cooking <- abs(means$mean[means$name == "cooking_dummy_unhealthy"] * est["B_UNHLTH"] )
unhlth_restaurant <- abs(means$mean[means$name == "restaurant_dummy_unhealthy"] * est["B_UNHLTH"] )

hlth_delivery <- abs(means$mean[means$name == "delivery_dummy_healthy"] * est["B_HLTH"] )
hlth_cooking <- abs(means$mean[means$name == "cooking_dummy_healthy"] * est["B_HLTH"] )
hlth_restaurant <- abs(means$mean[means$name == "restaurant_dummy_healthy"] * est["B_HLTH"] )

ts_restaurant <- abs(means$mean[means$name == "restaurant_dummy_ts_restaurant"] * est["B_SERV"] )


partworth <- rbind(#hour_delivery, hour_cooking, hour_restaurant,
                   minute_delivery, minute_cooking, minute_restaurant,
                   chf_delivery, chf_cooking, chf_restaurant,
                   lorg_delivery, lorg_cooking, lorg_restaurant,
                   unhlth_delivery, unhlth_cooking, unhlth_restaurant,
                   hlth_delivery, hlth_cooking, hlth_restaurant,
                   #parameters not in all models,
                   ts_restaurant)





partworth <- data.frame(partworth)
names(partworth) <- "partworth"
partworth <- partworth/sum(partworth)*100

partworth <- tibble::rownames_to_column(partworth)

partworth$rowname <- factor(partworth$rowname)
partworth <- partworth[order(partworth$partworth,decreasing = T),]

partworth$rowname <- factor(partworth$rowname, levels = partworth$rowname[order(partworth$partworth)])
partworth <- partworth[1:nrow(partworth),]

# plot variable importance:

p <- ggplot(partworth, aes(x=rowname, y=partworth), show.legend = FALSE) +
  geom_bar(stat='identity')  + 
  coord_flip() +  theme_minimal() + theme(axis.text.x = element_text(size = 20,colour = "black"),
                                          axis.text.y = element_text(size = 20,colour = "black"),
                                          axis.title = element_text(size = 20,colour = "black")) +
  ylab(paste0("Partworth [%]")) + xlab("Parameter x variable") + theme(legend.position = "none") + 
  scale_y_continuous(limits = c(0, max(partworth$partworth)*1.1))

ggsave(filename = paste0(outputpath,modelname,"_partworth.png"),device = "png",plot = p,width = 30,height = 15,units = "cm")


