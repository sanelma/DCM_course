
# ------------------------------------------------------------------------------------------------------------#
#
# ------- DEFINE STARTINGVALUES (MANUALLY) # e.g. for the first time you run a model -------
#
# ------------------------------------------------------------------------------------------------------------#

data_names = # starting values based on MNL
  est <- setNames(c(
     -0.1, # ASC_D
     -0.4, #ASC_R

     -0.1, # COST

     -0.02,  #B_C_TIME, 
     -0.02,  #B_D_TIME
     -0.01,  #B_R_TIME
     -0.01,  #B_D_TIME_FEMALE
     
     -0.8,  #B_UNHLTH
     -0.4,  #B_UNHLTH_VEGETARIAN
      0.5,  #B_HLTH
      0.3,  #B_LORG
      0.5,  #B_LORG_VEGETARIAN
     
     
     0.8,   #B_SERV
     
     -0.5  #LAMBDA_COST_BUDGET


  ), c('ASC_D',    # Constants: PT = reference
       'ASC_R',

       'B_COST',      #  cost

       'B_C_TIME',      # times
       'B_D_TIME',
       'B_R_TIME',
       'B_D_TIME_FEMALE',

       'B_UNHLTH',  
       'B_UNHLTH_VEGETARIAN', 
       'B_HLTH', 
       'B_LORG', 
       'B_LORG_VEGETARIAN',

       'B_SERV',

       "LAMBDA_COST_BUDGET"
  ))
