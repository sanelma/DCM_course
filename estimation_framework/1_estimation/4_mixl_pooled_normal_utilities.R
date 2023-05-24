
# ------------------------------------------------------------------------------------------------------------#
#
# ------- DEFINE UTILITIES -------
#
# ------------------------------------------------------------------------------------------------------------#

#removed local and organic female interaction due to correlation between female and vegetarian
#+ @B_LORG_FEMALE * $FEMALE
#+ 
#+ may want to give normal distribution if we expect sign could go either way
#+ and use log normal when we expect all people to have the same sign, just different magnitude

fast_mixl <- "

ASC_C_RNP = draw_1 * @SIGMA_C;
ASC_D_RNP = 0;
ASC_R_RNP = @ASC_R + draw_3 * @SIGMA_R;

B_COST_RND = ( @B_COST + draw_4 * @SIGMA_COST)  * pow( $BUDGET, @LAMBDA_COST_BUDGET)  ;

B_C_TIME_RND = @B_C_TIME + draw_5 * @SIGMA_C_TIME ;
B_D_TIME_RND = @B_D_TIME + @B_D_TIME_FEMALE * $FEMALE + draw_6 * @SIGMA_D_TIME ; 
B_R_TIME_RND = @B_R_TIME ;


B_UNHLTH_RND = @B_UNHLTH  + @B_UNHLTH_VEGETARIAN * $DIETVEGETARIAN + draw_8 * @SIGMA_UNHLTH;
B_HLTH_RND   = @B_HLTH + draw_9 * @SIGMA_HLTH;
B_LORG_RND   = @B_LORG  + @B_LORG_VEGETARIAN * $DIETVEGETARIAN ;

B_SERV_RND   = @B_SERV + draw_11 * @SIGMA_SERV;


U_1 = ASC_C_RNP + B_COST_RND * $cooking_cost + B_C_TIME_RND * $cooking_cooking_time + B_UNHLTH_RND * $cooking_dummy_unhealthy + B_HLTH_RND * $cooking_dummy_healthy + B_LORG_RND * $cooking_dummy_local_organic ;
U_2 = ASC_D_RNP + B_COST_RND * $delivery_cost + B_D_TIME_RND * $delivery_waiting_time_home + B_UNHLTH_RND * $delivery_dummy_unhealthy + B_HLTH_RND * $delivery_dummy_healthy + B_LORG_RND * $delivery_dummy_local_organic ;
U_3 = ASC_R_RNP + B_COST_RND * $restaurant_cost + B_R_TIME_RND * $restaurant_travel_time + B_SERV_RND * $restaurant_dummy_ts_restaurant + B_UNHLTH_RND * $restaurant_dummy_unhealthy + B_HLTH_RND * $restaurant_dummy_healthy + B_LORG_RND * $restaurant_dummy_local_organic ;

"



