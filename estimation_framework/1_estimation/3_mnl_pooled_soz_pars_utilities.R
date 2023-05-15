
# ------------------------------------------------------------------------------------------------------------#
#
# ------- DEFINE UTILITIES -------
#
# ------------------------------------------------------------------------------------------------------------#

# model includes observed heterogeneity from sociodemographics 
# excluded everything that wasn't significant - only very few variables had an effect
# age is scaled to have a mean of one, and then the one is subtracted to be zero centered

#pow( $BUDGET, @LAMBDA_COST) *
# * pow($AGE * @LAMBDA_AGE)

# tried some various interactions with age and cost, but wasn't significant

#B_COST_RND = ( @B_COST ) * pow($AGE, @LAMBDA_AGE)  ;
# @B_COST_A20 * $AGE20 +  @B_COST_A30 * $AGE30 etc;

fast_mixl <- "

ASC_C_RNP =      0 ;
ASC_D_RNP = @ASC_D ;
ASC_R_RNP = @ASC_R ;

B_COST_RND = ( @B_COST ) * pow( $BUDGET, @LAMBDA_COST) ;

B_C_TIME_RND = @B_C_TIME ;
B_D_TIME_RND = @B_D_TIME ; 
B_R_TIME_RND = @B_R_TIME ;


B_UNHLTH_RND = @B_UNHLTH  ;
B_HLTH_RND   = @B_HLTH   ;
B_LORG_RND   = @B_LORG   ;

B_SERV_RND   = @B_SERV   ;


U_1 = ASC_C_RNP + B_COST_RND * $cooking_cost + B_C_TIME_RND * $cooking_cooking_time + B_UNHLTH_RND * $cooking_dummy_unhealthy + B_HLTH_RND * $cooking_dummy_healthy + B_LORG_RND * $cooking_dummy_local_organic ;
U_2 = ASC_D_RNP + B_COST_RND * $delivery_cost + B_D_TIME_RND * $delivery_waiting_time_home + B_UNHLTH_RND * $delivery_dummy_unhealthy + B_HLTH_RND * $delivery_dummy_healthy + B_LORG_RND * $delivery_dummy_local_organic ;
U_3 = ASC_R_RNP + B_COST_RND * $restaurant_cost + B_R_TIME_RND * $restaurant_travel_time + B_SERV_RND * $restaurant_dummy_ts_restaurant + B_UNHLTH_RND * $restaurant_dummy_unhealthy + B_HLTH_RND * $restaurant_dummy_healthy + B_LORG_RND * $restaurant_dummy_local_organic ;

"



#fast_mixl <- "

#ASC_W_RNP  = @ASC_W + @A_LEIS_W * $leisure_dc  ;

#ASC_B_RNP  = @ASC_B + 
##              @A_AGE_B*( $age - 1 ) ;

#ASC_C_RNP  = @ASC_C + @A_SHOP_C * $shop_dc + @A_OTH_C * $other_dc + 
#              @A_AGE_C*( $age - 1 ) + @A_URBAN_C*$urban ;

#ASC_PT_RNP =      0 ;

#ASC_CS_RNP = @ASC_CS + @A_LEIS_CS * $leisure_dc + 
#              @A_MALE_CS*$sex ;

#ASC_CP_RNP = @ASC_CP + @A_LEIS_CP * $leisure_dc + 
#              @A_URBAN_CP*$urban ;



#B_COST_RND = ( @B_COST ) * pow( $distie, @LAMBDA_COST ) ;



#B_TT_W_RND  = ( @B_TT_W + @B_LEIS_TT_W * $leisure_dc ) ;

#B_TT_B_RND  = ( @B_TT_B  ) ;

#B_TT_C_RND  = ( @B_TT_C + 
#                @B_AGE_TT_C*( $age - 1 ) + @B_URBAN_TT_C*$urban ) ;

#B_TT_PT_RND = ( @B_TT_PT  ) ;

#B_TT_CS_RND = ( @B_TT_CS + @B_LEIS_TT_CS * $leisure_dc + 
#                @B_AGE_TT_CS*( $age - 1 ) ) ;

#B_TT_CP_RND = ( @B_TT_CP + 
##                @B_AGE_TT_CP*( $age - 1 ) + @B_URBAN_TT_CP*$urban ) ;



#U_1 =  1 * (ASC_W_RNP + B_TT_W_RND * $tt_w_rp / 60 ) ;

#U_2 =  1 * (ASC_B_RNP + B_TT_B_RND * $tt_b_rp / 60 ) ;

#U_3 =  1 * (ASC_C_RNP + ( B_TT_C_RND * $tt_c_rp / 60 + B_COST_RND * $tc_c_rp )) ;

#U_4 =  1 * (ASC_PT_RNP + ( B_TT_PT_RND * $tt_pt_rp / 60  + B_COST_RND * $tc_pt_rp_a1 +
#                        @B_TRNS_PT * $trns_pt_rp + @B_ACC_PT * $acc_pt_rp / 60 + @B_FREQ_PT * $freq_pt_rp / 60 )) ;


#U_5 =  @S_MC * ( ASC_W_RNP  + B_TT_W_RND * $tt_bf_mc / 60 ) ;

#U_6 =  @S_MC * ( ASC_B_RNP  + B_TT_B_RND * $tt_v_mc / 60 ) ;

#U_7 =  @S_MC * ( ASC_CP_RNP + B_TT_CP_RND * $tt_cp_mc / 60 + B_COST_RND * $c_cp_mc +
 #                   @B_ACC_CS_CP * $acc_cp_mc / 60 + @B_RISK_CP * $r_cp_mc ) ;

#U_8 =  @S_MC * ( ASC_CS_RNP + B_TT_CS_RND * $tt_cs_mc / 60 + B_COST_RND * $c_cs_mc + 
#                    @B_ACC_CS_CP * $acc_cs_mc / 60 );

#U_9 =  @S_MC * ( ASC_PT_RNP + B_TT_PT_RND * $tt_pt_mc / 60 + B_COST_RND * $c_pt_mc +
  #                  @B_TRNS_PT * $tr_pt_mc + @B_ACC_PT * $acc_pt_mc / 60 + @B_FREQ_PT * $s_pt_mc / 60 ) ;
                    

#U_10 =  @S_RCC *  (( B_TT_CS_RND * $tt_a1_rcc / 60 + B_COST_RND * $c_a1_rcc + 
#                                  @B_Q_CS * $q_a1_rcc / 60 + @B_ACC_CS_CP * $acc_a1_rcc / 60 )) ;

#U_11 =  @S_RCC *  (( B_TT_CS_RND * $tt_a2_rcc / 60 + B_COST_RND * $c_a2_rcc +
#                                  @B_Q_CS * $q_a2_rcc / 60 + @B_ACC_CS_CP * $acc_a2_rcc / 60 )) ;

#U_12 =  @S_RCC *  (( B_TT_CS_RND * $tt_a3_rcc / 60 + B_COST_RND * $c_a3_rcc + 
#                                  @B_Q_CS * $q_a3_rcc / 60 + @B_ACC_CS_CP * $acc_a3_rcc / 60 )) ;



#U_13 =  @S_RCPT *  (( B_TT_PT_RND * $tt_a1_rcpt / 60 + B_COST_RND * $c_a1_rcpt + 
 #                                  @B_ACC_PT * $acc_a1_rcpt / 60 + @B_TRNS_PT * $tr_a1_rcpt + @B_FREQ_PT * $f_a1_rcpt / 60 )) ;

#U_14 =  @S_RCPT *  (( B_TT_PT_RND * $tt_a2_rcpt / 60 + B_COST_RND * $c_a2_rcpt + 
#                                   @B_ACC_PT * $acc_a2_rcpt / 60 + @B_TRNS_PT * $tr_a2_rcpt + @B_FREQ_PT * $f_a2_rcpt / 60 )) ;

#U_15 =  @S_RCPT *  (( B_TT_PT_RND * $tt_a3_rcpt / 60 + B_COST_RND * $c_a3_rcpt + 
 #                                  @B_ACC_PT * $acc_a3_rcpt / 60 + @B_TRNS_PT * $tr_a3_rcpt + @B_FREQ_PT * $f_a3_rcpt / 60 )) ;


#"