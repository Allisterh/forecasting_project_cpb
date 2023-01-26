################################################
######## Code to Compute FM and FM-RMSE ########
######## 26.01.23 ##############################
################################################

#Clearing Environment
rm(list=ls())

df <- read.csv("~/Desktop/data-eur2023.csv", row.names=1)
colnames(df) <- c("pub.date","unempl","constr.conf","retail.conf",
                  "manuf.conf","cons.conf","export.logs","import.logs",
                  "AEX.val.logs", "AEX.vol")

######################################################################################################################
######################################################################################################################
                                                                                                                      
#(A) Make dataframe with Lags of y (1-12)                                                                            
#(B) Make dataframe with Factors of X (8)
#(C) Make dataframe with Lags of Factors (1-12, thus 96 in total)
#(D) Merge (A) and (C)

#(E) OLS Regression for 1-step ahead: need to shift y's 1 value up on (13-->314) - this is for 315
#(F) OLS Regression for 3-step ahead: need to shift y's 3 values up on (13-->312) - this is for 315
#(G) Write Function that performs regression and shifts y's; horizon, POOS, i (in 315-434) as input
#(H) Run Regression and safe coefficients + intercept for every training set (depends on i, length=[434-120-12-h+1(?)])

#(H) Constructing forecasts using coefficients + intercept saved in H
#(I) Computing RMSE for each predicted series
#(J) Create function that combines all individual functions to get to (I) as output

######################################################################################################################
######################################################################################################################

#(A)
create_lags <- function(data_frame, n_lags){
  
}

#(B)
create_factors <- function(data_frame, n_factors){
  
}


