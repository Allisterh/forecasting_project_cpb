####################################################
######## Code to Compute FM and FM-RMSE, US ########
######## Automated Script ##########################
######## 30.01.23 ##################################
####################################################

#Clearing Environment
rm(list=ls())
library("data.table")

df <- read.csv("~/Desktop/data122022.csv", row.names=1)
df <- df[2:709,]  #dec 2017
predictors <- subset(df,select=-c(UNRATE))
target <- df[,"UNRATE"]

h <- 3            #horizon
n_TL <- 4         #number of lags target
n_F <- 8          #number of factors
n_L <- 3          #number of lags of factors
poos <- 120       #number of forecasts (out-of-sample)

#Function to Create Lags
create_lags <- function(data_frame, n_lags){
  result <- as.data.frame(shift(data_frame, n=0:n_lags, type='lag', give.names=TRUE))
  return(result)
}

#Function to Create Factors and Lagged Factors
create_factors_aux <- function(data_frame, n_factors){
  pca <- prcomp(na.omit(data_frame), center = FALSE, scale. = FALSE) 
  result <- pca$x[1:n_factors]
  return(result)
}

create_factors <- function(data_frame, n_factors, n_lags, T){
  F <- data.frame(matrix(ncol = (n_factors*(n_lags+1)), nrow = T))
  for (t in n_factors:T){
    F[t,1:n_factors] <- create_factors_aux(data_frame[1:t,], n_factors)
  }
  for (i in 1:n_lags){
    F[(n_factors+i):T,(i*n_factors+1):((i+1)*n_factors)] <- head(F[n_factors:T,1:n_factors], - i)
  }
  colnames(F)=paste('F_',colnames(F),sep='')
  return(F)
}

#Function to Compute RMSE
computing_rmse <- function(data_frame,the_n_TL,the_h,the_predictors,the_n_F,the_n_L,the_poos){
  #Creating Predictors
  target_lags <- create_lags(data_frame[,"UNRATE"],the_n_TL)
  shifted_target <- c(data_frame[,"UNRATE"][-(seq(the_h))],rep(NA,the_h)) 
  
  scaled_predictors <- as.data.frame(scale(the_predictors))
  factors_lags_predictors <- create_factors(na.omit(scaled_predictors),the_n_F,the_n_L,nrow(data_frame))
  FM_predictors <- cbind(shifted_target,target_lags,factors_lags_predictors) 
  
  #Forecasting 
  prediction <- c(data_frame[1:(nrow(data_frame)-the_poos),"UNRATE"],rep(0,the_poos))
  for (i in (nrow(data_frame)-the_poos+1):nrow(data_frame)) {
    model <- lm(formula = shifted_target ~ ., data = FM_predictors[1:i-1,])
    forecast <- model$coefficients[1] + as.double(model$coefficients[-c(1)]) %*% as.double(FM_predictors[i,-c(1)])
    prediction[i] <- forecast
  }
  
  #Computing Residuals and RMSE
  resids <- na.omit(prediction[(nrow(data_frame)-the_poos+1):nrow(data_frame)] - data_frame[(nrow(data_frame)-the_poos+1):nrow(data_frame),"UNRATE"])
  rmse <- sqrt(mean(resids^2))
  return(rmse)
}

#Optimize RMSE Over Combinations n_Factors, n_Lags, n_Lags_Predictor
results_1m <- matrix(, nrow = 5, ncol = 5)
#results_3m <- matrix(, nrow = 5, ncol = 5)
#results_6m <- matrix(, nrow = 5, ncol = 5)

for (i in 1:5) {     #number of factors
  for (j in 1:5) {   #number of lags
    start <- Sys.time()
    results_1m[i,j] <- computing_rmse(df,the_n_TL = j, the_h = 1, the_predictors = predictors, the_n_F = i, the_n_L = j, the_poos = 120)
    end <- Sys.time()
    #results_3m[i,j] <- computing_rmse(df,the_n_TL = j, the_h = 3, the_predictors = predictors, the_n_F = i, the_n_L = j, the_poos = 120)
    #results_6m[i,j] <- computing_rmse(df,the_n_TL = j, the_h = 6, the_predictors = predictors, the_n_F = i, the_n_L = j, the_poos = 120)
    print(c(i,j,end-start))
  }
}
