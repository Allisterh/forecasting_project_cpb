####################################################
######## Code to Compute FM and FM-RMSE, US ########
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

target_lags <- create_lags(df[,"UNRATE"],n_TL)
shifted_target <- c(df[,"UNRATE"][-(seq(h))],rep(NA,h)) 

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

scaled_predictors <- as.data.frame(scale(predictors))
factors_lags_predictors <- create_factors(na.omit(scaled_predictors),n_factors=n_F,n_lags=n_L,nrow(df))
FM_predictors <- cbind(shifted_target,target_lags,factors_lags_predictors) 

#Forecasting 
prediction <- c(df[1:(nrow(df)-poos),"UNRATE"],rep(0,poos))
for (i in (nrow(df)-poos+1):nrow(df)) {
  model <- lm(formula = shifted_target ~ ., data = FM_predictors[1:i-1,])
  forecast <- model$coefficients[1] + as.double(model$coefficients[-c(1)]) %*% as.double(FM_predictors[i,-c(1)])
  prediction[i] <- forecast
}

#Computing Residuals and RMSE
resids <- na.omit(prediction[(nrow(df)-poos+1):nrow(df)] - df[(nrow(df)-poos+1):nrow(df),"UNRATE"])
rmse <- sqrt(mean(resids^2))
