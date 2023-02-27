### SEMINAR APPLIED ECONOMETRICS 
rm(list=ls())

library(BVAR)
library(randomForest)
library(vars)
library(data.table)
library(tseries)
library(xgboost)
library(caret)
library(tidyverse)	
library(cbsodataR)
library(lubridate)
library(dplyr)
library(yahoofinancer)
library(xtable)

source("Function_File.r")
source("Data_File.r")

# Define stationary dataframes
regressor_matrix <- new_df[-c(1)] # Dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)

# -- Parameter initalizations for feature matrix --
X_lags <- 12 # We use the data up until one year before
n_Factors <- 2 # Optimization
F_lags <- 12 # Same reason as X, also because Coulombe
P_MAF <- 12 # Summarize data up until one year 
n_MAF <- 6 # Optimization 
P_MARX <- 12 # Lags for MARX, same as X_lags, also Coulombe 

# -- Make feature matrices --

X <- as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE))

scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags)

scaled_X <- scale(X) #Scale lagged X Matrix for MAF
MAF <- MAF_function(scaled_X,X_lags,nrow(scaled_X),ncol(regressor_matrix), P_MAF, n_MAF, FALSE)

MARX <- MARX_function(regressor_matrix, P_MARX, n_var)

X_F <- cbind(X, F)
X_MAF <- cbind(X, MAF)
X_MARX <- cbind(X, MARX)
F_MAF <- cbind(F, MAF)
F_MARX <- cbind(F, MARX)
MAF_MARX <- cbind(MAF, MARX)
X_F_MAF <- cbind(X, F, MAF)
X_F_MARX <- cbind(X, F, MARX)
X_MAF_MARX <- cbind(X, MAF, MARX)
F_MAF_MARX <- cbind(F, MAF, MARX)
X_F_MAF_MARX <- cbind(X, F, MAF, MARX)

# ---- Forecasting Random Forest ----

# -- Forecast Initialization --
poos <- 120
Unempl <- new_df[,1] # (X_lags+1)
y_real <- Unempl[(length(Unempl)-poos+1):length(Unempl)] # prediction must start at Y_315. test set is therefore Z_315-h (f=1)
start_forecast <- length(Unempl)-poos+1
end_forecast <- length(Unempl)
horizons <- list(3, 6, 12, 18, 24) # 
ntrees <- 500 # Default value

# -- forecasting

RF_X_forecast <- Forecasting_function_RF(Unempl, X, poos, horizons, ntrees) # 
RF_X_F_forecast <- Forecasting_function_RF(Unempl, X_F, poos, horizons, ntrees) #
RF_X_MAF_forecast <- Forecasting_function_RF(Unempl, X_MAF, poos, horizons, ntrees) # 
RF_X_MARX_forecast <- Forecasting_function_RF(Unempl, X_MARX, poos, horizons, ntrees) # 
RF_F_MAF_forecast <- Forecasting_function_RF(Unempl, F_MAF, poos, horizons, ntrees) # 
RF_F_MARX_forecast <- Forecasting_function_RF(Unempl, F_MARX, poos, horizons, ntrees) # 
RF_MAF_MARX_forecast <- Forecasting_function_RF(Unempl, MAF_MARX, poos, horizons, ntrees) # 
RF_X_F_MAF_forecast <- Forecasting_function_RF(Unempl, X_F_MAF, poos, horizons, ntrees) # 
RF_X_F_MARX_forecast <- Forecasting_function_RF(Unempl, X_F_MARX, poos, horizons, ntrees) # 
RF_X_MAF_MARX_forecast <- Forecasting_function_RF(Unempl, X_MAF_MARX, poos, horizons, ntrees) #
RF_F_MAF_MARX_forecast <- Forecasting_function_RF(Unempl, F_MAF_MARX, poos, horizons, ntrees) #
RF_X_F_MAF_MARX_forecast <- Forecasting_function_RF(Unempl, X_F_MAF_MARX, poos, horizons, ntrees) #

# ---- XGB Forecast ----
XGB_X_forecast <- Forecasting_function_XGB(Unempl, X, poos, horizons) # 
XGB_X_F_forecast <- Forecasting_function_XGB(Unempl, X_F, poos, horizons) #
XGB_X_MAF_forecast <- Forecasting_function_XGB(Unempl, X_MAF, poos, horizons) # 
XGB_X_MARX_forecast <- Forecasting_function_XGB(Unempl, X_MARX, poos, horizons) # 
XGB_F_MAF_forecast <- Forecasting_function_XGB(Unempl, F_MAF, poos, horizons) # 
XGB_F_MARX_forecast <- Forecasting_function_XGB(Unempl, F_MARX, poos, horizons) # 
XGB_MAF_MARX_forecast <- Forecasting_function_XGB(Unempl, MAF_MARX, poos, horizons) # 
XGB_X_F_MAF_forecast <- Forecasting_function_XGB(Unempl, X_F_MAF, poos, horizons) # 
XGB_X_F_MARX_forecast <- Forecasting_function_XGB(Unempl, X_F_MARX, poos, horizons) # 
XGB_X_MAF_MARX_forecast <- Forecasting_function_XGB(Unempl, X_MAF_MARX, poos, horizons) #
XGB_F_MAF_MARX_forecast <- Forecasting_function_XGB(Unempl, F_MAF_MARX, poos, horizons) #
XGB_X_F_MAF_MARX_forecast <- Forecasting_function_XGB(Unempl, X_F_MAF_MARX, poos, horizons) #

sqrt(mean((XGB_X_forecast[,1]-y_real)^2))
