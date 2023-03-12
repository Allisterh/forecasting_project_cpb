### SEMINAR APPLIED ECONOMETRICS 
rm(list=ls())

library(BVAR)
library(randomForest)
library(vars)
library(data.table)
library(tseries)
library(caret)
library(xgboost)
library(tidyverse)

source("Function_File.r")

df_cpb_infl_stat <- read.csv("Extra data/data_core.csv")
df_additional_stat <- read.csv("Extra data/data_additional.csv")

# If big dataset: combine cpb.infl.stationary and additional.data.stationary
df_big <- cbind(df_cpb_infl_stat[50:433,], df_additional_stat)
y_differenced <- df_big$L2_LRHUTTTT

## -- BIG DATA --
# Define stationary dataframes
regressor_matrix <- df_big[-c(1)] # Dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)

# -- Parameter initializations for feature matrix --
X_lags <- 12 # We use the data up until one year before
n_Factors <- 8 # Coulombe (BIG DATA)
F_lags <- 12 # Same reason as X, also because Coulombe
P_MAF <- 12 # Summarize data up until one year 
n_MAF <- 2 # Coulombe 
P_MARX <- 12 # Lags for MARX, same as X_lags, also Coulombe 

# -- Make feature matrices --
source("Function_File.r")

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

# ---- Forecasting XGB ----

# -- Forecast Initialization --
poos <- 120
y_real <- y_differenced[(length(y_differenced)-poos+1):length(y_differenced)]
start_forecast <- length(y_differenced)-poos+1
end_forecast <- length(y_differenced)
horizons <- list(3, 6, 12, 18, 24) 

# -- forecasting
Forecasting_function_XGB <- function(y, Z, n_forecast, horizons){
  XGB_y_forecast <- data.frame(matrix(ncol = length(horizons), nrow = n_forecast))
  i <- 0
  
  for (h in horizons){
    cat("", sep="\n\n")
    cat("### Horizon: ", h)
    cat("", sep="\n\n")
    
    shift_y = as.data.frame(shift(y,n=h, type = 'lead', give.names=TRUE))
    colnames(shift_y) = 'y'

    y_Z <- na.omit(cbind(shift_y, Z))
    
    i <- i+1
    
    train_x <- y_Z[1:(nrow(y_Z)-n_forecast),2:ncol(y_Z)]
    train_y <- y_Z[1:(nrow(y_Z)-n_forecast),1]
    
    for (f in 1:n_forecast){
      print(f)
      train_x <- y_Z[1:(nrow(y_Z)-n_forecast+f-1),2:ncol(y_Z)]
      train_y <- y_Z[1:(nrow(y_Z)-n_forecast+f-1),1]
      test_x <- y_Z[nrow(y_Z)-n_forecast+f,2:ncol(y_Z)]
      test_y <- y_Z[nrow(y_Z)-n_forecast+f,1]
      
      if(f==1 || f%%24==0 & f!= 120){
        
        train_control = trainControl(method = "cv", number = 5, search = "grid")
        
        set.seed(2023)
        
        trainyx <- cbind(train_y, train_x)
        
        # Customising the tuning grid
        gbmGrid <-  expand.grid(max_depth = 6,
                                nrounds = c(50, 100, 200, 500), 
                                eta = c(0.01, 0.1, 0.3), 
                                gamma = 0, #default
                                subsample = 1, #default
                                min_child_weight = 1, #default
                                colsample_bytree = 0.6) #default
        
        # training a XGboost Regression tree model while tuning parameters
        XGB_tuned = train(train_y~., data = trainyx, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid, verbosity=0)
        print(XGB_tuned)
        
      }
      
      testyx <- cbind(test_y, test_x)
      XGB_y_forecast[f,i] <- predict(XGB_tuned, testyx)
    }
    colnames(XGB_y_forecast)[i]=paste('h=',h,sep='')
  }
  return(XGB_y_forecast)
}

RMSE_function <- function(actual, prediction){
  RMSE <- data.frame(matrix(ncol = length(horizons), nrow = 1))
  i <- 0
  for (h in horizons){
    i <- i+1
    RMSE[1,i] <- sqrt(mean((actual - prediction[,i])^2))
    colnames(RMSE)[i]=paste('h=',h,sep='')
  }
  return(RMSE)
}

XGB_X_forecast <- Forecasting_function_XGB(y_differenced, X, poos, horizons) 
RMSE_XGB_X_forecast <- RMSE_function(y_real,XGB_X_forecast)

XGB_F_forecast <- Forecasting_function_XGB(y_differenced, F, poos, horizons) 
RMSE_XGB_F_forecast <- RMSE_function(y_real,XGB_F_forecast)

XGB_MAF_forecast <- Forecasting_function_XGB(y_differenced, MAF, poos, horizons) 
RMSE_XGB_MAF_forecast <- RMSE_function(y_real,XGB_MAF_forecast)

XGB_MARX_forecast <- Forecasting_function_XGB(y_differenced, MARX, poos, horizons) 
RMSE_XGB_MARX_forecast <- RMSE_function(y_real,XGB_MARX_forecast)



