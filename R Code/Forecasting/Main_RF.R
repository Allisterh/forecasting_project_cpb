### SEMINAR APPLIED ECONOMETRICS 
rm(list=ls())

library(BVAR)
library(randomForest)
library(vars)
library(data.table)
library(tseries)

## ---- DATA ----

df <- read.csv("data-eur2023.csv", row.names = 1)
regressor_matrix <- df[-c(1,2)] #Remove pubdate and dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)

# -- transformations -- 

# Check if data stationary
adf.test(df[,3])

# Make data stationary

# -- Parameter initalizations for feature matrix --
X_lags <- 12 # We use the data up until one year before
n_Factors <- 2 # Optimization
F_lags <- 12 # Same reason as X, also because Coulombe
P_MAF <- 12 # Summarize data up until one year 
n_MAF <- 6 # Optimization 
P_MARX <- 12 # Lags for MARX, same as X_lags, also Coulombe 

# -- Make feature matrices --
source("Function Feature Matrix.r")

X <- as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE))

scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags)

scaled_X <- scale(X) #Scale lagged X Matrix for MAF
MAF <- MAF_function2(scaled_X,X_lags,nrow(scaled_X),ncol(regressor_matrix), P_MAF, n_MAF, FALSE)

MARX <- MARX_function(regressor_matrix, P_MARX)

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
Unempl <- df[,2] # (X_lags+1)
y_real <- Unempl[(length(Unempl)-poos+1):length(Unempl)] # prediction must start at Y_316. test set is therefore Z_316-h (f=1)
start_forecast <- length(Unempl)-poos+1
end_forecast <- length(Unempl)
horizons <- list(3, 6, 12, 18, 24) #
ntrees <- 500 # Default value

# -- forecasting

RF_X_forecast <- Forecasting_function_RF(Unempl, X, poos, horizons, ntrees) #1 min
y_real2 <- Unempl[(length(Unempl)-poos+1):length(Unempl)]
check <- RMSE_function(y_real2,)


# ---- Forecasting XGBoost ----



