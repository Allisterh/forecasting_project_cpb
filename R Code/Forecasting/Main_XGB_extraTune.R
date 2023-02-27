### SEMINAR APPLIED ECONOMETRICS 
rm(list=ls())

library(BVAR)
library(randomForest)
library(vars)
library(data.table)
library(tseries)
library(caret)
library(xgboost)

## ---- DATA ----
df <- read.csv("data-eur2023.csv", row.names = 1)

# -- transformations -- 
tobedifferenced_matrix <- df[-c(1)] # Remove pubdate 
colstodiff <- c(1, 2, 3, 6, 7, 8)

# take the first difference of the selected columns
diff_data <- diff(as.matrix(tobedifferenced_matrix[, colstodiff]), differences = 1)
diff_data <- data.frame(cbind(diff_data, df$L1_BSCI[2:434], df$L1_CSCICP02[2:434], df$L1_AEX[2:434]))

y_differenced <- diff_data[,1]
regressor_matrix_diff <- diff_data[-c(1)]
n_var <- ncol(regressor_matrix_diff)
T <- nrow(regressor_matrix_diff)

# Check if data stationary: H0: not stationary --> reject = ~ stationary
adf.test(y_differenced) #stationary
adf.test(regressor_matrix_diff[,1]) #stationary
adf.test(regressor_matrix_diff[,2]) #stationary
adf.test(regressor_matrix_diff[,3]) #stationary
adf.test(regressor_matrix_diff[,4]) #stationary
adf.test(regressor_matrix_diff[,5]) #stationary
adf.test(regressor_matrix_diff[,6]) #stationary
adf.test(regressor_matrix_diff[,7]) #stationary
adf.test(regressor_matrix_diff[,8]) #not stationary (not rejected)

diff2_AEX <- diff(regressor_matrix_diff[,8], differences = 1)
adf.test(diff2_AEX) #stationary
regressor_matrix_diff <- data.frame(cbind(regressor_matrix_diff[2:433,1:7], diff2_AEX)) 

# -- Parameter initalizations for feature matrix --
X_lags <- 12 # We use the data up until one year before
n_Factors <- 2 # Optimization
F_lags <- 12 # Same reason as X, also because Coulombe
P_MAF <- 12 # Summarize data up until one year 
n_MAF <- 6 # Optimization 
P_MARX <- 12 # Lags for MARX, same as X_lags, also Coulombe 

# -- Make feature matrices --
source("Function Feature Matrix.r")

X <- as.data.frame(shift(regressor_matrix_diff,n=0:X_lags, type = 'lag', give.names=TRUE))

scaled_regressor_matrix <- scale(regressor_matrix_diff) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags)

scaled_X <- scale(X) #Scale lagged X Matrix for MAF
MAF <- MAF_function2(scaled_X,X_lags,nrow(scaled_X),ncol(regressor_matrix_diff), P_MAF, n_MAF, FALSE)

MARX <- MARX_function(regressor_matrix_diff, P_MARX)

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
# Must predict Delta_Y --> We have as input y_differenced in Forecasting function
Unempl <- df[,2] # (X_lags+1)
y_real <- Unempl[(length(Unempl)-poos+1):length(Unempl)] # prediction must start at Y_316. test set is therefore Z_316-h (f=1)
start_forecast <- length(Unempl)-poos+1
end_forecast <- length(Unempl)
horizons <- list(3, 6, 12, 18, 24) #

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
    shift_y <- shift_y[2:433,] #As the last column is differenced twice, we need to remove first observation
    
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
      
      if(f==1 || f%%24==0 & f!=120){
        
        # specifying the CV technique which will be passed into the train() function later 
        #and number parameter is the "k" in K-fold cross validation
        train_control = trainControl(method = "cv", number = 5, search = "grid")
        
        set.seed(2023)
        
        trainyx <- cbind(train_y, train_x)
        
        # Customising the tuning grid
        gbmGrid <-  expand.grid(max_depth = 6, #Need sources to back up these numbers
                                nrounds = c(50, 100, 200, 500), #Need sources to back up these numbers    # number of trees
                                eta = c(0.01, 0.1, 0.3), # Need sources to back up these numbers
                                gamma = 0, #default
                                subsample = 1, #default
                                min_child_weight = 1, #default
                                colsample_bytree = 0.6) #default
        
        # training a XGboost Regression tree model while tuning parameters
        XGB_tuned = train(train_y~., data = trainyx, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid, verbosity=0)
        
        if(XGB_tuned$bestTune$eta == 0.01){
          gbmGrid <-  expand.grid(max_depth = 6, #Need sources to back up these numbers
                                  nrounds = c(50, 100, 200, 500), #Need sources to back up these numbers    # number of trees
                                  eta = c(0.01, 0.02, 0.03, 0.04, 0.05), # Need sources to back up these numbers
                                  gamma = 0, #default
                                  subsample = 1, #default
                                  min_child_weight = 1, #default
                                  colsample_bytree = 0.6) #default
          
          XGB_tuned = train(train_y~., data = trainyx, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid, verbosity=0)
          print(XGB_tuned$bestTune$eta)
        } 
        
        if(XGB_tuned$bestTune$eta == 0.1){
          gbmGrid <-  expand.grid(max_depth = 6, #Need sources to back up these numbers
                                  nrounds = c(50, 100, 200, 500), #Need sources to back up these numbers    # number of trees
                                  eta = c(0.06, 0.08, 0.1, 0.12, 0.14), # Need sources to back up these numbers
                                  gamma = 0, #default
                                  subsample = 1, #default
                                  min_child_weight = 1, #default
                                  colsample_bytree = 0.6) #default
          
          XGB_tuned = train(train_y~., data = trainyx, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid, verbosity=0)
          print(XGB_tuned$bestTune$eta)
        } 
        if(XGB_tuned$bestTune$eta == 0.3){
          gbmGrid <-  expand.grid(max_depth = 6, #Need sources to back up these numbers
                                  nrounds = c(50, 100, 200, 500), #Need sources to back up these numbers    # number of trees
                                  eta = c(0.2, 0.24, 0.28, 0.32, 0.36), # Need sources to back up these numbers
                                  gamma = 0, #default
                                  subsample = 1, #default
                                  min_child_weight = 1, #default
                                  colsample_bytree = 0.6) #default
          
          XGB_tuned = train(train_y~., data = trainyx, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid, verbosity=0)
          print(XGB_tuned$bestTune$eta)
        } 
        
      }
      
      testyx <- cbind(test_y, test_x)
      
      #add the first difference at the end, only to the dependent variable 
      #Forecasting value = Y_t-h + Delta_Y
      predicted_DeltaY <- predict(XGB_tuned, testyx)
      XGB_y_forecast[f,i] = y_real[f] + predicted_DeltaY
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

XGB_X_forecast2 <- Forecasting_function_XGB(y_differenced, X, poos, horizons) 
RMSE_XGB_X_forecast <- RMSE_function(y_real,XGB_X_forecast)
RMSE_XGB_X_forecast2 <- RMSE_function(y_real,XGB_X_forecast2)

XGB_F_forecast <- Forecasting_function_XGB(y_differenced, F, poos, horizons) 
RMSE_XGB_F_forecast <- RMSE_function(y_real,XGB_F_forecast)

XGB_MAF_forecast2 <- Forecasting_function_XGB(y_differenced, MAF, poos, horizons) 
RMSE_XGB_MAF_forecast2 <- RMSE_function(y_real,XGB_MAF_forecast2)

XGB_MARX_forecast <- Forecasting_function_XGB(y_differenced, MARX, poos, horizons) 
RMSE_XGB_MARX_forecast <- RMSE_function(y_real,XGB_MARX_forecast)



