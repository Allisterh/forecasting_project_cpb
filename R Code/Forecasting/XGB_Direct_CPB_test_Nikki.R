#Clearing Environment
rm(list=ls())

#Nikki:
df <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/data-eur2023.csv", row.names=1)

### ---- FEATURE ENGINEERING ----
library(vars)
library(randomForest)
regressor_matrix <- df[-c(1,2)] #Remove pubdate and dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)
X_lags <- 6 #average 4 and 7 (AR)
n_Factors <- 2 #Optimization
F_lags <- 6 #Average of 4 and 7 (AR)
P_MAF <- 6 #Average of 4 and 7 (AR)
n_MAF <- 6 #Optimization
P_MARX <- 6 #Average of 4 and 7 (AR)

## - X - 
library(data.table)
X_function <- function(regressor_matrix, X_lags){
  return(as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE)))
} 

## - F - 
factors_t <- function(regressor_matrix, n_Factors, boolo) {
  X_pca <- prcomp(regressor_matrix, center = boolo,scale. = boolo)
  factors_t <- X_pca$x[1:n_Factors]
  return(factors_t)
}

F_function <- function(regressor_matrix, n_Factors, F_lags, T, boolo){
  F <- data.frame(matrix(ncol = n_Factors, nrow = T))
  for (t in n_Factors:T){
    F[t,1:n_Factors] <- factors_t(regressor_matrix[1:t,], n_Factors, boolo)
  }
  F <- as.data.frame(shift(F, n=0:F_lags, type = 'lag', give.names=TRUE)) #shift function for lags of factors
  return(F)
}

## - MAF - 
MAF_function <- function(X, X_lags, T, n_var, P_MAF, n_MAF, boolo) { #Use X (Feature matrix with lags) as input!
  MAF <- data.frame(matrix(ncol = (n_MAF*n_var), nrow = T))
  for (v in 1:n_var){
    for (t in (2*P_MAF+1):T){
      X_pca <- prcomp(X[(t-P_MAF):t,(((v-1)*X_lags)+1):(v*X_lags)], center = boolo, scale. = boolo)
      MAF[t,(1+(v-1)*n_MAF):(v*n_MAF)] <- X_pca$x[1:n_MAF]
    }
  }
  colnames(MAF)=paste('MAF_',colnames(MAF),sep='')
  return(MAF)
}

## - MARX - 
MARX_function <- function(X, P_MARX, n_var) {
  var = VAR(X, p = P_MARX, type = "const")
  matata = as.matrix(var$datamat)
  mat_y = matata[,1:n_var] # Datamatrix Y_t
  mat_x = matata[,-c(1:n_var)] # Datamatrices Y_(t-1) ... Y_(t-P_MARX)
  mat_x_marx = mat_x
  for(l in 2:P_MARX){
    for(v in 1:n_var){
      whotoavg=seq(from=v,to=n_var*(l-1)+v,by=n_var) # List of indeces, selecting values in datamatrix mat_x --> to average
      mat_x_marx[,n_var*(l-1)+v]=apply(mat_x[,whotoavg],1,mean)
    }}
  colnames(mat_x_marx)=paste('MARX_',colnames(mat_x),sep='')
  mat_x_marx <- mat_x_marx[,1:(ncol(mat_x_marx)-1)] #Remove last column with MARX constant
  mat_x_marx <- data.frame(mat_x_marx)
  mat_x_marx <- rbind(mat_x_marx[1:P_MARX,]*NA, mat_x_marx) #Add NaN rows on top 
  return(mat_x_marx)
}

## -- Combinations of Z --
X <- X_function(regressor_matrix, X_lags)

scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags, T, FALSE)

scaled_X <- scale(X) # Scale Feature X for PCA in MAF
MAF <- MAF_function(scaled_X, X_lags, T, n_var, P_MAF, n_MAF, FALSE)

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

n_combinations <- 15

### ---- FORECASTING ----
# -- Forecasting Function -- 
library(ParBayesianOptimization)
library(xgboost)

dutch_forecasts <- c(434,315) #Begin forecasting from 315
start_forecast <- 315
end_forecast <- 434
horizons <- list(3, 6, 12, 18, 24)
Unempl <- df[,2]
n_forecast <- end_forecast-start_forecast # CPB time window
y_real <- Unempl[start_forecast:end_forecast]

#ntrees <- 200 #Accurate but slow (RF)

Forecasting_function <- function(y, Z, n_forecast, horizons){
  y_Z <- cbind(y, Z)
  BT_y_forecast <- data.frame(matrix(ncol = length(horizons), nrow = n_forecast))
  
  i <- 0
  for (h in horizons){
    shift_y = as.data.frame(shift(y,n=h, type = 'lead', give.names=TRUE))
    colnames(shift_y) = 'y'
    y_Z <- cbind(shift_y, Z)
    
    i <- i+1
    
    train_x <- y_Z[(P_MAF+1):start_forecast,2:ncol(y_Z)]
    train_y <- y_Z[(P_MAF+1):start_forecast,1]
    
    #Optimization
    set.seed(2021)
    
    scoring_function <- function(eta) {
      library(xgboost)
      dtrain <- xgb.DMatrix(as.matrix(train_x), label = as.matrix(train_y), missing = NA)
      
      pars <- list(eta = 0.3) #default, to be tuned
      
      xgbcv <- xgb.cv(
        params = pars,
        data = dtrain,
        nfold = 5, #Coulombe
        nrounds = 500, #Coulombe, max boosting rounds
        prediction = TRUE,
        showsd = TRUE,
        early_stopping_rounds = 10,
        maximize = FALSE, #TRUE --> FALSE : RMSE is error, so we minimize right?
        stratified = FALSE) # Do we set this to TRUE or FALSE?
      
      return(list(
        Score = max(-xgbcv$evaluation_log$test_rmse_mean), #max --> min; we MINIMIZE RMSE right?
        nrounds = xgbcv$best_iteration #iteration number with the best evaluation metric value
      )
      )
    }
    bounds <- list(eta = c(0, 1))
    
    opt_obj <- bayesOpt(FUN = scoring_function, bounds = bounds,
                        initPoints = 3, #Must be more than number of input in scoring function --> TEST WHETHER HIGHER NR EPOCHS INCREASES ACCURACY
                        iters.n = 2, #Number of Epochs, runs 2 times to find global optimum
                        parallel = FALSE)
    
    # take the optimal parameters for xgboost()
    print(getBestPars(opt_obj)[1])
    params <- list(eta = getBestPars(opt_obj)[1])
    
    # the numrounds which gives the max Score (rmse) --> CHANGE TO MIN??
    #print(opt_obj$scoreSummary)
    numrounds <- opt_obj$scoreSummary$nrounds[
      which(opt_obj$scoreSummary$Score
            == max(opt_obj$scoreSummary$Score))] #--> CHANGE TO MIN????
    print(numrounds)
    
    #for (f in 1:n_forecast){
    for (f in 1:5){
      # Boosted Trees
      #print(f)
      train_x <- y_Z[(P_MAF+1):(start_forecast+f-1),2:ncol(y_Z)]
      train_y <- y_Z[(P_MAF+1):(start_forecast+f-1),1]
      test_x <- y_Z[start_forecast+f,2:ncol(y_Z)]
      test_y <- y_Z[start_forecast+f,1]
      
      X_BT_tuned <- xgboost(params = params,
                            data = as.matrix(train_x),
                            label = as.matrix(train_y),
                            nrounds = numrounds, #max number of rounds to tune trained model
                            max.depth = 5, #Coulombe
                            eval_metric = "rmse", #regression
                            verbose = 0) #NIET output printen 
      
      xgb_test <- xgb.DMatrix(data = as.matrix(test_x), label = as.matrix(test_y))
      
      BT_y_forecast[f,i] = predict(X_BT_tuned, xgb_test)
      
    }
    colnames(BT_y_forecast)[i]=paste('h=',h,sep='')
  }
  return(BT_y_forecast)
}

Forecasting_function2 <- function(y, Z, n_forecast, horizons){
  y_Z <- cbind(y, Z)
  BT_y_forecast <- data.frame(matrix(ncol = length(horizons), nrow = n_forecast))
  
  i <- 0
  for (h in horizons){
    shift_y = as.data.frame(shift(y,n=h, type = 'lead', give.names=TRUE))
    colnames(shift_y) = 'y'
    y_Z <- cbind(shift_y, Z)
    
    i <- i+1
    
    train_x <- y_Z[(P_MAF+1):start_forecast,2:ncol(y_Z)]
    train_y <- y_Z[(P_MAF+1):start_forecast,1]
    
    #Optimization
    set.seed(2021)
  
    params <- list(eta = 0.3)
    numrounds <- 150
    
    for (f in 1:n_forecast){
    #for (f in 1:5){
      # Boosted Trees
      #print(f)
      train_x <- y_Z[(P_MAF+1):(start_forecast+f-1),2:ncol(y_Z)]
      train_y <- y_Z[(P_MAF+1):(start_forecast+f-1),1]
      test_x <- y_Z[start_forecast+f,2:ncol(y_Z)]
      test_y <- y_Z[start_forecast+f,1]
      
      X_BT_tuned <- xgboost(params = params,
                            data = as.matrix(train_x),
                            label = as.matrix(train_y),
                            nrounds = numrounds, #max number of rounds to tune trained model
                            max.depth = 5, #Coulombe
                            eval_metric = "rmse", #regression
                            verbose = 0) #NIET output printen 
      
      xgb_test <- xgb.DMatrix(data = as.matrix(test_x), label = as.matrix(test_y))
      
      BT_y_forecast[f,i] = predict(X_BT_tuned, xgb_test)
      
    }
    colnames(BT_y_forecast)[i]=paste('h=',h,sep='')
  }
  return(BT_y_forecast)
}
BT_X_forecast2 <- Forecasting_function2(Unempl, X, n_forecast, horizons)
BT_F_forecast2 <- Forecasting_function2(Unempl, F, n_forecast, horizons)
BT_MAF_forecast2 <- Forecasting_function2(Unempl, MAF, n_forecast, horizons)
BT_MARX_forecast2 <- Forecasting_function2(Unempl, MARX, n_forecast, horizons)
BT_X_F_forecast2 <- Forecasting_function2(Unempl, X_F, n_forecast, horizons)
BT_X_MAF_forecast2 <- Forecasting_function2(Unempl, X_MAF, n_forecast, horizons)
BT_X_MARX_forecast2 <- Forecasting_function2(Unempl, X_MARX, n_forecast, horizons)
BT_F_MAF_forecast2 <- Forecasting_function2(Unempl, F_MAF, n_forecast, horizons)
BT_F_MARX_forecast2 <- Forecasting_function2(Unempl, F_MARX, n_forecast, horizons)
BT_MAF_MARX_forecast2 <- Forecasting_function2(Unempl, MAF_MARX, n_forecast, horizons)
BT_X_F_MAF_forecast2 <- Forecasting_function2(Unempl, X_F_MAF, n_forecast, horizons)
BT_X_F_MARX_forecast2 <- Forecasting_function2(Unempl, X_F_MARX, n_forecast, horizons)
BT_X_MAF_MARX_forecast2 <- Forecasting_function2(Unempl, X_MAF_MARX, n_forecast, horizons)
BT_F_MAF_MARX_forecast2 <- Forecasting_function2(Unempl, F_MAF_MARX, n_forecast, horizons)
BT_X_F_MAF_MARX_forecast2 <- Forecasting_function2(Unempl, X_F_MAF_MARX, n_forecast, horizons)

## -- Forecasting Z -- 
BT_X_forecast <- Forecasting_function(Unempl, X, n_forecast, horizons)
BT_F_forecast <- Forecasting_function(Unempl, F, n_forecast, horizons)
BT_MAF_forecast <- Forecasting_function(Unempl, MAF, n_forecast, horizons)
BT_MARX_forecast <- Forecasting_function(Unempl, MARX, n_forecast, horizons)
BT_X_F_forecast <- Forecasting_function(Unempl, X_F, n_forecast, horizons)
BT_X_MAF_forecast <- Forecasting_function(Unempl, X_MAF, n_forecast, horizons)
BT_X_MARX_forecast <- Forecasting_function(Unempl, X_MARX, n_forecast, horizons)
BT_F_MAF_forecast <- Forecasting_function(Unempl, F_MAF, n_forecast, horizons)
BT_F_MARX_forecast <- Forecasting_function(Unempl, F_MARX, n_forecast, horizons)
BT_MAF_MARX_forecast <- Forecasting_function(Unempl, MAF_MARX, n_forecast, horizons)
BT_X_F_MAF_forecast <- Forecasting_function(Unempl, X_F_MAF, n_forecast, horizons)
BT_X_F_MARX_forecast <- Forecasting_function(Unempl, X_F_MARX, n_forecast, horizons)
BT_X_MAF_MARX_forecast <- Forecasting_function(Unempl, X_MAF_MARX, n_forecast, horizons)
BT_F_MAF_MARX_forecast <- Forecasting_function(Unempl, F_MAF_MARX, n_forecast, horizons)
BT_X_F_MAF_MARX_forecast <- Forecasting_function(Unempl, X_F_MAF_MARX, n_forecast, horizons)

## -- RMSE Function --
RMSE_BT <- data.frame(matrix(ncol = length(horizons), nrow = n_combinations))

# ARE THE Y AND Y-REAL ALIGNED?
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

RMSE_BT[1,] <- RMSE_function(y_real, BT_X_forecast)
RMSE_BT[2,] <- RMSE_function(y_real, BT_F_forecast)
RMSE_BT[3,] <- RMSE_function(y_real, BT_MAF_forecast)
RMSE_BT[4,] <- RMSE_function(y_real, BT_MARX_forecast)
RMSE_BT[5,] <- RMSE_function(y_real, BT_X_F_forecast)
RMSE_BT[6,] <- RMSE_function(y_real, BT_X_MAF_forecast)
RMSE_BT[7,] <- RMSE_function(y_real, BT_X_MARX_forecast)
RMSE_BT[8,] <- RMSE_function(y_real, BT_F_MAF_forecast)
RMSE_BT[9,] <- RMSE_function(y_real, BT_F_MARX_forecast)
RMSE_BT[10,] <- RMSE_function(y_real, BT_MAF_MARX_forecast)
RMSE_BT[11,] <- RMSE_function(y_real, BT_X_F_MAF_forecast)
RMSE_BT[12,] <- RMSE_function(y_real, BT_X_F_MARX_forecast)
RMSE_BT[13,] <- RMSE_function(y_real, BT_X_MAF_MARX_forecast)
RMSE_BT[14,] <- RMSE_function(y_real, BT_F_MAF_MARX_forecast)
RMSE_BT[15,] <- RMSE_function(y_real, BT_X_F_MAF_MARX_forecast)

rownames(RMSE_BT) <- c("X", "F", "MAF", "MARX", "X,F", "X,MAF", "X,MARX", "F,MAF", "F,MARX", "MAF,MARX", "X,F,MAF", "X,F,MARX", "X,MAF,MARX", "F,MAF,MARX", "X,F,MAF,MARX")
colnames(RMSE_BT) <- c("h=3", "h=6", "h=12", "h=18", "h=24")

# Saving Prediction Tables
write.csv(BT_X_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_X_forecast.csv", row.names=FALSE)
write.csv(BT_F_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_F_forecast.csv", row.names=FALSE)
write.csv(BT_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_MAF_forecast.csv", row.names=FALSE)
write.csv(BT_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_X_F_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_X_F_forecast.csv", row.names=FALSE)
write.csv(BT_X_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_X_MAF_forecast.csv", row.names=FALSE)
write.csv(BT_X_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_X_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_F_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_F_MAF_forecast.csv", row.names=FALSE)
write.csv(BT_F_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_F_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_X_F_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_X_F_MAF_forecast.csv", row.names=FALSE)
write.csv(BT_X_F_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_X_F_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_X_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_X_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_F_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_F_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_X_F_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_BT_X_F_MAF_MARX_forecast.csv", row.names=FALSE)

write.csv(RMSE_BT, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_RMSE_BT.csv", row.names=TRUE)
