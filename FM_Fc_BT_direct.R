#Clearing Environment
rm(list=ls())

## -- Importing Data --
#Nikki:
df <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/data-eur2023.csv", row.names=1)

### ---- FEATURE ENGINEERING ----
library(vars)
X_t <- df[-c(1,2)] #Remove pubdate and dependent variable
n_var <- ncol(X_t)
T <- nrow(X_t)
X_lags <- 4
n_Factors <- 3 
F_lags <- 4 #Paper Coulombe (check)
P_MAF <- 12 #Paper Coulombe
n_MAF <- 3 #Paper Coulombe
P_MARX <- 12 #Paper Coulombe

## - X - 
X_function <- function(X_t, X_lags, n_var){
  X <- data.frame(matrix(ncol = (X_lags+1)*n_var, nrow = T))
  X[,1:n_var] <- X_t
  for (l in 1:X_lags){
    X[,(l*n_var+1):((l+1)*n_var)] <- rbind(X_t[1:l,]*NA, head(X_t, - l))
  }
  colnames(X)=paste('X_',colnames(X),sep='')
  return(X)
}

## - F - 
F_function <- function(X, n_Factors, F_lags, T){
  factors_t <- function(X, n_Factors) {
    X_pca <- prcomp(X, center = TRUE,scale. = TRUE)
    factors_t <- X_pca$x[1:n_Factors]
    return(factors_t)
  }
  F <- data.frame(matrix(ncol = (n_Factors*(F_lags+1)), nrow = T))
  for (t in n_Factors:T){
    F[t,1:n_Factors] <- factors_t(X_t[1:t,], n_Factors)
  }
  for (i in 1:F_lags){
    F[(n_Factors+i):T,(i*n_Factors+1):((i+1)*n_Factors)] <- head(F[n_Factors:T,1:n_Factors], - i)
  }
  colnames(F)=paste('F_',colnames(F),sep='')
  return(F)
}

## - MAF - 
MAF_function <- function(X, T, n_var, P_MAF, n_MAF) {
  MAF <- data.frame(matrix(ncol = (n_MAF*n_var), nrow = T))
  for (v in 1:n_var){
    for (t in (P_MAF+1):T){
      X_pca <- prcomp(X_t[(t-P_MAF):t,v], center = TRUE,scale. = TRUE)
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
X <- X_function(X_t, X_lags, n_var)
F <- F_function(X_t, n_Factors, F_lags, T)
MAF <- MAF_function(X_t, T, n_var, P_MAF, n_MAF)
MARX <- MARX_function(X_t, P_MARX, n_var)

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
library(doParallel)

Unempl <- df[c(2)] 
y_real <- df[316:434,2]
horizons <- list(3, 6, 12, 18, 24)
#horizons <- list(3)
n_forecast <- 434-315 # Check timepoints for training and test set!

Forecasting_function <- function(y, Z, n_forecast, horizons){
  y_Z <- cbind(y, Z)
  BT_y_forecast <- data.frame(matrix(ncol = length(horizons), nrow = n_forecast))
  
  i <- 0
  for (h in horizons){
    i <- i+1
    for (f in 1:n_forecast){
      # Boosted Trees
      train_x <- y_Z[1:(315+f-h),2:ncol(y_Z)]
      train_y <- y_Z[1:(315+f-h),1]
      test_x <- y_Z[315+f,2:ncol(y_Z)]
      test_y <- y_Z[315+f,1]
      
      #Optimization
      set.seed(2021)
      
      no_cores <- detectCores() / 2 # use half of CPU cores to avoid crash  
      cl <- makeCluster(no_cores) # make a cluster
      registerDoParallel(cl) # register a parallel backend
      clusterExport(cl, c('train_x','train_y')) # import objects outside
      clusterEvalQ(cl,expr= { # launch library to be used in FUN
        library(xgboost)
      })
      
      scoring_function <- function(eta) {
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
            Score = min(xgbcv$evaluation_log$test_rmse_mean), #max --> min; we MINIMIZE RMSE right?
            nrounds = xgbcv$best_iteration #iteration number with the best evaluation metric value
          )
        )
      }
      bounds <- list(eta = c(0, 1))
      
      opt_obj <- bayesOpt(FUN = scoring_function, bounds = bounds,
          initPoints = 3, #Must be more than number of input in scoring function
          iters.n = 2, #Number of Epochs, runs 2 times to find global optimum
          parallel = TRUE)
        
      stopCluster(cl) # stop the cluster
      registerDoSEQ() # back to serial computing
      
      # take the optimal parameters for xgboost()
      print(getBestPars(opt_obj)[1])
      params <- list(eta = getBestPars(opt_obj)[1])
      
      # the numrounds which gives the max Score (rmse)
      #print(opt_obj$scoreSummary)
      numrounds <- opt_obj$scoreSummary$nrounds[
        which(opt_obj$scoreSummary$Score
              == max(opt_obj$scoreSummary$Score))]
      print(numrounds)
      
      X_BT_tuned <- xgboost(params = params,
                           data = as.matrix(train_x),
                           label = as.matrix(train_y),
                           nrounds = numrounds,
                           max.depth = 5,
                           eval_metric = "rmse",
                           verbose = 0)

      xgb_test <- xgb.DMatrix(data = as.matrix(test_x), label = as.matrix(test_y))
      
      BT_y_forecast[f,i] = predict(X_BT_tuned, xgb_test)

    }
    colnames(BT_y_forecast)[i]=paste('h=',h,sep='')
  }
  return(BT_y_forecast)
}

## -- Forecasting Z -- 
Z <- X
BT_X_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- F
BT_F_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- MAF
BT_MAF_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- MARX
BT_MARX_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- X_F
BT_X_F_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- X_MAF
BT_X_MAF_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- X_MARX
BT_X_MARX_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- F_MAF
BT_F_MAF_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- F_MARX
BT_F_MARX_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- MAF_MARX
BT_MAF_MARX_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- X_F_MAF
BT_X_F_MAF_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- X_F_MARX
BT_X_F_MARX_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- X_MAF_MARX
BT_X_MAF_MARX_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- F_MAF_MARX
BT_F_MAF_MARX_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

Z <- X_F_MAF_MARX
BT_X_F_MAF_MARX_forecast <- Forecasting_function(Unempl, Z, n_forecast, horizons)

## -- RMSE Function --
RMSE_BT <- data.frame(matrix(ncol = length(horizons), nrow = n_combinations))

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
write.csv(BT_X_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_X_forecast.csv", row.names=FALSE)
write.csv(BT_F_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_F_forecast.csv", row.names=FALSE)
write.csv(BT_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_MAF_forecast.csv", row.names=FALSE)
write.csv(BT_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_X_F_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_X_F_forecast.csv", row.names=FALSE)
write.csv(BT_X_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_X_MAF_forecast.csv", row.names=FALSE)
write.csv(BT_X_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_X_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_F_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_F_MAF_forecast.csv", row.names=FALSE)
write.csv(BT_F_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_F_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_X_F_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_X_F_MAF_forecast.csv", row.names=FALSE)
write.csv(BT_X_F_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_X_F_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_X_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_X_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_F_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_F_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_X_F_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/BT_X_F_MAF_MARX_forecast.csv", row.names=FALSE)

write.csv(RMSE_BT, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RMSE_BT.csv", row.names=TRUE)


## EXTRA
# Plotting RF Forecast with Z=X
error_Xrf <- abs(y_real - y_forecast)
time <- seq(1, n_forecast, 1)

plot(time, y_real, type = "l", frame = FALSE, pch = 19, 
     col = "red", xlab = "x", ylab = "y")
lines(time, y_forecast[,1], pch = 18, col = "blue", type = "l", lty = 2)
legend("topright", legend=c("Real y", "Forecast y (RF, Z=X)"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)

#Y lags <-- Do we put this in Z?
n_Ylags <- 12

Ylags_function <- function(Y, n_Ylags){
  Ylags <- Y
  for (l in 1:n_Ylags){
    new_y <- rbind(head(Y,-(T-l))*NA, head(Y, - l))
    colnames(new_y)=paste('Lag',l,sep='')
    Ylags <- cbind(Ylags, new_y)
  }
  colnames(Ylags)=paste('Ylags_',colnames(Ylags),sep='')
  return(Ylags)
}

Unempl_lags <- Ylags_function(Unempl, n_Ylags)
