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
library(randomForest)
Unempl <- df[c(2)] 
y_real <- df[316:434,2]
#horizons <- list(3, 6, 12, 18, 24)
horizons <- list(3, 6)
n_forecast <- 434-315 # Check timepoints for training and test set!

Forecasting_PA_function <- function(y, Z, n_forecast, horizons){
  y_Z <- cbind(y, Z)
  RF_y_forecast <- data.frame(matrix(ncol = length(horizons), nrow = n_forecast))
  #BF_y_forecast <- ...
  
  i <- 0
  for (h in horizons){
    i <- i+1
    RF_iterated_forecast <- data.frame(matrix(ncol = i, nrow = h))
    for (f in 1:n_forecast){
      for (n in 1:h){ # Loop to predict every horizon step before averaging
        y_Z_train <- y_Z[1:(315+f-h),]
        y_Z_test <- y_Z[315+f-h+n,] 
        
        # Random Forest
        X.rf <- randomForest(L2_LRHUTTTT ~ ., 
                             data = y_Z_train, 
                             ntree = 200, 
                             mtry = (ncol(Z)/3),
                             na.action = na.omit) # Paper Coulombe (Appendix): ntree=200, mtry=#Z/3
        RF_iterated_forecast[n,i] <- predict(X.rf, y_Z_test) # Predictions
        
        # Boosted Trees
        #X.BT <- ...
        #BT_y_forecast[f,] <- ...
      }
      RF_y_forecast[f,i] <- mean(RF_iterated_forecast[,i])
    }  
    colnames(RF_y_forecast)[i]=paste('h=',h,sep='')
  }
  return(RF_y_forecast)
}

## -- Forecasting Z -- 
Z <- X
RF_PA_X_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- F
RF_PA_F_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- MAF
RF_PA_MAF_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- MARX
RF_PA_MARX_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- X_F
RF_PA_X_F_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- X_MAF
RF_PA_X_MAF_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- X_MARX
RF_PA_X_MARX_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- F_MAF
RF_PA_F_MAF_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- F_MARX
RF_PA_F_MARX_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- MAF_MARX
RF_PA_MAF_MARX_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- X_F_MAF
RF_PA_X_F_MAF_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- X_F_MARX
RF_PA_X_F_MARX_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- X_MAF_MARX
RF_PA_X_MAF_MARX_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- F_MAF_MARX
RF_PA_F_MAF_MARX_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

Z <- X_F_MAF_MARX
RF_PA_X_F_MAF_MARX_forecast <- Forecasting_PA_function(Unempl, Z, n_forecast, horizons)

## -- RMSE Function --
RMSE_RF <- data.frame(matrix(ncol = length(horizons), nrow = n_combinations))

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

RMSE_PA_RF[1,] <- RMSE_function(y_real, RF_PA_X_forecast)
RMSE_PA_RF[2,] <- RMSE_function(y_real, RF_PA_F_forecast)
RMSE_PA_RF[3,] <- RMSE_function(y_real, RF_PA_MAF_forecast)
RMSE_PA_RF[4,] <- RMSE_function(y_real, RF_PA_MARX_forecast)
RMSE_PA_RF[5,] <- RMSE_function(y_real, RF_PA_X_F_forecast)
RMSE_PA_RF[6,] <- RMSE_function(y_real, RF_PA_X_MAF_forecast)
RMSE_PA_RF[7,] <- RMSE_function(y_real, RF_PA_X_MARX_forecast)
RMSE_PA_RF[8,] <- RMSE_function(y_real, RF_PA_F_MAF_forecast)
RMSE_PA_RF[9,] <- RMSE_function(y_real, RF_PA_F_MARX_forecast)
RMSE_PA_RF[10,] <- RMSE_function(y_real, RF_PA_MAF_MARX_forecast)
RMSE_PA_RF[11,] <- RMSE_function(y_real, RF_PA_X_F_MAF_forecast)
RMSE_PA_RF[12,] <- RMSE_function(y_real, RF_PA_X_F_MARX_forecast)
RMSE_PA_RF[13,] <- RMSE_function(y_real, RF_PA_X_MAF_MARX_forecast)
RMSE_PA_RF[14,] <- RMSE_function(y_real, RF_PA_F_MAF_MARX_forecast)
RMSE_PA_RF[15,] <- RMSE_function(y_real, RF_PA_X_F_MAF_MARX_forecast)

rownames(RMSE_PA_RF) <- c("X", "F", "MAF", "MARX", "X,F", "X,MAF", "X,MARX", "F,MAF", "F,MARX", "MAF,MARX", "X,F,MAF", "X,F,MARX", "X,MAF,MARX", "F,MAF,MARX", "X,F,MAF,MARX")
colnames(RMSE_PA_RF) <- c("h=3", "h=6", "h=12", "h=18", "h=24")

# Saving Prediction Tables
write.csv(RF_PA_X_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_X_forecast.csv", row.names=FALSE)
write.csv(RF_PA_F_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_F_forecast.csv", row.names=FALSE)
write.csv(RF_PA_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_MAF_forecast.csv", row.names=FALSE)
write.csv(RF_PA_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_PA_X_F_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_X_F_forecast.csv", row.names=FALSE)
write.csv(RF_PA_X_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_X_MAF_forecast.csv", row.names=FALSE)
write.csv(RF_PA_X_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_X_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_PA_F_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_F_MAF_forecast.csv", row.names=FALSE)
write.csv(RF_PA_F_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_F_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_PA_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_PA_X_F_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_X_F_MAF_forecast.csv", row.names=FALSE)
write.csv(RF_PA_X_F_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_X_F_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_PA_X_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_X_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_PA_F_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_F_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_PA_X_F_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_PA_X_F_MAF_MARX_forecast.csv", row.names=FALSE)

write.csv(RMSE_PA_RF, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RMSE_PA_RF.csv", row.names=TRUE)
