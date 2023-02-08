#Clearing Environment
rm(list=ls())

#Nikki:
df <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/data-eur2023.csv", row.names=1)

# CPB PA RF Prediction data
CPB_PA_RF_X_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/PA/CPB_PA_RF_X_forecast.csv")
CPB_PA_RF_F_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/PA/CPB_PA_RF_F_forecast.csv")
CPB_PA_RF_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/PA/CPB_PA_RF_MAF_forecast.csv")
CPB_PA_RF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/PA/CPB_PA_RF_MARX_forecast.csv")
CPB_PA_RF_X_F_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/PA/CPB_PA_RF_X_F_forecast.csv")
CPB_PA_RF_X_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/PA/CPB_PA_RF_X_MAF_forecast.csv")
CPB_PA_RF_X_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/PA/CPB_PA_RF_X_MARX_forecast.csv")
CPB_PA_RF_F_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/PA/CPB_PA_RF_F_MAF_forecast.csv")
CPB_PA_RF_F_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/PA/CPB_PA_RF_F_MARX_forecast.csv")
CPB_PA_RF_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/PA/CPB_PA_RF_MAF_MARX_forecast.csv")
CPB_PA_RF_X_F_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/PA/CPB_PA_RF_X_F_MAF_forecast.csv")

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
MARX_function <- function(regressor_matrix, P_MARX, n_var) {
  var = VAR(regressor_matrix, p = P_MARX, type = "const")
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
  mat_x_marx <- data.frame(mat_x_marx) #(NOT IN COULOMBE) --> make dataframe
  mat_x_marx <- mat_x_marx[,1:(ncol(mat_x_marx)-1)] #Remove last column with MARX constant (NOT IN COULOMBE)
  mat_x_marx <- rbind(mat_x_marx[1:P_MARX,]*NA, mat_x_marx) #Add NaN rows on top ; (NOT IN COULOMBE) --> must do this for alignment rows in Z
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
dutch_forecasts <- c(434,315) #Begin forecasting from 315
start_forecast <- 315
end_forecast <- 434
horizons <- list(3, 6, 12, 18, 24)
Unempl <- df[,2]
n_forecast <- end_forecast-start_forecast # CPB time window
y_real <- Unempl[start_forecast:end_forecast]

ntrees <- 200 #Accurate but slow

Forecasting_function <- function(y, Z, n_forecast, horizons, ntrees){
  #RF_y_forecast <- data.frame(matrix(ncol = length(horizons), nrow = n_forecast))
  RF_y_forecast <- data.frame(matrix(ncol = length(horizons), nrow = 1))
  for (f in 1:1){
    c <- 0
    nu <- Sys.time()
    for (h in horizons){
      c <- c+1
      RF_iterated_forecast <- data.frame(matrix(ncol = 1, nrow = h))
      for (i in 1:h){
        shift_y = as.data.frame(shift(y,n=i, type = 'lead', give.names=TRUE))
        colnames(shift_y) = 'y'
        y_Z <- cbind(shift_y, Z)
        y_Z_train <- y_Z[(P_MAF+1):(start_forecast+f-h-1),]
        y_Z_test <- y_Z[start_forecast+f-h,] 
        
        X.rf <- randomForest(y ~ ., 
                             data = y_Z_train, 
                             ntree = ntrees, 
                             mtry = (ncol(Z)/3),
                             na.action = na.omit) # Paper Coulombe (Appendix): ntree=200, mtry=#Z/3
        RF_iterated_forecast[i,1] <- predict(X.rf, y_Z_test) # Predictions
      }
      RF_y_forecast[f,c] <- mean(RF_iterated_forecast[,1])
      colnames(RF_y_forecast)[c]=paste('h=',h,sep='')
    }
    print(Sys.time()-nu)
  }
  return(RF_y_forecast)
}

## -- Forecasting Z -- 
nam <- function(M){
  i<-0
  for (h in horizons){
    i<-i+1
    colnames(M)[i]=paste('h=',h,sep='')
  }
  return(M)
}
CPB_PA_RF_X_forecast <- nam(CPB_PA_RF_X_forecast)
CPB_PA_RF_F_forecast <- nam(CPB_PA_RF_F_forecast)
CPB_PA_RF_MAF_forecast <- nam(CPB_PA_RF_MAF_forecast)
CPB_PA_RF_MARX_forecast <- nam(CPB_PA_RF_MARX_forecast)
CPB_PA_RF_X_F_forecast <- nam(CPB_PA_RF_X_F_forecast)
CPB_PA_RF_X_MAF_forecast <- nam(CPB_PA_RF_X_MAF_forecast)
CPB_PA_RF_X_MARX_forecast <- nam(CPB_PA_RF_X_MARX_forecast)
CPB_PA_RF_F_MAF_forecast <- nam(CPB_PA_RF_F_MAF_forecast)
CPB_PA_RF_F_MARX_forecast <- nam(CPB_PA_RF_F_MARX_forecast)
CPB_PA_RF_MAF_MARX_forecast <- nam(CPB_PA_RF_MAF_MARX_forecast)
CPB_PA_RF_X_F_MAF_forecast <- nam(CPB_PA_RF_X_F_MAF_forecast)

PA_RF_X_forecast <- rbind(Forecasting_function(Unempl, X, n_forecast, horizons, ntrees), CPB_PA_RF_X_forecast)
PA_RF_F_forecast <- rbind(Forecasting_function(Unempl, F, n_forecast, horizons, ntrees), CPB_PA_RF_F_forecast) #1 min
PA_RF_MAF_forecast <- rbind(Forecasting_function(Unempl, MAF, n_forecast, horizons, ntrees), CPB_PA_RF_MAF_forecast) #5 minutes
PA_RF_MARX_forecast <- rbind(Forecasting_function(Unempl, MARX, n_forecast, horizons, ntrees), CPB_PA_RF_MARX_forecast) # 5 minutes
PA_RF_X_F_forecast <- rbind(Forecasting_function(Unempl, X_F, n_forecast, horizons, ntrees), CPB_PA_RF_X_F_forecast) # minutes
PA_RF_X_MAF_forecast <- rbind(Forecasting_function(Unempl, X_MAF, n_forecast, horizons, ntrees), CPB_PA_RF_X_MAF_forecast) # minutes
PA_RF_X_MARX_forecast <- rbind(Forecasting_function(Unempl, X_MARX, n_forecast, horizons, ntrees), CPB_PA_RF_X_MARX_forecast) # hours
PA_RF_F_MAF_forecast <- rbind(Forecasting_function(Unempl, F_MAF, n_forecast, horizons, ntrees), CPB_PA_RF_F_MAF_forecast) # minutes
PA_RF_F_MARX_forecast <- rbind(Forecasting_function(Unempl, F_MARX, n_forecast, horizons, ntrees), CPB_PA_RF_F_MARX_forecast) # minutes
PA_RF_MAF_MARX_forecast <- rbind(Forecasting_function(Unempl, MAF_MARX, n_forecast, horizons, ntrees), CPB_PA_RF_MAF_MARX_forecast) # minutes
PA_RF_X_F_MAF_forecast <- rbind(Forecasting_function(Unempl, X_F_MAF, n_forecast, horizons, ntrees), CPB_PA_RF_X_F_MAF_forecast) # minutes

# PA_RF_X_forecast <- Forecasting_function(Unempl, X, n_forecast, horizons, ntrees) #70 min
# PA_RF_F_forecast <- Forecasting_function(Unempl, F, n_forecast, horizons, ntrees) #30 minutes
# PA_RF_MAF_forecast <- Forecasting_function(Unempl, MAF, n_forecast, horizons, ntrees) # minutes
# PA_RF_MARX_forecast <- Forecasting_function(Unempl, MARX, n_forecast, horizons, ntrees) # minutes
# PA_RF_X_F_forecast <- Forecasting_function(Unempl, X_F, n_forecast, horizons, ntrees) # minutes
# PA_RF_X_MAF_forecast <- Forecasting_function(Unempl, X_MAF, n_forecast, horizons, ntrees) # minutes
# PA_RF_X_MARX_forecast <- Forecasting_function(Unempl, X_MARX, n_forecast, horizons, ntrees) # hours
# PA_RF_F_MAF_forecast <- Forecasting_function(Unempl, F_MAF, n_forecast, horizons, ntrees) # minutes
# PA_RF_F_MARX_forecast <- Forecasting_function(Unempl, F_MARX, n_forecast, horizons, ntrees) # minutes
# PA_RF_MAF_MARX_forecast <- Forecasting_function(Unempl, MAF_MARX, n_forecast, horizons, ntrees) # minutes
# PA_RF_X_F_MAF_forecast <- Forecasting_function(Unempl, X_F_MAF, n_forecast, horizons, ntrees) # minutes
PA_RF_X_F_MARX_forecast <- Forecasting_function(Unempl, X_F_MARX, n_forecast, horizons, ntrees) #
PA_RF_X_MAF_MARX_forecast <- Forecasting_function(Unempl, X_MAF_MARX, n_forecast, horizons, ntrees) #
PA_RF_F_MAF_MARX_forecast <- Forecasting_function(Unempl, F_MAF_MARX, n_forecast, horizons, ntrees) #
PA_RF_X_F_MAF_MARX_forecast <- Forecasting_function(Unempl, X_F_MAF_MARX, n_forecast, horizons, ntrees) #

## -- RMSE Function --
PA_RMSE_RF <- data.frame(matrix(ncol = length(horizons), nrow = n_combinations))

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

# Compute benchmark AR(model)
#msft_ar <- arima(unemployment , order = c(1, 0, 0))

PA_RMSE_RF[1,] <- RMSE_function(y_real, PA_RF_X_forecast)
PA_RMSE_RF[2,] <- RMSE_function(y_real, PA_RF_F_forecast)
PA_RMSE_RF[3,] <- RMSE_function(y_real, PA_RF_MAF_forecast)
PA_RMSE_RF[4,] <- RMSE_function(y_real, PA_RF_MARX_forecast)
PA_RMSE_RF[5,] <- RMSE_function(y_real, PA_RF_X_F_forecast)
PA_RMSE_RF[6,] <- RMSE_function(y_real, PA_RF_X_MAF_forecast)
PA_RMSE_RF[7,] <- RMSE_function(y_real, PA_RF_X_MARX_forecast)
PA_RMSE_RF[8,] <- RMSE_function(y_real, PA_RF_F_MAF_forecast)
PA_RMSE_RF[9,] <- RMSE_function(y_real, PA_RF_F_MARX_forecast)
PA_RMSE_RF[10,] <- RMSE_function(y_real, PA_RF_MAF_MARX_forecast)
PA_RMSE_RF[11,] <- RMSE_function(y_real, PA_RF_X_F_MAF_forecast)
PA_RMSE_RF[12,] <- RMSE_function(y_real, PA_RF_X_F_MARX_forecast)
PA_RMSE_RF[13,] <- RMSE_function(y_real, PA_RF_X_MAF_MARX_forecast)
PA_RMSE_RF[14,] <- RMSE_function(y_real, PA_RF_F_MAF_MARX_forecast)
PA_RMSE_RF[15,] <- RMSE_function(y_real, PA_RF_X_F_MAF_MARX_forecast)

rownames(PA_RMSE_RF) <- c("X", "F", "MAF", "MARX", "X,F", "X,MAF", "X,MARX", "F,MAF", "F,MARX", "MAF,MARX", "X,F,MAF", "X,F,MARX", "X,MAF,MARX", "F,MAF,MARX", "X,F,MAF,MARX")
colnames(PA_RMSE_RF) <- c("h=3", "h=6", "h=12", "h=18", "h=24")

# Saving Prediction Tables
write.csv(PA_RF_X_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_X_forecast.csv", row.names=FALSE)
write.csv(PA_RF_F_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_F_forecast.csv", row.names=FALSE)
write.csv(PA_RF_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_MAF_forecast.csv", row.names=FALSE)
write.csv(PA_RF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_MARX_forecast.csv", row.names=FALSE)
write.csv(PA_RF_X_F_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_X_F_forecast.csv", row.names=FALSE)
write.csv(PA_RF_X_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_X_MAF_forecast.csv", row.names=FALSE)
write.csv(PA_RF_X_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_X_MARX_forecast.csv", row.names=FALSE)
write.csv(PA_RF_F_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_F_MAF_forecast.csv", row.names=FALSE)
write.csv(PA_RF_F_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_F_MARX_forecast.csv", row.names=FALSE)
write.csv(PA_RF_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(PA_RF_X_F_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_X_F_MAF_forecast.csv", row.names=FALSE)
write.csv(PA_RF_X_F_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_X_F_MARX_forecast.csv", row.names=FALSE)
write.csv(PA_RF_X_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_X_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(PA_RF_F_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_F_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(PA_RF_X_F_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB_PA_RF_X_F_MAF_MARX_forecast.csv", row.names=FALSE)

write.csv(PA_RMSE_RF, "C:/Users/Gebruiker/Documents/GitHub/project_cpb/Data en Forecasts/Output/CPB/PA_rmse_rf.csv", row.names=TRUE)

