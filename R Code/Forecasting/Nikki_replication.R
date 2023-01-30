# Dees forecasting replication Coulombe
library(BVAR)
library(randomForest)

#Nikki:
#df <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/data-eur2023.csv", row.names=1)
df <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/data122022.csv", row.names=1)

# Dees:
#df <- fredmd("D:/EUR/Master/Seminar Case studies in Applied Econometrics/R code/data122022.csv")
row.names(df) <- df[,1]
unemployment <- as.data.frame(df$UNRATE)
unemployment <- as.data.frame(unemployment[3:765,])
row.names(unemployment) <- row.names(df)[3:765]

df_wo_unrate <- df[-c(1,25) ]
df_wo_unrate <- df_wo_unrate[3:765,] # Delete first two rows because we transform and some do second differencign
# Delete columns with NA values/that do not have data from 1960 onwards
new_df <- df_wo_unrate[ , colSums(is.na(df_wo_unrate))==0]
scaled_df <- as.data.frame(scale(new_df))

### ---- FEATURE ENGINEERING ----
library(vars)
n_var <- ncol(new_df)
T <- nrow(new_df)
regressor_matrix <- new_df
X_lags <- 12
n_Factors <- 3 
F_lags <- 12 #Paper Coulombe (check FREDMD: Coulombe gebruikt er 12, CPB wil er 4)
P_MAF <- 12 #Paper Coulombe
n_MAF <- 3 #Paper Coulombe
P_MARX <- 12 #Paper Coulombe

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

#for (i in 1:ncol(regressor_matrix)) {
 # print(paste(colnames(regressor_matrix)[i],var(regressor_matrix[,i]),sep = ' '))}

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
Unempl <- unemployment
rownames(Unempl) <- as.data.frame(rownames(unemployment))
horizons <- list(3) # 6, 12, 18, 24
n_forecast <- 706-251 # Coulombe time window frame 1980M1 - 2017M12
y_real <- unemployment[251:705,]

dutch_forecasts <- c(435,315)
coulombe_forecasts <- c(706,251)

Forecasting_function <- function(y, Z, n_forecast, horizons, ntrees){
  RF_y_forecast <- data.frame(matrix(ncol = length(horizons), nrow = n_forecast))
  i <- 0
  
  for (h in horizons){
    shift_y = as.data.frame(shift(y,n=h, type = 'lead', give.names=TRUE))
    colnames(shift_y) = 'y'
    y_Z <- cbind(shift_y, Z)
    i <- i+1
    nu <- Sys.time()
    for (f in 1:n_forecast){
      y_Z_train <- y_Z[11:250+f-1,]
      y_Z_test <- y_Z[250+f,]
      
      # Random Forest
      X.rf <- randomForest(y ~ ., 
                           data = y_Z_train, 
                           ntree = ntrees, 
                           mtry = (ncol(Z)/3),
                           na.action = na.omit) # Paper Coulombe (Appendix): ntree=200, mtry=#Z/3
      RF_y_forecast[f,i] <- predict(X.rf, y_Z_test) # Predictions
      
      print(f)
      print(Sys.time()-nu)
    }
    colnames(RF_y_forecast)[i]=paste('h=',h,sep='')
    print(h)
  }
  return(RF_y_forecast)
}

## -- Forecasting Z -- 
ntrees <- 200

RF_X_forecast <- Forecasting_function(Unempl, X, n_forecast, horizons, ntrees) #50 minutes
RF_F_forecast <- Forecasting_function(Unempl, F, n_forecast, horizons, ntrees) #5 minutes
RF_MAF_forecast <- Forecasting_function(Unempl, MAF, n_forecast, horizons, ntrees) #10 minutes
RF_MARX_forecast <- Forecasting_function(Unempl, MARX, n_forecast, horizons, ntrees) #50 minutes
RF_X_F_forecast <- Forecasting_function(Unempl, X_F, n_forecast, horizons, ntrees) #60 minutes
RF_X_MAF_forecast <- Forecasting_function(Unempl, X_MAF, n_forecast, horizons, ntrees) #90 minutes
RF_X_MARX_forecast <- Forecasting_function(Unempl, X_MARX, n_forecast, horizons, ntrees) #2 hours
RF_F_MAF_forecast <- Forecasting_function(Unempl, F_MAF, n_forecast, horizons, ntrees) #15 minutes
RF_F_MARX_forecast <- Forecasting_function(Unempl, F_MARX, n_forecast, horizons, ntrees) #50 minutes
RF_MAF_MARX_forecast <- Forecasting_function(Unempl, MAF_MARX, n_forecast, horizons, ntrees) #60 minutes
RF_X_F_MAF_forecast <- Forecasting_function(Unempl, X_F_MAF, n_forecast, horizons, ntrees) #75 minutes
RF_X_F_MARX_forecast <- Forecasting_function(Unempl, X_F_MARX, n_forecast, horizons, ntrees) #2.3h
RF_X_MAF_MARX_forecast <- Forecasting_function(Unempl, X_MAF_MARX, n_forecast, horizons, ntrees) #3h
RF_F_MAF_MARX_forecast <- Forecasting_function(Unempl, F_MAF_MARX, n_forecast, horizons, ntrees) #1.5h
RF_X_F_MAF_MARX_forecast <- Forecasting_function(Unempl, X_F_MAF_MARX, n_forecast, horizons, ntrees) #2h

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

# Compute benchmark AR(model)
msft_ar <- arima(unemployment , order = c(1, 0, 0))

RMSE_RF[1,] <- RMSE_function(y_real, RF_X_forecast)
RMSE_RF[2,] <- RMSE_function(y_real, RF_F_forecast)
RMSE_RF[3,] <- RMSE_function(y_real, RF_MAF_forecast)
RMSE_RF[4,] <- RMSE_function(y_real, RF_MARX_forecast)
RMSE_RF[5,] <- RMSE_function(y_real, RF_X_F_forecast)
RMSE_RF[6,] <- RMSE_function(y_real, RF_X_MAF_forecast)
RMSE_RF[7,] <- RMSE_function(y_real, RF_X_MARX_forecast)
RMSE_RF[8,] <- RMSE_function(y_real, RF_F_MAF_forecast)
RMSE_RF[9,] <- RMSE_function(y_real, RF_F_MARX_forecast)
RMSE_RF[10,] <- RMSE_function(y_real, RF_MAF_MARX_forecast)
RMSE_RF[11,] <- RMSE_function(y_real, RF_X_F_MAF_forecast)
RMSE_RF[12,] <- RMSE_function(y_real, RF_X_F_MARX_forecast)
RMSE_RF[13,] <- RMSE_function(y_real, RF_X_MAF_MARX_forecast)
RMSE_RF[14,] <- RMSE_function(y_real, RF_F_MAF_MARX_forecast)
RMSE_RF[15,] <- RMSE_function(y_real, RF_X_F_MAF_MARX_forecast)

rownames(RMSE_RF) <- c("X", "F", "MAF", "MARX", "X,F", "X,MAF", "X,MARX", "F,MAF", "F,MARX", "MAF,MARX", "X,F,MAF", "X,F,MARX", "X,MAF,MARX", "F,MAF,MARX", "X,F,MAF,MARX")
colnames(RMSE_RF) <- c("h=3", "h=6", "h=12", "h=18", "h=24")

# Saving Prediction Tables
write.csv(RF_X_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_X_forecast.csv", row.names=FALSE)
write.csv(RF_F_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_F_forecast.csv", row.names=FALSE)
write.csv(RF_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_MAF_forecast.csv", row.names=FALSE)
write.csv(RF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_X_F_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_X_F_forecast.csv", row.names=FALSE)
write.csv(RF_X_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_X_MAF_forecast.csv", row.names=FALSE)
write.csv(RF_X_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_X_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_F_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_F_MAF_forecast.csv", row.names=FALSE)
write.csv(RF_F_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_F_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_X_F_MAF_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_X_F_MAF_forecast.csv", row.names=FALSE)
write.csv(RF_X_F_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_X_F_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_X_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_X_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_F_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_F_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(RF_X_F_MAF_MARX_forecast, "~/Documents/MSc Econometrics/Blok 3/Seminar/R code/Coulombe_RF_X_F_MAF_MARX_forecast.csv", row.names=FALSE)

write.csv(RMSE_RF, "C:/Users/Gebruiker/Documents/GitHub/project_cpb/Data en Forecasts/Output/Coulombe/rmse_rf.csv", row.names=TRUE)

