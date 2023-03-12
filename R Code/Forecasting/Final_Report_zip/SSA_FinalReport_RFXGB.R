#### ---- SSA Predictions and RMSEs ----
rm(list=ls())

### --- RF SMALL ---
setwd("/Users/vincentvanpul/Documents/GitHub/project_cpb/R Code/Forecasting")

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
library(forecast)
library(Rssa)
library(ggplot2)
library(gridExtra)
library(cowplot)

source("Function_File.r")
source("Data_File.r")
getwd()

# -- Loading in Data  --

new_df <- cpb.infl.stationary
regressor_matrix <- new_df[-c(1)] # Dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)

# -- Functions to Create SSA Factors  --

SSA.factors <- function(var,n,l){
  s <- ssa(var, L = l, neig = 40, kind = "1d-ssa")
  g <- grouping.auto(s, grouping.method = "wcor", method = "average", nclust = n)
  recon <- reconstruct(s, group = g)
  series <- cbind(recon$`1`, recon$`2`, recon$`3`)
  return(series)
}

SSA.factors.expanding <- function(var){
  series <- matrix(nrow = length(var), ncol=3)
  for (j in 1:4){
    series[j,] <- rep(var[j],3)
  }
  
  for (i in 5:length(var)){
    s <- ssa(var[1:i], kind = "1d-ssa")
    g <- grouping.auto(s, grouping.method = "wcor", method = "average", nclust = 3)
    recon <- reconstruct(s, group = g)
    series[i,] <- cbind(recon$`1`[i], recon$`2`[i], recon$`3`[i])
  }
  return(series)
}

SSA.factors.expanding2 <- function(var){
  series <- matrix(nrow = length(var), ncol=3)
  for (j in 1:20){
    series[j,] <- rep(var[j],3)
  }
  
  for (i in 21:length(var)){
    s <- ssa(var[1:i], kind = "1d-ssa")
    g <- grouping.auto(s, grouping.method = "wcor", method = "average", nclust = 3)
    recon <- reconstruct(s, group = g)
    series[i,] <- cbind(recon$`1`[i], recon$`2`[i], recon$`3`[i])
  }
  return(series)
}

SSA.dataframe <- function(df,n,l) {
  series <- SSA.factors(df[,1],n,l)
  for (i in 2:ncol(df)){
    series <- cbind(series,SSA.factors(df[,i],n,l))
  }
  return(as.data.frame(series))
}

SSA.dataframe.expanding <- function(df) {
  series <- SSA.factors.expanding(df[,1])
  for (i in 2:ncol(df)){
    if (i != 17) {
      series <- cbind(series,SSA.factors.expanding(df[,i]))
      print(i)
    }
    else {
      series <- cbind(series,SSA.factors.expanding2(df[,i]))
      print(i)
    }
  }
  return(as.data.frame(series))
}

# -- Parameter initalizations for feature matrix --
X_lags <- 12 # We use the data up until one year before
n_Factors <- 5 # Optimization
F_lags <- 12 # Same reason as X, also because Coulombe
P_MAF <- 12 # Summarize data up until one year 
n_MAF <- 2 # Optimization 
P_MARX <- 12 # Lags for MARX, same as X_lags, also Coulombe 

# -- Make feature matrices --

X <- as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE))
scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags)
scaled_X <- scale(X) #Scale lagged X Matrix for MAF
MAF <- MAF_function(scaled_X,X_lags,nrow(scaled_X),ncol(regressor_matrix), P_MAF, n_MAF, FALSE)
MARX <- MARX_function(regressor_matrix, P_MARX, n_var)
SSA <- SSA.dataframe.expanding(new_df[,-1])
write.csv(SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_SSA_Matrix.csv")



X_F <- cbind(X, F)
X_SSA <- cbind(X, SSA)
F_SSA <- cbind(F, SSA)
MAF_SSA <- cbind(MAF, SSA)
MARX_SSA <- cbind(MARX, SSA)
X_F_SSA <- cbind(X, F, SSA)
X_MAF <- cbind(X, MAF)
X_MAF_SSA <- cbind(X, MAF, SSA)
X_MARX <- cbind(X, MARX)
X_MARX_SSA <- cbind(X, MARX, SSA)
F_MAF <- cbind(F, MAF)
F_MAF_SSA <- cbind(F, MAF, SSA)
F_MARX <- cbind(F, MARX)
F_MARX_SSA <- cbind(F, MARX, SSA)
MAF_MARX <- cbind(MAF, MARX)
MAF_MARX_SSA <- cbind(MAF, MARX, SSA)
X_F_MAF <- cbind(X, F, MAF)
X_F_MAF_SSA <- cbind(X, F, MAF, SSA)
X_F_MARX <- cbind(X, F, MARX)
X_F_MARX_SSA <- cbind(X, F, MARX, SSA)
X_MAF_MARX <- cbind(X, MAF, MARX)
X_MAF_MARX_SSA <- cbind(X, MAF, MARX, SSA)
F_MAF_MARX <- cbind(F, MAF, MARX)
F_MAF_MARX_SSA <- cbind(F, MAF, MARX, SSA)
X_F_MAF_MARX <- cbind(X, F, MAF, MARX)
X_F_MAF_MARX_SSA <- cbind(X, F, MAF, MARX, SSA)

# -- Forecast Initialization --
poos <- 120
Unempl <- new_df[,1] # 
y_real <- Unempl[(length(Unempl)-poos+1):length(Unempl)] 
start_forecast <- length(Unempl)-poos+1
end_forecast <- length(Unempl)
horizons <- list(3, 6, 12, 18, 24) # 
ntrees <- 500 # Default value

# -- forecasting --

RF_SSA_forecast <- Forecasting_function_RF(Unempl, SSA, poos, horizons, ntrees) # 
write.csv(RF_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small_2/RF_Small_SSA.csv")

RF_X_SSA_forecast <- Forecasting_function_RF(Unempl, X_SSA, poos, horizons, ntrees) # 
write.csv(RF_X_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_X_SSA.csv")

RF_F_SSA_forecast <- Forecasting_function_RF(Unempl, F_SSA, poos, horizons, ntrees) # 
write.csv(RF_F_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_F_SSA.csv")

RF_MAF_SSA_forecast <- Forecasting_function_RF(Unempl, MAF_SSA, poos, horizons, ntrees) # 
write.csv(RF_MAF_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_MAF_SSA.csv")

RF_MARX_SSA_forecast <- Forecasting_function_RF(Unempl, MARX_SSA, poos, horizons, ntrees) # 
write.csv(RF_MARX_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_MARX_SSA.csv")

RF_X_F_SSA_forecast <- Forecasting_function_RF(Unempl, X_F_SSA, poos, horizons, ntrees) #
write.csv(RF_X_F_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_X_F_SSA.csv")

RF_X_MAF_SSA_forecast <- Forecasting_function_RF(Unempl, X_MAF_SSA, poos, horizons, ntrees) # 
write.csv(RF_X_MAF_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_X_MAF_SSA.csv")

RF_X_MARX_SSA_forecast <- Forecasting_function_RF(Unempl, X_MARX_SSA, poos, horizons, ntrees) # 
write.csv(RF_X_MARX_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_X_MARX_SSA.csv")

RF_F_MAF_SSA_forecast <- Forecasting_function_RF(Unempl, F_MAF_SSA, poos, horizons, ntrees) # 
write.csv(RF_F_MAF_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_F_MAF_SSA.csv")

RF_F_MARX_SSA_forecast <- Forecasting_function_RF(Unempl, F_MARX_SSA, poos, horizons, ntrees) # 
write.csv(RF_F_MARX_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_F_MARX_SSA.csv")

RF_MAF_MARX_SSA_forecast <- Forecasting_function_RF(Unempl, MAF_MARX_SSA, poos, horizons, ntrees) # 
write.csv(RF_MAF_MARX_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_MAF_MARX_SSA.csv")

RF_X_F_MAF_SSA_forecast <- Forecasting_function_RF(Unempl, X_F_MAF_SSA, poos, horizons, ntrees) # 
write.csv(RF_X_F_MAF_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_X_F_MAF_SSA.csv")

RF_X_F_MARX_SSA_forecast <- Forecasting_function_RF(Unempl, X_F_MARX_SSA, poos, horizons, ntrees) # 
write.csv(RF_X_F_MARX_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_X_F_MARX_SSA.csv")

RF_X_MAF_MARX_SSA_forecast <- Forecasting_function_RF(Unempl, X_MAF_MARX_SSA, poos, horizons, ntrees) #
write.csv(RF_X_MAF_MARX_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_X_MAF_MARX_SSA.csv")

RF_F_MAF_MARX_SSA_forecast <- Forecasting_function_RF(Unempl, F_MAF_MARX_SSA, poos, horizons, ntrees) #
write.csv(RF_F_MAF_MARX_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_F_MAF_MARX_SSA.csv")

RF_X_F_MAF_MARX_SSA_forecast <- Forecasting_function_RF(Unempl, X_F_MAF_MARX_SSA, poos, horizons, ntrees) #
write.csv(RF_X_F_MAF_MARX_SSA_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small/RF_Small_X_F_MAF_MARX_SSA.csv")

### --- RF BIG ---
setwd("/Users/vincentvanpul/Documents/GitHub/project_cpb/R Code/Forecasting")
getwd()
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
library(forecast)
library(Rssa)
library(ggplot2)
library(gridExtra)
library(cowplot)

source("Function_File.r")
source("Data_File.r")
getwd()

# If you use the small dataset: only cpb.infl.stationary


# If big dataset: combine cpb.infl.stationary and additional.data.stationary

# Define stationary dataframes
new_df <- cbind(cpb.infl.stationary[50:433,],additional.data.stationary)
regressor_matrix <- new_df[-c(1)] # Dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)

# -- Functions to Create SSA Factors  --

SSA.factors <- function(var,n,l){
  s <- ssa(var, L = l, neig = 40, kind = "1d-ssa")
  g <- grouping.auto(s, grouping.method = "wcor", method = "average", nclust = n)
  recon <- reconstruct(s, group = g)
  series <- cbind(recon$`1`, recon$`2`, recon$`3`)
  return(series)
}

SSA.factors.expanding <- function(var){
  series <- matrix(nrow = length(var), ncol=3)
  for (j in 1:4){
    series[j,] <- rep(var[j],3)
  }
  
  for (i in 5:length(var)){
    s <- ssa(var[1:i], kind = "1d-ssa")
    g <- grouping.auto(s, grouping.method = "wcor", method = "average", nclust = 3)
    recon <- reconstruct(s, group = g)
    series[i,] <- cbind(recon$`1`[i], recon$`2`[i], recon$`3`[i])
  }
  return(series)
}

SSA.factors.expanding2 <- function(var){
  series <- matrix(nrow = length(var), ncol=3)
  for (j in 1:20){
    series[j,] <- rep(var[j],3)
  }
  
  for (i in 21:length(var)){
    s <- ssa(var[1:i], kind = "1d-ssa")
    g <- grouping.auto(s, grouping.method = "wcor", method = "average", nclust = 3)
    recon <- reconstruct(s, group = g)
    series[i,] <- cbind(recon$`1`[i], recon$`2`[i], recon$`3`[i])
  }
  return(series)
}

SSA.dataframe <- function(df,n,l) {
  series <- SSA.factors(df[,1],n,l)
  for (i in 2:ncol(df)){
    series <- cbind(series,SSA.factors(df[,i],n,l))
  }
  return(as.data.frame(series))
}

SSA.dataframe.expanding <- function(df) {
  series <- SSA.factors.expanding(df[,1])
  for (i in 2:ncol(df)){
    if (i != 17) {
      series <- cbind(series,SSA.factors.expanding(df[,i]))
      print(i)
    }
    else {
      series <- cbind(series,SSA.factors.expanding2(df[,i]))
      print(i)
    }
  }
  return(as.data.frame(series))
}

# -- Parameter initalizations for feature matrix --
X_lags <- 12 # We use the data up until one year before
n_Factors <- 8 # Optimization
F_lags <- 12 # Same reason as X, also because Coulombe
P_MAF <- 12 # Summarize data up until one year 
n_MAF <- 2 # Optimization 
P_MARX <- 12 # Lags for MARX, same as X_lags, also Coulombe 

# -- Make feature matrices --

X <- as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE))
scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags)
scaled_X <- scale(X) #Scale lagged X Matrix for MAF
MAF <- MAF_function(scaled_X,X_lags,nrow(scaled_X),ncol(regressor_matrix), P_MAF, n_MAF, FALSE)
MARX <- MARX_function(regressor_matrix, P_MARX, n_var)
SSA <- SSA.dataframe.expanding(na.omit(new_df[,-1]))
write.csv(SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big/RF_Small_Big_Matrix.csv")


X_F <- cbind(X, F)
X_SSA <- cbind(X, SSA)
F_SSA <- cbind(F, SSA)
MAF_SSA <- cbind(MAF, SSA)
MARX_SSA <- cbind(MARX, SSA)
X_F_SSA <- cbind(X, F, SSA)
X_MAF <- cbind(X, MAF)
X_MAF_SSA <- cbind(X, MAF, SSA)
X_MARX <- cbind(X, MARX)
X_MARX_SSA <- cbind(X, MARX, SSA)
F_MAF <- cbind(F, MAF)
F_MAF_SSA <- cbind(F, MAF, SSA)
F_MARX <- cbind(F, MARX)
F_MARX_SSA <- cbind(F, MARX, SSA)
MAF_MARX <- cbind(MAF, MARX)
MAF_MARX_SSA <- cbind(MAF, MARX, SSA)
X_F_MAF <- cbind(X, F, MAF)
X_F_MAF_SSA <- cbind(X, F, MAF, SSA)
X_F_MARX <- cbind(X, F, MARX)
X_F_MARX_SSA <- cbind(X, F, MARX, SSA)
X_MAF_MARX <- cbind(X, MAF, MARX)
X_MAF_MARX_SSA <- cbind(X, MAF, MARX, SSA)
F_MAF_MARX <- cbind(F, MAF, MARX)
F_MAF_MARX_SSA <- cbind(F, MAF, MARX, SSA)
X_F_MAF_MARX <- cbind(X, F, MAF, MARX)
X_F_MAF_MARX_SSA <- cbind(X, F, MAF, MARX, SSA)

# -- Forecast Initialization --
poos <- 120
Unempl <- new_df[,1] 
y_real <- Unempl[(length(Unempl)-poos+1):length(Unempl)] 
start_forecast <- length(Unempl)-poos+1
end_forecast <- length(Unempl)
horizons <- list(3, 6, 12, 18, 24)  
ntrees <- 500 # Default value

# -- forecasting --

RF_SSA_2_forecast <- Forecasting_function_RF(Unempl, SSA, poos, horizons, ntrees) # 
write.csv(RF_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_SSA.csv")

RF_X_SSA_2_forecast <- Forecasting_function_RF(Unempl, X_SSA, poos, horizons, ntrees) # 
write.csv(RF_X_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_X_SSA.csv")

RF_F_SSA_2_forecast <- Forecasting_function_RF(Unempl, F_SSA, poos, horizons, ntrees) # 
write.csv(RF_F_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_F_SSA.csv")

RF_MAF_SSA_2_forecast <- Forecasting_function_RF(Unempl, MAF_SSA, poos, horizons, ntrees) # 
write.csv(RF_MAF_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_MAF_SSA.csv")

RF_MARX_SSA_2_forecast <- Forecasting_function_RF(Unempl, MARX_SSA, poos, horizons, ntrees) # 
write.csv(RF_MARX_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_MARX_SSA.csv")

RF_X_F_SSA_2_forecast <- Forecasting_function_RF(Unempl, X_F_SSA, poos, horizons, ntrees) #
write.csv(RF_X_F_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_X_F_SSA.csv")

RF_X_MAF_SSA_2_forecast <- Forecasting_function_RF(Unempl, X_MAF_SSA, poos, horizons, ntrees) # 
write.csv(RF_X_MAF_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_X_MAF_SSA.csv")

RF_X_MARX_SSA_2_forecast <- Forecasting_function_RF(Unempl, X_MARX_SSA, poos, horizons, ntrees) # 
write.csv(RF_X_MARX_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_X_MARX_SSA.csv")

RF_F_MAF_SSA_2_forecast <- Forecasting_function_RF(Unempl, F_MAF_SSA, poos, horizons, ntrees) # 
write.csv(RF_F_MAF_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_F_MAF_SSA.csv")

RF_F_MARX_SSA_2_forecast <- Forecasting_function_RF(Unempl, F_MARX_SSA, poos, horizons, ntrees) # 
write.csv(RF_F_MARX_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_F_MARX_SSA.csv")

RF_MAF_MARX_SSA_2_forecast <- Forecasting_function_RF(Unempl, MAF_MARX_SSA, poos, horizons, ntrees) # 
write.csv(RF_MAF_MARX_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_MAF_MARX_SSA.csv")

RF_X_F_MAF_SSA_2_forecast <- Forecasting_function_RF(Unempl, X_F_MAF_SSA, poos, horizons, ntrees) # 
write.csv(RF_X_F_MAF_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_X_F_MAF_SSA.csv")

RF_X_F_MARX_SSA_2_forecast <- Forecasting_function_RF(Unempl, X_F_MARX_SSA, poos, horizons, ntrees) # 
write.csv(RF_X_F_MARX_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_X_F_MARX_SSA.csv")

RF_X_MAF_MARX_SSA_2_forecast <- Forecasting_function_RF(Unempl, X_MAF_MARX_SSA, poos, horizons, ntrees) #
write.csv(RF_X_MAF_MARX_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_X_MAF_MARX_SSA.csv")

RF_F_MAF_MARX_SSA_2_forecast <- Forecasting_function_RF(Unempl, F_MAF_MARX_SSA, poos, horizons, ntrees) #
write.csv(RF_F_MAF_MARX_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_F_MAF_MARX_SSA.csv")

RF_X_F_MAF_MARX_SSA_2_forecast <- Forecasting_function_RF(Unempl, X_F_MAF_MARX_SSA, poos, horizons, ntrees) #
write.csv(RF_X_F_MAF_MARX_SSA_2_forecast,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2/RF_Big_X_F_MAF_MARX_SSA.csv")


print.summary <- function(var){
  print("mean")
  print(mean(var))
  print("sd")
  print(sd(var))
  print("min")
  print(min(var))
  print("max")
  print(max(var))
  print(adf.test(var))
}

print.summary(additional.data$Long_interest)

### --- XGB SMALL ---
setwd("/Users/vincentvanpul/Documents/GitHub/project_cpb/R Code/Forecasting")

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
library(forecast)
library(Rssa)
library(ggplot2)
library(gridExtra)
library(cowplot)

source("Function_File.r")
getwd()

# -- Loading in Data  --

new_df <- read.csv("data_core.csv")
regressor_matrix <- new_df[-c(1)] # Dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)

# -- Functions to Create SSA Factors  --

SSA.factors <- function(var,n,l){
  s <- ssa(var, L = l, neig = 40, kind = "1d-ssa")
  g <- grouping.auto(s, grouping.method = "wcor", method = "average", nclust = n)
  recon <- reconstruct(s, group = g)
  series <- cbind(recon$`1`, recon$`2`, recon$`3`)
  return(series)
}

SSA.factors.expanding <- function(var){
  series <- matrix(nrow = length(var), ncol=3)
  for (j in 1:4){
    series[j,] <- rep(var[j],3)
  }
  
  for (i in 5:length(var)){
    s <- ssa(var[1:i], kind = "1d-ssa")
    g <- grouping.auto(s, grouping.method = "wcor", method = "average", nclust = 3)
    recon <- reconstruct(s, group = g)
    series[i,] <- cbind(recon$`1`[i], recon$`2`[i], recon$`3`[i])
  }
  return(series)
}

SSA.dataframe <- function(df,n,l) {
  series <- SSA.factors(df[,1],n,l)
  for (i in 2:ncol(df)){
    series <- cbind(series,SSA.factors(df[,i],n,l))
  }
  return(as.data.frame(series))
}

SSA.dataframe.expanding <- function(df) {
  series <- SSA.factors.expanding(df[,1])
  for (i in 2:ncol(df)){
    series <- cbind(series,SSA.factors.expanding(df[,i]))
  }
  return(as.data.frame(series))
}

# -- Parameter initalizations for feature matrix --
X_lags <- 12 # We use the data up until one year before
n_Factors <- 5 # Optimization
F_lags <- 12 # Same reason as X, also because Coulombe
P_MAF <- 12 # Summarize data up until one year 
n_MAF <- 2 # Optimization 
P_MARX <- 12 # Lags for MARX, same as X_lags, also Coulombe 

# -- Make feature matrices --
X <- as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE))
scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags)
scaled_X <- scale(X) #Scale lagged X Matrix for MAF
MAF <- MAF_function(scaled_X,X_lags,nrow(scaled_X),ncol(regressor_matrix), P_MAF, n_MAF, FALSE)
MARX <- MARX_function(regressor_matrix, P_MARX, n_var)
SSA <- SSA.dataframe.expanding(new_df[,-1])

X_F <- cbind(X, F)
X_SSA <- cbind(X, SSA)
F_SSA <- cbind(F, SSA)
MAF_SSA <- cbind(MAF, SSA)
MARX_SSA <- cbind(MARX, SSA)
X_F_SSA <- cbind(X, F, SSA)
X_MAF <- cbind(X, MAF)
X_MAF_SSA <- cbind(X, MAF, SSA)
X_MARX <- cbind(X, MARX)
X_MARX_SSA <- cbind(X, MARX, SSA)
F_MAF <- cbind(F, MAF)
F_MAF_SSA <- cbind(F, MAF, SSA)
F_MARX <- cbind(F, MARX)
F_MARX_SSA <- cbind(F, MARX, SSA)
MAF_MARX <- cbind(MAF, MARX)
MAF_MARX_SSA <- cbind(MAF, MARX, SSA)
X_F_MAF <- cbind(X, F, MAF)
X_F_MAF_SSA <- cbind(X, F, MAF, SSA)
X_F_MARX <- cbind(X, F, MARX)
X_F_MARX_SSA <- cbind(X, F, MARX, SSA)
X_MAF_MARX <- cbind(X, MAF, MARX)
X_MAF_MARX_SSA <- cbind(X, MAF, MARX, SSA)
F_MAF_MARX <- cbind(F, MAF, MARX)
F_MAF_MARX_SSA <- cbind(F, MAF, MARX, SSA)
X_F_MAF_MARX <- cbind(X, F, MAF, MARX)
X_F_MAF_MARX_SSA <- cbind(X, F, MAF, MARX, SSA)

# -- Forecast Initialization --
poos <- 120
Unempl <- new_df[,1]  
y_real <- Unempl[(length(Unempl)-poos+1):length(Unempl)] 
start_forecast <- length(Unempl)-poos+1
end_forecast <- length(Unempl)
horizons <- list(3, 6, 12, 18, 24)  

# -- forecasting function
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

XGB_0 <- Forecasting_function_XGB(Unempl, regressor_matrix, poos, horizons)
write.csv(XGB_0,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_0.csv")

XGB_SSA <- Forecasting_function_XGB(Unempl, SSA, poos, horizons) # 
write.csv(XGB_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_SSA.csv")

XGB_X <- Forecasting_function_XGB(Unempl, X, poos, horizons) # 
write.csv(XGB_X,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X.csv")

XGB_X_SSA <- Forecasting_function_XGB(Unempl, X_SSA, poos, horizons) # 
write.csv(XGB_X_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_SSA.csv")

XGB_F <- Forecasting_function_XGB(Unempl, F, poos, horizons) # 
write.csv(XGB_F,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_F.csv")

XGB_F_SSA <- Forecasting_function_XGB(Unempl, F_SSA, poos, horizons) # 
write.csv(XGB_F_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_F_SSA.csv")

XGB_MAF <- Forecasting_function_XGB(Unempl, MAF, poos, horizons) # 
write.csv(XGB_MAF,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_MAF.csv")

XGB_MAF_SSA <- Forecasting_function_XGB(Unempl, MAF_SSA, poos, horizons) # 
write.csv(XGB_MAF_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_MAF_SSA.csv")

XGB_MARX <- Forecasting_function_XGB(Unempl, MARX, poos, horizons) # 
write.csv(XGB_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_MARX.csv")

XGB_MARX_SSA <- Forecasting_function_XGB(Unempl, MARX_SSA, poos, horizons) # 
write.csv(XGB_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_MARX_SSA.csv")

XGB_X_F <- Forecasting_function_XGB(Unempl, X_F, poos, horizons) #
write.csv(XGB_X_F,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_F.csv")

XGB_X_F_SSA <- Forecasting_function_XGB(Unempl, X_F_SSA, poos, horizons) #
write.csv(XGB_X_F_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_F_SSA.csv")

XGB_X_MAF <- Forecasting_function_XGB(Unempl, X_MAF, poos, horizons) # 
write.csv(XGB_X_MAF,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_MAF.csv")

XGB_X_MAF_SSA <- Forecasting_function_XGB(Unempl, X_MAF_SSA, poos, horizons) # 
write.csv(XGB_X_MAF_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_MAF_SSA.csv")

XGB_X_MARX <- Forecasting_function_XGB(Unempl, X_MARX, poos, horizons) # 
write.csv(XGB_X_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_MARX.csv")

XGB_X_MARX_SSA <- Forecasting_function_XGB(Unempl, X_MARX_SSA, poos, horizons) # 
write.csv(XGB_X_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_MARX_SSA.csv")

XGB_F_MAF <- Forecasting_function_XGB(Unempl, F_MAF, poos, horizons) # 
write.csv(XGB_F_MAF,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_F_MAF.csv")

XGB_F_MAF_SSA <- Forecasting_function_XGB(Unempl, F_MAF_SSA, poos, horizons) # 
write.csv(XGB_F_MAF_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_F_MAF_SSA.csv")

XGB_F_MARX <- Forecasting_function_XGB(Unempl, F_MARX, poos, horizons) # 
write.csv(XGB_F_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_F_MARX.csv")

XGB_F_MARX_SSA <- Forecasting_function_XGB(Unempl, F_MARX_SSA, poos, horizons) # 
write.csv(XGB_F_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_F_MARX_SSA.csv")

XGB_MAF_MARX <- Forecasting_function_XGB(Unempl, MAF_MARX, poos, horizons) # 
write.csv(XGB_MAF_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_MAF_MARX.csv")

XGB_MAF_MARX_SSA <- Forecasting_function_XGB(Unempl, MAF_MARX_SSA, poos, horizons) # 
write.csv(XGB_MAF_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_MAF_MARX_SSA.csv")

XGB_X_F_MAF <- Forecasting_function_XGB(Unempl, X_F_MAF, poos, horizons) # 
write.csv(XGB_X_F_MAF,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_F_MAF.csv")

XGB_X_F_MAF_SSA <- Forecasting_function_XGB(Unempl, X_F_MAF_SSA, poos, horizons) # 
write.csv(XGB_X_F_MAF_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_F_MAF_SSA.csv")

XGB_X_F_MARX <- Forecasting_function_XGB(Unempl, X_F_MARX, poos, horizons) # 
write.csv(XGB_X_F_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_F_MARX.csv")

XGB_X_F_MARX_SSA <- Forecasting_function_XGB(Unempl, X_F_MARX_SSA, poos, horizons) # 
write.csv(XGB_X_F_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_F_MARX_SSA.csv")

XGB_X_MAF_MARX <- Forecasting_function_XGB(Unempl, X_MAF_MARX, poos, horizons) #
write.csv(XGB_X_MAF_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_MAF_MARX.csv")

XGB_X_MAF_MARX_SSA <- Forecasting_function_XGB(Unempl, X_MAF_MARX_SSA, poos, horizons) #
write.csv(XGB_X_MAF_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_MAF_MARX_SSA.csv")

XGB_F_MAF_MARX <- Forecasting_function_XGB(Unempl, F_MAF_MARX, poos, horizons) #
write.csv(XGB_F_MAF_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_F_MAF_MARX.csv")

XGB_F_MAF_MARX_SSA <- Forecasting_function_XGB(Unempl, F_MAF_MARX_SSA, poos, horizons) #
write.csv(XGB_F_MAF_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_F_MAF_MARX_SSA.csv")

XGB_X_F_MAF_MARX <- Forecasting_function_XGB(Unempl, X_F_MAF_MARX, poos, horizons) #
write.csv(XGB_X_F_MAF_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_F_MAF_MARX.csv")

XGB_X_F_MAF_MARX_SSA <- Forecasting_function_XGB(Unempl, X_F_MAF_MARX_SSA, poos, horizons) #
write.csv(XGB_X_F_MAF_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Small/XGB_Small_X_F_MAF_MARX_SSA.csv")

### --- XGB BIG ---
setwd("/Users/vincentvanpul/Documents/GitHub/project_cpb/R Code/Forecasting")

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
library(forecast)
library(Rssa)
library(ggplot2)
library(gridExtra)
library(cowplot)

source("Function_File.r")
source("Data_File.r")
getwd()

# -- Loading in Data  --

new_df <- cbind(cpb.infl.stationary[50:433,],additional.data.stationary)
regressor_matrix <- new_df[-c(1)] # Dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)

# -- Functions to Create SSA Factors  --

SSA.factors <- function(var,n,l){
  s <- ssa(var, L = l, neig = 40, kind = "1d-ssa")
  g <- grouping.auto(s, grouping.method = "wcor", method = "average", nclust = n)
  recon <- reconstruct(s, group = g)
  series <- cbind(recon$`1`, recon$`2`, recon$`3`)
  return(series)
}

SSA.factors.expanding <- function(var){
  series <- matrix(nrow = length(var), ncol=3)
  for (j in 1:4){
    series[j,] <- rep(var[j],3)
  }
  
  for (i in 5:length(var)){
    s <- ssa(var[1:i], kind = "1d-ssa")
    g <- grouping.auto(s, grouping.method = "wcor", method = "average", nclust = 3)
    recon <- reconstruct(s, group = g)
    series[i,] <- cbind(recon$`1`[i], recon$`2`[i], recon$`3`[i])
  }
  return(series)
}

SSA.factors.expanding2 <- function(var){
  series <- matrix(nrow = length(var), ncol=3)
  for (j in 1:20){
    series[j,] <- rep(var[j],3)
  }
  
  for (i in 21:length(var)){
    s <- ssa(var[1:i], kind = "1d-ssa")
    g <- grouping.auto(s, grouping.method = "wcor", method = "average", nclust = 3)
    recon <- reconstruct(s, group = g)
    series[i,] <- cbind(recon$`1`[i], recon$`2`[i], recon$`3`[i])
  }
  return(series)
}

SSA.dataframe <- function(df,n,l) {
  series <- SSA.factors(df[,1],n,l)
  for (i in 2:ncol(df)){
    series <- cbind(series,SSA.factors(df[,i],n,l))
  }
  return(as.data.frame(series))
}

SSA.dataframe.expanding <- function(df) {
  series <- SSA.factors.expanding(df[,1])
  for (i in 2:ncol(df)){
    if (i != 17) {
      series <- cbind(series,SSA.factors.expanding(df[,i]))
      print(i)
    }
    else {
      series <- cbind(series,SSA.factors.expanding2(df[,i]))
      print(i)
    }
  }
  return(as.data.frame(series))
}

# -- Parameter initalizations for feature matrix --
X_lags <- 12 # We use the data up until one year before
n_Factors <- 8 # Optimization
F_lags <- 12 # Same reason as X, also because Coulombe
P_MAF <- 12 # Summarize data up until one year 
n_MAF <- 2 # Optimization 
P_MARX <- 12 # Lags for MARX, same as X_lags, also Coulombe 

# -- Make feature matrices --
X <- as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE))
scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags)
scaled_X <- scale(X) #Scale lagged X Matrix for MAF
MAF <- MAF_function(scaled_X,X_lags,nrow(scaled_X),ncol(regressor_matrix), P_MAF, n_MAF, FALSE)
MARX <- MARX_function(regressor_matrix, P_MARX, n_var)
SSA <- SSA.dataframe.expanding(new_df[,-1])

X_F <- cbind(X, F)
X_SSA <- cbind(X, SSA)
F_SSA <- cbind(F, SSA)
MAF_SSA <- cbind(MAF, SSA)
MARX_SSA <- cbind(MARX, SSA)
X_F_SSA <- cbind(X, F, SSA)
X_MAF <- cbind(X, MAF)
X_MAF_SSA <- cbind(X, MAF, SSA)
X_MARX <- cbind(X, MARX)
X_MARX_SSA <- cbind(X, MARX, SSA)
F_MAF <- cbind(F, MAF)
F_MAF_SSA <- cbind(F, MAF, SSA)
F_MARX <- cbind(F, MARX)
F_MARX_SSA <- cbind(F, MARX, SSA)
MAF_MARX <- cbind(MAF, MARX)
MAF_MARX_SSA <- cbind(MAF, MARX, SSA)
X_F_MAF <- cbind(X, F, MAF)
X_F_MAF_SSA <- cbind(X, F, MAF, SSA)
X_F_MARX <- cbind(X, F, MARX)
X_F_MARX_SSA <- cbind(X, F, MARX, SSA)
X_MAF_MARX <- cbind(X, MAF, MARX)
X_MAF_MARX_SSA <- cbind(X, MAF, MARX, SSA)
F_MAF_MARX <- cbind(F, MAF, MARX)
F_MAF_MARX_SSA <- cbind(F, MAF, MARX, SSA)
X_F_MAF_MARX <- cbind(X, F, MAF, MARX)
X_F_MAF_MARX_SSA <- cbind(X, F, MAF, MARX, SSA)

# -- Forecast Initialization --
poos <- 120
Unempl <- new_df[,1] # 
y_real <- Unempl[(length(Unempl)-poos+1):length(Unempl)] 
start_forecast <- length(Unempl)-poos+1
end_forecast <- length(Unempl)
horizons <- list(3, 6, 12, 18, 24) # 

# -- forecasting function
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

XGB_SSA <- Forecasting_function_XGB(Unempl, SSA, poos, horizons) # 
write.csv(XGB_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_SSA.csv")

XGB_X <- Forecasting_function_XGB(Unempl, X, poos, horizons) # 
write.csv(XGB_X,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X.csv")

XGB_X_SSA <- Forecasting_function_XGB(Unempl, X_SSA, poos, horizons) # 
write.csv(XGB_X_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_SSA.csv")

XGB_F <- Forecasting_function_XGB(Unempl, F, poos, horizons) # 
write.csv(XGB_F,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_F.csv")

XGB_F_SSA <- Forecasting_function_XGB(Unempl, F_SSA, poos, horizons) # 
write.csv(XGB_F_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_F_SSA.csv")

XGB_MAF <- Forecasting_function_XGB(Unempl, MAF, poos, horizons) # 
write.csv(XGB_MAF,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_MAF.csv")

XGB_MAF_SSA <- Forecasting_function_XGB(Unempl, MAF_SSA, poos, horizons) # 
write.csv(XGB_MAF_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_MAF_SSA.csv")

XGB_MARX <- Forecasting_function_XGB(Unempl, MARX, poos, horizons) # 
write.csv(XGB_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_MARX.csv")

XGB_MARX_SSA <- Forecasting_function_XGB(Unempl, MARX_SSA, poos, horizons) # 
write.csv(XGB_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_MARX_SSA.csv")

XGB_X_F <- Forecasting_function_XGB(Unempl, X_F, poos, horizons) #
write.csv(XGB_X_F,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_F.csv")

XGB_X_F_SSA <- Forecasting_function_XGB(Unempl, X_F_SSA, poos, horizons) #
write.csv(XGB_X_F_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_F_SSA.csv")

XGB_X_MAF <- Forecasting_function_XGB(Unempl, X_MAF, poos, horizons) # 
write.csv(XGB_X_MAF,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_MAF.csv")

XGB_X_MAF_SSA <- Forecasting_function_XGB(Unempl, X_MAF_SSA, poos, horizons) # 
write.csv(XGB_X_MAF_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_MAF_SSA.csv")

XGB_X_MARX <- Forecasting_function_XGB(Unempl, X_MARX, poos, horizons) # 
write.csv(XGB_X_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_MARX.csv")

XGB_X_MARX_SSA <- Forecasting_function_XGB(Unempl, X_MARX_SSA, poos, horizons) # 
write.csv(XGB_X_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_MARX_SSA.csv")

XGB_F_MAF <- Forecasting_function_XGB(Unempl, F_MAF, poos, horizons) # 
write.csv(XGB_F_MAF,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_F_MAF.csv")

XGB_F_MAF_SSA <- Forecasting_function_XGB(Unempl, F_MAF_SSA, poos, horizons) # 
write.csv(XGB_F_MAF_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_F_MAF_SSA.csv")

XGB_F_MARX <- Forecasting_function_XGB(Unempl, F_MARX, poos, horizons) # 
write.csv(XGB_F_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_F_MARX.csv")

XGB_F_MARX_SSA <- Forecasting_function_XGB(Unempl, F_MARX_SSA, poos, horizons) # 
write.csv(XGB_F_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_F_MARX_SSA.csv")

XGB_MAF_MARX <- Forecasting_function_XGB(Unempl, MAF_MARX, poos, horizons) # 
write.csv(XGB_MAF_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_MAF_MARX.csv")

XGB_MAF_MARX_SSA <- Forecasting_function_XGB(Unempl, MAF_MARX_SSA, poos, horizons) # 
write.csv(XGB_MAF_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_MAF_MARX_SSA.csv")

XGB_X_F_MAF <- Forecasting_function_XGB(Unempl, X_F_MAF, poos, horizons) # 
write.csv(XGB_X_F_MAF,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_F_MAF.csv")

XGB_X_F_MAF_SSA <- Forecasting_function_XGB(Unempl, X_F_MAF_SSA, poos, horizons) # 
write.csv(XGB_X_F_MAF_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_F_MAF_SSA.csv")

XGB_X_F_MARX <- Forecasting_function_XGB(Unempl, X_F_MARX, poos, horizons) # 
write.csv(XGB_X_F_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_F_MARX.csv")

XGB_X_F_MARX_SSA <- Forecasting_function_XGB(Unempl, X_F_MARX_SSA, poos, horizons) # 
write.csv(XGB_X_F_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_F_MARX_SSA.csv")

XGB_X_MAF_MARX <- Forecasting_function_XGB(Unempl, X_MAF_MARX, poos, horizons) #
write.csv(XGB_X_MAF_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_MAF_MARX.csv")

XGB_X_MAF_MARX_SSA <- Forecasting_function_XGB(Unempl, X_MAF_MARX_SSA, poos, horizons) #
write.csv(XGB_X_MAF_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_MAF_MARX_SSA.csv")

XGB_F_MAF_MARX <- Forecasting_function_XGB(Unempl, F_MAF_MARX, poos, horizons) #
write.csv(XGB_F_MAF_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_F_MAF_MARX.csv")

XGB_F_MAF_MARX_SSA <- Forecasting_function_XGB(Unempl, F_MAF_MARX_SSA, poos, horizons) #
write.csv(XGB_F_MAF_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_F_MAF_MARX_SSA.csv")

XGB_X_F_MAF_MARX <- Forecasting_function_XGB(Unempl, X_F_MAF_MARX, poos, horizons) #
write.csv(XGB_X_F_MAF_MARX,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_F_MAF_MARX.csv")

XGB_X_F_MAF_MARX_SSA <- Forecasting_function_XGB(Unempl, X_F_MAF_MARX_SSA, poos, horizons) #
write.csv(XGB_X_F_MAF_MARX_SSA,"/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/XGB_SSA_Big/XGB_Big_X_F_MAF_MARX_SSA.csv")

### --- RMSEs ---
getwd()
rm(list=ls())

setwd("/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small_2")

Small_SSA <- read.csv("RF_Small_SSA.csv")[,-1]
Small_X_SSA <- read.csv("RF_Small_X_SSA.csv")[,-1]
Small_F_SSA <- read.csv("RF_Small_F_SSA.csv")[,-1]
Small_MAF_SSA <- read.csv("RF_Small_MAF_SSA.csv")[,-1]
Small_MARX_SSA <- read.csv("RF_Small_MARX_SSA.csv")[,-1]
Small_X_F_SSA <- read.csv("RF_Small_X_F_SSA.csv")[,-1]
Small_X_MAF_SSA <- read.csv("RF_Small_X_MAF_SSA.csv")[,-1]
Small_X_MARX_SSA <- read.csv("RF_Small_X_MARX_SSA.csv")[,-1]
Small_F_MAF_SSA <- read.csv("RF_Small_F_MAF_SSA.csv")[,-1]
Small_F_MARX_SSA <- read.csv("RF_Small_F_MARX_SSA.csv")[,-1]
Small_MAF_MARX_SSA <- read.csv("RF_Small_MAF_MARX_SSA.csv")[,-1]
Small_X_F_MAF_SSA <- read.csv("RF_Small_X_F_MAF_SSA.csv")[,-1]
Small_X_F_MARX_SSA <- read.csv("RF_Small_X_F_MARX_SSA.csv")[,-1]
Small_X_MAF_MARX_SSA <- read.csv("RF_Small_X_MAF_MARX_SSA.csv")[,-1]
Small_F_MAF_MARX_SSA <- read.csv("RF_Small_F_MAF_MARX_SSA.csv")[,-1]
Small_X_F_MAF_MARX_SSA <- read.csv("RF_Small_X_F_MAF_MARX_SSA.csv")[,-1]

setwd("/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting")

y <- read.csv("data_core.csv")[314:433,1]
horizons <- c(3,6,12,18,24)

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

RMSE_Small_SSA <- RMSE_function(y,Small_SSA)
RMSE_Small_X_SSA <- RMSE_function(y,Small_X_SSA)
RMSE_Small_F_SSA <- RMSE_function(y,Small_F_SSA)
RMSE_Small_MAF_SSA <- RMSE_function(y,Small_MAF_SSA)
RMSE_Small_MARX_SSA <- RMSE_function(y,Small_MARX_SSA)
RMSE_Small_X_F_SSA <- RMSE_function(y,Small_X_F_SSA)
RMSE_Small_X_MAF_SSA <- RMSE_function(y,Small_X_MAF_SSA)
RMSE_Small_X_MARX_SSA <- RMSE_function(y,Small_X_MARX_SSA)
RMSE_Small_F_MAF_SSA <- RMSE_function(y,Small_F_MAF_SSA)
RMSE_Small_F_MARX_SSA <- RMSE_function(y,Small_F_MARX_SSA)
RMSE_Small_MAF_MARX_SSA <- RMSE_function(y,Small_MAF_MARX_SSA)
RMSE_Small_X_F_MAF_SSA <- RMSE_function(y,Small_X_F_MAF_SSA)
RMSE_Small_X_F_MARX_SSA <- RMSE_function(y,Small_X_F_MARX_SSA)
RMSE_Small_X_MAF_MARX_SSA <- RMSE_function(y,Small_X_MAF_MARX_SSA)
RMSE_Small_F_MAF_MARX_SSA <- RMSE_function(y,Small_F_MAF_MARX_SSA)
RMSE_Small_X_F_MAF_MARX_SSA <- RMSE_function(y,Small_X_F_MAF_MARX_SSA)

RMSE_Small_SSA_ALL <- rbind(RMSE_Small_SSA, RMSE_Small_X_SSA,RMSE_Small_F_SSA,
                            RMSE_Small_MAF_SSA, RMSE_Small_MARX_SSA, RMSE_Small_X_F_SSA,
                            RMSE_Small_X_MAF_SSA, RMSE_Small_X_MARX_SSA, RMSE_Small_F_MAF_SSA, 
                            RMSE_Small_F_MARX_SSA, RMSE_Small_MAF_MARX_SSA, RMSE_Small_X_F_MAF_SSA, 
                            RMSE_Small_X_F_MARX_SSA, RMSE_Small_X_MAF_MARX_SSA, RMSE_Small_F_MAF_MARX_SSA, 
                            RMSE_Small_X_F_MAF_MARX_SSA)

setwd("/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Small_2")

write.csv(RMSE_Small_SSA_ALL, "RMSE_Small_SSA_ALL_2.csv", row.names=FALSE)

#LARGE DATASET

setwd("/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2")

Big_SSA <- read.csv("RF_Big_SSA.csv")[,-1]
Big_X_SSA <- read.csv("RF_Big_X_SSA.csv")[,-1]
Big_F_SSA <- read.csv("RF_Big_F_SSA.csv")[,-1]
Big_MAF_SSA <- read.csv("RF_Big_MAF_SSA.csv")[,-1]
Big_MARX_SSA <- read.csv("RF_Big_MARX_SSA.csv")[,-1]
Big_X_F_SSA <- read.csv("RF_Big_X_F_SSA.csv")[,-1]
Big_X_MAF_SSA <- read.csv("RF_Big_X_MAF_SSA.csv")[,-1]
Big_X_MARX_SSA <- read.csv("RF_Big_X_MARX_SSA.csv")[,-1]
Big_F_MAF_SSA <- read.csv("RF_Big_F_MAF_SSA.csv")[,-1]
Big_F_MARX_SSA <- read.csv("RF_Big_F_MARX_SSA.csv")[,-1]
Big_MAF_MARX_SSA <- read.csv("RF_Big_MAF_MARX_SSA.csv")[,-1]
Big_X_F_MAF_SSA <- read.csv("RF_Big_X_F_MAF_SSA.csv")[,-1]
Big_X_F_MARX_SSA <- read.csv("RF_Big_X_F_MARX_SSA.csv")[,-1]
Big_X_MAF_MARX_SSA <- read.csv("RF_Big_X_MAF_MARX_SSA.csv")[,-1] 
Big_F_MAF_MARX_SSA <- read.csv("RF_Big_F_MAF_MARX_SSA.csv")[,-1]
Big_X_F_MAF_MARX_SSA <- read.csv("RF_Big_X_F_MAF_MARX_SSA.csv")[,-1]

setwd("/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting")

y <- read.csv("data_core.csv")[314:433,1]
horizons <- c(3,6,12,18,24)

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

RMSE_Big_SSA <- RMSE_function(y,Big_SSA)
RMSE_Big_X_SSA <- RMSE_function(y,Big_X_SSA)
RMSE_Big_F_SSA <- RMSE_function(y,Big_F_SSA)
RMSE_Big_MAF_SSA <- RMSE_function(y,Big_MAF_SSA)
RMSE_Big_MARX_SSA <- RMSE_function(y,Big_MARX_SSA)
RMSE_Big_X_F_SSA <- RMSE_function(y,Big_X_F_SSA)
RMSE_Big_X_MAF_SSA <- RMSE_function(y,Big_X_MAF_SSA)
RMSE_Big_X_MARX_SSA <- RMSE_function(y,Big_X_MARX_SSA)
RMSE_Big_F_MAF_SSA <- RMSE_function(y,Big_F_MAF_SSA)
RMSE_Big_F_MARX_SSA <- RMSE_function(y,Big_F_MARX_SSA)
RMSE_Big_MAF_MARX_SSA <- RMSE_function(y,Big_MAF_MARX_SSA)
RMSE_Big_X_F_MAF_SSA <- RMSE_function(y,Big_X_F_MAF_SSA)
RMSE_Big_X_F_MARX_SSA <- RMSE_function(y,Big_X_F_MARX_SSA)
RMSE_Big_X_MAF_MARX_SSA <- RMSE_function(y,Big_X_MAF_MARX_SSA) 
RMSE_Big_F_MAF_MARX_SSA <- RMSE_function(y,Big_F_MAF_MARX_SSA)
RMSE_Big_X_F_MAF_MARX_SSA <- RMSE_function(y,Big_X_F_MAF_MARX_SSA)

RMSE_Big_SSA_ALL <- rbind(RMSE_Big_SSA, RMSE_Big_X_SSA,RMSE_Big_F_SSA,
                          RMSE_Big_MAF_SSA, RMSE_Big_MARX_SSA, RMSE_Big_X_F_SSA,
                          RMSE_Big_X_MAF_SSA, RMSE_Big_X_MARX_SSA, RMSE_Big_F_MAF_SSA, 
                          RMSE_Big_F_MARX_SSA, RMSE_Big_MAF_MARX_SSA, RMSE_Big_X_F_MAF_SSA, 
                          RMSE_Big_X_F_MARX_SSA, RMSE_Big_X_MAF_MARX_SSA, RMSE_Big_F_MAF_MARX_SSA, 
                          RMSE_Big_X_F_MAF_MARX_SSA)

setwd("/Users/vincentvanpul/Documents/GitHub/project_cpb/R\ Code/Forecasting/RF_SSA_Big_2")

write.csv(RMSE_Big_SSA_ALL, "RMSE_Big_SSA_ALL_2.csv", row.names=FALSE)





