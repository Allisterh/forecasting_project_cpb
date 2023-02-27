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

## ---- DATA ---- CBS
 
# CPB Data
df <- read.csv("data-eur2023.csv", row.names = 1)

# Inflation CPI year-on-year 
inflation <- read.csv("Extra data/inflation.csv", sep = ';')
inflation <- inflation[278:711,3]

df <- cbind(df,inflation)

# Faillisementen
faillisementen <- read.csv("Extra data/faillisementen.csv", sep = ';')
faillisementen <- faillisementen[62:495,4]

# Cost of building, starts at 1990
buildingcost <- read.csv("Extra data/bouwkosten.csv", sep = ';')
buildingcost <- buildingcost[,-c(1,3,4,5,6,8)]

# Building permits
buildpermits1 <- read.csv("Extra data/Bouwvergunning19902016.csv", sep = ';')
buildpermits1 <- buildpermits1[,-c(1,2,3)] # to compare with other choose rows 265:324
colnames(buildpermits1)[2] = c("BouwVerg")

buildpermits2 <- read.csv("Extra data/Bouwvergunning20122022.csv", sep = ';')
buildpermits2 <- buildpermits2[,c(4,6)] # same 1:60
colnames(buildpermits2)[2] = c("BouwVerg")

buildpermits<- rbind(buildpermits1[3:324,1:2],buildpermits2[61:123,]) # Starts at 1990
rm(buildpermits1,buildpermits2)

#checkbuild <- cbind(buildpermits1[265:324,1:2], buildpermits2[1:60,2])
#colnames(checkbuild)[2:3] = c("BouwVerg","BouwVerg2")

#ggplot(checkbuild, aes(x=1:60))+
#  geom_line(aes(y=BouwVerg), colour = 'red')+
 # geom_line(aes(y=BouwVerg2), colour = 'blue')+
  #theme_classic()

adf.test(buildpermits[,2])

diff_data <- as.data.frame(diff(as.matrix(buildpermits[,2]), differences = 1))

ggplot(diff_data, aes(x=1:59))+
  geom_line(aes(y=BouwVerg), colour = 'red')+
  geom_line(aes(y=BouwVerg2), colour = 'blue')+
  theme_classic()

# -- Full data set consumer confidence -- 
consconf <- read.csv("Extra data/consconf.csv", sep = ';')

# -- Additional data of stocks -- We need to check alignment of the data

# German stocks, begint vanaf 1988
dax <- read.csv("Extra data/DAX.csv", row.names = 1)
dax <- log(dax[27:411,-c(1,2,3,5,6)])

# French stocks, begint van 1990
cac40 <- read.csv("Extra data/FCHI.csv", row.names = 1)
cac40 <- log(cac40[1:385,-c(1,2,3,5,6)])

# FTSE London, vanaf altijd, yahoo Finance
ftse100 <- Index$new('^FTSE')
ftse <- ftse100$get_history(interval = "1mo", start = "1986-02-01", end = "2022-04-01")[,c(1,6)] # if only close price, replace c(1,6) with 6
ftse$date <- format(ftse$date,format = "%Y-%m-%d")
ftse <- log(ftse[50:434,2])

# SP500, vanaf altijd, yahoo Finance
spindex <- Index$new('^GSPC')
sp500 <- spindex$get_history(interval = "1mo", start = "1986-02-01", end = "2022-04-01")[,c(1,6)] # if only close price, replace c(1,6) with 6
sp500$date <- format(sp500$date,format = "%Y-%m-%d")
sp500 <- log(sp500[50:434,2])

logstocks <- as.data.frame(cbind(df$L1_AEX[50:434],dax,cac40,ftse,sp500))

ggplot(logstocks, aes(x=1:385))+
  geom_line(aes(y=V1), colour = 'orange')+
  geom_line(aes(y=dax), colour = 'red')+
  geom_line(aes(y=cac40), colour = 'blue')+
  geom_line(aes(y=ftse), colour = 'black')+
  geom_line(aes(y=sp500), colour = 'green')+
  theme_classic()

rm(ftse100,spindex,dax,cac40,ftse,sp500)

# ECB financial stress index, European Central Bank
fin_stress <- read.csv("Extra data/fin_stress.csv")
fin_stress <- rev(fin_stress[9:442,2])

# -- Commodity prices (Gas, Oil, Gold) -- World Bank
commodities <- read.csv("Extra data/commodities2.csv", sep = ';')
commodities <- commodities[363:747,]

# Interest rates, source OECD
interestshort <- read.csv("Extra data/interestratesshort.csv", sep = ',')
interestshort <- dplyr::filter(interestshort, LOCATION %in% c("NLD"))
interestshort <- interestshort[6:439,6:7]
interestlong <- read.csv("Extra data/interestrateslong.csv", sep = ',')
interestlong <- dplyr::filter(interestlong, LOCATION %in% c("NLD"))
interestlong <- interestlong[1:434,6:7]
interest <- cbind(interestshort, interestlong[,2])
colnames(interest)[2:3] <- c("Short_int", "Long_int")

rm(interestshort,interestlong)

# Combine data
df_full <- cbind(df,inflation, faillisementen,fin_stress,commodities[,2:4], )

# -- transformations -- 

# Check if data stationary
adf.test(df[,2])
adf.test(df[,3])
adf.test(df[,4])
adf.test(df[,5])
adf.test(df[,6])
adf.test(df[,7])
adf.test(df[,8])
adf.test(df[,9])
adf.test(df[,10])

# Make data stationary with first differences
colstodiff <- c(2, 3, 4, 7, 8, 9)
diff_data <- diff(as.matrix(df[, colstodiff]), differences = 1)
new_df <- cbind(diff_data[,1:3], df[2:434,5:6],diff_data[,4:6],df[2:434,10])
colnames(new_df)[9] = "AEX.VOL"

# Check if actually stationary
adf.test(diff_data[,1])
adf.test(diff_data[,2])
adf.test(diff_data[,3])
adf.test(diff_data[,4])
adf.test(diff_data[,5])
adf.test(diff_data[,6])

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
