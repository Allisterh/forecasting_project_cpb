################################################
######## Code to Compute AR and AR-RMSE ########
######## 25.01.23 ##############################
################################################

#Clearing Environment
rm(list=ls())

install.packages("AR")
library("AR")

#Benchmark FM Model
#Autoregressive diffusion index model (Stock and Watson 2002a,b)
#For now, focus on AR-model as benchmark

#Importing Data
df <- read.csv("~/Desktop/data-eur2023.csv", row.names=1)
colnames(df) <- c("pub.date","unempl","constr.conf","retail.conf",
                  "manuf.conf","cons.conf","export.logs","import.logs",
                  "AEX.val.logs", "AEX.vol")

#Selects best model AR (does not use any other vars, just unemployment)
y1 <- ar(df["unempl"],aic = TRUE)
order <- y1$order
rmse <- sqrt(mean(na.omit(y1$resid)^2))

##############################
##### Setting Parameters #####
##############################

poos <- 120

#Selecting model on observation until end-10Y
y2 <- ar(df[1:(nrow(df)-poos),"unempl"],aic = TRUE)
order2 <- y2$order
rmse2 <- mean(na.omit(y2$resid)^2)
coefs2 <- y2$ar

#Forecasting 10Y Out-of-Sample
pred <- c(df[1:(nrow(df)-poos),"unempl"],rep(0,poos))
for (i in (nrow(df)-poos+1):nrow(df)) {
  new.value <- pred[(i-order2):(i-1)] %*% coefs2
  pred[i] <- new.value
}

#RMSE for 10Y Out-of-Sample
resids2 <- pred[(nrow(df)-poos+1):434] - df[(nrow(df)-poos+1):434,"unempl"]
rmse2 <- sqrt(mean(resids2^2))

#Note; obviously, forecasting error becomes larger as horizon increases 
#This does not yet use the shifting window
#Start with Forecasting Horizon of 1 period

#Forecasting 10Y Out-of-Sample - Horizon of 1M 
pred_1m <- c(df[1:(nrow(df)-poos),"unempl"],rep(0,poos))

for (i in (nrow(df)-poos+1):nrow(df)) {
  y <- ar(df[(i-(nrow(df)-poos)):i-1,"unempl"],aic = TRUE)
  order <- y$order
  coefs <- y$ar
  new.value <- df[(i-order):(i-1),"unempl"] %*% coefs
  pred_1m[i] <- new.value
}

#RMSE for 10Y Out-of-Sample - Horizon of 1M 
resids_1m <- pred_1m[(nrow(df)-poos+1):434] - df[(nrow(df)-poos+1):434,"unempl"]
rmse_1m <- sqrt(mean(resids_1m^2))

