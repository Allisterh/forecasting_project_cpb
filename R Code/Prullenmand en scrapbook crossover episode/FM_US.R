####################################################
######## Code to Compute FM and FM-RMSE, US ########
######## 30.01.23 ##################################
####################################################

#Clearing Environment
rm(list=ls())
library("data.table")
install.packages("readxl")
library("readxl")

df <- read_excel("~/Desktop/fred_md_data.xlsx")
df <- df[,c(-1)]
predictors <- subset(df,select=-c(UNRATE))
target <- df[,"UNRATE"]

#(A)
create_lags <- function(data_frame, n_lags){
  result <- as.data.frame(shift(data_frame, n=0:n_lags, type='lag', give.names=TRUE))
  return(result)
}

h <- 1
n_TL <- 12
target_lags <- create_lags(df[,"UNRATE"],n_TL)
shifted_target <- c(target[-(seq(h)),],rep(NA,h))[1] #Vreemde Bug, Werkt Nu 

#(B)
create_factors_aux <- function(data_frame, n_factors){
  pca <- prcomp(data_frame, center = FALSE, scale. = FALSE)
  result <- pca$x[1:n_factors]
  return(result)
}

create_factors <- function(data_frame, n_factors, n_lags, T){
  F <- data.frame(matrix(ncol = (n_factors*(n_lags+1)), nrow = T))
  for (t in n_factors:T){
    F[t,1:n_factors] <- create_factors_aux(data_frame[1:t,], n_factors)
  }
  for (i in 1:n_lags){
    F[(n_factors+i):T,(i*n_factors+1):((i+1)*n_factors)] <- head(F[n_factors:T,1:n_factors], - i)
  }
  colnames(F)=paste('F_',colnames(F),sep='')
  return(F)
}

#(C)
scaled_predictors <- as.data.frame(scale(predictors))
n_F <- 8
n_L <- 12
factors_lags_predictors <- create_factors(scaled_predictors,n_factors=n_F,n_lags=n_L,T=nrow(df))

#(D)
FM_predictors <- cbind(shifted_target,target_lags,factors_lags_predictors) #Number NA's = n_factors - 1

#(E)
poos <- 120
prediction <- c(df[1:(nrow(df)-poos),"unempl"],rep(0,poos))
for (i in (nrow(df)-poos+1):nrow(df)) {
  model <- lm(formula = shifted_target ~ ., data = FM_predictors[1:i-1,])
  forecast <- model$coefficients[1] + as.double(model$coefficients[-c(1)]) %*% as.double(FM_predictors[i,-c(1)])
  prediction[i] <- forecast
}
resids <- prediction[(nrow(df)-poos+1):434] - df[(nrow(df)-poos+1):434,"unempl"]
rmse <- sqrt(mean(resids^2))