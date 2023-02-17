####################################################
######## Code to Compute FM and FM-RMSE, US ########
######## Automated Script ##########################
######## 30.01.23 ##################################
####################################################

#Clearing Environment
rm(list=ls())
library("data.table")

df <- read.csv("~/Desktop/data122022.csv", row.names=1)
df <- df[2:709,]  #dec 2017
predictors <- subset(df,select=-c(UNRATE))
target <- df[,"UNRATE"]

h <- 3            #horizon
n_TL <- 4         #number of lags target
n_F <- 8          #number of factors
n_L <- 3          #number of lags of factors
poos <- 120       #number of forecasts (out-of-sample)

#Function to Create Lags
create_lags <- function(data_frame, n_lags){
  result <- as.data.frame(shift(data_frame, n=0:n_lags, type='lag', give.names=TRUE))
  return(result)
}

#Function to Create Factors and Lagged Factors
create_factors_aux <- function(data_frame, n_factors){
  pca <- prcomp(na.omit(data_frame), center = FALSE, scale. = FALSE) 
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

#Function to Compute RMSE
computing_rmse <- function(data_frame,the_n_TL,the_h,the_predictors,the_n_F,the_n_L,the_poos){
  #Creating Predictors
  target_lags <- create_lags(data_frame[,"UNRATE"],the_n_TL)
  shifted_target <- c(data_frame[,"UNRATE"][-(seq(the_h))],rep(NA,the_h)) 
  
  scaled_predictors <- as.data.frame(scale(the_predictors))
  factors_lags_predictors <- create_factors(na.omit(scaled_predictors),the_n_F,the_n_L,nrow(data_frame))
  FM_predictors <- cbind(shifted_target,target_lags,factors_lags_predictors) 
  
  #Forecasting 
  prediction <- c(data_frame[1:(nrow(data_frame)-the_poos),"UNRATE"],rep(0,the_poos))
  for (i in (nrow(data_frame)-the_poos+1):nrow(data_frame)) {
    model <- lm(formula = shifted_target ~ ., data = FM_predictors[1:i-1,])
    forecast <- model$coefficients[1] + as.double(model$coefficients[-c(1)]) %*% as.double(FM_predictors[i,-c(1)])
    prediction[i] <- forecast
  }
  
  #Computing Residuals and RMSE
  resids <- na.omit(prediction[(nrow(data_frame)-the_poos+1):nrow(data_frame)] - data_frame[(nrow(data_frame)-the_poos+1):nrow(data_frame),"UNRATE"])
  rmse <- sqrt(mean(resids^2))
  return(rmse)
}

#Optimize RMSE Over Combinations n_Factors, n_Lags, n_Lags_Predictor
results_1m_f <- matrix(, nrow = 12, ncol = 12)
results_3m_f <- matrix(, nrow = 12, ncol = 12)
results_6m_f <- matrix(, nrow = 12, ncol = 12)
results_9m_f <- matrix(, nrow = 12, ncol = 12)
results_12m_f <- matrix(, nrow = 12, ncol = 12)
results_24m_f <- matrix(, nrow = 12, ncol = 12)
k<- 0
for (i in 1:12) {     #number of factors
  for (j in 1:12) {   #number of lags
    start_1 <- Sys.time()
    results_1m_f[i,j] <- computing_rmse(df,the_n_TL = i, the_h = 1, the_predictors = predictors, the_n_F = 3, the_n_L = j, the_poos = 456)
    end_1 <- Sys.time()
    k <- k+1
    print(c(as.integer(i),as.integer(j),as.integer(1),as.integer(end_1-start_1),100*k/864))
    
    start_3 <- Sys.time()
    results_3m_f[i,j] <- computing_rmse(df,the_n_TL = i, the_h = 3, the_predictors = predictors, the_n_F = 3, the_n_L = j, the_poos = 456)
    end_3 <- Sys.time()
    k <- k+1
    print(c(as.integer(i),as.integer(j),as.integer(3),as.integer(end_3-start_3),100*k/864))
    
    start_6 <- Sys.time()
    results_6m_f[i,j] <- computing_rmse(df,the_n_TL = i, the_h = 6, the_predictors = predictors, the_n_F = 3, the_n_L = j, the_poos = 456)
    end_6 <- Sys.time()
    k <- k+1
    print(c(as.integer(i),as.integer(j),as.integer(6),as.integer(end_6-start_6),100*k/864))
    
    start_9 <- Sys.time()
    results_9m_f[i,j] <- computing_rmse(df,the_n_TL = i, the_h = 9, the_predictors = predictors, the_n_F = 3, the_n_L = j, the_poos = 456)
    end_9 <- Sys.time()
    k <- k+1
    print(c(as.integer(i),as.integer(j),as.integer(9),as.integer(end_9-start_9),100*k/864))
    
    start_12 <- Sys.time()
    results_12m_f[i,j] <- computing_rmse(df,the_n_TL = i, the_h = 12, the_predictors = predictors, the_n_F = 3, the_n_L = j, the_poos = 456)
    end_12 <- Sys.time()
    k <- k+1
    print(c(as.integer(i),as.integer(j),as.integer(12),as.integer(end_12-start_12),100*k/864))
    
    start_24 <- Sys.time()
    results_24m_f[i,j] <- computing_rmse(df,the_n_TL = i, the_h = 24, the_predictors = predictors, the_n_F = 3, the_n_L = j, the_poos = 456)
    end_24 <- Sys.time()
    k <- k+1
    print(c(as.integer(i),as.integer(j),as.integer(24),as.integer(end_24-start_24),100*k/864))
  }
}

#Creating Forecasts: 3 Factors, 3 Lags of Factors, 4 Lags of Target
#dates = rbind(seq(from = as.Date("1960-01-01"), to = as.Date("2017-12-01"), by = 'month'),"RMSE")
h_1 <- computing_rmse(df,4,the_h=1,predictors,3,3,456)
h_3 <- computing_rmse(df,4,the_h=3,predictors,3,3,456)
h_6 <- computing_rmse(df,4,the_h=6,predictors,3,3,456)
h_9 <- computing_rmse(df,4,the_h=9,predictors,3,3,456)
h_12 <- computing_rmse(df,4,the_h=12,predictors,3,3,456)
h_24 <- computing_rmse(df,4,the_h=24,predictors,3,3,456)

forecasts <- as.data.frame(cbind(h_1,h_3,h_6,h_9,h_12,h_24))
write.csv(forecasts, "~/Desktop/forecasts_v2.csv", row.names=FALSE)

write.csv(results_1m_f, "~/Desktop/rmse_1m_f.csv")
write.csv(results_3m_f, "~/Desktop/rmse_3m_f.csv")
write.csv(results_6m_f, "~/Desktop/rmse_6m_f.csv")
write.csv(results_9m_f, "~/Desktop/rmse_9m_f.csv")
write.csv(results_12m_f, "~/Desktop/rmse_12m_f.csv")
write.csv(results_24m_f, "~/Desktop/rmse_24m_f.csv")

