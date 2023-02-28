#Clearing Environment
rm(list=ls())

df <- read.csv("data-eur2023.csv", row.names = 1)

#Vincent: 
#df <- read.csv("/Users/vincentvanpul/Desktop/data-eur2023.csv", row.names=1)

#Sophia:
df <- read.csv("C:/Users/sophi_j0d2ugq/OneDrive/Documents/Data Seminar/data-eur2023.csv", row.names=1)

### ---- FEATURE ENGINEERING ----
library(vars)
library(randomForest)
library(urca)

tobedifferenced_matrix <- df[-c(1)] # Remove pubdate 
colstodiff <- c(1, 2, 3, 6, 7, 8)

# take the first difference of the selected columns
diff_data <- diff(as.matrix(tobedifferenced_matrix[, colstodiff]), differences = 1)
diff_data <- data.frame(cbind(diff_data, df$L1_BSCI[2:434], df$L1_CSCICP02[2:434], df$L1_AEX[2:434]))

regressor_matrix_diff <- diff_data[-c(1)]
# MAYBE WE NEED TO OPTIMIZE THE LAGS / FACTORS AGAIN 
n_var <- ncol(regressor_matrix_diff)
T <- nrow(regressor_matrix_diff)
X_lags <- 6 #average 4 and 7 (AR) (THIS, IS BULLSHIT)
n_Factors <- 2 #Optimization (WE STILL HAVE TO DO THIS FOR THE DIFFERENCED VARIABLES)
F_lags <- 6 #Average of 4 and 7 (AR) (THIS IS BULLSHIT)
P_MAF <- 6 #Average of 4 and 7 (AR)
n_MAF <- 6 #Optimization
P_MARX <- 6 #Average of 4 and 7 (AR)

## - X - 
library(data.table)
X_function <- function(regressor_matrix_diff, X_lags){
  return(as.data.frame(shift(regressor_matrix_diff,n=0:X_lags, type = 'lag', give.names=TRUE)))
} 

## - F - 
factors_t <- function(regressor_matrix_diff, n_Factors, boolo) {
  X_pca <- prcomp(regressor_matrix_diff, center = boolo,scale. = boolo)
  factors_t <- X_pca$x[1:n_Factors]
  return(factors_t)
}

F_function <- function(regressor_matrix_diff, n_Factors, F_lags, T, boolo){
  F <- data.frame(matrix(ncol = n_Factors, nrow = T))
  for (t in n_Factors:T){
    F[t,1:n_Factors] <- factors_t(regressor_matrix_diff[1:t,], n_Factors, boolo)
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
X <- X_function(regressor_matrix_diff, X_lags)

scaled_regressor_matrix_diff <- scale(regressor_matrix_diff) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix_diff, n_Factors, F_lags, T, FALSE)

scaled_X <- scale(X) # Scale Feature X for PCA in MAF
MAF <- MAF_function(scaled_X, X_lags, T, n_var, P_MAF, n_MAF, FALSE)

MARX <- MARX_function(regressor_matrix_diff, P_MARX, n_var)

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
library(xgboost)

horizons <- list(3, 6, 12, 18, 24)
#Unempl <- df[,2]
Unempl <- diff_data[,1]
n_forecast <- 120 # CPB time window
#y_real <- Unempl[315:434] #MAYBE NEEDS TO BE CHANGED TO ACCOUNT FOR THE DIFFERENCES 
y_real <- Unempl[314:433]
ntrees <- 200 #Accurate but slow

Forecasting_function <- function(y, Z, n_forecast, horizons){
  BT_y_forecast <- data.frame(matrix(ncol = length(horizons), nrow = n_forecast))
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
    
    #Optimization
    set.seed(2021)

    
    for (f in 1:n_forecast){
      # Boosted Trees
      #print(f)
      train_x <- y_Z[1:(nrow(y_Z)-n_forecast+f-1),2:ncol(y_Z)]
      train_y <- y_Z[1:(nrow(y_Z)-n_forecast+f-1),1]
      test_x <- y_Z[nrow(y_Z)-n_forecast+f,2:ncol(y_Z)]
      test_y <- y_Z[nrow(y_Z)-n_forecast+f,1]
      
      if(f==1 || f%%24==0){
        
        # Set up parameter grid
        params <- expand.grid(
          nrounds = c(50, 100, 200, 500), #Need sources to back up these numbers
          eta = c(0.01, 0.1, 0.3) # Need sources to back up these numbers
        )
        
        # Define CV method
        ctrl <- trainControl(
          method = "repeatedcv",
          number = 5,
          repeats = 3,
          verboseIter = FALSE,
          returnResamp = "all"
        )
        
        # Train and tune the XGBoost model using grid search
        xgb <- xgb.train(
          data = train_x,
          label = train_y,
          method = "xgbTree",
          trControl = ctrl,
          tuneGrid = params,
          verbose = FALSE
        )
        
        # Use the best parameters to train the final model on the full training set
        optimizednrounds = xgb$bestTune$nrounds
        optimizedeta = xgb$bestTune$eta
      }
      
      X_BT_tuned <- xgboost(eta = optimizedeta,
                            data = as.matrix(train_x),
                            label = as.matrix(train_y),
                            nrounds = optimizednrounds,
                            max.depth = 5, # Dit is misschien wel laag voor XGB? 
                            eval_metric = "rmse",
                            verbose = 0)
      
      xgb_test <- xgb.DMatrix(data = as.matrix(test_x), label = as.matrix(test_y))
      
      BT_y_forecast[f,i] = predict(X_BT_tuned, xgb_test) # add the first difference at the end, only to the dependent variable 
      
    }
    colnames(BT_y_forecast)[i]=paste('h=',h,sep='')
  }
  return(BT_y_forecast)
}

## -- Forecasting Z -- 
k <- 0
time_1 <- Sys.time()

BT_X_forecast <- Forecasting_function(Unempl, X, n_forecast, horizons)
k <- k + 1
time_2 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_X: ", time_2 - time_1)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_2 - time_1)
cat("", sep="\n\n")

BT_F_forecast <- Forecasting_function(Unempl, F, n_forecast, horizons)
k <- k + 1
time_3 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_F: ", time_3 - time_2)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_3 - time_1)
cat("", sep="\n\n")

BT_MAF_forecast <- Forecasting_function(Unempl, MAF, n_forecast, horizons)
k <- k + 1
time_4 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_MAF: ", time_4 - time_3)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_4 - time_1)
cat("", sep="\n\n")

BT_MARX_forecast <- Forecasting_function(Unempl, MARX, n_forecast, horizons)
k <- k + 1
time_5 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_MARX: ", time_5 - time_4)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_5 - time_1)
cat("", sep="\n\n")

BT_X_F_forecast <- Forecasting_function(Unempl, X_F, n_forecast, horizons)
k <- k + 1
time_6 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_X_F: ", time_6 - time_5)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_6 - time_1)
cat("", sep="\n\n")

BT_X_MAF_forecast <- Forecasting_function(Unempl, X_MAF, n_forecast, horizons)
k <- k + 1
time_7 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_X_MAF: ", time_7 - time_6)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_7 - time_1)
cat("", sep="\n\n")

BT_X_MARX_forecast <- Forecasting_function(Unempl, X_MARX, n_forecast, horizons)
k <- k + 1
time_8 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_X_MARX: ", time_8 - time_7)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_8 - time_1)
cat("", sep="\n\n")

BT_F_MAF_forecast <- Forecasting_function(Unempl, F_MAF, n_forecast, horizons)
k <- k + 1
time_9 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_F_MAF: ", time_9 - time_8)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_9 - time_1)
cat("", sep="\n\n")

BT_F_MARX_forecast <- Forecasting_function(Unempl, F_MARX, n_forecast, horizons)
k <- k + 1
time_10 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_F_MARX: ", time_10 - time_9)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_10 - time_1)
cat("", sep="\n\n")

BT_MAF_MARX_forecast <- Forecasting_function(Unempl, MAF_MARX, n_forecast, horizons)
k <- k + 1
time_11 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_MAF_MARX: ", time_11 - time_10)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_11 - time_1)
cat("", sep="\n\n")

BT_X_F_MAF_forecast <- Forecasting_function(Unempl, X_F_MAF, n_forecast, horizons)
k <- k + 1
time_12 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_X_F_MAF: ", time_12 - time_11)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_12 - time_1)
cat("", sep="\n\n")

BT_X_F_MARX_forecast <- Forecasting_function(Unempl, X_F_MARX, n_forecast, horizons)
k <- k + 1
time_13 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_X_F_MARX: ", time_13 - time_12)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_13 - time_1)
cat("", sep="\n\n")

BT_X_MAF_MARX_forecast <- Forecasting_function(Unempl, X_MAF_MARX, n_forecast, horizons)
k <- k + 1
time_14 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_X_MAF_MARX: ", time_14 - time_13)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_14 - time_1)
cat("", sep="\n\n")

BT_F_MAF_MARX_forecast <- Forecasting_function(Unempl, F_MAF_MARX, n_forecast, horizons)
k <- k + 1
time_15 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_F_MAF_MARX: ", time_15 - time_14)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_15 - time_1)
cat("", sep="\n\n")

BT_X_F_MAF_MARX_forecast <- Forecasting_function(Unempl, X_F_MAF_MARX, n_forecast, horizons)
k <- k + 1
time_16 <- Sys.time()
cat("", sep="\n\n")
cat("Running Time BT_X_F_MAF_MARX: ", time_16 - time_15)
cat("Progress: ", 100*k/15)
cat("Total Running Time: ", time_16 - time_1)
cat("", sep="\n\n")

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

# -- DB Function -- 
library(forecast)
DB_BT <- data.frame(matrix(ncol = length(horizons), nrow = n_combinations))
numberofhorizons <- c(3,6,12,18,24)

for(i in 1:5){
DB_BT[1,i] <- dm.test((y_real - BT_X_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i]) # Only there for completeness, but this value is redundant
DB_BT[2,i] <- dm.test((y_real - BT_F_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[3,i] <- dm.test((y_real - BT_MAF_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[4,i] <- dm.test((y_real - BT_MARX_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[5,i] <- dm.test((y_real - BT_X_F_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[6,i] <- dm.test((y_real - BT_X_MAF_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[7,i] <- dm.test((y_real - BT_X_MARX_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[8,i] <- dm.test((y_real - BT_F_MAF_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[9,i] <- dm.test((y_real - BT_F_MARX_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[10,i] <- dm.test((y_real - BT_MAF_MARX_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[11,i] <- dm.test((y_real - BT_X_F_MAF_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[12,i] <- dm.test((y_real - BT_X_F_MARX_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[13,i] <- dm.test((y_real - BT_X_MAF_MARX_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[14,i] <- dm.test((y_real - BT_F_MAF_MARX_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
DB_BT[15,i] <- dm.test((y_real - BT_X_F_MAF_MARX_forecast), (y_real - BT_X_forecast), h = numberofhorizons[i])
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

#Saving row and column names for Diebold Mariano
rownames(DB_BT) <- c("X", "F", "MAF", "MARX", "X,F", "X,MAF", "X,MARX", "F,MAF", "F,MARX", "MAF,MARX", "X,F,MAF", "X,F,MARX", "X,MAF,MARX", "F,MAF,MARX", "X,F,MAF,MARX")
colnames(DB_BT) <- c("h=3", "h=6", "h=12", "h=18", "h=24")

# Saving Prediction Tables
write.csv(BT_X_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_X_forecast.csv", row.names=FALSE)
write.csv(BT_F_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_F_forecast.csv", row.names=FALSE)
write.csv(BT_MAF_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_MAF_forecast.csv", row.names=FALSE)
write.csv(BT_MARX_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_X_F_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_X_F_forecast.csv", row.names=FALSE)
write.csv(BT_X_MAF_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_X_MAF_forecast.csv", row.names=FALSE)
write.csv(BT_X_MARX_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_X_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_F_MAF_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_F_MAF_forecast.csv", row.names=FALSE)
write.csv(BT_F_MARX_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_F_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_MAF_MARX_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_X_F_MAF_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_X_F_MAF_forecast.csv", row.names=FALSE)
write.csv(BT_X_F_MARX_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_X_F_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_X_MAF_MARX_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_X_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_F_MAF_MARX_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_F_MAF_MARX_forecast.csv", row.names=FALSE)
write.csv(BT_X_F_MAF_MARX_forecast, "/Users/vincentvanpul/Desktop/R_Output/CPB_BT_X_F_MAF_MARX_forecast.csv", row.names=FALSE)

write.csv(RMSE_BT, "/Users/vincentvanpul/Desktop/R_Output/CPB_RMSE_BT.csv", row.names=TRUE)
write.csv(RMSE_BT, "/Users/vincentvanpul/Desktop/R_Output/CPB_DB_BT.csv", row.names=TRUE)