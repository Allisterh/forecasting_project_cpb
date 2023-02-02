### ---- Function file ----
# File with all function that we use, run at the start of any script to have the functions available
# Load all libraries first
library(data.table)
library(vars)
library(BVAR)
library(randomForest)

## -- Feature Engineering --

## - X - Feature matrix of (lagged) regressors X

X_function <- function(regressor_matrix, X_lags=12){
  return(as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE)))
} 

## - F - Feature matrix of Factors and their lags
F_function <- function(regressor_matrix, n_Factors, F_lags,boolo= FALSE){
  F <- data.frame(matrix(ncol = n_Factors, nrow = nrow(regressor_matrix)))
  for (t in n_Factors:nrow(regressor_matrix)){
    newpca <- prcomp(regressor_matrix[1:t,], center = boolo,scale. = boolo)
    F[t,1:n_Factors] <- newpca$x[1:n_Factors]
  }
  #F <- as.data.frame(shift(F, n=0:F_lags, type = 'lag', give.names=TRUE)) #shift function for lags of factors
  return(F)
}

## - MAF - 
MAF_function <- function(lagged_regressors, X_lags=12, P_MAF=12, n_MAF=3, boolo = FALSE) { #Use X (Feature matrix with lags) as input!
  n_var <- ncol(lagged_regressors)/(P_MAF+1)
  MAF <- data.frame(matrix(ncol = (n_MAF*n_var), nrow = nrow(lagged_regressors)))
  for (v in 1:n_var){
    for (t in (2*P_MAF+1):T){
      X_pca <- prcomp(lagged_regressors[(t-P_MAF):t,(((v-1)*X_lags)+1):(v*X_lags + 1)], center = boolo, scale. = boolo)
      MAF[t,(1+(v-1)*n_MAF):(v*n_MAF)] <- X_pca$x[1:n_MAF]
    }
  }
  colnames(MAF)=paste('MAF_',colnames(MAF),sep='')
  return(MAF)
}

## - MAF2 - 
MAF_function2 <- function(X, X_lags, T, n_var, P_MAF, n_MAF, boolo) { #Use X (Feature matrix with lags) as input!
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
MARX_function <- function(regressor_matrix, P_MARX) {
  var = VAR(regressor_matrix, p = P_MARX, type = "const")
  matata = as.matrix(var$datamat)
  n_var <- ncol(regressor_matrix)
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

## - Create all feature matrices and put in list
Create_all_feature_matrices <- function(regressor_matrix,X_lags = 12,n_Factors = 3,F_lags = 12,P_MAF = 12,n_MAF = 3,P_MARX = 12){
  
  X_with_lags <- X_function(regressor_matrix, X_lags)
  
  scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
  F <- F_function(scaled_regressor_matrix, n_Factors, F_lags)
  
  scaled_x_lags <- scale(X_with_lags) #Scale lagged X Matrix for MAF
  MAF <- MAF_function2(scaled_x_lags,X_lags,nrow(scaled_x_lags),ncol(regressor_matrix), P_MAF, n_MAF, FALSE)
  
  MARX <- MARX_function(regressor_matrix, P_MARX)
  
  X_F <- cbind(X_with_lags, F)
  X_MAF <- cbind(X_with_lags, MAF)
  X_MARX <- cbind(X_with_lags, MARX)
  F_MAF <- cbind(F, MAF)
  F_MARX <- cbind(F, MARX)
  MAF_MARX <- cbind(MAF, MARX)
  X_F_MAF <- cbind(X_with_lags, F, MAF)
  X_F_MARX <- cbind(X_with_lags, F, MARX)
  X_MAF_MARX <- cbind(X_with_lags, MAF, MARX)
  F_MAF_MARX <- cbind(F, MAF, MARX)
  X_F_MAF_MARX <- cbind(X_with_lags, F, MAF, MARX)
  
  return(list(X_F,X_MAF,X_MARX,F_MAF,F_MARX,MAF_MARX,X_F_MAF,X_F_MARX,X_MAF_MARX,F_MAF_MARX,X_F_MAF_MARX))
}

# ---- BIC optimization for Factor Model

BIC_opt <- function(){
  for (h in c(1,3,6,9,12,24)){
    old_BIC <- 0
    new_BIC <- 0
    best_params = c(0,0)
    for (f in 1:12) { # loop over number of lags for F
      for (k in 1:12) { # loop over number of lags for y
        shift_unempl <- as.data.frame(shift(unemployment,n=h, type = 'lead', give.names=TRUE))
        p_y <- as.data.frame(shift(unemployment,n=1:k, type = 'lag', give.names=TRUE))
        p_f <- as.data.frame(shift(F_Factor_Model,n=1:f, type = 'lag', give.names=TRUE))
        colnames(p_y)[1]= "Unemployment"
        loop_data <- cbind(shift_unempl,p_y,p_f)
        if(f==1 && k == 1) {
          old_BIC <- BIC(lm(formula = Unemployment~.,data = loop_data[8:763,]))
          best_params = c(f,k)
        } else { 
          new_BIC <- BIC(lm(formula = Unemployment~.,data = loop_data[8:763,]))
          if (new_BIC < old_BIC){
            old_BIC <- new_BIC
            best_params = c(f,k)
          }
        }
      }
    }
    print(paste("best model for h = ",h," is #factor: ", best_params[1], " and #lags: ", best_params[2]))
  }
}

# - Forecasting function

Forecasting_function <- function(y, Z, n_forecast, horizons,ntrees=200){
  RF_y_forecast <- data.frame(matrix(ncol = length(horizons), nrow = n_forecast))
  #BF_y_forecast <- ...
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
      
      # Boosted Trees
      #X.BT <- ...
      #BT_y_forecast[f,] <- ...'
      print(f)
      print(Sys.time()-nu)
    }
    colnames(RF_y_forecast)[i]=paste('h=',h,sep='')
    print(h)
  }
  return(RF_y_forecast)
}

