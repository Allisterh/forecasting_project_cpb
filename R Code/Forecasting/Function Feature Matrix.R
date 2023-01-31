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
  F <- as.data.frame(shift(F, n=0:F_lags, type = 'lag', give.names=TRUE)) #shift function for lags of factors
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
  MAF <- MAF_function(scaled_x_lags,X_lags, P_MAF, n_MAF)
  
  MARX <- MARX_function(regressor_matrix, P_MARX)
  
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
  
  return(list(X_F,X_MAF,X_MARX,F_MAF,F_MARX,MAF_MARX,X_F_MAF,X_F_MARX,X_MAF_MARX,F_MAF_MARX,X_F_MAF_MARX))
}
