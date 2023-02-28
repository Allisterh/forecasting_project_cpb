#Clearing Environment
rm(list=ls())

#Nikki:
df_small <- read.csv("Extra data/data_core.csv")
df_big <- read.csv("Extra data/data_additional.csv")
df_big_new <- cbind(df_small[((433-384+1):433),], df_big)

### ---- FEATURE ENGINEERING ----
regressor_matrix <- df_big_new[-c(1)] #Remove dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)
X_lags <- 12
n_Factors <- 3 #TO BE ESTIMATED
F_lags <- 12 
P_MAF <- 12 
n_MAF <- 3 #TO BE ESTIMATED
P_MARX <- 12 

## - X - 
library(data.table)
X_function <- function(regressor_matrix, X_lags){
  return(as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE)))
} 

## - F - 
n_factors_opt <- function(regressor_matrix, T, boolo){
  PCs <- data.frame(matrix(ncol = 1, nrow = T))
  
  for (t in 1:(T)){
    X_pca <- prcomp(regressor_matrix[1:t,], center = boolo, scale. = boolo)
    
    eigenvalues <- X_pca$sdev^2
    numberofPCs <- 0
    for (i in 1:length(eigenvalues)){
      if (eigenvalues[i] > 1){
        numberofPCs <- numberofPCs + 1
      }
    }
    PCs[t,] <- numberofPCs
  }
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  # Create the vector with numbers.
  v <- PCs
  print(PCs)
  
  # Calculate the mode using the user function.
  result <- getmode(v)
  return(result)
}

scaled_regressor_matrix <- scale(regressor_matrix)
n_factors <- n_factors_opt(scaled_regressor_matrix, T, FALSE)

scaled_X <- scale(X)
n_factors <- n_factors_opt(scaled_X[13:T,], T-13, FALSE)

## - MAF - 
MAF_function <- function(X, X_lags, T, n_var, P_MAF, n_MAF) { #Use X (Feature matrix with lags) as input!
  MAF <- data.frame(matrix(ncol = (n_MAF*n_var), nrow = T))
  for (v in 1:n_var){
    for (t in (2*P_MAF+1):T){
      X_pca <- prcomp(X[(t-P_MAF):t,(((v-1)*X_lags)+1):(v*X_lags)], center = TRUE, scale. = TRUE)
      MAF[t,(1+(v-1)*n_MAF):(v*n_MAF)] <- X_pca$x[1:n_MAF]
    }
  }
  colnames(MAF)=paste('MAF_',colnames(MAF),sep='')
  return(MAF)
}

## -- Combinations of Z --
X <- X_function(regressor_matrix, X_lags)

#scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F  # NOW IN FUNCTION AS TRUE
F <- F_function(regressor_matrix, n_Factors, F_lags, T)

#scaled_X <- scale(X) # Scale Feature X for PCA in MAF # NOW IN FUNCTION AS TRUE
MAF <- MAF_function(X, X_lags, T, n_var, P_MAF, n_MAF)
