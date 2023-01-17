#hoi
#Clearing Environment
rm(list=ls())

#Importing Data
df <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/data-eur2023.csv", row.names=1)

### ---- FEATURE ENGINEERING ----
library(vars)
X_t <- df[-c(1,2)] #Remove pubdate and dependent variable
n_var <- ncol(X_t)
T <- nrow(X_t)
X_lags <- 4
n_Factors <- 3
F_lags <- 4
P_MAF <- 12
n_MAF <- 3
P_MARX = 12

## - Feature Matrix (Total) -
FeatureMatrix <- function(X_t, T, X_lags, n_var, n_Factors, F_lags, P_MAF, n_MAF, P_MARX){
  X_function <- function(X_t, X_lags, n_var){
    X <- data.frame(matrix(ncol = (X_lags+1)*n_var, nrow = T))
    X[,1:n_var] <- X_t
    for (l in 1:X_lags){
      X[,(l*n_var+1):((l+1)*n_var)] <- rbind(X_t[1:l,]*NA, head(X_t, - l))
    }
    colnames(X)=paste('X_',colnames(X),sep='')
    return(X)
  }
  
  X <- X_function(X_t, X_lags, n_var)
  
  F_function <- function(X, n_Factors, F_lags, T){
    factors_t <- function(X, n_Factors) {
      X_pca <- prcomp(X, center = TRUE,scale. = TRUE)
      factors_t <- X_pca$x[1:n_Factors]
      return(factors_t)
    }
    F <- data.frame(matrix(ncol = (n_Factors*(F_lags+1)), nrow = T))
    for (t in n_Factors:T){
      F[t,1:n_Factors] <- factors_t(X_t[1:t,], n_Factors)
    }
    for (i in 1:F_lags){
      F[(n_Factors+i):T,(i*n_Factors+1):((i+1)*n_Factors)] <- head(F[n_Factors:T,1:n_Factors], - i)
    }
    colnames(F)=paste('F_',colnames(F),sep='')
    return(F)
  }
  
  F <- F_function(X_t, n_Factors, F_lags, T)
  
  MAF_function <- function(X, T, n_var, P_MAF, n_MAF) {
    MAF <- data.frame(matrix(ncol = (n_MAF*n_var), nrow = T))
    for (v in 1:n_var){
      for (t in (P_MAF+1):T){
        X_pca <- prcomp(X_t[(t-P_MAF):t,v], center = TRUE,scale. = TRUE)
        MAF[t,(1+(v-1)*n_MAF):(v*n_MAF)] <- X_pca$x[1:n_MAF]
      }
    }
    colnames(MAF)=paste('MAF_',colnames(MAF),sep='')
    return(MAF)
  }
  
  MAF <- MAF_function(X_t, T, n_var, P_MAF, n_MAF)
  
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
  
  MARX <- MARX_function(X_t, P_MARX, n_var)
  
  FeatureMatrix <- cbind(X, F, MAF, MARX)
  return(FeatureMatrix)
}

FeatureMatrix <- FeatureMatrix(X_t, T, X_lags, n_var, n_Factors, F_lags, P_MAF, n_MAF, P_MARX)

## - X - 
X_function <- function(X_t, X_lags, n_var){
  X <- data.frame(matrix(ncol = (X_lags+1)*n_var, nrow = T))
  X[,1:n_var] <- X_t
  for (l in 1:X_lags){
    X[,(l*n_var+1):((l+1)*n_var)] <- rbind(X_t[1:l,]*NA, head(X_t, - l))
  }
  colnames(X)=paste('X_',colnames(X),sep='')
  return(X)
}

X <- X_function(X_t, X_lags, n_var)

## - F - 
F_function <- function(X, n_Factors, F_lags, T){
  factors_t <- function(X, n_Factors) {
    X_pca <- prcomp(X, center = TRUE,scale. = TRUE)
    factors_t <- X_pca$x[1:n_Factors]
    return(factors_t)
  }
  F <- data.frame(matrix(ncol = (n_Factors*(F_lags+1)), nrow = T))
  for (t in n_Factors:T){
    F[t,1:n_Factors] <- factors_t(X_t[1:t,], n_Factors)
  }
  for (i in 1:F_lags){
    F[(n_Factors+i):T,(i*n_Factors+1):((i+1)*n_Factors)] <- head(F[n_Factors:T,1:n_Factors], - i)
  }
  colnames(F)=paste('F_',colnames(F),sep='')
  return(F)
}

F <- F_function(X_t, n_Factors, F_lags, T)

## - MAF - 
MAF_function <- function(X, T, n_var, P_MAF, n_MAF) {
  MAF <- data.frame(matrix(ncol = (n_MAF*n_var), nrow = T))
  for (v in 1:n_var){
    for (t in (P_MAF+1):T){
      X_pca <- prcomp(X_t[(t-P_MAF):t,v], center = TRUE,scale. = TRUE)
      MAF[t,(1+(v-1)*n_MAF):(v*n_MAF)] <- X_pca$x[1:n_MAF]
    }
  }
  colnames(MAF)=paste('MAF_',colnames(MAF),sep='')
  return(MAF)
}

MAF <- MAF_function(X_t, T, n_var, P_MAF, n_MAF)

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

MARX <- MARX_function(X_t, P_MARX, n_var)

## - Combinations - 
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

### ---- RANDOM FOREST ----
install.packages("randomForest")
library(randomForest)
Unempl <- df[c(2)] # Store unemployment in target variable y

# Y lags <-- Do we put this in Z?
n_Ylags <- 12

Ylags_function <- function(Y, n_Ylags){
  Ylags <- Y
  for (l in 1:n_Ylags){
    new_y <- rbind(head(Y,-(T-l))*NA, head(Y, - l))
    colnames(new_y)=paste('Lag',l,sep='')
    Ylags <- cbind(Ylags, new_y)
  }
  colnames(Ylags)=paste('Ylags_',colnames(Ylags),sep='')
  return(Ylags)
}

Unempl_lags <- Ylags_function(Unempl, n_Ylags)

# - RF: X - 
Z <- X
y_Z <- cbind(Unempl, Z)

n_forecast <- 434-315
y_forecast <- data.frame(matrix(ncol = 1, nrow = n_forecast))

for (f in 1:n_forecast){
  y_Z_train <- y_Z[1:(315+f-1),]
  y_Z_test <- y_Z[315+f,]
  
  X.rf <- randomForest(L2_LRHUTTTT ~ ., data = y_Z_train, ntree = 200, mtry = (ncol(Z)/3),
                       importance = TRUE, na.action = na.omit)
  y_forecast[f,] <- predict(X.rf, y_Z_test)
}

# Plotting RF Forecast with Z=X
y_real <- y_Z[316:434,1]
error_Xrf <- abs(y_real - y_forecast)
time <- seq(1, n_forecast, 1)

plot(time, y_real, type = "l", frame = FALSE, pch = 19, 
     col = "red", xlab = "x", ylab = "y")
lines(time, y_forecast[,1], pch = 18, col = "blue", type = "l", lty = 2)
legend("topright", legend=c("Real y", "Forecast y (RF, Z=X)"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)

