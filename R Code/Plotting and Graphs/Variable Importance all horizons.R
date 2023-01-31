#Clearing Environment
rm(list=ls())

#Nikki:
df <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/data-eur2023.csv", row.names=1)

### ---- FEATURE ENGINEERING ----
library(vars)
library(randomForest)
regressor_matrix <- df[-c(1,2)] #Remove pubdate and dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)
X_lags <- 6 #average 4 and 7 (AR)
n_Factors <- 2 #Optimization
F_lags <- 6 #Average of 4 and 7 (AR)
P_MAF <- 6 #Average of 4 and 7 (AR)
n_MAF <- 6 #Optimization
P_MARX <- 6 #Average of 4 and 7 (AR)

## - X - 
library(data.table)
X_function <- function(regressor_matrix, X_lags){
  return(as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE)))
} 

## - F - 
factors_t <- function(regressor_matrix, n_Factors, boolo) {
  X_pca <- prcomp(regressor_matrix, center = boolo,scale. = boolo)
  factors_t <- X_pca$x[1:n_Factors]
  return(factors_t)
}

F_function <- function(regressor_matrix, n_Factors, F_lags, T, boolo){
  F <- data.frame(matrix(ncol = n_Factors, nrow = T))
  for (t in n_Factors:T){
    F[t,1:n_Factors] <- factors_t(regressor_matrix[1:t,], n_Factors, boolo)
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
MARX_function <- function(regressor_matrix, P_MARX, n_var) {
  var = VAR(regressor_matrix, p = P_MARX, type = "const")
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
  mat_x_marx <- data.frame(mat_x_marx) #(NOT IN COULOMBE) --> make dataframe
  mat_x_marx <- mat_x_marx[,1:(ncol(mat_x_marx)-1)] #Remove last column with MARX constant (NOT IN COULOMBE)
  mat_x_marx <- rbind(mat_x_marx[1:P_MARX,]*NA, mat_x_marx) #Add NaN rows on top ; (NOT IN COULOMBE) --> must do this for alignment rows in Z
  return(mat_x_marx)
}

## -- Combinations of Z --
X <- X_function(regressor_matrix, X_lags)

scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags, T, FALSE)

scaled_X <- scale(X) # Scale Feature X for PCA in MAF
MAF <- MAF_function(scaled_X, X_lags, T, n_var, P_MAF, n_MAF, FALSE)

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

n_combinations <- 15

### --- VARIABLE IMPORTANCE ---
horizons <- list(3, 6, 12, 18, 24)
Unempl <- df[,2]
n_forecast <- 434-315 # CPB time window
y_real <- Unempl[316:434]

dutch_forecasts <- c(434,315) #Begin forecasting from 316

# RANDOM FOREST - direct
library(varImp)
library(caret)
ntrees <- 200 #Accurate but slow

VI_function <- function(y, Z, n_forecast, horizons, ntrees){
  importance <- data.frame(matrix(ncol = length(horizons), nrow = ncol(Z)))
  i <- 0
  for (h in horizons){
    shift_y = as.data.frame(shift(y,n=h, type = 'lead', give.names=TRUE))
    colnames(shift_y) = 'y'
    y_Z <- cbind(shift_y, Z)
    i <- i+1
    nu <- Sys.time()
    
    y_Z_train <- y_Z[1:434,]
    
    X.rf <- randomForest(y ~ ., 
                         data = y_Z_train, 
                         ntree = ntrees, 
                         mtry = (ncol(Z)/3),
                         na.action = na.omit)
    #relImp <- calc.relimp(X.rf, type="lmg",rela=TRUE)
    
    importance[,i] <- caret::varImp(X.rf, conditional=TRUE) # conditional=True, adjusts for correlations between predictors
    
    print(Sys.time()-nu)
  }
  return(importance)
}

# --- IMPORTANCE X_F_MAF_MARX ---
variable_imp_XFMAFMARX <- VI_function(Unempl, X_F_MAF_MARX, n_forecast, horizons, ntrees)

X_importance_XFMAFMARX <- variable_imp_XFMAFMARX[1:ncol(X),]
X_importance_XFMAFMARX <- colSums(X_importance_XFMAFMARX)

F_importance_XFMAFMARX <- variable_imp_XFMAFMARX[(ncol(X)+1):(ncol(X)+ncol(F)),]
F_importance_XFMAFMARX <- colSums(F_importance_XFMAFMARX)

MAF_importance_XFMAFMARX <- variable_imp_XFMAFMARX[(ncol(X)+ncol(F)+1):
                                           (ncol(X)+ncol(F)+ncol(MAF)),]
MAF_importance_XFMAFMARX <- colSums(MAF_importance_XFMAFMARX)

MARX_importance_XFMAFMARX <- variable_imp_XFMAFMARX[(ncol(X)+ncol(F)+ncol(MAF)+1):
                                            (ncol(X)+ncol(F)+ncol(MAF)+ncol(MARX)),]
MARX_importance_XFMAFMARX <- colSums(MARX_importance_XFMAFMARX)

for (c in 1:length(horizons)){
  X_importance_XFMAFMARX_norm[c] <- X_importance_XFMAFMARX[c] / (X_importance_XFMAFMARX[c] + F_importance_XFMAFMARX[c] 
                                                           + MAF_importance_XFMAFMARX[c] + MARX_importance_XFMAFMARX[c]) *100
  
  F_importance_XFMAFMARX_norm[c] <- F_importance_XFMAFMARX[c] / (X_importance_XFMAFMARX[c] + F_importance_XFMAFMARX[c] 
                                                           + MAF_importance_XFMAFMARX[c] + MARX_importance_XFMAFMARX[c]) *100
  
  MAF_importance_XFMAFMARX_norm[c] <- MAF_importance_XFMAFMARX[c] / (X_importance_XFMAFMARX[c] + F_importance_XFMAFMARX[c] 
                                                               + MAF_importance_XFMAFMARX[c] + MARX_importance_XFMAFMARX[c]) *100
  
  MARX_importance_XFMAFMARX_norm[c] <- MARX_importance_XFMAFMARX[c] / (X_importance_XFMAFMARX[c] + F_importance_XFMAFMARX[c] 
                                                                 + MAF_importance_XFMAFMARX[c] + MARX_importance_XFMAFMARX[c]) *100
}

## Plotting
df_XFMAFMARX <- data.frame(matrix(ncol=3, nrow=length(horizons)*4))
df_XFMAFMARX[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 4))
df_XFMAFMARX[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MAF" , 5) , rep("MARX" , 5))
df_XFMAFMARX[1:5, 3] <- X_importance_XFMAFMARX_norm
df_XFMAFMARX[6:10, 3] <- F_importance_XFMAFMARX_norm
df_XFMAFMARX[11:15, 3] <- MAF_importance_XFMAFMARX_norm
df_XFMAFMARX[16:20, 3] <- MARX_importance_XFMAFMARX_norm

df_XFMAFMARX_2 <- df_XFMAFMARX[order(df_XFMAFMARX$X1),]

plot_VI_XFMAFMARX <- ggplot(df_XFMAFMARX_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity")


## --- Importance X_F_MAF ---
variable_imp_XFMAF <- VI_function(Unempl, X_F_MAF, n_forecast, horizons, ntrees)

X_importance_XFMAF <- variable_imp_XFMAF[1:(ncol(X)),]
X_importance_XFMAF <- colSums(X_importance_XFMAF)

F_importance_XFMAF <- variable_imp_XFMAF[(ncol(X)+1):(ncol(X)+ncol(F)),]
F_importance_XFMAF <- colSums(F_importance_XFMAF)

MAF_importance_XFMAF <- variable_imp_XFMAF[(ncol(X)+ncol(F)+1):
                                                     (ncol(X)+ncol(F)+ncol(MAF)),]
MAF_importance_XFMAF <- colSums(MAF_importance_XFMAF)

X_importance_XFMAF_norm <- c(0,0,0,0,0)
F_importance_XFMAF_norm <- c(0,0,0,0,0)
MAF_importance_XFMAF_norm <- c(0,0,0,0,0)

for (c in 1:length(horizons)){
  X_importance_XFMAF_norm[c] <- X_importance_XFMAF[c] / (X_importance_XFMAF[c] + F_importance_XFMAF[c] 
                                                                 + MAF_importance_XFMAF[c]) *100
  
  F_importance_XFMAF_norm[c] <- F_importance_XFMAF[c] / (X_importance_XFMAF[c] + F_importance_XFMAF[c] 
                                                                 + MAF_importance_XFMAF[c]) *100
  
  MAF_importance_XFMAF_norm[c] <- MAF_importance_XFMAF[c] / (X_importance_XFMAF[c] + F_importance_XFMAF[c] 
                                                                     + MAF_importance_XFMAF[c]) *100
}

## Plotting
df_XFMAF <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_XFMAF[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_XFMAF[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MAF" , 5))
df_XFMAF[1:5, 3] <- X_importance_XFMAF_norm
df_XFMAF[6:10, 3] <- F_importance_XFMAF_norm
df_XFMAF[11:15, 3] <- MAF_importance_XFMAF_norm

df_XFMAF_2 <- df_XFMAF[order(df_XFMAF$X1),]

plot_VI_XFMAF <- ggplot(df_XFMAF_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity")

## --- Importance X_F_MARX ---
variable_imp_XFMARX <- VI_function(Unempl, X_F_MARX, n_forecast, horizons, ntrees)

X_importance_XFMARX <- variable_imp_XFMARX[1:(ncol(X)),]
X_importance_XFMARX <- colSums(X_importance_XFMARX)

F_importance_XFMARX <- variable_imp_XFMARX[(ncol(X)+1):(ncol(X)+ncol(F)+2),]
F_importance_XFMARX <- colSums(F_importance_XFMARX)

MARX_importance_XFMARX <- variable_imp_XFMARX[(ncol(X)+ncol(F)+1):
                                                (ncol(X)+ncol(F)+ncol(MARX)),]
MARX_importance_XFMARX <- colSums(MARX_importance_XFMARX)

X_importance_XFMARX_norm <- c(0,0,0,0,0)
F_importance_XFMARX_norm <- c(0,0,0,0,0)
MARX_importance_XFMARX_norm <- c(0,0,0,0,0)

for (c in 1:length(horizons)){
  X_importance_XFMARX_norm[c] <- X_importance_XFMARX[c] / (X_importance_XFMARX[c] + F_importance_XFMARX[c] 
                                                         + MARX_importance_XFMARX[c]) *100
  
  F_importance_XFMARX_norm[c] <- F_importance_XFMARX[c] / (X_importance_XFMARX[c] + F_importance_XFMARX[c] 
                                                         + MARX_importance_XFMARX[c]) *100
  
  MARX_importance_XFMARX_norm[c] <- MARX_importance_XFMARX[c] / (X_importance_XFMARX[c] + F_importance_XFMARX[c] 
                                                             + MARX_importance_XFMARX[c]) *100
}

## Plotting
df_XFMARX <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_XFMARX[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_XFMARX[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MARX" , 5))
df_XFMARX[1:5, 3] <- X_importance_XFMARX_norm
df_XFMARX[6:10, 3] <- F_importance_XFMARX_norm
df_XFMARX[11:15, 3] <- MARX_importance_XFMARX_norm

df_XFMARX_2 <- df_XFMARX[order(df_XFMARX$X1),]

plot_VI_XFMARX <- ggplot(df_XFMARX_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity")

## --- IMPORTANCE X_MAF_MARX ---
variable_imp_XMAFMARX <- VI_function(Unempl, X_MAF_MARX, n_forecast, horizons, ntrees)

X_importance_XMAFMARX <- variable_imp_XMAFMARX[1:(ncol(X)),]
X_importance_XMAFMARX <- colSums(X_importance_XMAFMARX)

MAF_importance_XMAFMARX <- variable_imp_XMAFMARX[(ncol(X)+1):
                                             (ncol(X)+ncol(MAF)),]
MAF_importance_XMAFMARX <- colSums(MAF_importance_XMAFMARX)

MARX_importance_XMAFMARX <- variable_imp_XMAFMARX[(ncol(X)+ncol(MAF)+1):
                                                (ncol(X)+ncol(MAF)+ncol(MARX)),]
MARX_importance_XMAFMARX <- colSums(MARX_importance_XMAFMARX)

X_importance_XMAFMARX_norm <- c(0,0,0,0,0)
MAF_importance_XMAFMARX_norm <- c(0,0,0,0,0)
MARX_importance_XMAFMARX_norm <- c(0,0,0,0,0)

for (c in 1:length(horizons)){
  X_importance_XMAFMARX_norm[c] <- X_importance_XMAFMARX[c] / (X_importance_XMAFMARX[c] + MAF_importance_XMAFMARX[c] 
                                                           + MARX_importance_XMAFMARX[c]) *100
  
  MAF_importance_XMAFMARX_norm[c] <- MAF_importance_XMAFMARX[c] / (X_importance_XMAFMARX[c] + MAF_importance_XMAFMARX[c] 
                                                           + MARX_importance_XMAFMARX[c]) *100
  
  MARX_importance_XMAFMARX_norm[c] <- MARX_importance_XMAFMARX[c] / (X_importance_XMAFMARX[c] + MAF_importance_XMAFMARX[c] 
                                                                 + MARX_importance_XMAFMARX[c]) *100
}

## Plotting
df_XMAFMARX <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_XMAFMARX[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_XMAFMARX[,2] <- c(rep("X" , 5) , rep("MAF" , 5) , rep("MARX" , 5))
df_XMAFMARX[1:5, 3] <- X_importance_XMAFMARX_norm
df_XMAFMARX[6:10, 3] <- MAF_importance_XMAFMARX_norm
df_XMAFMARX[11:15, 3] <- MARX_importance_XMAFMARX_norm

df_XMAFMARX_2 <- df_XMAFMARX[order(df_XMAFMARX$X1),]

plot_VI_XMAFMARX <- ggplot(df_XMAFMARX_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity")

### --- IMPORTANCE F_MAF_MARX ---
variable_imp_FMAFMARX <- VI_function(Unempl, F_MAF_MARX, n_forecast, horizons, ntrees)

F_importance_FMAFMARX <- variable_imp_FMAFMARX[1:(ncol(F)),]
F_importance_FMAFMARX <- colSums(F_importance_FMAFMARX)

MAF_importance_FMAFMARX <- variable_imp_FMAFMARX[(ncol(F)+1):
                                                   (ncol(F)+ncol(MAF)),]
MAF_importance_FMAFMARX <- colSums(MAF_importance_FMAFMARX)

MARX_importance_FMAFMARX <- variable_imp_FMAFMARX[(ncol(F)+ncol(MAF)+1):
                                                    (ncol(F)+ncol(MAF)+ncol(MARX)),]
MARX_importance_FMAFMARX <- colSums(MARX_importance_FMAFMARX)

F_importance_FMAFMARX_norm <- c(0,0,0,0,0)
MAF_importance_FMAFMARX_norm <- c(0,0,0,0,0)
MARX_importance_FMAFMARX_norm <- c(0,0,0,0,0)

for (c in 1:length(horizons)){
  F_importance_FMAFMARX_norm[c] <- F_importance_FMAFMARX[c] / (F_importance_FMAFMARX[c] + MAF_importance_FMAFMARX[c] 
                                                               + MARX_importance_FMAFMARX[c]) *100
  
  MAF_importance_FMAFMARX_norm[c] <- MAF_importance_FMAFMARX[c] / (F_importance_FMAFMARX[c] + MAF_importance_FMAFMARX[c] 
                                                                   + MARX_importance_FMAFMARX[c]) *100
  
  MARX_importance_FMAFMARX_norm[c] <- MARX_importance_FMAFMARX[c] / (F_importance_FMAFMARX[c] + MAF_importance_FMAFMARX[c] 
                                                                     + MARX_importance_FMAFMARX[c]) *100
}

## Plotting
df_FMAFMARX <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_FMAFMARX[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_FMAFMARX[,2] <- c(rep("F" , 5) , rep("MAF" , 5) , rep("MARX" , 5))
df_FMAFMARX[1:5, 3] <- F_importance_FMAFMARX_norm
df_FMAFMARX[6:10, 3] <- MAF_importance_FMAFMARX_norm
df_FMAFMARX[11:15, 3] <- MARX_importance_FMAFMARX_norm

df_FMAFMARX_2 <- df_FMAFMARX[order(df_FMAFMARX$X1),]

plot_VI_FMAFMARX <- ggplot(df_FMAFMARX_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity")




### PLOTTING ALL 
library("gridExtra")   

grid.arrange(plot_VI_XFMAFMARX, plot_VI_XFMAF, plot_VI_XFMARX, plot_VI_XMAFMARX, plot_VI_FMAFMARX, ncol = 5)

