#Clearing Environment
rm(list=ls())

### ---- FEATURE ENGINEERING ----
library(vars)
library(randomForest)
library(varImp)
library(caret)
library(tseries)
library(data.table)
library(gridExtra)   

## ---- DATA ----
df <- read.csv("data-eur2023.csv", row.names = 1) #Working directory must be same location as this file

# -- transformations -- 
tobedifferenced_matrix <- df[-c(1)] # Remove pubdate 
colstodiff <- c(1, 2, 3, 6, 7, 8)

# take the first difference of the selected columns
diff_data <- diff(as.matrix(tobedifferenced_matrix[, colstodiff]), differences = 1)
diff_data <- data.frame(cbind(diff_data, df$L1_BSCI[2:434], df$L1_CSCICP02[2:434], df$L1_AEX[2:434]))

y_differenced <- diff_data[,1]
regressor_matrix_diff <- diff_data[-c(1)]

# Check if data stationary: H0: not stationary --> reject = ~ stationary
adf.test(y_differenced) #stationary
adf.test(regressor_matrix_diff[,1]) #stationary
adf.test(regressor_matrix_diff[,2]) #stationary
adf.test(regressor_matrix_diff[,3]) #stationary
adf.test(regressor_matrix_diff[,4]) #stationary
adf.test(regressor_matrix_diff[,5]) #stationary
adf.test(regressor_matrix_diff[,6]) #stationary
adf.test(regressor_matrix_diff[,7]) #stationary
adf.test(regressor_matrix_diff[,8]) #not stationary (not rejected)

diff2_AEX <- diff(regressor_matrix_diff[,8], differences = 1)
adf.test(diff2_AEX) #stationary
regressor_matrix_diff <- data.frame(cbind(regressor_matrix_diff[2:433,1:7], diff2_AEX)) 

n_var <- ncol(regressor_matrix_diff)
T <- nrow(regressor_matrix_diff)

# -- Parameter initalizations for feature matrix --
X_lags <- 12 # We use the data up until one year before
n_Factors <- 2 # Optimization
F_lags <- 12 # Same reason as X, also because Coulombe
P_MAF <- 12 # Summarize data up until one year 
n_MAF <- 6 # Optimization 
P_MARX <- 12 # Lags for MARX, same as X_lags, also Coulombe 

## - X - 
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
MARX_function <- function(regressor_matrix_diff, P_MARX, n_var) {
  var = VAR(regressor_matrix_diff, p = P_MARX, type = "const")
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

### --- VARIABLE IMPORTANCE ---
poos <- 120
Unempl <- df[,2] 
y_real <- Unempl[(length(Unempl)-poos+1):length(Unempl)] 
start_forecast <- length(Unempl)-poos+1
end_forecast <- length(Unempl)
horizons <- list(3, 6, 12, 18, 24) 
ntrees <- 500 # Default value

VI_function <- function(y_differenced, Z, poos, horizons, ntrees){
  importance <- data.frame(matrix(ncol = length(horizons), nrow = ncol(Z)))
  i <- 0
  for (h in horizons){
    shift_y = as.data.frame(shift(y_differenced,n=h, type = 'lead', give.names=TRUE))
    shift_y <- shift_y[2:433,] #As the last column is differenced twice, we need to remove first observation
    y_Z <- cbind(shift_y, Z)
    i <- i+1
    nu <- Sys.time()
    
    y_Z_train <- y_Z[1:(nrow(y_Z)-poos),] #Compute VI on training set
    print(y_Z_train)
    
    X.rf <- randomForest(shift_y ~ ., 
                         data = y_Z_train, 
                         ntree = ntrees, 
                         mtry = (ncol(Z)/3),
                         na.action = na.omit)
    
    importance[,i] <- caret::varImp(X.rf, conditional=TRUE) # conditional=True, adjusts for correlations between predictors
    
    print(Sys.time()-nu)
  }
  return(importance) #returns VI. columns: horizons, rows: VI calculated for according column in Z
}

### --- VARIABLE IMPORTANCE: 2 COMBINATIONS ---
two_feature_importance <- function(feature1, feature2, poos, horizons, ntrees){
  Z <- cbind(feature1, feature2)
  allvarimp <- VI_function(y_differenced, Z, poos, horizons, ntrees)  #output is a matrix of VI values. columns: horizons, rows: VI of given column in the feature matrix (Z)
  f1_imp <- allvarimp[1:(ncol(feature1)),] #select VI for first feature. Example: X has 104 columns. Hence, select in VI matrix the first 104 rows.
  f1_imp <- colSums(f1_imp) / ncol(feature1) #Reweight: sum the individual series calculated one line above. divide by the number of columns of that feature.
  #Hence, if a feature has a larger fraction of Z, it is downweighted --> relative measure: total feature VI per column.
  
  f2_imp <- allvarimp[(ncol(feature1)+1):(ncol(feature1)+ncol(feature2)),]
  f2_imp <- colSums(f2_imp) / ncol(feature2)
  
  f1_imp_norm <- c(0,0,0,0,0)
  f2_imp_norm <- c(0,0,0,0,0)
  
  #We calculate percentages
  for (c in 1:length(horizons)){
    f1_imp_norm[c] <- f1_imp[c] / (f1_imp[c] + f2_imp[c]) *100
    
    f2_imp_norm[c] <- f2_imp[c] / (f1_imp[c] + f2_imp[c]) *100
  }
  return(list(f1_imp_norm, f2_imp_norm)) #return percentage of the "total feature VI per column" given combination Z.
}

variable_imp_XF <- two_feature_importance(X, F, poos, horizons, ntrees)
variable_imp_XMAF <- two_feature_importance(X, MAF, poos, horizons, ntrees)
variable_imp_XMARX <- two_feature_importance(X, MARX, poos, horizons, ntrees)
variable_imp_FMAF <- two_feature_importance(F, MAF, poos, horizons, ntrees)
variable_imp_FMARX <- two_feature_importance(F, MARX, poos, horizons, ntrees)
variable_imp_MAFMARX <- two_feature_importance(MAF, MARX, poos, horizons, ntrees)


### --- VARIABLE IMPORTANCE: 3 COMBINATIONS ---
three_feature_importance <- function(feature1, feature2, feature3, poos, horizons, ntrees){
  Z <- cbind(feature1, feature2, feature3)
  
  allvarimp <- VI_function(y_differenced, Z, poos, horizons, ntrees)
  
  f1_imp <- allvarimp[1:(ncol(feature1)),]
  f1_imp <- colSums(f1_imp) / ncol(feature1)
  
  f2_imp <- allvarimp[(ncol(feature1)+1):(ncol(feature1)+ncol(feature2)),]
  f2_imp <- colSums(f2_imp) / ncol(feature2)
  
  f3_imp <- allvarimp[(ncol(feature1)+ncol(feature2)+1):(ncol(feature1)+ncol(feature2)+ncol(feature3)),]
  f3_imp <- colSums(f3_imp) / ncol(feature3)
  
  f1_imp_norm <- c(0,0,0,0,0)
  f2_imp_norm <- c(0,0,0,0,0)
  f3_imp_norm <- c(0,0,0,0,0)
  
  for (c in 1:length(horizons)){
    f1_imp_norm[c] <- f1_imp[c] / (f1_imp[c] + f2_imp[c] + f3_imp[c]) *100
    f2_imp_norm[c] <- f2_imp[c] / (f1_imp[c] + f2_imp[c] + f3_imp[c]) *100
    f3_imp_norm[c] <- f3_imp[c] / (f1_imp[c] + f2_imp[c] + f3_imp[c]) *100
  }
  return(list(f1_imp_norm, f2_imp_norm, f3_imp_norm))
}

variable_imp_XFMAF <- three_feature_importance(X, F, MAF, poos, horizons, ntrees)
variable_imp_XFMARX <- three_feature_importance(X, F, MARX, poos, horizons, ntrees)
variable_imp_XMAFMARX <- three_feature_importance(X, MAF, MARX, poos, horizons, ntrees)
variable_imp_FMAFMARX <- three_feature_importance(F, MAF, MARX, poos, horizons, ntrees)

# --- IMPORTANCE X_F_MAF_MARX ---
variable_imp_XFMAFMARX <- VI_function(y_differenced, X_F_MAF_MARX, poos, horizons, ntrees)

X_importance_XFMAFMARX <- variable_imp_XFMAFMARX[1:ncol(X),]
X_importance_XFMAFMARX <- colSums(X_importance_XFMAFMARX) / ncol(X)

F_importance_XFMAFMARX <- variable_imp_XFMAFMARX[(ncol(X)+1):(ncol(X)+ncol(F)),]
F_importance_XFMAFMARX <- colSums(F_importance_XFMAFMARX) / ncol(F)

MAF_importance_XFMAFMARX <- variable_imp_XFMAFMARX[(ncol(X)+ncol(F)+1):
                                                     (ncol(X)+ncol(F)+ncol(MAF)),]
MAF_importance_XFMAFMARX <- colSums(MAF_importance_XFMAFMARX) / ncol(MAF)

MARX_importance_XFMAFMARX <- variable_imp_XFMAFMARX[(ncol(X)+ncol(F)+ncol(MAF)+1):
                                                      (ncol(X)+ncol(F)+ncol(MAF)+ncol(MARX)),]
MARX_importance_XFMAFMARX <- colSums(MARX_importance_XFMAFMARX) / ncol(MARX)

X_importance_XFMAFMARX_norm <- c(0,0,0,0,0)
F_importance_XFMAFMARX_norm <- c(0,0,0,0,0)
MAF_importance_XFMAFMARX_norm <- c(0,0,0,0,0)
MARX_importance_XFMAFMARX_norm <- c(0,0,0,0,0)

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

## -- Plotting --
size_title <- 10 #Title size

df_XFMAFMARX <- data.frame(matrix(ncol=3, nrow=length(horizons)*4))
df_XFMAFMARX[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 4))
df_XFMAFMARX[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MAF" , 5) , rep("MARX" , 5))
df_XFMAFMARX[1:5, 3] <- X_importance_XFMAFMARX_norm
df_XFMAFMARX[6:10, 3] <- F_importance_XFMAFMARX_norm
df_XFMAFMARX[11:15, 3] <- MAF_importance_XFMAFMARX_norm
df_XFMAFMARX[16:20, 3] <- MARX_importance_XFMAFMARX_norm

df_XFMAFMARX_2 <- df_XFMAFMARX[order(df_XFMAFMARX$X1),]
colnames(df_XFMAFMARX_2) <- c("horizon", "Feature", "VarImpval")

plot_VI_XFMAFMARX <- ggplot(df_XFMAFMARX_2, aes(fill=Feature, y=VarImpval, x=horizon)) + 
  geom_bar(position="stack", stat="identity")+
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#009EFA","#4D8076")) + ggtitle("X-F-MAF-MARX")

## Plotting XFMAF
df_XFMAF <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_XFMAF[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_XFMAF[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MAF" , 5))
df_XFMAF[1:5, 3] <- variable_imp_XFMAF[1]
df_XFMAF[6:10, 3] <- variable_imp_XFMAF[2]
df_XFMAF[11:15, 3] <- variable_imp_XFMAF[3]

df_XFMAF_2 <- df_XFMAF[order(df_XFMAF$X1),]

plot_VI_XFMAF <- ggplot(df_XFMAF_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(legend.position = "none",plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank())  + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#4D8076")) + ggtitle("X-F-MAF")

## Plotting XFMARX
df_XFMARX <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_XFMARX[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_XFMARX[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MARX" , 5))
df_XFMARX[1:5, 3] <- variable_imp_XFMARX[1]
df_XFMARX[6:10, 3] <- variable_imp_XFMARX[2]
df_XFMARX[11:15, 3] <- variable_imp_XFMARX[3]

df_XFMARX_2 <- df_XFMARX[order(df_XFMARX$X1),]

plot_VI_XFMARX <- ggplot(df_XFMARX_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(legend.position = "none",plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank())  + 
  scale_fill_manual(values=c("#845EC2","#009EFA","#4D8076")) + ggtitle("X-F-MARX")

## Plotting XMAFMARX
df_XMAFMARX <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_XMAFMARX[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_XMAFMARX[,2] <- c(rep("X" , 5) , rep("MAF" , 5) , rep("MARX" , 5))
df_XMAFMARX[1:5, 3] <- variable_imp_XMAFMARX[1]
df_XMAFMARX[6:10, 3] <- variable_imp_XMAFMARX[2]
df_XMAFMARX[11:15, 3] <- variable_imp_XMAFMARX[3]

df_XMAFMARX_2 <- df_XMAFMARX[order(df_XMAFMARX$X1),]

plot_VI_XMAFMARX <- ggplot(df_XMAFMARX_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(legend.position = "none",plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + 
  scale_fill_manual(values=c("#00C9A7","#009EFA","#4D8076")) + ggtitle("X-MAF-MARX")

## Plotting FMAFMARX
df_FMAFMARX <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_FMAFMARX[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_FMAFMARX[,2] <- c(rep("F" , 5) , rep("MAF" , 5) , rep("MARX" , 5))
df_FMAFMARX[1:5, 3] <- variable_imp_FMAFMARX[1]
df_FMAFMARX[6:10, 3] <- variable_imp_FMAFMARX[2]
df_FMAFMARX[11:15, 3] <- variable_imp_FMAFMARX[3]

df_FMAFMARX_2 <- df_FMAFMARX[order(df_FMAFMARX$X1),]

plot_VI_FMAFMARX <- ggplot(df_FMAFMARX_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(legend.position = "none", plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#009EFA")) + ggtitle("F-MAF-MARX")

## Plotting XF
df_XF <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_XF[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_XF[,2] <- c(rep("X" , 5) , rep("F" , 5))
df_XF[1:5, 3] <- variable_imp_XF[1]
df_XF[6:10, 3] <- variable_imp_XF[2]

df_XF_2 <- df_XF[order(df_XF$X1),]

plot_VI_XF <- ggplot(df_XF_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2", "#4D8076")) + theme(legend.position = "none") + ggtitle("X-F")

## Plotting XMAF
df_XMAF <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_XMAF[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_XMAF[,2] <- c(rep("X" , 5) , rep("MAF" , 5))
df_XMAF[1:5, 3] <- variable_imp_XMAF[1]
df_XMAF[6:10, 3] <- variable_imp_XMAF[2]

df_XMAF_2 <- df_XMAF[order(df_XMAF$X1),]

plot_VI_XMAF <- ggplot(df_XMAF_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#00C9A7", "#4D8076")) + theme(legend.position = "none") + ggtitle("X-MAF")

## Plotting XMARX
df_XMARX <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_XMARX[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_XMARX[,2] <- c(rep("X" , 5) , rep("MARX" , 5))
df_XMARX[1:5, 3] <- variable_imp_XMARX[1]
df_XMARX[6:10, 3] <- variable_imp_XMARX[2]

df_XMARX_2 <- df_XMARX[order(df_XMARX$X1),]

plot_VI_XMARX <- ggplot(df_XMARX_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#009EFA", "#4D8076")) + theme(legend.position = "none") + ggtitle("X-MARX")

## Plotting FMAF
df_FMAF <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_FMAF[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_FMAF[,2] <- c(rep("F" , 5) , rep("MAF" , 5))
df_FMAF[1:5, 3] <- variable_imp_FMAF[1]
df_FMAF[6:10, 3] <- variable_imp_FMAF[2]

df_FMAF_2 <- df_FMAF[order(df_FMAF$X1),]

plot_VI_FMAF <- ggplot(df_FMAF_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2", "#00C9A7")) + theme(legend.position = "none") + ggtitle("F-MAF")

## Plotting FMARX
df_FMARX <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_FMARX[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_FMARX[,2] <- c(rep("F" , 5) , rep("MARX" , 5))
df_FMARX[1:5, 3] <- variable_imp_FMARX[1]
df_FMARX[6:10, 3] <- variable_imp_FMARX[2]

df_FMARX_2 <- df_FMARX[order(df_FMARX$X1),]

plot_VI_FMARX <- ggplot(df_FMARX_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2", "#009EFA")) + theme(legend.position = "none") + ggtitle("F-MARX")


#### ---- PLOTTING ALL ----
grid.arrange(plot_VI_XFMAF, plot_VI_XFMARX, 
             plot_VI_XMAFMARX, plot_VI_FMAFMARX, plot_VI_XFMAFMARX, ncol = 5) 

grid.arrange(plot_VI_XF, plot_VI_XMAF, 
             plot_VI_XMARX, plot_VI_FMAF, plot_VI_FMARX, ncol = 5) 
