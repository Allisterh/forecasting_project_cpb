#### ---- Variable Importance Measures (RF) ----

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

### --- SMALL DATA ---
SSA <- read.csv("RF_Small_SSA_Matrix.csv")[,2:28]

## ---- DATA ----
df_cpb_infl_stat <- read.csv("Extra data/data_core.csv")
y_differenced <- df_cpb_infl_stat$L2_LRHUTTTT
df_small <- df_cpb_infl_stat
regressor_matrix <- df_small[-c(1)] # Dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)

# -- Parameter initalizations for feature matrix --
X_lags <- 12 # We use the data up until one year before
n_Factors <- 5 # 
F_lags <- 12 # Same reason as X, also because Coulombe
P_MAF <- 12 # Summarize data up until one year 
n_MAF <- 2 #  
P_MARX <- 12 # Lags for MARX, same as X_lags, also Coulombe 

# -- Make feature matrices --
source("Function_File.r")

X <- as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE))

scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags)

scaled_X <- scale(X) #Scale lagged X Matrix for MAF
MAF <- MAF_function(scaled_X,X_lags,nrow(scaled_X),ncol(regressor_matrix), P_MAF, n_MAF, FALSE)

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

X_F_SSA <- cbind(X, F, SSA)
X_MAF_SSA <- cbind(X, MAF, SSA)
X_MARX_SSA <- cbind(X, MARX, SSA)
F_MAF_SSA <- cbind(F, MAF, SSA)
F_MARX_SSA <- cbind(F, MARX, SSA)
MAF_MARX_SSA <- cbind(MAF, MARX, SSA)
X_F_MAF_SSA <- cbind(X, F, MAF, SSA)
X_F_MARX_SSA <- cbind(X, F, MARX, SSA)
X_MAF_MARX_SSA <- cbind(X, MAF, MARX, SSA)
F_MAF_MARX_SSA <- cbind(F, MAF, MARX, SSA)
X_F_MAF_MARX_SSA <- cbind(X, F, MAF, MARX, SSA)

### --- VARIABLE IMPORTANCE ---
poos <- 120
y_real <- y_differenced[(length(y_differenced)-poos+1):length(y_differenced)]
start_forecast <- length(y_differenced)-poos+1
end_forecast <- length(y_differenced)
horizons <- list(3, 6, 12, 18, 24) 
ntrees <- 500 # Default value
colors <- c("F"= "#FF0000FF", "MAF"= "#CCFF00FF", "MARX"= "#00FF66FF", "SSA"= "#0066FFFF")
colors <- c("F"= "green", "MAF"= "yellow", "MARX"= "red", "SSA"= "blue")

VI_function <- function(y_differenced, Z, poos, horizons, ntrees){
  importance <- data.frame(matrix(ncol = length(horizons), nrow = ncol(Z)))
  i <- 0
  for (h in horizons){
    shift_y = as.data.frame(shift(y_differenced,n=h, type = 'lead', give.names=TRUE))
    colnames(shift_y) <- "y"
    y_Z <- cbind(shift_y, Z)
    i <- i+1
    nu <- Sys.time()
    
    y_Z_train <- y_Z[1:(nrow(y_Z)-poos),] #Compute VI on training set
    
    X.rf <- randomForest(y ~ ., 
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

variable_imp_XSSA <- two_feature_importance(X, SSA, poos, horizons, ntrees)
variable_imp_FSSA <- two_feature_importance(F, SSA, poos, horizons, ntrees)
variable_imp_MAFSSA <- two_feature_importance(MAF, SSA, poos, horizons, ntrees)
variable_imp_MARXSSA <- two_feature_importance(MARX, SSA, poos, horizons, ntrees)

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

variable_imp_XFSSA <- three_feature_importance(X, F, SSA, poos, horizons, ntrees)
variable_imp_XMAFSSA <- three_feature_importance(X, MAF, SSA, poos, horizons, ntrees)
variable_imp_XMARXSSA <- three_feature_importance(X, MARX, SSA, poos, horizons, ntrees)
variable_imp_FMAFSSA <- three_feature_importance(F, MAF, SSA, poos, horizons, ntrees)
variable_imp_FMARXSSA <- three_feature_importance(F, MARX, SSA, poos, horizons, ntrees)
variable_imp_MAFMARXSSA <- three_feature_importance(MAF, MARX, SSA, poos, horizons, ntrees)

### --- VARIABLE IMPORTANCE: 4 COMBINATIONS ---
four_feature_importance <- function(feature1, feature2, feature3, feature4, poos, horizons, ntrees){
  Z <- cbind(feature1, feature2, feature3, feature4)
  
  allvarimp <- VI_function(y_differenced, Z, poos, horizons, ntrees)
  
  f1_imp <- allvarimp[1:(ncol(feature1)),]
  f1_imp <- colSums(f1_imp) / ncol(feature1)
  
  f2_imp <- allvarimp[(ncol(feature1)+1):(ncol(feature1)+ncol(feature2)),]
  f2_imp <- colSums(f2_imp) / ncol(feature2)
  
  f3_imp <- allvarimp[(ncol(feature1)+ncol(feature2)+1):(ncol(feature1)+ncol(feature2)+ncol(feature3)),]
  f3_imp <- colSums(f3_imp) / ncol(feature3)
  
  f4_imp <- allvarimp[(ncol(feature1)+ncol(feature2)+ncol(feature3)+1):(ncol(feature1)+ncol(feature2)+ncol(feature3)+ncol(feature4)),]
  f4_imp <- colSums(f4_imp) / ncol(feature4)
  
  f1_imp_norm <- c(0,0,0,0,0)
  f2_imp_norm <- c(0,0,0,0,0)
  f3_imp_norm <- c(0,0,0,0,0)
  f4_imp_norm <- c(0,0,0,0,0)
  
  for (c in 1:length(horizons)){
    f1_imp_norm[c] <- f1_imp[c] / (f1_imp[c] + f2_imp[c] + f3_imp[c] + f4_imp[c]) *100
    f2_imp_norm[c] <- f2_imp[c] / (f1_imp[c] + f2_imp[c] + f3_imp[c] + f4_imp[c]) *100
    f3_imp_norm[c] <- f3_imp[c] / (f1_imp[c] + f2_imp[c] + f3_imp[c] + f4_imp[c]) *100
    f4_imp_norm[c] <- f4_imp[c] / (f1_imp[c] + f2_imp[c] + f3_imp[c] + f4_imp[c]) *100
  }
  return(list(f1_imp_norm, f2_imp_norm, f3_imp_norm, f4_imp_norm))
}

variable_imp_XFMAFSSA <- four_feature_importance(X, F, MAF, SSA, poos, horizons, ntrees)
variable_imp_XFMARXSSA <- four_feature_importance(X, F, MARX, SSA, poos, horizons, ntrees)
variable_imp_XMAFMARXSSA <- four_feature_importance(X, MAF, MARX, SSA, poos, horizons, ntrees)
variable_imp_FMAFMARXSSA <- four_feature_importance(F, MAF, MARX, SSA, poos, horizons, ntrees)

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
  theme(legend.position = "none",plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#009EFA","#4D8076")) + ggtitle("X-F-MAF-MARX")

# --- IMPORTANCE X_F_MAF_MARX_SSA ---
variable_imp_XFMAFMARXSSA <- VI_function(y_differenced, X_F_MAF_MARX_SSA, poos, horizons, ntrees)

X_importance_XFMAFMARXSSA <- variable_imp_XFMAFMARXSSA[1:ncol(X),]
X_importance_XFMAFMARXSSA <- colSums(X_importance_XFMAFMARXSSA) / ncol(X)

F_importance_XFMAFMARXSSA <- variable_imp_XFMAFMARXSSA[(ncol(X)+1):(ncol(X)+ncol(F)),]
F_importance_XFMAFMARXSSA <- colSums(F_importance_XFMAFMARXSSA) / ncol(F)

MAF_importance_XFMAFMARXSSA <- variable_imp_XFMAFMARXSSA[(ncol(X)+ncol(F)+1):
                                                     (ncol(X)+ncol(F)+ncol(MAF)),]
MAF_importance_XFMAFMARXSSA <- colSums(MAF_importance_XFMAFMARXSSA) / ncol(MAF)

MARX_importance_XFMAFMARXSSA <- variable_imp_XFMAFMARXSSA[(ncol(X)+ncol(F)+ncol(MAF)+1):
                                                      (ncol(X)+ncol(F)+ncol(MAF)+ncol(MARX)),]
MARX_importance_XFMAFMARXSSA <- colSums(MARX_importance_XFMAFMARXSSA) / ncol(MARX)

SSA_importance_XFMAFMARXSSA <- variable_imp_XFMAFMARXSSA[(ncol(X)+ncol(F)+ncol(MAF)+ncol(MARX)+1):
                                                           (ncol(X)+ncol(F)+ncol(MAF)+ncol(MARX)+ncol(SSA)),]
SSA_importance_XFMAFMARXSSA <- colSums(SSA_importance_XFMAFMARXSSA) / ncol(SSA)

X_importance_XFMAFMARXSSA_norm <- c(0,0,0,0,0)
F_importance_XFMAFMARXSSA_norm <- c(0,0,0,0,0)
MAF_importance_XFMAFMARXSSA_norm <- c(0,0,0,0,0)
MARX_importance_XFMAFMARXSSA_norm <- c(0,0,0,0,0)
SSA_importance_XFMAFMARXSSA_norm <- c(0,0,0,0,0)

for (c in 1:length(horizons)){
  X_importance_XFMAFMARXSSA_norm[c] <- X_importance_XFMAFMARXSSA[c] / (X_importance_XFMAFMARXSSA[c] + F_importance_XFMAFMARXSSA[c] 
                                                                 + MAF_importance_XFMAFMARXSSA[c] + MARX_importance_XFMAFMARXSSA[c] + SSA_importance_XFMAFMARXSSA[c]) *100
  
  F_importance_XFMAFMARXSSA_norm[c] <- F_importance_XFMAFMARXSSA[c] / (X_importance_XFMAFMARXSSA[c] + F_importance_XFMAFMARXSSA[c] 
                                                                 + MAF_importance_XFMAFMARXSSA[c] + MARX_importance_XFMAFMARXSSA[c] + SSA_importance_XFMAFMARXSSA[c]) *100
  
  MAF_importance_XFMAFMARXSSA_norm[c] <- MAF_importance_XFMAFMARXSSA[c] / (X_importance_XFMAFMARXSSA[c] + F_importance_XFMAFMARXSSA[c] 
                                                                     + MAF_importance_XFMAFMARXSSA[c] + MARX_importance_XFMAFMARXSSA[c] + SSA_importance_XFMAFMARXSSA[c]) *100
  
  MARX_importance_XFMAFMARXSSA_norm[c] <- MARX_importance_XFMAFMARXSSA[c] / (X_importance_XFMAFMARXSSA[c] + F_importance_XFMAFMARXSSA[c] 
                                                                       + MAF_importance_XFMAFMARXSSA[c] + MARX_importance_XFMAFMARXSSA[c] + SSA_importance_XFMAFMARXSSA[c]) *100
  
  SSA_importance_XFMAFMARXSSA_norm[c] <- SSA_importance_XFMAFMARXSSA[c] / (X_importance_XFMAFMARXSSA[c] + F_importance_XFMAFMARXSSA[c] 
                                                                             + MAF_importance_XFMAFMARXSSA[c] + MARX_importance_XFMAFMARXSSA[c] + SSA_importance_XFMAFMARXSSA[c]) *100
}

## -- Plotting --
size_title <- 10 #Title size

df_XFMAFMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*5))
df_XFMAFMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 5))
df_XFMAFMARXSSA[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MAF" , 5) , rep("MARX" , 5), rep("SSA" , 5))
df_XFMAFMARXSSA[1:5, 3] <- X_importance_XFMAFMARXSSA_norm
df_XFMAFMARXSSA[6:10, 3] <- F_importance_XFMAFMARXSSA_norm
df_XFMAFMARXSSA[11:15, 3] <- MAF_importance_XFMAFMARXSSA_norm
df_XFMAFMARXSSA[16:20, 3] <- MARX_importance_XFMAFMARXSSA_norm
df_XFMAFMARXSSA[21:25, 3] <- SSA_importance_XFMAFMARXSSA_norm

df_XFMAFMARXSSA_2 <- df_XFMAFMARXSSA[order(df_XFMAFMARXSSA$X1),]
colnames(df_XFMAFMARXSSA_2) <- c("horizon", "Feature", "VarImpval")

plot_VI_XFMAFMARXSSA <- ggplot(df_XFMAFMARXSSA_2, aes(fill=Feature, y=VarImpval, x=horizon)) + 
  geom_bar(position="stack", stat="identity")+
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2","#00C9A7","#009EFA", "#ff8c00", "#4D8076")) + 
  ggtitle("X-F-MAF-MARX-SSA") #+ theme(legend.position = "none")

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

## Plotting XFMAFSSA
df_XFMAFSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*4))
df_XFMAFSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 4))
df_XFMAFSSA[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MAF" , 5), rep("SSA" , 5))
df_XFMAFSSA[1:5, 3] <- variable_imp_XFMAFSSA[1]
df_XFMAFSSA[6:10, 3] <- variable_imp_XFMAFSSA[2]
df_XFMAFSSA[11:15, 3] <- variable_imp_XFMAFSSA[3]
df_XFMAFSSA[16:20, 3] <- variable_imp_XFMAFSSA[4]

df_XFMAFSSA_2 <- df_XFMAFSSA[order(df_XFMAFSSA$X1),]

plot_VI_XFMAFSSA <- ggplot(df_XFMAFSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank())  + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#ff8c00", "#4D8076")) + ggtitle("X-F-MAF-SSA") + theme(legend.position = "none")

## Plotting XFMARXSSA
df_XFMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*4))
df_XFMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 4))
df_XFMARXSSA[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MARX" , 5), rep("SSA" , 5))
df_XFMARXSSA[1:5, 3] <- variable_imp_XFMARXSSA[1]
df_XFMARXSSA[6:10, 3] <- variable_imp_XFMARXSSA[2]
df_XFMARXSSA[11:15, 3] <- variable_imp_XFMARXSSA[3]
df_XFMARXSSA[16:20, 3] <- variable_imp_XFMARXSSA[4]

df_XFMARXSSA_2 <- df_XFMARXSSA[order(df_XFMARXSSA$X1),]

plot_VI_XFMARXSSA <- ggplot(df_XFMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank())  + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#ff8c00", "#4D8076")) + ggtitle("X-F-MARX-SSA") + theme(legend.position = "none")

## Plotting XFMARXSSA
df_XFMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*4))
df_XFMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 4))
df_XFMARXSSA[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MARX" , 5), rep("SSA" , 5))
df_XFMARXSSA[1:5, 3] <- variable_imp_XFMARXSSA[1]
df_XFMARXSSA[6:10, 3] <- variable_imp_XFMARXSSA[2]
df_XFMARXSSA[11:15, 3] <- variable_imp_XFMARXSSA[3]
df_XFMARXSSA[16:20, 3] <- variable_imp_XFMARXSSA[4]

df_XFMARXSSA_2 <- df_XFMARXSSA[order(df_XFMARXSSA$X1),]

plot_VI_XFMARXSSA <- ggplot(df_XFMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank())  + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#ff8c00", "#4D8076")) + ggtitle("X-F-MARX-SSA") + theme(legend.position = "none")

## Plotting XMAFMARXSSA
df_XMAFMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*4))
df_XMAFMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 4))
df_XMAFMARXSSA[,2] <- c(rep("X" , 5) , rep("MAF" , 5) , rep("MARX" , 5), rep("SSA" , 5))
df_XMAFMARXSSA[1:5, 3] <- variable_imp_XMAFMARXSSA[1]
df_XMAFMARXSSA[6:10, 3] <- variable_imp_XMAFMARXSSA[2]
df_XMAFMARXSSA[11:15, 3] <- variable_imp_XMAFMARXSSA[3]
df_XMAFMARXSSA[16:20, 3] <- variable_imp_XMAFMARXSSA[4]

df_XMAFMARXSSA_2 <- df_XMAFMARXSSA[order(df_XMAFMARXSSA$X1),]

plot_VI_XMAFMARXSSA <- ggplot(df_XMAFMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank())  + 
  scale_fill_manual(values=c("#00C9A7","#009EFA","#ff8c00", "#4D8076")) + ggtitle("X-MAF-MARX-SSA") + theme(legend.position = "none")

## Plotting FMAFMARXSSA
df_FMAFMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*4))
df_FMAFMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 4))
df_FMAFMARXSSA[,2] <- c(rep("F" , 5) , rep("MAF" , 5) , rep("MARX" , 5), rep("SSA" , 5))
df_FMAFMARXSSA[1:5, 3] <- variable_imp_FMAFMARXSSA[1]
df_FMAFMARXSSA[6:10, 3] <- variable_imp_FMAFMARXSSA[2]
df_FMAFMARXSSA[11:15, 3] <- variable_imp_FMAFMARXSSA[3]
df_FMAFMARXSSA[16:20, 3] <- variable_imp_FMAFMARXSSA[4]

df_FMAFMARXSSA_2 <- df_FMAFMARXSSA[order(df_FMAFMARXSSA$X1),]

plot_VI_FMAFMARXSSA <- ggplot(df_FMAFMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank())  + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#009EFA","#ff8c00")) + ggtitle("X-MAF-MARX-SSA") + theme(legend.position = "none")

## Plotting XFSSA
df_XFSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_XFSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_XFSSA[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("SSA" , 5))
df_XFSSA[1:5, 3] <- variable_imp_XFSSA[1]
df_XFSSA[6:10, 3] <- variable_imp_XFSSA[2]
df_XFSSA[11:15, 3] <- variable_imp_XFSSA[3]

df_XFSSA_2 <- df_XFSSA[order(df_XFSSA$X1),]

plot_VI_XFSSA <- ggplot(df_XFSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2", "#ff8c00", "#4D8076")) + theme(legend.position = "none") + ggtitle("X-F-SSA")

## Plotting XMAFSSA
df_XMAFSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_XMAFSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_XMAFSSA[,2] <- c(rep("X" , 5) , rep("MAF" , 5), rep("SSA", 5))
df_XMAFSSA[1:5, 3] <- variable_imp_XMAFSSA[1]
df_XMAFSSA[6:10, 3] <- variable_imp_XMAFSSA[2]
df_XMAFSSA[11:15, 3] <- variable_imp_XMAFSSA[3]

df_XMAFSSA_2 <- df_XMAFSSA[order(df_XMAFSSA$X1),]

plot_VI_XMAFSSA <- ggplot(df_XMAFSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#00C9A7", "#ff8c00","#4D8076")) + theme(legend.position = "none") + ggtitle("X-MAF")

## Plotting XMARXSSA
df_XMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_XMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_XMARXSSA[,2] <- c(rep("X" , 5) , rep("MARX" , 5), rep("SSA", 5))
df_XMARXSSA[1:5, 3] <- variable_imp_XMARXSSA[1]
df_XMARXSSA[6:10, 3] <- variable_imp_XMARXSSA[2]
df_XMARXSSA[11:15, 3] <- variable_imp_XMARXSSA[3]

df_XMARXSSA_2 <- df_XMARXSSA[order(df_XMARXSSA$X1),]

plot_VI_XMARXSSA <- ggplot(df_XMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#009EFA","#ff8c00", "#4D8076")) + theme(legend.position = "none") + ggtitle("X-MARX")

## Plotting FMAFSSA
df_FMAFSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_FMAFSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_FMAFSSA[,2] <- c(rep("F" , 5) , rep("MAF" , 5),  rep("SSA", 5))
df_FMAFSSA[1:5, 3] <- variable_imp_FMAFSSA[1]
df_FMAFSSA[6:10, 3] <- variable_imp_FMAFSSA[2]
df_FMAFSSA[11:15, 3] <- variable_imp_FMAFSSA[3]

df_FMAFSSA_2 <- df_FMAFSSA[order(df_FMAFSSA$X1),]

plot_VI_FMAFSSA <- ggplot(df_FMAFSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2","#00C9A7", "#ff8c00")) + theme(legend.position = "none") + ggtitle("F-MAF")

## Plotting FMARXSSA
df_FMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_FMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_FMARXSSA[,2] <- c(rep("F" , 5) , rep("MARX" , 5), rep("SSA", 5))
df_FMARXSSA[1:5, 3] <- variable_imp_FMARXSSA[1]
df_FMARXSSA[6:10, 3] <- variable_imp_FMARXSSA[2]
df_FMARXSSA[11:15, 3] <- variable_imp_FMARXSSA[3]

df_FMARXSSA_2 <- df_FMARXSSA[order(df_FMARXSSA$X1),]

plot_VI_FMARXSSA <- ggplot(df_FMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2", "#009EFA", "#ff8c00")) + ggtitle("F-MARX") + theme(legend.position = "none") 

## Plotting MAFMARXSSA
df_MAFMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_MAFMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_MAFMARXSSA[,2] <- c(rep("MAF" , 5) , rep("MARX" , 5), rep("SSA", 5))
df_MAFMARXSSA[1:5, 3] <- variable_imp_MAFMARXSSA[1]
df_MAFMARXSSA[6:10, 3] <- variable_imp_MAFMARXSSA[2]
df_MAFMARXSSA[11:15, 3] <- variable_imp_MAFMARXSSA[3]

df_MAFMARXSSA_2 <- df_MAFMARXSSA[order(df_MAFMARXSSA$X1),]

plot_VI_MAFMARXSSA <- ggplot(df_MAFMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#00C9A7", "#009EFA","#ff8c00")) + ggtitle("MAF-MARX-SSA") + theme(legend.position = "none") 

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
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2", "#009EFA")) + ggtitle("F-MARX") + theme(legend.position = "none") 

## Plotting MAFMARX
df_MAFMARX <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_MAFMARX[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_MAFMARX[,2] <- c(rep("MAF" , 5) , rep("MARX" , 5))
df_MAFMARX[1:5, 3] <- variable_imp_MAFMARX[1]
df_MAFMARX[6:10, 3] <- variable_imp_MAFMARX[2]

df_MAFMARX_2 <- df_MAFMARX[order(df_MAFMARX$X1),]

plot_VI_MAFMARX <- ggplot(df_MAFMARX_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#00C9A7", "#009EFA")) + ggtitle("MAF-MARX") + theme(legend.position = "none") 

## Plotting XSSA
df_XSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_XSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_XSSA[,2] <- c(rep("X" , 5) , rep("SSA" , 5))
df_XSSA[1:5, 3] <- variable_imp_XSSA[1]
df_XSSA[6:10, 3] <- variable_imp_XSSA[2]

df_XSSA_2 <- df_XSSA[order(df_XSSA$X1),]

plot_VI_XSSA <- ggplot(df_XSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#ff8c00", "#4D8076")) + theme(legend.position = "none") + ggtitle("X-SSA")

## Plotting FSSA
df_FSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_FSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_FSSA[,2] <- c(rep("F" , 5) , rep("SSA" , 5))
df_FSSA[1:5, 3] <- variable_imp_FSSA[1]
df_FSSA[6:10, 3] <- variable_imp_FSSA[2]

df_FSSA_2 <- df_FSSA[order(df_FSSA$X1),]

plot_VI_FSSA <- ggplot(df_FSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2","#ff8c00")) + theme(legend.position = "none") + ggtitle("F-SSA")

## Plotting MAFSSA
df_MAFSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_MAFSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_MAFSSA[,2] <- c(rep("MAF" , 5) , rep("SSA" , 5))
df_MAFSSA[1:5, 3] <- variable_imp_MAFSSA[1]
df_MAFSSA[6:10, 3] <- variable_imp_MAFSSA[2]

df_MAFSSA_2 <- df_MAFSSA[order(df_MAFSSA$X1),]

plot_VI_MAFSSA <- ggplot(df_MAFSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#00C9A7","#ff8c00")) + theme(legend.position = "none") + ggtitle("MAF-SSA")

## Plotting MARXSSA
df_MARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_MARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_MARXSSA[,2] <- c(rep("MARX" , 5) , rep("SSA" , 5))
df_MARXSSA[1:5, 3] <- variable_imp_MARXSSA[1]
df_MARXSSA[6:10, 3] <- variable_imp_MARXSSA[2]

df_MARXSSA_2 <- df_MARXSSA[order(df_MARXSSA$X1),]

plot_VI_MARXSSA <- ggplot(df_MARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#009EFA","#ff8c00")) + theme(legend.position = "none") + ggtitle("MARX-SSA")

#### ---- PLOTTING ALL ----
# 2 combinations
grid.arrange(plot_VI_XF, plot_VI_XMAF, plot_VI_XMARX, plot_VI_FMAF, plot_VI_FMARX, plot_VI_MAFMARX, plot_VI_XSSA, plot_VI_FSSA, plot_VI_MAFSSA, plot_VI_MARXSSA, nrow=2, ncol=5)

# 3 combinations
grid.arrange(plot_VI_XFMAF, plot_VI_XFMARX, plot_VI_XMAFMARX, plot_VI_FMAFMARX, plot_VI_XFSSA, plot_VI_XMAFSSA, plot_VI_XMARXSSA, plot_VI_FMAFSSA, plot_VI_FMARXSSA, plot_VI_MAFMARXSSA, nrow=2, ncol=5)

# 4 combinations
grid.arrange(plot_VI_XFMAFMARX, plot_VI_XFMAFSSA, 
             plot_VI_XFMARXSSA, plot_VI_XMAFMARXSSA, plot_VI_FMAFMARXSSA, ncol = 5) 

grid.arrange(plot_VI_XFMAFMARX, plot_VI_XFMAFMARXSSA, ncol = 2) 

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

### --- BIG DATA ---
SSA <- read.csv("RF_Small_Big_Matrix.csv")[,2:64]

## ---- DATA ----
# If you use the small dataset: only cpb.infl.stationary
df_cpb_infl_stat <- read.csv("Extra data/data_core.csv")
df_additional_stat <- read.csv("Extra data/data_additional.csv")

# If big dataset: combine cpb.infl.stationary and additional.data.stationary
df_big <- cbind(df_cpb_infl_stat[50:433,], df_additional_stat)
y_differenced <- df_big$L2_LRHUTTTT

regressor_matrix <- df_big[-c(1)] # Dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)

# -- Parameter initalizations for feature matrix --
X_lags <- 12 # We use the data up until one year before
n_Factors <- 8 # Coulombe (BIG DATA) 
F_lags <- 12 # Same reason as X, also because Coulombe
P_MAF <- 12 # Summarize data up until one year 
n_MAF <- 2 # Coulombe
P_MARX <- 12 # Lags for MARX, same as X_lags, also Coulombe 

# -- Make feature matrices --
source("Function_File.r")

X <- as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE))

scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags)

scaled_X <- scale(X) #Scale lagged X Matrix for MAF
MAF <- MAF_function(scaled_X,X_lags,nrow(scaled_X),ncol(regressor_matrix), P_MAF, n_MAF, FALSE)

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

X_F_SSA <- cbind(X, F, SSA)
X_MAF_SSA <- cbind(X, MAF, SSA)
X_MARX_SSA <- cbind(X, MARX, SSA)
F_MAF_SSA <- cbind(F, MAF, SSA)
F_MARX_SSA <- cbind(F, MARX, SSA)
MAF_MARX_SSA <- cbind(MAF, MARX, SSA)
X_F_MAF_SSA <- cbind(X, F, MAF, SSA)
X_F_MARX_SSA <- cbind(X, F, MARX, SSA)
X_MAF_MARX_SSA <- cbind(X, MAF, MARX, SSA)
F_MAF_MARX_SSA <- cbind(F, MAF, MARX, SSA)
X_F_MAF_MARX_SSA <- cbind(X, F, MAF, MARX, SSA)

### --- VARIABLE IMPORTANCE ---
poos <- 120
y_real <- y_differenced[(length(y_differenced)-poos+1):length(y_differenced)]
start_forecast <- length(y_differenced)-poos+1
end_forecast <- length(y_differenced)
horizons <- list(3, 6, 12, 18, 24) 
ntrees <- 500 # Default value
colors <- c("F"= "#FF0000FF", "MAF"= "#CCFF00FF", "MARX"= "#00FF66FF", "SSA"= "#0066FFFF")
colors <- c("F"= "green", "MAF"= "yellow", "MARX"= "red", "SSA"= "blue")

VI_function <- function(y_differenced, Z, poos, horizons, ntrees){
  importance <- data.frame(matrix(ncol = length(horizons), nrow = ncol(Z)))
  i <- 0
  for (h in horizons){
    shift_y = as.data.frame(shift(y_differenced,n=h, type = 'lead', give.names=TRUE))
    colnames(shift_y) <- "y"
    y_Z <- cbind(shift_y, Z)
    i <- i+1
    nu <- Sys.time()
    
    y_Z_train <- y_Z[1:(nrow(y_Z)-poos),] #Compute VI on training set
    
    X.rf <- randomForest(y ~ ., 
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
  print(allvarimp)
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

variable_imp_XSSA <- two_feature_importance(X, SSA, poos, horizons, ntrees)
variable_imp_FSSA <- two_feature_importance(F, SSA, poos, horizons, ntrees)
variable_imp_MAFSSA <- two_feature_importance(MAF, SSA, poos, horizons, ntrees)
variable_imp_MARXSSA <- two_feature_importance(MARX, SSA, poos, horizons, ntrees)

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

variable_imp_XFSSA <- three_feature_importance(X, F, SSA, poos, horizons, ntrees)
variable_imp_XMAFSSA <- three_feature_importance(X, MAF, SSA, poos, horizons, ntrees)
variable_imp_XMARXSSA <- three_feature_importance(X, MARX, SSA, poos, horizons, ntrees)
variable_imp_FMAFSSA <- three_feature_importance(F, MAF, SSA, poos, horizons, ntrees)
variable_imp_FMARXSSA <- three_feature_importance(F, MARX, SSA, poos, horizons, ntrees)
variable_imp_MAFMARXSSA <- three_feature_importance(MAF, MARX, SSA, poos, horizons, ntrees)

### --- VARIABLE IMPORTANCE: 4 COMBINATIONS ---
four_feature_importance <- function(feature1, feature2, feature3, feature4, poos, horizons, ntrees){
  Z <- cbind(feature1, feature2, feature3, feature4)
  
  allvarimp <- VI_function(y_differenced, Z, poos, horizons, ntrees)
  
  f1_imp <- allvarimp[1:(ncol(feature1)),]
  f1_imp <- colSums(f1_imp) / ncol(feature1)
  
  f2_imp <- allvarimp[(ncol(feature1)+1):(ncol(feature1)+ncol(feature2)),]
  f2_imp <- colSums(f2_imp) / ncol(feature2)
  
  f3_imp <- allvarimp[(ncol(feature1)+ncol(feature2)+1):(ncol(feature1)+ncol(feature2)+ncol(feature3)),]
  f3_imp <- colSums(f3_imp) / ncol(feature3)
  
  f4_imp <- allvarimp[(ncol(feature1)+ncol(feature2)+ncol(feature3)+1):(ncol(feature1)+ncol(feature2)+ncol(feature3)+ncol(feature4)),]
  f4_imp <- colSums(f4_imp) / ncol(feature4)
  
  f1_imp_norm <- c(0,0,0,0,0)
  f2_imp_norm <- c(0,0,0,0,0)
  f3_imp_norm <- c(0,0,0,0,0)
  f4_imp_norm <- c(0,0,0,0,0)
  
  for (c in 1:length(horizons)){
    f1_imp_norm[c] <- f1_imp[c] / (f1_imp[c] + f2_imp[c] + f3_imp[c] + f4_imp[c]) *100
    f2_imp_norm[c] <- f2_imp[c] / (f1_imp[c] + f2_imp[c] + f3_imp[c] + f4_imp[c]) *100
    f3_imp_norm[c] <- f3_imp[c] / (f1_imp[c] + f2_imp[c] + f3_imp[c] + f4_imp[c]) *100
    f4_imp_norm[c] <- f4_imp[c] / (f1_imp[c] + f2_imp[c] + f3_imp[c] + f4_imp[c]) *100
  }
  return(list(f1_imp_norm, f2_imp_norm, f3_imp_norm, f4_imp_norm))
}

variable_imp_XFMAFSSA <- four_feature_importance(X, F, MAF, SSA, poos, horizons, ntrees)
variable_imp_XFMARXSSA <- four_feature_importance(X, F, MARX, SSA, poos, horizons, ntrees)
variable_imp_XMAFMARXSSA <- four_feature_importance(X, MAF, MARX, SSA, poos, horizons, ntrees)
variable_imp_FMAFMARXSSA <- four_feature_importance(F, MAF, MARX, SSA, poos, horizons, ntrees)

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
  theme(legend.position = "none",plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#009EFA","#4D8076")) + ggtitle("X-F-MAF-MARX")

# --- IMPORTANCE X_F_MAF_MARX_SSA ---
variable_imp_XFMAFMARXSSA <- VI_function(y_differenced, X_F_MAF_MARX_SSA, poos, horizons, ntrees)

X_importance_XFMAFMARXSSA <- variable_imp_XFMAFMARXSSA[1:ncol(X),]
X_importance_XFMAFMARXSSA <- colSums(X_importance_XFMAFMARXSSA) / ncol(X)

F_importance_XFMAFMARXSSA <- variable_imp_XFMAFMARXSSA[(ncol(X)+1):(ncol(X)+ncol(F)),]
F_importance_XFMAFMARXSSA <- colSums(F_importance_XFMAFMARXSSA) / ncol(F)

MAF_importance_XFMAFMARXSSA <- variable_imp_XFMAFMARXSSA[(ncol(X)+ncol(F)+1):
                                                           (ncol(X)+ncol(F)+ncol(MAF)),]
MAF_importance_XFMAFMARXSSA <- colSums(MAF_importance_XFMAFMARXSSA) / ncol(MAF)

MARX_importance_XFMAFMARXSSA <- variable_imp_XFMAFMARXSSA[(ncol(X)+ncol(F)+ncol(MAF)+1):
                                                            (ncol(X)+ncol(F)+ncol(MAF)+ncol(MARX)),]
MARX_importance_XFMAFMARXSSA <- colSums(MARX_importance_XFMAFMARXSSA) / ncol(MARX)

SSA_importance_XFMAFMARXSSA <- variable_imp_XFMAFMARXSSA[(ncol(X)+ncol(F)+ncol(MAF)+ncol(MARX)+1):
                                                           (ncol(X)+ncol(F)+ncol(MAF)+ncol(MARX)+ncol(SSA)),]
SSA_importance_XFMAFMARXSSA <- colSums(SSA_importance_XFMAFMARXSSA) / ncol(SSA)

X_importance_XFMAFMARXSSA_norm <- c(0,0,0,0,0)
F_importance_XFMAFMARXSSA_norm <- c(0,0,0,0,0)
MAF_importance_XFMAFMARXSSA_norm <- c(0,0,0,0,0)
MARX_importance_XFMAFMARXSSA_norm <- c(0,0,0,0,0)
SSA_importance_XFMAFMARXSSA_norm <- c(0,0,0,0,0)

for (c in 1:length(horizons)){
  X_importance_XFMAFMARXSSA_norm[c] <- X_importance_XFMAFMARXSSA[c] / (X_importance_XFMAFMARXSSA[c] + F_importance_XFMAFMARXSSA[c] 
                                                                       + MAF_importance_XFMAFMARXSSA[c] + MARX_importance_XFMAFMARXSSA[c] + SSA_importance_XFMAFMARXSSA[c]) *100
  
  F_importance_XFMAFMARXSSA_norm[c] <- F_importance_XFMAFMARXSSA[c] / (X_importance_XFMAFMARXSSA[c] + F_importance_XFMAFMARXSSA[c] 
                                                                       + MAF_importance_XFMAFMARXSSA[c] + MARX_importance_XFMAFMARXSSA[c] + SSA_importance_XFMAFMARXSSA[c]) *100
  
  MAF_importance_XFMAFMARXSSA_norm[c] <- MAF_importance_XFMAFMARXSSA[c] / (X_importance_XFMAFMARXSSA[c] + F_importance_XFMAFMARXSSA[c] 
                                                                           + MAF_importance_XFMAFMARXSSA[c] + MARX_importance_XFMAFMARXSSA[c] + SSA_importance_XFMAFMARXSSA[c]) *100
  
  MARX_importance_XFMAFMARXSSA_norm[c] <- MARX_importance_XFMAFMARXSSA[c] / (X_importance_XFMAFMARXSSA[c] + F_importance_XFMAFMARXSSA[c] 
                                                                             + MAF_importance_XFMAFMARXSSA[c] + MARX_importance_XFMAFMARXSSA[c] + SSA_importance_XFMAFMARXSSA[c]) *100
  
  SSA_importance_XFMAFMARXSSA_norm[c] <- SSA_importance_XFMAFMARXSSA[c] / (X_importance_XFMAFMARXSSA[c] + F_importance_XFMAFMARXSSA[c] 
                                                                           + MAF_importance_XFMAFMARXSSA[c] + MARX_importance_XFMAFMARXSSA[c] + SSA_importance_XFMAFMARXSSA[c]) *100
}

## -- Plotting --
size_title <- 10 #Title size

df_XFMAFMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*5))
df_XFMAFMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 5))
df_XFMAFMARXSSA[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MAF" , 5) , rep("MARX" , 5), rep("SSA" , 5))
df_XFMAFMARXSSA[1:5, 3] <- X_importance_XFMAFMARXSSA_norm
df_XFMAFMARXSSA[6:10, 3] <- F_importance_XFMAFMARXSSA_norm
df_XFMAFMARXSSA[11:15, 3] <- MAF_importance_XFMAFMARXSSA_norm
df_XFMAFMARXSSA[16:20, 3] <- MARX_importance_XFMAFMARXSSA_norm
df_XFMAFMARXSSA[21:25, 3] <- SSA_importance_XFMAFMARXSSA_norm

df_XFMAFMARXSSA_2 <- df_XFMAFMARXSSA[order(df_XFMAFMARXSSA$X1),]
colnames(df_XFMAFMARXSSA_2) <- c("horizon", "Feature", "VarImpval")

plot_VI_XFMAFMARXSSA <- ggplot(df_XFMAFMARXSSA_2, aes(fill=Feature, y=VarImpval, x=horizon)) + 
  geom_bar(position="stack", stat="identity")+
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2","#00C9A7","#009EFA", "#ff8c00", "#4D8076")) + 
  ggtitle("X-F-MAF-MARX-SSA") + theme(legend.position = "none")

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

## Plotting XFMAFSSA
df_XFMAFSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*4))
df_XFMAFSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 4))
df_XFMAFSSA[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MAF" , 5), rep("SSA" , 5))
df_XFMAFSSA[1:5, 3] <- variable_imp_XFMAFSSA[1]
df_XFMAFSSA[6:10, 3] <- variable_imp_XFMAFSSA[2]
df_XFMAFSSA[11:15, 3] <- variable_imp_XFMAFSSA[3]
df_XFMAFSSA[16:20, 3] <- variable_imp_XFMAFSSA[4]

df_XFMAFSSA_2 <- df_XFMAFSSA[order(df_XFMAFSSA$X1),]

plot_VI_XFMAFSSA <- ggplot(df_XFMAFSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank())  + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#ff8c00", "#4D8076")) + ggtitle("X-F-MAF-SSA") + theme(legend.position = "none")

## Plotting XFMARXSSA
df_XFMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*4))
df_XFMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 4))
df_XFMARXSSA[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MARX" , 5), rep("SSA" , 5))
df_XFMARXSSA[1:5, 3] <- variable_imp_XFMARXSSA[1]
df_XFMARXSSA[6:10, 3] <- variable_imp_XFMARXSSA[2]
df_XFMARXSSA[11:15, 3] <- variable_imp_XFMARXSSA[3]
df_XFMARXSSA[16:20, 3] <- variable_imp_XFMARXSSA[4]

df_XFMARXSSA_2 <- df_XFMARXSSA[order(df_XFMARXSSA$X1),]

plot_VI_XFMARXSSA <- ggplot(df_XFMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank())  + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#ff8c00", "#4D8076")) + ggtitle("X-F-MARX-SSA") + theme(legend.position = "none")

## Plotting XFMARXSSA
df_XFMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*4))
df_XFMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 4))
df_XFMARXSSA[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("MARX" , 5), rep("SSA" , 5))
df_XFMARXSSA[1:5, 3] <- variable_imp_XFMARXSSA[1]
df_XFMARXSSA[6:10, 3] <- variable_imp_XFMARXSSA[2]
df_XFMARXSSA[11:15, 3] <- variable_imp_XFMARXSSA[3]
df_XFMARXSSA[16:20, 3] <- variable_imp_XFMARXSSA[4]

df_XFMARXSSA_2 <- df_XFMARXSSA[order(df_XFMARXSSA$X1),]

plot_VI_XFMARXSSA <- ggplot(df_XFMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank())  + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#ff8c00", "#4D8076")) + ggtitle("X-F-MARX-SSA") + theme(legend.position = "none")

## Plotting XMAFMARXSSA
df_XMAFMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*4))
df_XMAFMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 4))
df_XMAFMARXSSA[,2] <- c(rep("X" , 5) , rep("MAF" , 5) , rep("MARX" , 5), rep("SSA" , 5))
df_XMAFMARXSSA[1:5, 3] <- variable_imp_XMAFMARXSSA[1]
df_XMAFMARXSSA[6:10, 3] <- variable_imp_XMAFMARXSSA[2]
df_XMAFMARXSSA[11:15, 3] <- variable_imp_XMAFMARXSSA[3]
df_XMAFMARXSSA[16:20, 3] <- variable_imp_XMAFMARXSSA[4]

df_XMAFMARXSSA_2 <- df_XMAFMARXSSA[order(df_XMAFMARXSSA$X1),]

plot_VI_XMAFMARXSSA <- ggplot(df_XMAFMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank())  + 
  scale_fill_manual(values=c("#00C9A7","#009EFA","#ff8c00", "#4D8076")) + ggtitle("X-MAF-MARX-SSA") + theme(legend.position = "none")

## Plotting FMAFMARXSSA
df_FMAFMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*4))
df_FMAFMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 4))
df_FMAFMARXSSA[,2] <- c(rep("F" , 5) , rep("MAF" , 5) , rep("MARX" , 5), rep("SSA" , 5))
df_FMAFMARXSSA[1:5, 3] <- variable_imp_FMAFMARXSSA[1]
df_FMAFMARXSSA[6:10, 3] <- variable_imp_FMAFMARXSSA[2]
df_FMAFMARXSSA[11:15, 3] <- variable_imp_FMAFMARXSSA[3]
df_FMAFMARXSSA[16:20, 3] <- variable_imp_FMAFMARXSSA[4]

df_FMAFMARXSSA_2 <- df_FMAFMARXSSA[order(df_FMAFMARXSSA$X1),]

plot_VI_FMAFMARXSSA <- ggplot(df_FMAFMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank())  + 
  scale_fill_manual(values=c("#845EC2","#00C9A7","#009EFA","#ff8c00")) + ggtitle("X-MAF-MARX-SSA") + theme(legend.position = "none")

## Plotting XFSSA
df_XFSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_XFSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_XFSSA[,2] <- c(rep("X" , 5) , rep("F" , 5) , rep("SSA" , 5))
df_XFSSA[1:5, 3] <- variable_imp_XFSSA[1]
df_XFSSA[6:10, 3] <- variable_imp_XFSSA[2]
df_XFSSA[11:15, 3] <- variable_imp_XFSSA[3]

df_XFSSA_2 <- df_XFSSA[order(df_XFSSA$X1),]

plot_VI_XFSSA <- ggplot(df_XFSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2", "#ff8c00", "#4D8076")) + theme(legend.position = "none") + ggtitle("X-F-SSA")

## Plotting XMAFSSA
df_XMAFSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_XMAFSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_XMAFSSA[,2] <- c(rep("X" , 5) , rep("MAF" , 5), rep("SSA", 5))
df_XMAFSSA[1:5, 3] <- variable_imp_XMAFSSA[1]
df_XMAFSSA[6:10, 3] <- variable_imp_XMAFSSA[2]
df_XMAFSSA[11:15, 3] <- variable_imp_XMAFSSA[3]

df_XMAFSSA_2 <- df_XMAFSSA[order(df_XMAFSSA$X1),]

plot_VI_XMAFSSA <- ggplot(df_XMAFSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#00C9A7", "#ff8c00","#4D8076")) + theme(legend.position = "none") + ggtitle("X-MAF")

## Plotting XMARXSSA
df_XMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_XMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_XMARXSSA[,2] <- c(rep("X" , 5) , rep("MARX" , 5), rep("SSA", 5))
df_XMARXSSA[1:5, 3] <- variable_imp_XMARXSSA[1]
df_XMARXSSA[6:10, 3] <- variable_imp_XMARXSSA[2]
df_XMARXSSA[11:15, 3] <- variable_imp_XMARXSSA[3]

df_XMARXSSA_2 <- df_XMARXSSA[order(df_XMARXSSA$X1),]

plot_VI_XMARXSSA <- ggplot(df_XMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#009EFA","#ff8c00", "#4D8076")) + theme(legend.position = "none") + ggtitle("X-MARX")

## Plotting FMAFSSA
df_FMAFSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_FMAFSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_FMAFSSA[,2] <- c(rep("F" , 5) , rep("MAF" , 5),  rep("SSA", 5))
df_FMAFSSA[1:5, 3] <- variable_imp_FMAFSSA[1]
df_FMAFSSA[6:10, 3] <- variable_imp_FMAFSSA[2]
df_FMAFSSA[11:15, 3] <- variable_imp_FMAFSSA[3]

df_FMAFSSA_2 <- df_FMAFSSA[order(df_FMAFSSA$X1),]

plot_VI_FMAFSSA <- ggplot(df_FMAFSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2","#00C9A7", "#ff8c00")) + theme(legend.position = "none") + ggtitle("F-MAF")

## Plotting FMARXSSA
df_FMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_FMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_FMARXSSA[,2] <- c(rep("F" , 5) , rep("MARX" , 5), rep("SSA", 5))
df_FMARXSSA[1:5, 3] <- variable_imp_FMARXSSA[1]
df_FMARXSSA[6:10, 3] <- variable_imp_FMARXSSA[2]
df_FMARXSSA[11:15, 3] <- variable_imp_FMARXSSA[3]

df_FMARXSSA_2 <- df_FMARXSSA[order(df_FMARXSSA$X1),]

plot_VI_FMARXSSA <- ggplot(df_FMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2", "#009EFA", "#ff8c00")) + ggtitle("F-MARX") + theme(legend.position = "none") 

## Plotting MAFMARXSSA
df_MAFMARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*3))
df_MAFMARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 3))
df_MAFMARXSSA[,2] <- c(rep("MAF" , 5) , rep("MARX" , 5), rep("SSA", 5))
df_MAFMARXSSA[1:5, 3] <- variable_imp_MAFMARXSSA[1]
df_MAFMARXSSA[6:10, 3] <- variable_imp_MAFMARXSSA[2]
df_MAFMARXSSA[11:15, 3] <- variable_imp_MAFMARXSSA[3]

df_MAFMARXSSA_2 <- df_MAFMARXSSA[order(df_MAFMARXSSA$X1),]

plot_VI_MAFMARXSSA <- ggplot(df_MAFMARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#00C9A7", "#009EFA","#ff8c00")) + ggtitle("MAF-MARX-SSA") + theme(legend.position = "none") 

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
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2", "#009EFA")) + ggtitle("F-MARX") + theme(legend.position = "none") 

## Plotting MAFMARX
df_MAFMARX <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_MAFMARX[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_MAFMARX[,2] <- c(rep("MAF" , 5) , rep("MARX" , 5))
df_MAFMARX[1:5, 3] <- variable_imp_MAFMARX[1]
df_MAFMARX[6:10, 3] <- variable_imp_MAFMARX[2]

df_MAFMARX_2 <- df_MAFMARX[order(df_MAFMARX$X1),]

plot_VI_MAFMARX <- ggplot(df_MAFMARX_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#00C9A7", "#009EFA")) + ggtitle("MAF-MARX") + theme(legend.position = "none") 

## Plotting XSSA
df_XSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_XSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_XSSA[,2] <- c(rep("X" , 5) , rep("SSA" , 5))
df_XSSA[1:5, 3] <- variable_imp_XSSA[1]
df_XSSA[6:10, 3] <- variable_imp_XSSA[2]

df_XSSA_2 <- df_XSSA[order(df_XSSA$X1),]

plot_VI_XSSA <- ggplot(df_XSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#ff8c00", "#4D8076")) + theme(legend.position = "none") + ggtitle("X-SSA")

## Plotting FSSA
df_FSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_FSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_FSSA[,2] <- c(rep("F" , 5) , rep("SSA" , 5))
df_FSSA[1:5, 3] <- variable_imp_FSSA[1]
df_FSSA[6:10, 3] <- variable_imp_FSSA[2]

df_FSSA_2 <- df_FSSA[order(df_FSSA$X1),]

plot_VI_FSSA <- ggplot(df_FSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2","#ff8c00")) + theme(legend.position = "none") + ggtitle("F-SSA")

## Plotting MAFSSA
df_MAFSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_MAFSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_MAFSSA[,2] <- c(rep("MAF" , 5) , rep("SSA" , 5))
df_MAFSSA[1:5, 3] <- variable_imp_MAFSSA[1]
df_MAFSSA[6:10, 3] <- variable_imp_MAFSSA[2]

df_MAFSSA_2 <- df_MAFSSA[order(df_MAFSSA$X1),]

plot_VI_MAFSSA <- ggplot(df_MAFSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#00C9A7","#ff8c00")) + theme(legend.position = "none") + ggtitle("MAF-SSA")

## Plotting MARXSSA
df_MARXSSA <- data.frame(matrix(ncol=3, nrow=length(horizons)*2))
df_MARXSSA[,1] <- c(rep(c("1) h=3", "2) h=6", "3) h=12", "4) h=18", "5) h=24"), 2))
df_MARXSSA[,2] <- c(rep("MARX" , 5) , rep("SSA" , 5))
df_MARXSSA[1:5, 3] <- variable_imp_MARXSSA[1]
df_MARXSSA[6:10, 3] <- variable_imp_MARXSSA[2]

df_MARXSSA_2 <- df_MARXSSA[order(df_MARXSSA$X1),]

plot_VI_MARXSSA <- ggplot(df_MARXSSA_2, aes(fill=X2, y=X3, x=X1)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#009EFA","#ff8c00")) + theme(legend.position = "none") + ggtitle("MARX-SSA")

#### ---- PLOTTING ALL ----
# 2 combinations
grid.arrange(plot_VI_XF, plot_VI_XMAF, plot_VI_XMARX, plot_VI_FMAF, plot_VI_FMARX, plot_VI_MAFMARX, plot_VI_XSSA, plot_VI_FSSA, plot_VI_MAFSSA, plot_VI_MARXSSA, nrow=2, ncol=5)

# 3 combinations
grid.arrange(plot_VI_XFMAF, plot_VI_XFMARX, plot_VI_XMAFMARX, plot_VI_FMAFMARX, plot_VI_XFSSA, plot_VI_XMAFSSA, plot_VI_XMARXSSA, plot_VI_FMAFSSA, plot_VI_FMARXSSA, plot_VI_MAFMARXSSA, nrow=2, ncol=5)

# 4 combinations
grid.arrange(plot_VI_XFMAFMARX, plot_VI_XFMAFSSA, 
             plot_VI_XFMARXSSA, plot_VI_XMAFMARXSSA, plot_VI_FMAFMARXSSA, ncol = 5) 

grid.arrange(plot_VI_XFMAFMARX, plot_VI_XFMAFMARXSSA, ncol = 2) 

