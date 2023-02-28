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

### --- VARIABLE IMPORTANCE ---
poos <- 120
y_real <- y_differenced[(length(y_differenced)-poos+1):length(y_differenced)]
start_forecast <- length(y_differenced)-poos+1
end_forecast <- length(y_differenced)
horizons <- list(3, 6, 12, 18, 24) 
ntrees <- 500 # Default value

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
  theme(legend.position = "none",plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
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
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2", "#009EFA")) + ggtitle("F-MARX") #+ theme(legend.position = "none") 


#### ---- PLOTTING ALL ----
grid.arrange(plot_VI_XFMAF, plot_VI_XFMARX, 
             plot_VI_XMAFMARX, plot_VI_FMAFMARX, plot_VI_XFMAFMARX, ncol = 5) 

grid.arrange(plot_VI_XF, plot_VI_XMAF, 
             plot_VI_XMARX, plot_VI_FMAF, plot_VI_FMARX, ncol = 5) 


### --- BIG DATA ---
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

### --- VARIABLE IMPORTANCE ---
poos <- 120
y_real <- y_differenced[(length(y_differenced)-poos+1):length(y_differenced)]
start_forecast <- length(y_differenced)-poos+1
end_forecast <- length(y_differenced)
horizons <- list(3, 6, 12, 18, 24) 
ntrees <- 500 # Default value

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
  theme(legend.position = "none",plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
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
  theme(legend.position = "none", plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  axis.text.x=element_blank()) + scale_fill_manual(values=c("#845EC2", "#00C9A7")) + ggtitle("F-MAF")

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


#### ---- PLOTTING ALL ----
grid.arrange(plot_VI_XFMAF, plot_VI_XFMARX, 
             plot_VI_XMAFMARX, plot_VI_FMAFMARX, plot_VI_XFMAFMARX, ncol = 5) 

grid.arrange(plot_VI_XF, plot_VI_XMAF, 
             plot_VI_XMARX, plot_VI_FMAF, plot_VI_FMARX, ncol = 5) 



