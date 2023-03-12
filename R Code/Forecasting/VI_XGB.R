### SEMINAR APPLIED ECONOMETRICS 
rm(list=ls())

library(tidyverse)
library(tidyselect)
library(BVAR)
library(randomForest)
library(vars)
library(data.table)
library(tseries)
library(xgboost)
library(caret)
library(cbsodataR)
library(lubridate)
library(dplyr)
library(yahoofinancer)
library(xtable)
library(Rssa)
library(ggplot2)
library(gridExtra)
library(cowplot)

source("Function_File.r")
source("Data_File.r")

# If you use the small dataset: only cpb.infl.stationary

# If big dataset: combine cpb.infl.stationary and additional.data.stationary

new_df_small <- cbind(cpb.infl.stationary)
new_df_large <- cbind(cpb.infl.stationary[50:433,],additional.data.stationary)
new_df <- new_df_small

# Define stationary dataframes
regressor_matrix <- new_df[-c(1)] # Dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)

# -- Parameter initalizations for feature matrix --
X_lags <- 12 # We use the data up until one year before
n_Factors <- 5 # Optimization and Coulombe
F_lags <- 12 # Same reason as X, also because Coulombe
P_MAF <- 12 # Summarize data up until one year 
n_MAF <- 2 # Optimization 
P_MARX <- 12 # Lags for MARX, same as X_lags, also Coulombe 

# -- Make feature matrices --

X <- as.data.frame(shift(regressor_matrix,n=0:X_lags, type = 'lag', give.names=TRUE))

scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags)
for (i in 1:ncol(F)) {
  fac <- ceiling(i/13)
  lags <- ifelse( i < 14, i-1, (i - (fac-1)*13) - 1)
  colnames(F)[i] <- paste('Fac_',fac,'_lag_',lags)
}

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

BIG_SSA <- read.csv("Extra Data/BIG_SSA.csv",row.names = 1)
SMALL_SSA <- read.csv("Extra Data/SMALL_SSA.csv",row.names = 1)
for (i in 1:ncol(SMALL_SSA)) {colnames(SMALL_SSA)[i] <- paste("SSA_",i,sep='')}

Z <- cbind(X, F, MAF, MARX, SMALL_SSA)
horizons = c(3,6,12,18,24)

for (h in horizons){
y <- new_df[,1]
shift_y = as.data.frame(shift(y,n=h, type = 'lead', give.names=TRUE))
colnames(shift_y) = 'y'
y_Z <- na.omit(cbind(shift_y, Z))

#define final training and testing sets
xgb_test = xgb.DMatrix(data = data.matrix(y_Z[,-1]), label = y_Z[,1])

# Define CV method
train_control <- trainControl(method = "cv", number = 5, search = "grid")

# Customising the tuning grid
define_grid <-  expand.grid(max_depth = c(3, 5, 7), 
                            nrounds = c(50, 100, 200, 500),    # number of trees
                            eta = seq(0.05,0.3,0.05),
                            # default values below
                            gamma = 0,
                            subsample = 1,
                            min_child_weight = 1,
                            colsample_bytree = 0.6)

# Training a XGboost Regression tree model while tuning parameters
model <- train(y~., data = y_Z, method = "xgbTree", trControl = train_control, tuneGrid = define_grid)

y_Z <- data.matrix(y_Z)

tuned_model <- xgboost(eta = model$bestTune$eta,
                      data = xgb_test,
                      nrounds = model$bestTune$nrounds,
                      max.depth = model$bestTune$max_depth, # Dit is misschien wel laag voor XGB? 
                      eval_metric = "rmse",
                      verbose = 0)

# Compute feature importance matrix
importance_matrix = xgb.importance(colnames(Z), model = tuned_model)
importance_matrix$FM <- 'X'
for (i in 1:nrow(importance_matrix)) {
  if (length(grep('SSA',importance_matrix[i,1], fixed = TRUE))>0){importance_matrix[i,5] <- 'SSA'}
  if (length(grep('Fac',importance_matrix[i,1], fixed = TRUE))>0){importance_matrix[i,5] <- 'F'}
  if (length(grep('MAF',importance_matrix[i,1], fixed = TRUE))>0){importance_matrix[i,5] <- 'MAF'}
  if (length(grep('MARX',importance_matrix[i,1], fixed = TRUE))>0){importance_matrix[i,5] <- 'MARX'}
}
grouped_fi <- group_by(importance_matrix,FM)
result <- grouped_fi %>% summarise(Gain=sum(Gain))
hor <- rep(paste("h=",h,sep=''),5)
result <- cbind(hor,result)
colnames(result)[1] <- "horizon"
FI_X_F_MAF_MARX_SSA <- rbind(FI_X_F_MAF_MARX_SSA,result)
}

FI_X_F_MAF_MARX_SSA <- data.frame(1,1,1)
FI_X_F_MAF_MARX_SSA <- FI_X_F_MAF_MARX_SSA[-1,]
colnames(FI_X_F_MAF_MARX_SSA) <- c("horizon","FM","Gain")

FI_X_F_MAF_MARX_SSA$new_horizon <- 3
FI_X_F_MAF_MARX_SSA[6:10,4] <- 6
FI_X_F_MAF_MARX_SSA[11:15,4] <- 12
FI_X_F_MAF_MARX_SSA[16:20,4] <- 18
FI_X_F_MAF_MARX_SSA[21:25,4] <- 24
FI_X_F_MAF_MARX_SSA[,3] <- FI_X_F_MAF_MARX_SSA[,3]*100

FI_XGB_SMALL_NO_SSA <- FI_X_F_MAF_MARX %>%
  mutate(horizon = fct_reorder(horizon, new_horizon)) %>%
  ggplot( aes(fill=FM, y=Gain, x=horizon)) +
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=10), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  
        axis.text.x=element_blank()) + 
  theme(legend.position = "none") + 
  ggtitle("X-F-MAF-MARX")+
  scale_fill_manual(values=c("#845EC2","#00C9A7","#009EFA","#4D8076"))
print(FI_XGB_SMALL_NO_SSA)

FI_XGB_SMALL_SSA <- FI_X_F_MAF_MARX_SSA %>%
  mutate(horizon = fct_reorder(horizon, new_horizon)) %>%
  ggplot( aes(fill=FM, y=Gain, x=horizon)) +
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=10), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  
        axis.text.x=element_blank()) + 
  theme(legend.position = "none") + 
  ggtitle("X-F-MAF-MARX-SSA")+
  scale_fill_manual(values=c("#845EC2","#00C9A7","#009EFA","#FFA500","#4D8076"))
print(FI_XGB_SMALL_SSA)


FI_XGB_BIG_NO_SSA <- FI_X_F_MAF_MARX_BIG %>%
  mutate(horizon = fct_reorder(horizon, new_horizon)) %>%
  ggplot( aes(fill=FM, y=Gain, x=horizon)) +
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=10), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  
        axis.text.x=element_blank()) + 
  theme(legend.position = "none") + 
  ggtitle("X-F-MAF-MARX")+
  scale_fill_manual(values=c("#845EC2","#00C9A7","#009EFA","#4D8076"))
print(FI_XGB_BIG_NO_SSA)

FI_XGB_BIG_SSA <- FI_X_F_MAF_MARX_BIG_SSA %>%
  mutate(horizon = fct_reorder(horizon, new_horizon)) %>%
  ggplot( aes(fill=FM, y=Gain, x=horizon)) +
  geom_bar(position="stack", stat="identity") +
  theme(plot.title = element_text(hjust=0.5, size=10), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  
        axis.text.x=element_blank()) + 
  #theme(legend.position = "none") + 
  ggtitle("X-F-MAF-MARX-SSA")+
  scale_fill_manual(values=c("#845EC2","#00C9A7","#009EFA","#FFA500","#4D8076"))
print(FI_XGB_BIG_SSA)

png("XGB_FI.png",width=1000, height = 600)
grid.arrange(FI_XGB_SMALL_NO_SSA,FI_XGB_SMALL_SSA,FI_XGB_BIG_NO_SSA,FI_XGB_BIG_SSA,ncol=4)
dev.off()
