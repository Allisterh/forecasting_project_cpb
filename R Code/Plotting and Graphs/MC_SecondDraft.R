#Clearing Environment
rm(list=ls())

## -- Importing Data --
df_cpb_infl_stat <- read.csv("Extra data/data_core.csv")
y_differenced <- df_cpb_infl_stat$L2_LRHUTTTT
df_small <- df_cpb_infl_stat
regressor_matrix <- df_small[-c(1)] # Dependent variable
n_var <- ncol(regressor_matrix)
T <- nrow(regressor_matrix)

# RF SMALL Prediction data
RF_Small_X <- read.csv("RF_small/RF_Small_X.csv")
RF_Small_F <- read.csv("RF_small/RF_Small_F.csv")
RF_Small_MAF <- read.csv("RF_small/RF_Small_MAF.csv")
RF_Small_MARX <- read.csv("RF_small/RF_Small_MARX.csv")
RF_Small_MARX <- RF_Small_MARX[,2:6]
RF_Small_X_F <- read.csv("RF_small/RF_Small_X_F.csv")
RF_Small_X_MAF <- read.csv("RF_small/RF_Small_X_MAF.csv")
RF_Small_X_MARX <- read.csv("RF_small/RF_Small_X_MARX.csv")
RF_Small_F_MAF <- read.csv("RF_small/RF_Small_F_MAF.csv")
RF_Small_F_MARX <- read.csv("RF_small/RF_Small_F_MARX.csv")
RF_Small_MAF_MARX <- read.csv("RF_small/RF_Small_MAF_MARX.csv")
RF_Small_X_F_MAF <- read.csv("RF_small/RF_Small_X_F_MAF.csv")
RF_Small_X_F_MARX <- read.csv("RF_small/RF_Small_X_F_MARX.csv")
RF_Small_X_MAF_MARX <- read.csv("RF_small/RF_Small_X_MAF_MARX.csv")
RF_Small_F_MAF_MARX <- read.csv("RF_small/RF_Small_F_MAF_MARX.csv")
RF_Small_X_F_MAF_MARX <- read.csv("RF_small/RF_Small_X_F_MAF_MARX.csv")

# RF BIG Prediction data
RF_Big_X <- read.csv("RF_Big/RF_Big_X.csv")[,2:6]
RF_Big_F <- read.csv("RF_Big/RF_Big_F.csv")[,2:6]
RF_Big_MAF <- read.csv("RF_Big/RF_Big_MAF.csv")[,2:6]
RF_Big_MARX <- read.csv("RF_Big/RF_Big_MARX.csv")[,2:6]
RF_Big_X_F <- read.csv("RF_Big/RF_Big_X_F.csv")[,2:6]
RF_Big_X_MAF <- read.csv("RF_Big/RF_Big_X_MAF.csv")[,2:6]
RF_Big_X_MARX <- read.csv("RF_Big/RF_Big_X_MARX.csv")[,2:6]
RF_Big_F_MAF <- read.csv("RF_Big/RF_Big_F_MAF.csv")[,2:6]
RF_Big_F_MARX <- read.csv("RF_Big/RF_Big_F_MARX.csv")[,2:6]
RF_Big_MAF_MARX <- read.csv("RF_Big/RF_Big_MAF_MARX.csv")[,2:6]
RF_Big_X_F_MAF <- read.csv("RF_Big/RF_Big_X_F_MAF.csv")[,2:6]
RF_Big_X_F_MARX <- read.csv("RF_Big/RF_Big_X_F_MARX.csv")[,2:6]
RF_Big_X_MAF_MARX <- read.csv("RF_Big/RF_Big_X_MAF_MARX.csv")[,2:6]
RF_Big_F_MAF_MARX <- read.csv("RF_Big/RF_Big_F_MAF_MARX.csv")[,2:6]
RF_Big_X_F_MAF_MARX <- read.csv("RF_Big/RF_Big_X_F_MAF_MARX.csv")[,2:6]

# XGB SMALL Prediction data
XGB_X_forecast_small <- read.csv("XGB_Predictions_small/XGB_X_forecast_small.csv")
XGB_F_forecast_small <- read.csv("XGB_Predictions_small/XGB_F_forecast_small.csv")
XGB_MAF_forecast_small <- read.csv("XGB_Predictions_small/XGB_MAF_forecast_small.csv")
XGB_MARX_forecast_small <- read.csv("XGB_Predictions_small/XGB_MARX_forecast_small.csv")
XGB_X_F_forecast_small <- read.csv("XGB_Predictions_small/XGB_X_F_forecast_small.csv")
XGB_X_MAF_forecast_small <- read.csv("XGB_Predictions_small/XGB_X_MAF_forecast_small.csv")
XGB_X_MARX_forecast_small <- read.csv("XGB_Predictions_small/XGB_X_MARX_forecast_small.csv")
XGB_F_MAF_forecast_small <- read.csv("XGB_Predictions_small/XGB_F_MAF_forecast_small.csv")
XGB_F_MARX_forecast_small <- read.csv("XGB_Predictions_small/XGB_F_MARX_forecast_small.csv")
XGB_MAF_MARX_forecast_small <- read.csv("XGB_Predictions_small/XGB_MAF_MARX_forecast_small.csv")
XGB_X_F_MAF_forecast_small <- read.csv("XGB_Predictions_small/XGB_X_F_MAF_forecast_small.csv")
XGB_X_F_MARX_forecast_small <- read.csv("XGB_Predictions_small/XGB_X_F_MARX_forecast_small.csv")
XGB_X_MAF_MARX_forecast_small <- read.csv("XGB_Predictions_small/XGB_X_MAF_MARX_forecast_small.csv")
XGB_F_MAF_MARX_forecast_small <- read.csv("XGB_Predictions_small/XGB_F_MAF_MARX_forecast_small.csv")
XGB_X_F_MAF_MARX_forecast_small <- read.csv("XGB_Predictions_small/XGB_X_F_MAF_MARX_forecast_small.csv")

# XGB BIG Prediction data
XGB_X_forecast_big <- read.csv("XGB_Predictions_big/XGB_X_forecast_big.csv")
XGB_F_forecast_big <- read.csv("XGB_Predictions_big/XGB_F_forecast_big.csv")
XGB_MAF_forecast_big <- read.csv("XGB_Predictions_big/XGB_MAF_forecast_big.csv")
XGB_MARX_forecast_big <- read.csv("XGB_Predictions_big/XGB_MARX_forecast_big.csv")
XGB_X_F_forecast_big <- read.csv("XGB_Predictions_big/XGB_X_F_forecast_big.csv")
XGB_X_MAF_forecast_big <- read.csv("XGB_Predictions_big/XGB_X_MAF_forecast_big.csv")
XGB_X_MARX_forecast_big <- read.csv("XGB_Predictions_big/XGB_X_MARX_forecast_big.csv")
XGB_F_MAF_forecast_big <- read.csv("XGB_Predictions_big/XGB_F_MAF_forecast_big.csv")
XGB_F_MARX_forecast_big <- read.csv("XGB_Predictions_big/XGB_F_MARX_forecast_big.csv")
XGB_MAF_MARX_forecast_big <- read.csv("XGB_Predictions_big/XGB_MAF_MARX_forecast_big.csv")
XGB_X_F_MAF_forecast_big <- read.csv("XGB_Predictions_big/XGB_X_F_MAF_forecast_big.csv")
XGB_X_F_MARX_forecast_big <- read.csv("XGB_Predictions_big/XGB_X_F_MARX_forecast_big.csv")
XGB_X_MAF_MARX_forecast_big <- read.csv("XGB_Predictions_big/XGB_X_MAF_MARX_forecast_big.csv")
XGB_F_MAF_MARX_forecast_big <- read.csv("XGB_Predictions_big/XGB_F_MAF_MARX_forecast_big.csv")
XGB_X_F_MAF_MARX_forecast_big <- read.csv("XGB_Predictions_big/XGB_X_F_MAF_MARX_forecast_big.csv")

### --- Marginal Effects --- 
# - Initializing -
library(ggplot2)
library(gridExtra)   

poos <- 120
y_real <- y_differenced[(length(y_differenced)-poos+1):length(y_differenced)]
start_forecast <- length(y_differenced)-poos+1
end_forecast <- length(y_differenced)
horizons <- list(3, 6, 12, 18, 24) 

## -- Functions --
# - R-Squared Function -
Rsquared_function <- function(y_real, y_pred){
  e_p <- sum((y_real - y_pred)^2)
  e_m <- sum((y_real - mean(y_real))^2)
  Rsquared <- 1-(e_p/e_m)
  return(Rsquared)
}

# - Marginal Contribution Function -
# MF_matrices: list of matrices of predictions made including the feature under study
# F_matrices: list of matrices of predictions made excluding the feature under study
marginal_function <- function(horizons, y_real, Mf_matrices, F_matrices){
  n_marg_combinations <- length(Mf_matrices)
  Rsquared_Mf <- data.frame(matrix(ncol = length(horizons), nrow = n_marg_combinations)) #1/2) set up dataframe for calculated Rsquared for feature combination. 
  Rsquared_F <- data.frame(matrix(ncol = length(horizons), nrow = n_marg_combinations)) #2/2) columns: horizons, rows: featurecombinations
  
  for (f in 1:n_marg_combinations){
    y_pred_Mf <- Mf_matrices[[f]] #select matrix from list of matrices in Mf (including feature)
    y_pred_F <- F_matrices[[f]] #select matrix from list of matrices in F (excluding feature)
    for (h in 1:length(horizons)){
      Rsquared_Mf[f,h] <- Rsquared_function(y_real, y_pred_Mf[,h]) #calculate Rsquared including feature
      Rsquared_F[f,h] <- Rsquared_function(y_real, y_pred_F[,h])  #calculate Rsquared excluding feature
    }
  }
  Delta_Rsquared <- Rsquared_Mf - Rsquared_F  #substract to calculate difference in Rsquared. columns: horizons, rows: feature combinations
  return(Delta_Rsquared)
}

## -- Calculating Marginal Effects (RF Small)--
# F
Mf_matrices <- list(RF_Small_X_F, RF_Small_F_MAF, RF_Small_F_MARX, RF_Small_X_F_MAF, RF_Small_X_F_MARX, RF_Small_F_MAF_MARX, RF_Small_X_F_MAF_MARX)
F_matrices <- list(RF_Small_X, RF_Small_MAF, RF_Small_MARX, RF_Small_X_MAF, RF_Small_X_MARX, RF_Small_MAF_MARX, RF_Small_X_MAF_MARX)

RF_Small_F_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

# MAF
Mf_matrices <- list(RF_Small_X_MAF, RF_Small_F_MAF, RF_Small_MAF_MARX, RF_Small_X_F_MAF, RF_Small_X_MAF_MARX, RF_Small_F_MAF_MARX, RF_Small_X_F_MAF_MARX)
F_matrices <- list(RF_Small_X, RF_Small_F, RF_Small_MARX, RF_Small_X_F, RF_Small_X_MARX, RF_Small_F_MARX, RF_Small_X_F_MARX)

RF_Small_MAF_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

# MARX
Mf_matrices <- list(RF_Small_X_MARX, RF_Small_MAF_MARX, RF_Small_F_MARX, RF_Small_X_MAF_MARX, RF_Small_F_MAF_MARX, RF_Small_X_F_MAF_MARX, RF_Small_X_F_MARX)
F_matrices <- list(RF_Small_X, RF_Small_MAF, RF_Small_F, RF_Small_X_MAF, RF_Small_F_MAF, RF_Small_X_F_MAF, RF_Small_X_F)

RF_Small_MARX_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

## -- Plotting Marginal Effects (RF Small) --
Marginal_allhorizons_RF_Small <- data.frame(matrix(ncol = 6, nrow = 3*length(Mf_matrices)))
Marginal_allhorizons_RF_Small[,1] <- t(cbind(t(rep("F",length(Mf_matrices))), t(rep("MAF",length(Mf_matrices))), t(rep("MARX",length(Mf_matrices)))))
colnames(Marginal_allhorizons_RF_Small) <- c("Feature", "h3", "h6", "h12", "h18", "h24")

for (f in 1:length(horizons)){
  Marginal_allhorizons_RF_Small[,f+1] <- t(cbind(t(RF_Small_F_marginal_Rsquared[,f]), t(RF_Small_MAF_marginal_Rsquared[,f]), t(RF_Small_MARX_marginal_Rsquared[,f])))
}
Marginal_allhorizons_RF_Small$Feature <- as.factor(Marginal_allhorizons_RF_Small$Feature)

# - Grid Arrange -  
colors = c("F"= "#845EC2", "MAF" = "#00C9A7", "MARX" = "#009EFA")
size_title <- 10

ggp1 <- ggplot(Marginal_allhorizons_RF_Small, aes(x=Feature, y=h3, color=Feature)) + geom_boxplot() + 
  coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                                                         axis.text.y=element_blank())+ggtitle("h=3") + scale_fill_manual(values=colors)

ggp2 <- ggplot(Marginal_allhorizons_RF_Small, aes(x=Feature, y=h6, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=6")

ggp3 <- ggplot(Marginal_allhorizons_RF_Small, aes(x=Feature, y=h12, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=12")

ggp4 <- ggplot(Marginal_allhorizons_RF_Small, aes(x=Feature, y=h18, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=18")

ggp5 <- ggplot(Marginal_allhorizons_RF_Small, aes(x=Feature, y=h24, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=24")

grid.arrange(ggp1, ggp2, ggp3, ggp4, ggp5, ncol = 5)

## -- Calculating Marginal Effects (RF BIG)--
# F
Mf_matrices <- list(RF_Big_X_F, RF_Big_F_MAF, RF_Big_F_MARX, RF_Big_X_F_MAF, RF_Big_X_F_MARX, RF_Big_F_MAF_MARX, RF_Big_X_F_MAF_MARX)
F_matrices <- list(RF_Big_X, RF_Big_MAF, RF_Big_MARX, RF_Big_X_MAF, RF_Big_X_MARX, RF_Big_MAF_MARX, RF_Big_X_MAF_MARX)

RF_Big_F_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

# MAF
Mf_matrices <- list(RF_Big_X_MAF, RF_Big_F_MAF, RF_Big_MAF_MARX, RF_Big_X_F_MAF, RF_Big_X_MAF_MARX, RF_Big_F_MAF_MARX, RF_Big_X_F_MAF_MARX)
F_matrices <- list(RF_Big_X, RF_Big_F, RF_Big_MARX, RF_Big_X_F, RF_Big_X_MARX, RF_Big_F_MARX, RF_Big_X_F_MARX)

RF_Big_MAF_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

# MARX
Mf_matrices <- list(RF_Big_X_MARX, RF_Big_MAF_MARX, RF_Big_F_MARX, RF_Big_X_MAF_MARX, RF_Big_F_MAF_MARX, RF_Big_X_F_MAF_MARX, RF_Big_X_F_MARX)
F_matrices <- list(RF_Big_X, RF_Big_MAF, RF_Big_F, RF_Big_X_MAF, RF_Big_F_MAF, RF_Big_X_F_MAF, RF_Big_X_F)

RF_Big_MARX_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

## -- Plotting Marginal Effects (RF Big) --
Marginal_allhorizons_RF_Big <- data.frame(matrix(ncol = 6, nrow = 3*length(Mf_matrices)))
Marginal_allhorizons_RF_Big[,1] <- t(cbind(t(rep("F",length(Mf_matrices))), t(rep("MAF",length(Mf_matrices))), t(rep("MARX",length(Mf_matrices)))))
colnames(Marginal_allhorizons_RF_Big) <- c("Feature", "h3", "h6", "h12", "h18", "h24")

for (f in 1:length(horizons)){
  Marginal_allhorizons_RF_Big[,f+1] <- t(cbind(t(RF_Big_F_marginal_Rsquared[,f]), t(RF_Big_MAF_marginal_Rsquared[,f]), t(RF_Big_MARX_marginal_Rsquared[,f])))
}
Marginal_allhorizons_RF_Big$Feature <- as.factor(Marginal_allhorizons_RF_Big$Feature)

# - Grid Arrange -  
colors = c("F"= "#845EC2", "MAF" = "#00C9A7", "MARX" = "#009EFA")
size_title <- 10

ggp1 <- ggplot(Marginal_allhorizons_RF_Big, aes(x=Feature, y=h3, color=Feature)) + geom_boxplot() + 
  coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                                                         axis.text.y=element_blank())+ggtitle("h=3") + scale_fill_manual(values=colors)

ggp2 <- ggplot(Marginal_allhorizons_RF_Big, aes(x=Feature, y=h6, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=6")

ggp3 <- ggplot(Marginal_allhorizons_RF_Big, aes(x=Feature, y=h12, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=12")

ggp4 <- ggplot(Marginal_allhorizons_RF_Big, aes(x=Feature, y=h18, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=18")

ggp5 <- ggplot(Marginal_allhorizons_RF_Big, aes(x=Feature, y=h24, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=24")

grid.arrange(ggp1, ggp2, ggp3, ggp4, ggp5, ncol = 5)

## -- Calculating Marginal Effects (XGB SMALL)--
# F
Mf_matrices <- list(XGB_X_F_forecast_small, XGB_F_MAF_forecast_small, XGB_F_MARX_forecast_small, XGB_X_F_MAF_forecast_small, XGB_X_F_MARX_forecast_small, XGB_F_MAF_MARX_forecast_small, XGB_X_F_MAF_MARX_forecast_small)
F_matrices <- list(XGB_X_forecast_small, XGB_MAF_forecast_small, XGB_MARX_forecast_small, XGB_X_MAF_forecast_small, XGB_X_MARX_forecast_small, XGB_MAF_MARX_forecast_small, XGB_X_MAF_MARX_forecast_small)

XGB_SMALL_F_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

# MAF
Mf_matrices <- list(XGB_X_MAF_forecast_small, XGB_F_MAF_forecast_small, XGB_MAF_MARX_forecast_small, XGB_X_F_MAF_forecast_small, XGB_X_MAF_MARX_forecast_small, XGB_F_MAF_MARX_forecast_small, XGB_X_F_MAF_MARX_forecast_small)
F_matrices <- list(XGB_X_forecast_small, XGB_F_forecast_small, XGB_MARX_forecast_small, XGB_X_F_forecast_small, XGB_X_MARX_forecast_small, XGB_F_MARX_forecast_small, XGB_X_F_MARX_forecast_small)

XGB_SMALL_MAF_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

# MARX
Mf_matrices <- list(XGB_X_MARX_forecast_small, XGB_MAF_MARX_forecast_small, XGB_F_MARX_forecast_small, XGB_X_MAF_MARX_forecast_small, XGB_F_MAF_MARX_forecast_small, XGB_X_F_MAF_MARX_forecast_small, XGB_X_F_MARX_forecast_small)
F_matrices <- list(XGB_X_forecast_small, XGB_MAF_forecast_small, XGB_F_forecast_small, XGB_X_MAF_forecast_small, XGB_F_MAF_forecast_small, XGB_X_F_MAF_forecast_small, XGB_X_F_forecast_small)

XGB_SMALL_MARX_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

## -- Plotting Marginal Effects (XGB SMALL) --
size_title <- 10
Marginal_allhorizons_XGB_SMALL <- data.frame(matrix(ncol = 6, nrow = 3*length(Mf_matrices)))
Marginal_allhorizons_XGB_SMALL[,1] <- t(cbind(t(rep("F",length(Mf_matrices))), t(rep("MAF",length(Mf_matrices))), t(rep("MARX",length(Mf_matrices)))))
colnames(Marginal_allhorizons_XGB_SMALL) <- c("Feature", "h3", "h6", "h12", "h18", "h24")

for (f in 1:length(horizons)){
  Marginal_allhorizons_XGB_SMALL[,f+1] <- t(cbind(t(XGB_SMALL_F_marginal_Rsquared[,f]), t(XGB_SMALL_MAF_marginal_Rsquared[,f]), t(XGB_SMALL_MARX_marginal_Rsquared[,f])))
}
Marginal_allhorizons_XGB_SMALL$Feature <- as.factor(Marginal_allhorizons_XGB_SMALL$Feature)

# - Grid Arrange -  
ggp1 <- ggplot(Marginal_allhorizons_XGB_SMALL, aes(x=Feature, y=h3, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none", plot.title = element_text(hjust=0.5, size=size_title), 
  axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=3") 

ggp2 <- ggplot(Marginal_allhorizons_XGB_SMALL, aes(x=Feature, y=h6, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none",plot.title = element_text(hjust=0.5, size=size_title),
  axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=6") 

ggp3 <- ggplot(Marginal_allhorizons_XGB_SMALL, aes(x=Feature, y=h12, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none",plot.title = element_text(hjust=0.5, size=size_title),
  axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=12")

ggp4 <- ggplot(Marginal_allhorizons_XGB_SMALL, aes(x=Feature, y=h18, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none",plot.title = element_text(hjust=0.5, size=size_title),
  axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=18")

ggp5 <- ggplot(Marginal_allhorizons_XGB_SMALL, aes(x=Feature, y=h24, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none",plot.title = element_text(hjust=0.5, size=size_title), 
  axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=24")

grid.arrange(ggp1, ggp2, ggp3, ggp4, ggp5, ncol = 5)

## -- Calculating Marginal Effects (XGB BIG)--
# F
Mf_matrices <- list(XGB_X_F_forecast_big, XGB_F_MAF_forecast_big, XGB_F_MARX_forecast_big, XGB_X_F_MAF_forecast_big, XGB_X_F_MARX_forecast_big, XGB_F_MAF_MARX_forecast_big, XGB_X_F_MAF_MARX_forecast_big)
F_matrices <- list(XGB_X_forecast_big, XGB_MAF_forecast_big, XGB_MARX_forecast_big, XGB_X_MAF_forecast_big, XGB_X_MARX_forecast_big, XGB_MAF_MARX_forecast_big, XGB_X_MAF_MARX_forecast_big)

XGB_big_F_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

# MAF
Mf_matrices <- list(XGB_X_MAF_forecast_big, XGB_F_MAF_forecast_big, XGB_MAF_MARX_forecast_big, XGB_X_F_MAF_forecast_big, XGB_X_MAF_MARX_forecast_big, XGB_F_MAF_MARX_forecast_big, XGB_X_F_MAF_MARX_forecast_big)
F_matrices <- list(XGB_X_forecast_big, XGB_F_forecast_big, XGB_MARX_forecast_big, XGB_X_F_forecast_big, XGB_X_MARX_forecast_big, XGB_F_MARX_forecast_big, XGB_X_F_MARX_forecast_big)

XGB_big_MAF_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

# MARX
Mf_matrices <- list(XGB_X_MARX_forecast_big, XGB_MAF_MARX_forecast_big, XGB_F_MARX_forecast_big, XGB_X_MAF_MARX_forecast_big, XGB_F_MAF_MARX_forecast_big, XGB_X_F_MAF_MARX_forecast_big, XGB_X_F_MARX_forecast_big)
F_matrices <- list(XGB_X_forecast_big, XGB_MAF_forecast_big, XGB_F_forecast_big, XGB_X_MAF_forecast_big, XGB_F_MAF_forecast_big, XGB_X_F_MAF_forecast_big, XGB_X_F_forecast_big)

XGB_big_MARX_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

## -- Plotting Marginal Effects (XGB BIG) --
size_title <- 10
Marginal_allhorizons_XGB_big <- data.frame(matrix(ncol = 6, nrow = 3*length(Mf_matrices)))
Marginal_allhorizons_XGB_big[,1] <- t(cbind(t(rep("F",length(Mf_matrices))), t(rep("MAF",length(Mf_matrices))), t(rep("MARX",length(Mf_matrices)))))
colnames(Marginal_allhorizons_XGB_big) <- c("Feature", "h3", "h6", "h12", "h18", "h24")

for (f in 1:length(horizons)){
  Marginal_allhorizons_XGB_big[,f+1] <- t(cbind(t(XGB_big_F_marginal_Rsquared[,f]), t(XGB_big_MAF_marginal_Rsquared[,f]), t(XGB_big_MARX_marginal_Rsquared[,f])))
}
Marginal_allhorizons_XGB_big$Feature <- as.factor(Marginal_allhorizons_XGB_big$Feature)

# - Grid Arrange -  
ggp1 <- ggplot(Marginal_allhorizons_XGB_big, aes(x=Feature, y=h3, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=3")

ggp2 <- ggplot(Marginal_allhorizons_XGB_big, aes(x=Feature, y=h6, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=6")

ggp3 <- ggplot(Marginal_allhorizons_XGB_big, aes(x=Feature, y=h12, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=12")

ggp4 <- ggplot(Marginal_allhorizons_XGB_big, aes(x=Feature, y=h18, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=18")

ggp5 <- ggplot(Marginal_allhorizons_XGB_big, aes(x=Feature, y=h24, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=24")

grid.arrange(ggp1, ggp2, ggp3, ggp4, ggp5, ncol = 5)

## -- MEDIANS --
# XGB
median_xgb_small_h3_F <- median(Marginal_allhorizons_XGB_SMALL[1:7,2])
median_xgb_small_h3_MAF <- median(Marginal_allhorizons_XGB_SMALL[8:14,2])
median_xgb_small_h3_MARX <- median(Marginal_allhorizons_XGB_SMALL[15:21,2])

median_xgb_small_h24_F <- median(Marginal_allhorizons_XGB_SMALL[1:7,6])
median_xgb_small_h24_MAF <- median(Marginal_allhorizons_XGB_SMALL[8:14,6])
median_xgb_small_h24_MARX <- median(Marginal_allhorizons_XGB_SMALL[15:21,6])

median_xgb_big_h3_F <- median(Marginal_allhorizons_XGB_big[1:7,2])
median_xgb_big_h3_MAF <- median(Marginal_allhorizons_XGB_big[8:14,2])
median_xgb_big_h3_MARX <- median(Marginal_allhorizons_XGB_big[15:21,2])

median_xgb_big_h24_F <- median(Marginal_allhorizons_XGB_big[1:7,6])
median_xgb_big_h24_MAF <- median(Marginal_allhorizons_XGB_big[8:14,6])
median_xgb_big_h24_MARX <- median(Marginal_allhorizons_XGB_big[15:21,6])

# RF
median_RF_small_h3_F <- median(Marginal_allhorizons_RF_Small[1:7,2])
median_RF_small_h3_MAF <- median(Marginal_allhorizons_RF_Small[8:14,2])
median_RF_small_h3_MARX <- median(Marginal_allhorizons_RF_Small[15:21,2])

median_RF_small_h24_F <- median(Marginal_allhorizons_RF_Small[1:7,6])
median_RF_small_h24_MAF <- median(Marginal_allhorizons_RF_Small[8:14,6])
median_RF_small_h24_MARX <- median(Marginal_allhorizons_RF_Small[15:21,6])

median_RF_big_h3_F <- median(Marginal_allhorizons_RF_Big[1:7,2])
median_RF_big_h3_MAF <- median(Marginal_allhorizons_RF_Big[8:14,2])
median_RF_big_h3_MARX <- median(Marginal_allhorizons_RF_Big[15:21,2])

median_RF_big_h24_F <- median(Marginal_allhorizons_RF_Big[1:7,6])
median_RF_big_h24_MAF <- median(Marginal_allhorizons_RF_Big[8:14,6])
median_RF_big_h24_MARX <- median(Marginal_allhorizons_RF_Big[15:21,6])
