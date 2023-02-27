#Clearing Environment
rm(list=ls())

## -- Importing Data --
#Nikki:
df <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/data-eur2023.csv", row.names=1)

# CPB Direct RF Prediction data
#load data

# CPB Direct XGB Prediction data
#load data

### --- Marginal Effects --- 
# - Initializing -
library(ggplot2)
library(gridExtra)   

poos <- 120
Unempl <- df[,2] 
y_real <- Unempl[(length(Unempl)-poos+1):length(Unempl)] 
start_forecast <- length(Unempl)-poos+1
end_forecast <- length(Unempl)
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

## -- Calculating Marginal Effects (Direct RF)--
# F
Mf_matrices <- list(CPB_Direct_RF_X_F_forecast, CPB_Direct_RF_F_MAF_forecast, CPB_Direct_RF_F_MARX_forecast, CPB_Direct_RF_X_F_MAF_forecast, CPB_Direct_RF_X_F_MARX_forecast, CPB_Direct_RF_F_MAF_MARX_forecast, CPB_Direct_RF_X_F_MAF_MARX_forecast)
F_matrices <- list(CPB_Direct_RF_X_forecast, CPB_Direct_RF_MAF_forecast, CPB_Direct_RF_MARX_forecast, CPB_Direct_RF_X_MAF_forecast, CPB_Direct_RF_X_MARX_forecast, CPB_Direct_RF_MAF_MARX_forecast, CPB_Direct_RF_X_MAF_MARX_forecast)

CPB_Direct_RF_F_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

# MAF
Mf_matrices <- list(CPB_Direct_RF_X_MAF_forecast, CPB_Direct_RF_F_MAF_forecast, CPB_Direct_RF_MAF_MARX_forecast, CPB_Direct_RF_X_F_MAF_forecast, CPB_Direct_RF_X_MAF_MARX_forecast, CPB_Direct_RF_F_MAF_MARX_forecast, CPB_Direct_RF_X_F_MAF_MARX_forecast)
F_matrices <- list(CPB_Direct_RF_X_forecast, CPB_Direct_RF_F_forecast, CPB_Direct_RF_MARX_forecast, CPB_Direct_RF_X_F_forecast, CPB_Direct_RF_X_MARX_forecast, CPB_Direct_RF_F_MARX_forecast, CPB_Direct_RF_X_F_MARX_forecast)

CPB_Direct_RF_MAF_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

# MARX
Mf_matrices <- list(CPB_Direct_RF_X_MARX_forecast, CPB_Direct_RF_MAF_MARX_forecast, CPB_Direct_RF_F_MARX_forecast, CPB_Direct_RF_X_MAF_MARX_forecast, CPB_Direct_RF_F_MAF_MARX_forecast, CPB_Direct_RF_X_F_MAF_MARX_forecast, CPB_Direct_RF_X_F_MARX_forecast)
F_matrices <- list(CPB_Direct_RF_X_forecast, CPB_Direct_RF_MAF_forecast, CPB_Direct_RF_F_forecast, CPB_Direct_RF_X_MAF_forecast, CPB_Direct_RF_F_MAF_forecast, CPB_Direct_RF_X_F_MAF_forecast, CPB_Direct_RF_X_F_forecast)

CPB_Direct_RF_MARX_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

## -- Plotting Marginal Effects (Direct RF) --
Marginal_allhorizons_RF_CPB_direct <- data.frame(matrix(ncol = 6, nrow = 3*length(Mf_matrices)))
Marginal_allhorizons_RF_CPB_direct[,1] <- t(cbind(t(rep("F",length(Mf_matrices))), t(rep("MAF",length(Mf_matrices))), t(rep("MARX",length(Mf_matrices)))))
colnames(Marginal_allhorizons_RF_CPB_direct) <- c("Feature", "h3", "h6", "h12", "h18", "h24")

for (f in 1:length(horizons)){
  Marginal_allhorizons_RF_CPB_direct[,f+1] <- t(cbind(t(CPB_Direct_RF_F_marginal_Rsquared[,f]), t(CPB_Direct_RF_MAF_marginal_Rsquared[,f]), t(CPB_Direct_RF_MARX_marginal_Rsquared[,f])))
}
Marginal_allhorizons_RF_CPB_direct$Feature <- as.factor(Marginal_allhorizons_RF_CPB_direct$Feature)

# - Grid Arrange -  
colors = c("F"= "#845EC2", "MAF" = "#00C9A7", "MARX" = "#009EFA")
size_title <- 10

ggp1 <- ggplot(Marginal_allhorizons_RF_CPB_direct, aes(x=Feature, y=h3, color=Feature)) + geom_boxplot() + 
  coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                                                         axis.text.y=element_blank())+ggtitle("h=3") + scale_fill_manual(values=colors)

ggp2 <- ggplot(Marginal_allhorizons_RF_CPB_direct, aes(x=Feature, y=h6, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=6")

ggp3 <- ggplot(Marginal_allhorizons_RF_CPB_direct, aes(x=Feature, y=h12, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=12")

ggp4 <- ggplot(Marginal_allhorizons_RF_CPB_direct, aes(x=Feature, y=h18, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=18")

ggp5 <- ggplot(Marginal_allhorizons_RF_CPB_direct, aes(x=Feature, y=h24, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip()  + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=24")

grid.arrange(ggp1, ggp2, ggp3, ggp4, ggp5, ncol = 5)

## -- Calculating Marginal Effects (Direct XGB)--
# F
Mf_matrices <- list(CPB_Direct_XGB_X_F_forecast, CPB_Direct_XGB_F_MAF_forecast, CPB_Direct_XGB_F_MARX_forecast, CPB_Direct_XGB_X_F_MAF_forecast, CPB_Direct_XGB_X_F_MARX_forecast, CPB_Direct_XGB_F_MAF_MARX_forecast, CPB_Direct_XGB_X_F_MAF_MARX_forecast)
F_matrices <- list(CPB_Direct_XGB_X_forecast, CPB_Direct_XGB_MAF_forecast, CPB_Direct_XGB_MARX_forecast, CPB_Direct_XGB_X_MAF_forecast, CPB_Direct_XGB_X_MARX_forecast, CPB_Direct_XGB_MAF_MARX_forecast, CPB_Direct_XGB_X_MAF_MARX_forecast)

CPB_Direct_XGB_F_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

# MAF
Mf_matrices <- list(CPB_Direct_XGB_X_MAF_forecast, CPB_Direct_XGB_F_MAF_forecast, CPB_Direct_XGB_MAF_MARX_forecast, CPB_Direct_XGB_X_F_MAF_forecast, CPB_Direct_XGB_X_MAF_MARX_forecast, CPB_Direct_XGB_F_MAF_MARX_forecast, CPB_Direct_XGB_X_F_MAF_MARX_forecast)
F_matrices <- list(CPB_Direct_XGB_X_forecast, CPB_Direct_XGB_F_forecast, CPB_Direct_XGB_MARX_forecast, CPB_Direct_XGB_X_F_forecast, CPB_Direct_XGB_X_MARX_forecast, CPB_Direct_XGB_F_MARX_forecast, CPB_Direct_XGB_X_F_MARX_forecast)

CPB_Direct_XGB_MAF_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

# MARX
Mf_matrices <- list(CPB_Direct_XGB_X_MARX_forecast, CPB_Direct_XGB_MAF_MARX_forecast, CPB_Direct_XGB_F_MARX_forecast, CPB_Direct_XGB_X_MAF_MARX_forecast, CPB_Direct_XGB_F_MAF_MARX_forecast, CPB_Direct_XGB_X_F_MAF_MARX_forecast, CPB_Direct_XGB_X_F_MARX_forecast)
F_matrices <- list(CPB_Direct_XGB_X_forecast, CPB_Direct_XGB_MAF_forecast, CPB_Direct_XGB_F_forecast, CPB_Direct_XGB_X_MAF_forecast, CPB_Direct_XGB_F_MAF_forecast, CPB_Direct_XGB_X_F_MAF_forecast, CPB_Direct_XGB_X_F_forecast)

CPB_Direct_XGB_MARX_marginal_Rsquared <- marginal_function(horizons, y_real, Mf_matrices, F_matrices)

## -- Plotting Marginal Effects (Direct XGB) --
size_title <- 10
Marginal_allhorizons_XGB_CPB_direct <- data.frame(matrix(ncol = 6, nrow = 3*length(Mf_matrices)))
Marginal_allhorizons_XGB_CPB_direct[,1] <- t(cbind(t(rep("F",length(Mf_matrices))), t(rep("MAF",length(Mf_matrices))), t(rep("MARX",length(Mf_matrices)))))
colnames(Marginal_allhorizons_XGB_CPB_direct) <- c("Feature", "h3", "h6", "h12", "h18", "h24")

for (f in 1:length(horizons)){
  Marginal_allhorizons_XGB_CPB_direct[,f+1] <- t(cbind(t(CPB_Direct_XGB_F_marginal_Rsquared[,f]), t(CPB_Direct_XGB_MAF_marginal_Rsquared[,f]), t(CPB_Direct_XGB_MARX_marginal_Rsquared[,f])))
}
Marginal_allhorizons_XGB_CPB_direct$Feature <- as.factor(Marginal_allhorizons_XGB_CPB_direct$Feature)

# - Grid Arrange -  
ggp1 <- ggplot(Marginal_allhorizons_XGB_CPB_direct, aes(x=Feature, y=h3, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=3")

ggp2 <- ggplot(Marginal_allhorizons_XGB_CPB_direct, aes(x=Feature, y=h6, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=6")

ggp3 <- ggplot(Marginal_allhorizons_XGB_CPB_direct, aes(x=Feature, y=h12, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=12")

ggp4 <- ggplot(Marginal_allhorizons_XGB_CPB_direct, aes(x=Feature, y=h18, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=18")

ggp5 <- ggplot(Marginal_allhorizons_XGB_CPB_direct, aes(x=Feature, y=h24, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=24")

grid.arrange(ggp1, ggp2, ggp3, ggp4, ggp5, ncol = 5)
