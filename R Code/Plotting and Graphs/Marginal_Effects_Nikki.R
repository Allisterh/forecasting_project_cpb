#Clearing Environment
rm(list=ls())

## -- Importing Data --
#Nikki:
df <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/data-eur2023.csv", row.names=1)

# CPB Direct RF Prediction data
CPB_Direct_RF_X_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_X_forecast.csv")
CPB_Direct_RF_F_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_F_forecast.csv")
CPB_Direct_RF_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_MAF_forecast.csv")
CPB_Direct_RF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_MARX_forecast.csv")
CPB_Direct_RF_X_F_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_X_F_forecast.csv")
CPB_Direct_RF_X_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_X_MAF_forecast.csv")
CPB_Direct_RF_X_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_X_MARX_forecast.csv")
CPB_Direct_RF_F_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_F_MAF_forecast.csv")
CPB_Direct_RF_F_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_F_MARX_forecast.csv")
CPB_Direct_RF_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_MAF_MARX_forecast.csv")
CPB_Direct_RF_X_F_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_X_F_MAF_forecast.csv")
CPB_Direct_RF_X_F_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_X_F_MARX_forecast.csv")
CPB_Direct_RF_X_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_X_MAF_MARX_forecast.csv")
CPB_Direct_RF_F_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_F_MAF_MARX_forecast.csv")
CPB_Direct_RF_X_F_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB RF Direct (new)/CPB_Direct_RF_X_F_MAF_MARX_forecast.csv")

# CPB Path Average RF Prediction data
PA_RF_X_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_X_forecast.csv")
PA_RF_F_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_F_forecast.csv")
PA_RF_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_MAF_forecast.csv")
PA_RF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_MARX_forecast.csv")
PA_RF_X_F_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_X_F_forecast.csv")
PA_RF_X_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_X_MAF_forecast.csv")
PA_RF_X_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_X_MARX_forecast.csv")
PA_RF_F_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_F_MAF_forecast.csv")
PA_RF_F_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_F_MARX_forecast.csv")
PA_RF_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_MAF_MARX_forecast.csv")
PA_RF_X_F_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_X_F_MAF_forecast.csv")
PA_RF_X_F_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_X_F_MARX_forecast.csv")
PA_RF_X_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_X_MAF_MARX_forecast.csv")
PA_RF_F_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_F_MAF_MARX_forecast.csv")
PA_RF_X_F_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/RF/CPB_PA_RF_X_F_MAF_MARX_forecast.csv")

# CPB Direct XGB Prediction data
CPB_Direct_BT_X_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_X_forecast.csv")
CPB_Direct_BT_F_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_F_forecast.csv")
CPB_Direct_BT_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_MAF_forecast.csv")
CPB_Direct_BT_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_MARX_forecast.csv")
CPB_Direct_BT_X_F_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_X_F_forecast.csv")
CPB_Direct_BT_X_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_X_MAF_forecast.csv")
CPB_Direct_BT_X_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_X_MARX_forecast.csv")
CPB_Direct_BT_F_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_F_MAF_forecast.csv")
CPB_Direct_BT_F_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_F_MARX_forecast.csv")
CPB_Direct_BT_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_MAF_MARX_forecast.csv")
CPB_Direct_BT_X_F_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_X_F_MAF_forecast.csv")
CPB_Direct_BT_X_F_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_X_F_MARX_forecast.csv")
CPB_Direct_BT_X_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_X_MAF_MARX_forecast.csv")
CPB_Direct_BT_F_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_F_MAF_MARX_forecast.csv")
CPB_Direct_BT_X_F_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/CPB/BT/Direct2/def_CPB_BT_X_F_MAF_MARX_forecast.csv")

### --- Marginal Effects --- 
# - Initializing -
library(ggplot2)
library(gridExtra)   

dutch_forecasts <- c(434,315) #Begin forecasting from 315
start_forecast <- 315
end_forecast <- 434
horizons <- list(3, 6, 12, 18, 24)
Unempl <- df[,2]
n_forecast <- 120 # CPB time window
y_actual <- Unempl[start_forecast:end_forecast]

# - Functions - 
Rsquared_function <- function(y_actual, y_pred){
  e_p <- sum((y_actual - y_pred)^2)
  e_m <- sum((y_actual - mean(y_actual))^2)
  Rsquared <- 1-(e_p/e_m)
  return(Rsquared)
}

marginal_function <- function(horizons, y_actual, Mf_matrices, F_matrices){
  n_marg_combinations <- length(Mf_matrices)
  Rsquared_Mf <- data.frame(matrix(ncol = length(horizons), nrow = n_marg_combinations))
  Rsquared_F <- data.frame(matrix(ncol = length(horizons), nrow = n_marg_combinations))
  
  for (f in 1:n_marg_combinations){
    y_pred_Mf <- Mf_matrices[[f]]
    y_pred_F <- F_matrices[[f]]
    for (h in 1:length(horizons)){
      Rsquared_Mf[f,h] <- Rsquared_function(y_actual, y_pred_Mf[,h])
      Rsquared_F[f,h] <- Rsquared_function(y_actual, y_pred_F[,h])
    }
  }
  Delta_Rsquared <- Rsquared_Mf - Rsquared_F
  return(Delta_Rsquared)
}

## -- Calculating Marginal Effects (Direct RF)--
# X
Mf_matrices <- list(CPB_Direct_RF_X_F_forecast, CPB_Direct_RF_X_MAF_forecast, CPB_Direct_RF_X_MARX_forecast, CPB_Direct_RF_X_F_MAF_forecast, CPB_Direct_RF_X_F_MARX_forecast, CPB_Direct_RF_X_MAF_MARX_forecast, CPB_Direct_RF_X_F_MAF_MARX_forecast)
F_matrices <- list(CPB_Direct_RF_F_forecast, CPB_Direct_RF_MAF_forecast, CPB_Direct_RF_MARX_forecast, CPB_Direct_RF_F_MAF_forecast, CPB_Direct_RF_F_MARX_forecast, CPB_Direct_RF_MAF_MARX_forecast, CPB_Direct_RF_F_MAF_MARX_forecast)

CPB_Direct_RF_X_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# F
Mf_matrices <- list(CPB_Direct_RF_X_F_forecast, CPB_Direct_RF_F_MAF_forecast, CPB_Direct_RF_F_MARX_forecast, CPB_Direct_RF_X_F_MAF_forecast, CPB_Direct_RF_X_F_MARX_forecast, CPB_Direct_RF_F_MAF_MARX_forecast, CPB_Direct_RF_X_F_MAF_MARX_forecast)
F_matrices <- list(CPB_Direct_RF_X_forecast, CPB_Direct_RF_MAF_forecast, CPB_Direct_RF_MARX_forecast, CPB_Direct_RF_X_MAF_forecast, CPB_Direct_RF_X_MARX_forecast, CPB_Direct_RF_MAF_MARX_forecast, CPB_Direct_RF_X_MAF_MARX_forecast)

CPB_Direct_RF_F_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# MAF
Mf_matrices <- list(CPB_Direct_RF_X_MAF_forecast, CPB_Direct_RF_F_MAF_forecast, CPB_Direct_RF_MAF_MARX_forecast, CPB_Direct_RF_X_F_MAF_forecast, CPB_Direct_RF_X_MAF_MARX_forecast, CPB_Direct_RF_F_MAF_MARX_forecast, CPB_Direct_RF_X_F_MAF_MARX_forecast)
F_matrices <- list(CPB_Direct_RF_X_forecast, CPB_Direct_RF_F_forecast, CPB_Direct_RF_MARX_forecast, CPB_Direct_RF_X_F_forecast, CPB_Direct_RF_X_MARX_forecast, CPB_Direct_RF_F_MARX_forecast, CPB_Direct_RF_X_F_MARX_forecast)

CPB_Direct_RF_MAF_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# MARX
Mf_matrices <- list(CPB_Direct_RF_X_MARX_forecast, CPB_Direct_RF_MAF_MARX_forecast, CPB_Direct_RF_F_MARX_forecast, CPB_Direct_RF_X_MAF_MARX_forecast, CPB_Direct_RF_F_MAF_MARX_forecast, CPB_Direct_RF_X_F_MAF_MARX_forecast, CPB_Direct_RF_X_F_MARX_forecast)
F_matrices <- list(CPB_Direct_RF_X_forecast, CPB_Direct_RF_MAF_forecast, CPB_Direct_RF_F_forecast, CPB_Direct_RF_X_MAF_forecast, CPB_Direct_RF_F_MAF_forecast, CPB_Direct_RF_X_F_MAF_forecast, CPB_Direct_RF_X_F_forecast)

CPB_Direct_RF_MARX_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

## -- Plotting Marginal Effects (Direct RF) --
Marginal_allhorizons_RF_CPB_direct <- data.frame(matrix(ncol = 6, nrow = 4*length(Mf_matrices)))
Marginal_allhorizons_RF_CPB_direct[,1] <- t(cbind(t(rep("X",length(Mf_matrices))), t(rep("F",length(Mf_matrices))), t(rep("MAF",length(Mf_matrices))), t(rep("MARX",length(Mf_matrices)))))
colnames(Marginal_allhorizons_RF_CPB_direct) <- c("Feature", "h3", "h6", "h12", "h18", "h24")

for (f in 1:length(horizons)){
  Marginal_allhorizons_RF_CPB_direct[,f+1] <- t(cbind(t(CPB_Direct_RF_X_marginal_Rsquared[,f]), t(CPB_Direct_RF_F_marginal_Rsquared[,f]), t(CPB_Direct_RF_MAF_marginal_Rsquared[,f]), t(CPB_Direct_RF_MARX_marginal_Rsquared[,f])))
}
Marginal_allhorizons_RF_CPB_direct$Feature <- as.factor(Marginal_allhorizons_RF_CPB_direct$Feature)

# - Grid Arrange -  
colors = c("X" = "#4D8076", "F"= "#845EC2", "MAF" = "#00C9A7", "MARX" = "#009EFA")
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
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=24")

grid.arrange(ggp1, ggp2, ggp3, ggp4, ggp5, ncol = 5)

## -- Calculating Marginal Effects (Direct BT)--
# X
Mf_matrices <- list(CPB_Direct_BT_X_F_forecast, CPB_Direct_BT_X_MAF_forecast, CPB_Direct_BT_X_MARX_forecast, CPB_Direct_BT_X_F_MAF_forecast, CPB_Direct_BT_X_F_MARX_forecast, CPB_Direct_BT_X_MAF_MARX_forecast, CPB_Direct_BT_X_F_MAF_MARX_forecast)
F_matrices <- list(CPB_Direct_BT_F_forecast, CPB_Direct_BT_MAF_forecast, CPB_Direct_BT_MARX_forecast, CPB_Direct_BT_F_MAF_forecast, CPB_Direct_BT_F_MARX_forecast, CPB_Direct_BT_MAF_MARX_forecast, CPB_Direct_BT_F_MAF_MARX_forecast)

CPB_Direct_BT_X_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# F
Mf_matrices <- list(CPB_Direct_BT_X_F_forecast, CPB_Direct_BT_F_MAF_forecast, CPB_Direct_BT_F_MARX_forecast, CPB_Direct_BT_X_F_MAF_forecast, CPB_Direct_BT_X_F_MARX_forecast, CPB_Direct_BT_F_MAF_MARX_forecast, CPB_Direct_BT_X_F_MAF_MARX_forecast)
F_matrices <- list(CPB_Direct_BT_X_forecast, CPB_Direct_BT_MAF_forecast, CPB_Direct_BT_MARX_forecast, CPB_Direct_BT_X_MAF_forecast, CPB_Direct_BT_X_MARX_forecast, CPB_Direct_BT_MAF_MARX_forecast, CPB_Direct_BT_X_MAF_MARX_forecast)

CPB_Direct_BT_F_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# MAF
Mf_matrices <- list(CPB_Direct_BT_X_MAF_forecast, CPB_Direct_BT_F_MAF_forecast, CPB_Direct_BT_MAF_MARX_forecast, CPB_Direct_BT_X_F_MAF_forecast, CPB_Direct_BT_X_MAF_MARX_forecast, CPB_Direct_BT_F_MAF_MARX_forecast, CPB_Direct_BT_X_F_MAF_MARX_forecast)
F_matrices <- list(CPB_Direct_BT_X_forecast, CPB_Direct_BT_F_forecast, CPB_Direct_BT_MARX_forecast, CPB_Direct_BT_X_F_forecast, CPB_Direct_BT_X_MARX_forecast, CPB_Direct_BT_F_MARX_forecast, CPB_Direct_BT_X_F_MARX_forecast)

CPB_Direct_BT_MAF_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# MARX
Mf_matrices <- list(CPB_Direct_BT_X_MARX_forecast, CPB_Direct_BT_MAF_MARX_forecast, CPB_Direct_BT_F_MARX_forecast, CPB_Direct_BT_X_MAF_MARX_forecast, CPB_Direct_BT_F_MAF_MARX_forecast, CPB_Direct_BT_X_F_MAF_MARX_forecast, CPB_Direct_BT_X_F_MARX_forecast)
F_matrices <- list(CPB_Direct_BT_X_forecast, CPB_Direct_BT_MAF_forecast, CPB_Direct_BT_F_forecast, CPB_Direct_BT_X_MAF_forecast, CPB_Direct_BT_F_MAF_forecast, CPB_Direct_BT_X_F_MAF_forecast, CPB_Direct_BT_X_F_forecast)

CPB_Direct_BT_MARX_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

## -- Plotting Marginal Effects (Direct BT) --
Marginal_allhorizons_BT_CPB_direct <- data.frame(matrix(ncol = 6, nrow = 4*length(Mf_matrices)))
Marginal_allhorizons_BT_CPB_direct[,1] <- t(cbind(t(rep("X",length(Mf_matrices))), t(rep("F",length(Mf_matrices))), t(rep("MAF",length(Mf_matrices))), t(rep("MARX",length(Mf_matrices)))))
colnames(Marginal_allhorizons_BT_CPB_direct) <- c("Feature", "h3", "h6", "h12", "h18", "h24")

for (f in 1:length(horizons)){
  Marginal_allhorizons_BT_CPB_direct[,f+1] <- t(cbind(t(CPB_Direct_BT_X_marginal_Rsquared[,f]), t(CPB_Direct_BT_F_marginal_Rsquared[,f]), t(CPB_Direct_BT_MAF_marginal_Rsquared[,f]), t(CPB_Direct_BT_MARX_marginal_Rsquared[,f])))
}
Marginal_allhorizons_BT_CPB_direct$Feature <- as.factor(Marginal_allhorizons_BT_CPB_direct$Feature)

# - Grid Arrange -  
ggp1 <- ggplot(Marginal_allhorizons_BT_CPB_direct, aes(x=Feature, y=h3, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=3")

ggp2 <- ggplot(Marginal_allhorizons_BT_CPB_direct, aes(x=Feature, y=h6, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=6")

ggp3 <- ggplot(Marginal_allhorizons_BT_CPB_direct, aes(x=Feature, y=h12, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=12")

ggp4 <- ggplot(Marginal_allhorizons_BT_CPB_direct, aes(x=Feature, y=h18, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=18")

ggp5 <- ggplot(Marginal_allhorizons_BT_CPB_direct, aes(x=Feature, y=h24, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=24")

grid.arrange(ggp1, ggp2, ggp3, ggp4, ggp5, ncol = 5)

## -- Calculating Marginal Effects (Path Average RF)--
PA_RF_X_F_MARX_forecast <-  PA_RF_X_F_MARX_forecast[1:120,]
# X
Mf_matrices <- list(PA_RF_X_F_forecast, PA_RF_X_MAF_forecast, PA_RF_X_MARX_forecast, PA_RF_X_F_MAF_forecast, PA_RF_X_F_MARX_forecast, PA_RF_X_MAF_MARX_forecast, PA_RF_X_F_MAF_MARX_forecast)
F_matrices <- list(PA_RF_F_forecast, PA_RF_MAF_forecast, PA_RF_MARX_forecast, PA_RF_F_MAF_forecast, PA_RF_F_MARX_forecast, PA_RF_MAF_MARX_forecast, PA_RF_F_MAF_MARX_forecast)

PA_RF_X_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# F
Mf_matrices <- list(PA_RF_X_F_forecast, PA_RF_F_MAF_forecast, PA_RF_F_MARX_forecast, PA_RF_X_F_MAF_forecast, PA_RF_X_F_MARX_forecast, PA_RF_F_MAF_MARX_forecast, PA_RF_X_F_MAF_MARX_forecast)
F_matrices <- list(PA_RF_X_forecast, PA_RF_MAF_forecast, PA_RF_MARX_forecast, PA_RF_X_MAF_forecast, PA_RF_X_MARX_forecast, PA_RF_MAF_MARX_forecast, PA_RF_X_MAF_MARX_forecast)

PA_RF_F_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# MAF
Mf_matrices <- list(PA_RF_X_MAF_forecast, PA_RF_F_MAF_forecast, PA_RF_MAF_MARX_forecast, PA_RF_X_F_MAF_forecast, PA_RF_X_MAF_MARX_forecast, PA_RF_F_MAF_MARX_forecast, PA_RF_X_F_MAF_MARX_forecast)
F_matrices <- list(PA_RF_X_forecast, PA_RF_F_forecast, PA_RF_MARX_forecast, PA_RF_X_F_forecast, PA_RF_X_MARX_forecast, PA_RF_F_MARX_forecast, PA_RF_X_F_MARX_forecast)

PA_RF_MAF_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# MARX
Mf_matrices <- list(PA_RF_X_MARX_forecast, PA_RF_MAF_MARX_forecast, PA_RF_F_MARX_forecast, PA_RF_X_MAF_MARX_forecast, PA_RF_F_MAF_MARX_forecast, PA_RF_X_F_MAF_MARX_forecast, PA_RF_X_F_MARX_forecast)
F_matrices <- list(PA_RF_X_forecast, PA_RF_MAF_forecast, PA_RF_F_forecast, PA_RF_X_MAF_forecast, PA_RF_F_MAF_forecast, PA_RF_X_F_MAF_forecast, PA_RF_X_F_forecast)

PA_RF_MARX_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

## -- Plotting Marginal Effects (Path Average RF) --
Marginal_allhorizons_RF_PA <- data.frame(matrix(ncol = 6, nrow = 4*length(Mf_matrices)))
Marginal_allhorizons_RF_PA[,1] <- t(cbind(t(rep("X",length(Mf_matrices))), t(rep("F",length(Mf_matrices))), t(rep("MAF",length(Mf_matrices))), t(rep("MARX",length(Mf_matrices)))))
colnames(Marginal_allhorizons_RF_PA) <- c("Feature", "h3", "h6", "h12", "h18", "h24")

for (f in 1:length(horizons)){
  Marginal_allhorizons_RF_PA[,f+1] <- t(cbind(t(PA_RF_X_marginal_Rsquared[,f]), t(PA_RF_F_marginal_Rsquared[,f]), t(PA_RF_MAF_marginal_Rsquared[,f]), t(PA_RF_MARX_marginal_Rsquared[,f])))
}
Marginal_allhorizons_RF_PA$Feature <- as.factor(Marginal_allhorizons_RF_PA$Feature)

# - Grid Arrange -  
colors = c("X" = "#4D8076", "F"= "#845EC2", "MAF" = "#00C9A7", "MARX" = "#009EFA")
size_title <- 10

ggp1 <- ggplot(Marginal_allhorizons_RF_PA, aes(x=Feature, y=h3, color=Feature)) + geom_boxplot() + 
  coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                                                         axis.text.y=element_blank())+ggtitle("h=3") #+ scale_fill_manual(values=colors)

ggp2 <- ggplot(Marginal_allhorizons_RF_PA, aes(x=Feature, y=h6, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=6")

ggp3 <- ggplot(Marginal_allhorizons_RF_PA, aes(x=Feature, y=h12, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=12")

ggp4 <- ggplot(Marginal_allhorizons_RF_PA, aes(x=Feature, y=h18, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=18")

ggp5 <- ggplot(Marginal_allhorizons_RF_PA, aes(x=Feature, y=h24, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5, size=size_title), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=24")

grid.arrange(ggp1, ggp2, ggp3, ggp4, ggp5, ncol = 5)