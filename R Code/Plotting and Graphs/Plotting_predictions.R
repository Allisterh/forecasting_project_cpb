#Clearing Environment
#rm(list=ls())

## -- Importing Data --
#Nikki:
df <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/data-eur2023.csv", row.names=1)

# Prediction data
RF_X_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_X_forecast.csv")
RF_F_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_F_forecast.csv")
RF_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_MAF_forecast.csv")
RF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_MARX_forecast.csv")
RF_X_F_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_X_F_forecast.csv")
RF_X_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_X_MAF_forecast.csv")
RF_X_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_X_MARX_forecast.csv")
RF_F_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_F_MAF_forecast.csv")
RF_F_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_F_MARX_forecast.csv")
RF_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_MAF_MARX_forecast.csv")
RF_X_F_MAF_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_X_F_MAF_forecast.csv")
RF_X_F_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_X_F_MARX_forecast.csv")
RF_X_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_X_MAF_MARX_forecast.csv")
RF_F_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_F_MAF_MARX_forecast.csv")
RF_X_F_MAF_MARX_forecast <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/RF_X_F_MAF_MARX_forecast.csv")

## --- Random Forest ---
# -- Plotting Predictions --
Unempl <- df[c(2)] 
T <- length(Unempl)
n_forecast <- 434-315
NaNrows <- data.frame(seq(1,T-n_forecast, 1)*NA)
names(NaNrows) <- "h=3"
time <- seq(1, T, 1)
pubdate <- df$pub_date
pubdate <- as.Date(pubdate, "%Y-%m-%d")

# h=3
Plot_RF_X_F <- data.frame(RF_X_F_forecast$`h=3`)
names(Plot_RF_X_F) <- "h=3"
Plot_RF_X_F <- rbind(NaNrows, Plot_RF_X_F)

Plot_RF_F_MARX <- data.frame(RF_F_MARX_forecast$`h=3`)
names(Plot_RF_F_MARX) <- "h=3"
Plot_RF_F_MARX <- rbind(NaNrows, Plot_RF_F_MARX)

Plot_RF_X_F_MARX <- data.frame(RF_X_F_MARX_forecast$`h=3`)
names(Plot_RF_X_F_MARX) <- "h=3"
Plot_RF_X_F_MARX <- rbind(NaNrows, Plot_RF_X_F_MARX)

# Plot h=3
plot(pubdate[300:434], Unempl[300:434,], type = "l", frame = FALSE, pch = 19, 
     col = "black", xlab = "Date", ylab = "Unemployment Rate", cex.lab = 2)
lines(pubdate[300:434], Plot_RF_X_F[300:434,], pch = 18, col = "blue", type = "l", lty = 1)
lines(pubdate[300:434], Plot_RF_F_MARX[300:434,], pch = 18, col = "red", type = "l", lty = 1)
lines(pubdate[300:434], Plot_RF_X_F_MARX[300:434,], pch = 18, col = "green", type = "l", lty = 1)
abline(v = pubdate[316], lty=2)
op <- par(cex = 0.6)
legend("topright", legend=c("Unemployment", "X+F", "F+MARX", "X+F+MARX"),
       col=c("black", "blue", "red", "green"), lty = 1)
box(col = "black") 

## -- Plotting marginal effects -- 
Rsquared_function <- function(y_actual, y_pred){
  e_p <- sum((y_actual - y_pred)^2)
  e_m <- sum((y_actual - mean(y_actual))^2)
  Rsquared <- 1-e_p/e_m
  return(Rsquared)
}

marginal_function <- function(horizons, y_actual, Mf_matrices, F_matrices){
  n_marg_combinations <- length(Mf_matrices)
  Rsquared_Mf <- data.frame(matrix(ncol = length(horizons), n_marg_combinations))
  Rsquared_F <- data.frame(matrix(ncol = length(horizons), n_marg_combinations))
  
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

horizons <- list(3, 6, 12, 18, 24)
y_actual <- Unempl[316:434,]

# X
Mf_matrices <- list(RF_X_F_forecast, RF_X_MAF_forecast, RF_X_MARX_forecast, RF_X_F_MAF_forecast, RF_X_F_MARX_forecast, RF_X_MAF_MARX_forecast, RF_X_F_MAF_MARX_forecast)
F_matrices <- list(RF_F_forecast, RF_MAF_forecast, RF_MARX_forecast, RF_F_MAF_forecast, RF_F_MARX_forecast, RF_MAF_MARX_forecast, RF_F_MAF_MARX_forecast)

X_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# F
Mf_matrices <- list(RF_X_F_forecast, RF_F_MAF_forecast, RF_F_MARX_forecast, RF_X_F_MAF_forecast, RF_X_F_MARX_forecast, RF_F_MAF_MARX_forecast, RF_X_F_MAF_MARX_forecast)
F_matrices <- list(RF_X_forecast, RF_MAF_forecast, RF_MARX_forecast, RF_X_MAF_forecast, RF_X_MARX_forecast, RF_MAF_MARX_forecast, RF_X_MAF_MARX_forecast)

F_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# MAF
Mf_matrices <- list(RF_X_MAF_forecast, RF_F_MAF_forecast, RF_MAF_MARX_forecast, RF_X_F_MAF_forecast, RF_X_MAF_MARX_forecast, RF_F_MAF_MARX_forecast, RF_X_F_MAF_MARX_forecast)
F_matrices <- list(RF_X_forecast, RF_F_forecast, RF_MARX_forecast, RF_X_F_forecast, RF_X_MARX_forecast, RF_F_MARX_forecast, RF_X_F_MARX_forecast)

MAF_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# MARX
Mf_matrices <- list(RF_X_MARX_forecast, RF_MAF_MARX_forecast, RF_F_MARX_forecast, RF_X_MAF_MARX_forecast, RF_F_MAF_MARX_forecast, RF_X_F_MAF_MARX_forecast, RF_X_F_MARX_forecast)
F_matrices <- list(RF_X_forecast, RF_MAF_forecast, RF_F_forecast, RF_X_MAF_forecast, RF_F_MAF_forecast, RF_X_F_MAF_forecast, RF_X_F_forecast)

MARX_marginal_Rsquared <- marginal_function(horizons, y_actual, Mf_matrices, F_matrices)

# Plotting
library(ggplot2)

h3 <- data.frame(matrix(ncol = 2, 4*7))
h3[1:7,] <- cbind(X_marginal_Rsquared[,1], X_marginal_Rsquared[,1]/X_marginal_Rsquared[,1])
h3[8:14,] <- cbind(F_marginal_Rsquared[,1], X_marginal_Rsquared[,1]/X_marginal_Rsquared[,1]*2)
h3[15:21,] <- cbind(MAF_marginal_Rsquared[,1], X_marginal_Rsquared[,1]/X_marginal_Rsquared[,1]*3)
h3[22:28,] <- cbind(MARX_marginal_Rsquared[,1], X_marginal_Rsquared[,1]/X_marginal_Rsquared[,1]*4)
colnames(h3) <- c("Marginal_R2", "Feature")
for (i in 1:(4*7)){
  h3[i,2] <-ifelse(h3[i,2]==1,"X",ifelse(h3[i,2]==2,"F",ifelse(h3[i,2]==3,"MAF","MARX")))
}
h3$Feature <- as.factor(h3$Feature)

h6 <- data.frame(matrix(ncol = 2, 4*7))
h6[1:7,] <- cbind(X_marginal_Rsquared[,2], X_marginal_Rsquared[,2]/X_marginal_Rsquared[,2])
h6[8:14,] <- cbind(F_marginal_Rsquared[,2], X_marginal_Rsquared[,2]/X_marginal_Rsquared[,2]*2)
h6[15:21,] <- cbind(MAF_marginal_Rsquared[,2], X_marginal_Rsquared[,2]/X_marginal_Rsquared[,2]*3)
h6[22:28,] <- cbind(MARX_marginal_Rsquared[,2], X_marginal_Rsquared[,2]/X_marginal_Rsquared[,2]*4)
colnames(h6) <- c("Marginal_R2", "Feature")
for (i in 1:(4*7)){
  h6[i,2] <-ifelse(h6[i,2]==1,"X",ifelse(h6[i,2]==2,"F",ifelse(h6[i,2]==3,"MAF","MARX")))
}
h6$Feature <- as.factor(h6$Feature)

h12 <- data.frame(matrix(ncol = 2, 4*7))
h12[1:7,] <- cbind(X_marginal_Rsquared[,3], X_marginal_Rsquared[,3]/X_marginal_Rsquared[,3])
h12[8:14,] <- cbind(F_marginal_Rsquared[,3], X_marginal_Rsquared[,3]/X_marginal_Rsquared[,3]*2)
h12[15:21,] <- cbind(MAF_marginal_Rsquared[,3], X_marginal_Rsquared[,3]/X_marginal_Rsquared[,3]*3)
h12[22:28,] <- cbind(MARX_marginal_Rsquared[,3], X_marginal_Rsquared[,3]/X_marginal_Rsquared[,3]*4)
colnames(h12) <- c("Marginal_R2", "Feature")
for (i in 1:(4*7)){
  h12[i,2] <-ifelse(h12[i,2]==1,"X",ifelse(h12[i,2]==2,"F",ifelse(h12[i,2]==3,"MAF","MARX")))
}
h12$Feature <- as.factor(h12$Feature)

h18 <- data.frame(matrix(ncol = 2, 4*7))
h18[1:7,] <- cbind(X_marginal_Rsquared[,4], X_marginal_Rsquared[,4]/X_marginal_Rsquared[,4])
h18[8:14,] <- cbind(F_marginal_Rsquared[,4], X_marginal_Rsquared[,4]/X_marginal_Rsquared[,4]*2)
h18[15:21,] <- cbind(MAF_marginal_Rsquared[,4], X_marginal_Rsquared[,4]/X_marginal_Rsquared[,4]*3)
h18[22:28,] <- cbind(MARX_marginal_Rsquared[,4], X_marginal_Rsquared[,4]/X_marginal_Rsquared[,4]*4)
colnames(h18) <- c("Marginal_R2", "Feature")
for (i in 1:(4*7)){
  h18[i,2] <-ifelse(h18[i,2]==1,"X",ifelse(h18[i,2]==2,"F",ifelse(h18[i,2]==3,"MAF","MARX")))
}
h18$Feature <- as.factor(h18$Feature)

h24 <- data.frame(matrix(ncol = 2, 4*7))
h24[1:7,] <- cbind(X_marginal_Rsquared[,5], X_marginal_Rsquared[,5]/X_marginal_Rsquared[,5])
h24[8:14,] <- cbind(F_marginal_Rsquared[,5], X_marginal_Rsquared[,5]/X_marginal_Rsquared[,5]*2)
h24[15:21,] <- cbind(MAF_marginal_Rsquared[,5], X_marginal_Rsquared[,5]/X_marginal_Rsquared[,5]*3)
h24[22:28,] <- cbind(MARX_marginal_Rsquared[,5], X_marginal_Rsquared[,5]/X_marginal_Rsquared[,5]*4)
colnames(h24) <- c("Marginal_R2", "Feature")
for (i in 1:(4*7)){
  h24[i,2] <-ifelse(h24[i,2]==1,"X",ifelse(h24[i,2]==2,"F",ifelse(h24[i,2]==3,"MAF","MARX")))
}
h24$Feature <- as.factor(h24$Feature)

install.packages("gridExtra")               
library("gridExtra")   

ggp1 <- ggplot(h3, aes(x=Feature, y=Marginal_R2, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=3")

ggp2 <- ggplot(h6, aes(x=Feature, y=Marginal_R2, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=6")

ggp3 <- ggplot(h12, aes(x=Feature, y=Marginal_R2, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=12")

ggp4 <- ggplot(h18, aes(x=Feature, y=Marginal_R2, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=18")

ggp5 <- ggplot(h24, aes(x=Feature, y=Marginal_R2, color=Feature)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() + theme(legend.position = "none") + theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank())+ggtitle("h=24")

grid.arrange(ggp1, ggp2, ggp3, ggp4, ggp5, ncol = 5)


### --- PLOTTING CUMULATIVE SQUARED ERROR ---
n_forecast <- 119
pubdate <- df$pub_date
pubdate <- as.Date(pubdate, "%Y-%m-%d")

cumulative_sq_error <- function(actual, prediction, n_forecast){
  #CSE_forecast <- data.frame(matrix(ncol=length(horizons), nrow=n_forecast))*0
  CSE_forecast <- data.frame(matrix(ncol=2, nrow=n_forecast))*0  
  #for (h in 1:length(horizons)){
  for (h in 1:2){    
    for (t in 1:n_forecast){
      if (t==1){
        CSE_forecast[t,h] <- (actual[t] - prediction[t,h])^2
      } else {
      CSE_forecast[t,h] <- CSE_forecast[t-1,h] + (actual[t] - prediction[t,h])^2
      }
    }
  }
  return(CSE_forecast)
}

CSE_PA_RF_X_forecast <- cumulative_sq_error(y_real, PA_RF_X_forecast, n_forecast)

# Plot h=3 and h=6
plot(pubdate[316:434], CSE_PA_RF_X_forecast[,1], type = "l", frame = FALSE, pch = 19, 
     col = "black", xlab = "Date", ylab = "CSE", cex.lab = 2)
lines(pubdate[316:434], CSE_PA_RF_X_forecast[,2], pch = 18, col = "blue", type = "l", lty = 1)
op <- par(cex = 0.6)
legend("topleft", legend=c("h=3", "h=6"),
       col=c("black", "blue"), lty = 1)
box(col = "black") 
