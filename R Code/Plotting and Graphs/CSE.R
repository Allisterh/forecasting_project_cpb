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
PA_RF_X_F_MARX_forecast <-  PA_RF_X_F_MARX_forecast[1:120,]

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

### --- PLOTTING CUMULATIVE SQUARED ERROR ---
colors = c("h=3" = "#4D8076", "h=6"= "#845EC2", "h=12" = "#00C9A7", "h=18" = "#009EFA", "h=24" = "black")
pubdate <- df$pub_date
pubdate <- as.Date(pubdate, "%Y-%m-%d")
dutch_forecasts <- c(434,315) #Begin forecasting from 315
start_forecast <- 315
end_forecast <- 434
horizons <- list(3, 6, 12, 18, 24)
Unempl <- df[,2]
n_forecast <- 120 # CPB time window
y_real <- Unempl[start_forecast:end_forecast]

cumulative_sq_error <- function(actual, prediction, n_forecast){
  #CSE_forecast <- data.frame(matrix(ncol=length(horizons), nrow=n_forecast))*0
  CSE_forecast <- data.frame(matrix(ncol=2, nrow=n_forecast))*0  
  for (h in 1:length(horizons)){
  #for (h in 1:2){    
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

## -- RF Direct --
CSE_Direct_RF_X_forecast <- cumulative_sq_error(y_real, CPB_Direct_RF_X_forecast, n_forecast)
CSE_Direct_RF_F_forecast <- cumulative_sq_error(y_real, CPB_Direct_RF_F_forecast, n_forecast)
CSE_Direct_RF_MAF_forecast <- cumulative_sq_error(y_real, CPB_Direct_RF_MAF_forecast, n_forecast)
CSE_Direct_RF_MARX_forecast <- cumulative_sq_error(y_real, CPB_Direct_RF_MARX_forecast, n_forecast)

# X
plot(pubdate[315:434], CSE_Direct_RF_X_forecast[,1], type = "l", frame = FALSE, pch = 19, 
     col = "#4D8076", xlab = "Date", ylab = "CSE", cex.lab = 2)
for (h in 2:length(horizons)){
  lines(pubdate[315:434], CSE_Direct_RF_X_forecast[,h], pch = 18, col = colors[h], type = "l", lty = 1)
}
op <- par(cex = 0.6)
legend("topleft", legend=c("h=3", "h=6", "h=12", "h=18", "h=24"), col=colors, lty = 1)
box(col = "black") 

# F
plot(pubdate[315:434], CSE_Direct_RF_F_forecast[,1], type = "l", frame = FALSE, pch = 19, 
     col = "#4D8076", xlab = "Date", ylab = "CSE", cex.lab = 2)
for (h in 2:length(horizons)){
  lines(pubdate[315:434], CSE_Direct_RF_F_forecast[,h], pch = 18, col = colors[h], type = "l", lty = 1)
}
op <- par(cex = 0.6)
legend("topleft", legend=c("h=3", "h=6", "h=12", "h=18", "h=24"), col=colors, lty = 1)
box(col = "black") 

# MAF
plot(pubdate[315:434], CSE_Direct_RF_MAF_forecast[,1], type = "l", frame = FALSE, pch = 19, 
     col = "#4D8076", xlab = "Date", ylab = "CSE", cex.lab = 2)
for (h in 2:length(horizons)){
  lines(pubdate[315:434], CSE_Direct_RF_MAF_forecast[,h], pch = 18, col = colors[h], type = "l", lty = 1)
}
op <- par(cex = 0.6)
legend("topleft", legend=c("h=3", "h=6", "h=12", "h=18", "h=24"), col=colors, lty = 1)
box(col = "black") 

# MARX
plot(pubdate[315:434], CSE_Direct_RF_MARX_forecast[,1], type = "l", frame = FALSE, pch = 19, 
     col = "#4D8076", xlab = "Date", ylab = "CSE", cex.lab = 2)
for (h in 2:length(horizons)){
  lines(pubdate[315:434], CSE_Direct_RF_MARX_forecast[,h], pch = 18, col = colors[h], type = "l", lty = 1)
}
op <- par(cex = 0.6)
legend("topleft", legend=c("h=3", "h=6", "h=12", "h=18", "h=24"), col=colors, lty = 1)
box(col = "black") 
