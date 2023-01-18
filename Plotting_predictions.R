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

# Cumulative Squared Error plots







