### Level forecasting with all the data in Output/Coulombe/Coulombe_XXXXXX
'
df_original <- fredmd("D:/EUR/Master/Seminar Case studies in Applied Econometrics/R code/data122022.csv",transform = FALSE)'
y_real_level <- df_original$UNRATE[256:710]
rmse_rf_x <- sqrt(mean((y_real_level-RF_X_forecast)^2))

RF_X_forecast <- read.csv("C:/Users/Gebruiker/Documents/GitHub/project_cpb/Data en Forecasts/Output/Coulombe/Coulombe_RF_X_forecast.csv")
RF_X_forecast <- RF_X_forecast[,1]

RMSE_RF[1,] <- RMSE_function(y_real_level, RF_X_forecast)
RMSE_RF[2,] <- RMSE_function(y_real_level, RF_F_forecast)
RMSE_RF[3,] <- RMSE_function(y_real_level, RF_MAF_forecast)
RMSE_RF[4,] <- RMSE_function(y_real_level, RF_MARX_forecast)
RMSE_RF[5,] <- RMSE_function(y_real_level, RF_X_F_forecast)
RMSE_RF[6,] <- RMSE_function(y_real_level, RF_X_MAF_forecast)
RMSE_RF[7,] <- RMSE_function(y_real_level, RF_X_MARX_forecast)
RMSE_RF[8,] <- RMSE_function(y_real_level, RF_F_MAF_forecast)
RMSE_RF[9,] <- RMSE_function(y_real_level, RF_F_MARX_forecast)
RMSE_RF[10,] <- RMSE_function(y_real_level, RF_MAF_MARX_forecast)
RMSE_RF[11,] <- RMSE_function(y_real_level, RF_X_F_MAF_forecast)
RMSE_RF[12,] <- RMSE_function(y_real_level, RF_X_F_MARX_forecast)
RMSE_RF[13,] <- RMSE_function(y_real_level, RF_X_MAF_MARX_forecast)
RMSE_RF[14,] <- RMSE_function(y_real_level, RF_F_MAF_MARX_forecast)
RMSE_RF[15,] <- RMSE_function(y_real_level, RF_X_F_MAF_MARX_forecast)