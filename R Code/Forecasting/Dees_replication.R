# Dees forecasting replication Coulombe
library(BVAR)
library(randomForest)
library(vars)
library(fbi)
library(gsubfn)
library(flexmix)

#Nikki:
#df <- read.csv("~/Documents/MSc Econometrics/Blok 3/Seminar/R code/data-eur2023.csv", row.names=1)

# Dees:
df <- fredmd("D:/EUR/Master/Seminar Case studies in Applied Econometrics/R code/data122022.csv")
row.names(df) <- df[,1]
unemployment <- as.data.frame(df$UNRATE)
unemployment <- as.data.frame(unemployment[3:765,])
row.names(unemployment) <- row.names(df)[3:765]

df_wo_unrate <- df[-c(1,25) ]
df_wo_unrate <- df_wo_unrate[3:765,] # Delete first two rows because we transform and some do second differencign
# Delete columns with NA values/that do not have data from 1960 onwards
new_df <- df_wo_unrate[ , colSums(is.na(df_wo_unrate))==0]
scaled_df <- as.data.frame(scale(new_df))

### ---- FEATURE ENGINEERING ----

source("Function Feature Matrix.R")

X_lags <- 12
n_Factors <- 3
F_lags <- 12
P_MAF <- 12
n_MAF <- 3
P_MARX <- 12
regressor_matrix <- new_df

X_with_lags <- X_function(regressor_matrix, X_lags)

scaled_regressor_matrix <- scale(regressor_matrix) #Scale Regressor Matrix for PCA in F
F <- F_function(scaled_regressor_matrix, n_Factors, F_lags)

scaled_x_lags <- scale(X_with_lags) #Scale lagged X Matrix for MAF
MAF <- MAF_function2(scaled_x_lags,X_lags,nrow(scaled_x_lags),ncol(regressor_matrix), P_MAF, n_MAF, FALSE)

MARX <- MARX_function(regressor_matrix, P_MARX)

X_F <- cbind(X_with_lags, F)
X_MAF <- cbind(X_with_lags, MAF)
X_MARX <- cbind(X_with_lags, MARX)
F_MAF <- cbind(F, MAF)
F_MARX <- cbind(F, MARX)
MAF_MARX <- cbind(MAF, MARX)
X_F_MAF <- cbind(X_with_lags, F, MAF)
X_F_MARX <- cbind(X_with_lags, F, MARX)
X_MAF_MARX <- cbind(X_with_lags, MAF, MARX)
F_MAF_MARX <- cbind(F, MAF, MARX)

#list[X_F,X_MAF,X_MARX,F_MAF,F_MARX,MAF_MARX,X_F_MAF,X_F_MARX,X_MAF_MARX,F_MAF_MARX,X_F_MAF_MARX] <- Create_all_feature_matrices(new_df)

### ---- FORECASTING ----
# -- Forecasting Function -- 

Unempl <- unemployment
rownames(Unempl) <- as.data.frame(rownames(unemployment))
y_real <- unemployment[251:762,]
horizons <- list(3) #, 6, 12, 18, 24
n_forecast <- 706-251 # Coulombe time window frame 1980M1 - 2017M12


### ---- Factor Model ----

# - Optimization of factor model with BIC -

  for (h in c(1,3,6,9,12,24)){
    old_BIC <- 0
    new_BIC <- 0
    for (nr_fac in 1:8) { # loop over nr of factors
      for (f in 1:12) { # loop over number of lags for F
        for (k in 1:12) { # loop over number of lags for y
            shift_unempl <- as.data.frame(shift(unemployment,n=h, type = 'lead', give.names=TRUE))
            p_y <- as.data.frame(shift(unemployment,n=1:k, type = 'lag', give.names=TRUE))
            p_f <- as.data.frame(shift(F_Factor_Model[,1:nr_fac],n=0:f, type = 'lag', give.names=TRUE))
            colnames(p_y)[1]= "Unemployment"
            loop_data <- cbind(shift_unempl,p_y,p_f)
            if(f==1 && k == 1 & nr_fac == 1) {
              old_BIC <- BIC(lm(formula = Unemployment~.,data = loop_data[10:763,]))
              best_params = c(f,k)
            } else { 
              new_BIC <- BIC(lm(formula = Unemployment~.,data = loop_data[10:763,]))
              if (new_BIC < old_BIC){
                old_BIC <- new_BIC
                best_params = c(f,k)
              }
            }
        }
      }
      
    }
    print(paste("H = ",h,"# factors included: ", nr_fac, ": lags for factors: ", best_params[1], " and lags of y: ", best_params[2]))
}

# - Actual Factor Model -
F_Factor_Model <- F_function(scaled_regressor_matrix, 8, F_lags)
shift_unempl <- as.data.frame(shift(unemployment,n=12, type = 'lead', give.names=TRUE))
lags_unempl <- as.data.frame(shift(unemployment,n=1:8, type = 'lag', give.names=TRUE))
lags_factors <- as.data.frame(shift(F_Factor_Model[,1:3],n=0:3, type = 'lag', give.names=TRUE))
FM_model_data <- cbind(shift_unempl,lags_unempl,lags_factors)
colnames(FM_model_data)[1] = "Unemployment"
model <- lm(formula = Unemployment~.,data = FM_model_data)

FM_model_data <- cbind(shift_unempl,lags_unempl[,1:4],lags_factors)
colnames(FM_model_data)[1] = "Unemployment"
model1 <- lm(formula = Unemployment~.,data = FM_model_data)
BIC(model1)
FM_model_data <- cbind(shift_unempl,lags_unempl[,1],lags_factors[,1])
colnames(FM_model_data)[1] = "Unemployment"
model2 <- lm(formula = Unemployment~.,data = FM_model_data)
BIC(model2)

Unempl <- unemployment
rownames(Unempl) <- as.data.frame(rownames(unemployment))
y_real <- unemployment[251:706,]
horizons <- list(1,3,6,9,12,24) #, 6, 12, 18, 24
n_forecast <- 706-250 # Coulombe time window frame 1980M1 - 2017M12

FM_y_forecast <- data.frame(matrix(ncol = length(horizons), nrow = n_forecast))

i <- 1
for (h in horizons) {
  shift_unempl <- as.data.frame(shift(unemployment,n=h, type = 'lead', give.names=TRUE))
  lags_unempl <- as.data.frame(shift(unemployment,n=1:4, type = 'lag', give.names=TRUE))
  lags_factors <- as.data.frame(shift(F_Factor_Model[,1:3],n=0:3, type = 'lag', give.names=TRUE))
  y_z <- cbind(shift_unempl,lags_unempl,lags_factors)
  colnames(y_z)[1] = 'y'
  
  for (f in 1:n_forecast) {
    y_Z_train <- y_z[11:250+f-1,]
    y_Z_test <- y_z[250+f,]
    model <- lm(formula = y~.,data = y_Z_train)
    FM_y_forecast[f,i] <- predict(model,y_Z_test)
  }
  colnames(FM_y_forecast)[i]=paste('h=',h,sep='')
  i <- i + 1
  print(h)
}

rmse_fm_h1 <- RMSE_function(y_real, FM_y_forecast[,1])



RMSE_function <- function(actual, prediction){
  RMSE <- data.frame(matrix(ncol = length(horizons), nrow = 1))
  i <- 0
  for (h in horizons){
    i <- i+1
    RMSE[1,i] <- sqrt(mean((actual - prediction[,i])^2))
    colnames(RMSE)[i]=paste('h=',h,sep='')
  }
  return(RMSE)
}

write.csv(RMSE_RF, "C:/Users/Gebruiker/Documents/GitHub/project_cpb/Data en Forecasts/Output/Coulombe/rmse_rf.csv", row.names=TRUE)

