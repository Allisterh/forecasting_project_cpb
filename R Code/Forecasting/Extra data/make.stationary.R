rm(list=ls())

library(BVAR)
library(randomForest)
library(vars)
library(data.table)
library(tseries)
library(xgboost)
library(caret)
library(tidyverse)	
library(cbsodataR)
library(lubridate)
library(dplyr)
library(yahoofinancer)

## ---- DATA ---- CBS

# CPB Data
df <- read.csv("/Users/vincentvanpul/Desktop/Seminar\ Case\ Studies/data-eur2023.csv", row.names = 1)
df <- df[,-1]

make.stationary <- function(df){
  df_2 = data.frame(matrix(nrow = nrow(df)-1, ncol = ncol(df))) 
  for (i in 1:ncol(df)) {
    if (adf.test(df[,i])$p.value > 0.05){
      df_2[,i] <- diff(df[,i],differences=1)
    }
    else {
      df_2[,i] <- df[-1,i]
    }
  }
  colnames(df_2) <- colnames(df)
  return(df_2)
}

for (i in 1:ncol(df)) {
  print(colnames(df[,i]))
  if (adf.test(df[,i])$p.value > 0.05){
    print("non-stationary")
  }
  else {
    print("stationary")
  }
}


stationary.data <- make.stationary(df)
