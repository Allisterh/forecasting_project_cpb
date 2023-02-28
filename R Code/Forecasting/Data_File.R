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

source("Function_File.r")

## ---- DATA ---- CBS

# CPB Data
df <- read.csv("data-eur2023.csv", row.names = 1)

# Inflation CPI year-on-year 
inflation <- read.csv("Extra data/inflation.csv", sep = ';')
inflation <- inflation[278:711,3]

df <- cbind(df,inflation)
rm(inflation)

## ---- Additional Data ----

# Faillisementen
faillisementen <- read.csv("Extra data/faillisementen.csv", sep = ';')
faillisementen <- faillisementen[111:495,4]

# Cost of building, starts at 1990
buildingcost <- read.csv("Extra data/bouwkosten.csv", sep = ';')
buildingcost <- buildingcost[3:387,-c(1,2,3,4,5,6,8)]

# Building permits
buildpermits1 <- read.csv("Extra data/Bouwvergunning19902016.csv", sep = ';')
buildpermits1 <- buildpermits1[,-c(1,2,3)] # to compare with other choose rows 265:324
colnames(buildpermits1)[2] = c("BouwVerg")

buildpermits2 <- read.csv("Extra data/Bouwvergunning20122022.csv", sep = ';')
buildpermits2 <- buildpermits2[,c(4,6)] # same 1:60
colnames(buildpermits2)[2] = c("BouwVerg")

buildpermits<- append(buildpermits1[3:324,2],buildpermits2[61:123,2]) # Starts at 1990
rm(buildpermits1,buildpermits2)

# -- Additional data of stocks -- We need to check alignment of the data

# German stocks, begint vanaf 1988
dax <- read.csv("Extra data/DAX.csv", row.names = 1)
dax <- log(dax[27:411,-c(1,2,3,5,6)])

# French stocks, begint van 1990
cac40 <- read.csv("Extra data/FCHI.csv", row.names = 1)
cac40 <- log(cac40[1:385,-c(1,2,3,5,6)])

# FTSE London, vanaf altijd, yahoo Finance
ftse100 <- Index$new('^FTSE')
ftse <- ftse100$get_history(interval = "1mo", start = "1986-02-01", end = "2022-04-01")[,c(1,6)] # if only close price, replace c(1,6) with 6
ftse$date <- format(ftse$date,format = "%Y-%m-%d")
ftse <- log(ftse[50:434,2])

# SP500, vanaf altijd, yahoo Finance
spindex <- Index$new('^GSPC')
sp500 <- spindex$get_history(interval = "1mo", start = "1986-02-01", end = "2022-04-01")[,c(1,6)] # if only close price, replace c(1,6) with 6
sp500$date <- format(sp500$date,format = "%Y-%m-%d")
sp500 <- log(sp500[50:434,2])

logstocks <- as.data.frame(cbind(df$L1_AEX[50:434],dax,cac40,ftse,sp500))

ggplot(logstocks, aes(x=1:385))+
  geom_line(aes(y=V1), colour = 'orange')+
  geom_line(aes(y=dax), colour = 'red')+
  geom_line(aes(y=cac40), colour = 'blue')+
  geom_line(aes(y=ftse), colour = 'black')+
  geom_line(aes(y=sp500), colour = 'green')+
  theme_classic()

rm(ftse100,spindex,dax,cac40,ftse,sp500)

# ECB financial stress index, European Central Bank
fin_stress <- read.csv("Extra data/fin_stress.csv")
fin_stress <- rev(fin_stress[9:393,2])
plot(x=1:385,y=fin_stress, type = 'l')

# -- Commodity prices (Gas, Oil, Gold) -- World Bank
commodities <- read.csv("Extra data/commodities2.csv", sep = ';')
commodities <- commodities[363:747,2:4]
commodities[,2:3] <- log(commodities[,2:3])

# Interest rates, source OECD
interestshort <- read.csv("Extra data/interestratesshort.csv", sep = ',')
interestshort <- dplyr::filter(interestshort, LOCATION %in% c("NLD"))
interestshort <- interestshort[55:439,6:7]
interestlong <- read.csv("Extra data/interestrateslong.csv", sep = ',')
interestlong <- dplyr::filter(interestlong, LOCATION %in% c("NLD"))
interestlong <- interestlong[50:434,6:7]
interest <- cbind(interestshort[,2], interestlong[,2])
colnames(interest) <- c("Short_interest","Long_interest")

rm(interestshort,interestlong)

# Combine data
df_additional <- cbind(buildingcost,buildpermits,faillisementen,logstocks[,2:5],fin_stress,commodities,interest)

# -- transformations -- 

# Check if (additional) data stationary
df_core <- make.stationary(df[,-1])
df_addstat <- make.stationary(df_additional[,-1])

check.stationary(df_core)
check.stationary(df_addstat)

write.csv(df_core, "Extra data/data_core.csv", row.names=FALSE)
write.csv(df_additional, "Extra data/data_additional.csv", row.names=FALSE)

df_original <- df
#df_full <- cbind(df_core[49:433,],)

rm(logstocks,interest,commodities,df,buildingcost,buildpermits,faillisementen,fin_stress)
