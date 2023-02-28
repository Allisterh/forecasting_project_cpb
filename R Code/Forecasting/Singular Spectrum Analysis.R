# Singular Spectrum Analysis file

library(Rssa)
library(ggplot2)
library(gridExtra)
library(cowplot)
#library(Rfssa)

data = read.csv("D:/EUR/Master/Seminar Case studies in Applied Econometrics/data-eur2023.csv", sep=';',row.names = 1)


plot(data$L1_BRCI, type = 'l')
L = sqrt(nrow(data))
L = 50
L = 120

U = ssa(data$L2_LRHUTTTT,L,neig = 40,kind = "1d-ssa")
X = ssa(data$L1_BRCI,L,kind = "1d-ssa")
Z = ssa(data$L1_BSCI,L,kind = "1d-ssa")
plot(log(U$sigma))

plot(U, main = "BCCI")
plot(X, main = "BRCI")
plot(Z, main = "BSCI")

U_cor = wcor(U)
g <- grouping.auto(U, grouping.method = "wcor", 
                   method = "average", nclust = 3)
newcor = wcor(U, groups = g)
plot(U, groups = g)
rcon = reconstruct(U, groups = g)
plot(rcon)

plot(U, type = "vectors", vectors = "factor", idx = 1:20)

# Unemployment
SSA_un = ssa(data$L2_LRHUTTTT,L,neig = 40,kind = "1d-ssa")
SSA_un_cor = wcor(SSA_un)
pdf("D:/EUR/Master/Seminar Case studies in Applied Econometrics/SSA Graphs/Unemployment_ssa.pdf")
plot(data$L2_LRHUTTTT, type = 'l', main = "Unemployment")
plot(SSA_un, ylab = "Contribution", xlab = "Eigentriple", main = "Unemployment")
plot(SSA_un_cor, scales = list(at = c(10, 20, 30,40)))
plot(SSA_un, type = "vectors", idx = 1:20)
plot(SSA_un, type = "paired", idx = 1:20, plot.contrib = FALSE)
dev.off()

g <- grouping.auto(X, grouping.method = "wcor", 
                   method = "average", nclust = 10)
recon = reconstruct(X, group = g)
recon = reconstruct(X, group = list(1,2,c(4,6),c(3,5)))
plot(recon$'2')


# Producer confidence: Construction
SSA_construction = ssa(data$L1_BCCI,L,neig = 40,kind = "1d-ssa")
SSA_construction_cor = wcor(SSA_construction)
pdf("D:/EUR/Master/Seminar Case studies in Applied Econometrics/SSA Graphs/Construction_ssa.pdf")
plot(data$L1_BCCI, type = 'l', main = "Construction")
plot(SSA_construction, ylab = "Contribution", xlab = "Eigentriple", main = "Construction")
plot(SSA_construction_cor, scales = list(at = c(10, 20, 30,40)))
plot(SSA_construction, type = "vectors", idx = 1:20)
plot(SSA_construction, type = "paired", idx = 1:20, plot.contrib = FALSE)
dev.off()

recon = reconstruct(X, group = list(1,2,3,4,5))
plot(recon)


# Producer confidence: Retail
SSA_Retail = ssa(data$L1_BRCI,L,neig = 40,kind = "1d-ssa")
SSA_Retail_cor = wcor(SSA_Retail)
pdf("D:/EUR/Master/Seminar Case studies in Applied Econometrics/SSA Graphs/Retail_ssa.pdf")
plot(data$L1_BRCI, type = 'l', main = "Retail")
plot(SSA_Retail, ylab = "Contribution", xlab = "Eigentriple", main = "Retail")
plot(SSA_Retail_cor, scales = list(at = c(10, 20, 30,40)))
plot(SSA_Retail, type = "vectors", idx = 1:20)
plot(SSA_Retail, type = "paired", idx = 1:20, plot.contrib = FALSE)
dev.off()

# Producer confidence: Manufacturing
SSA_Manufacturing = ssa(data$L1_BSCI,L,neig = 40,kind = "1d-ssa")
SSA_Manufacturing_cor = wcor(SSA_Manufacturing)
pdf("D:/EUR/Master/Seminar Case studies in Applied Econometrics/SSA Graphs/Manufacturing_ssa.pdf")
plot(data$L1_BSCI, type = 'l', main = "Manufacturing")
plot(SSA_Manufacturing, ylab = "Contribution", xlab = "Eigentriple", main = "Manufacturing")
plot(SSA_Manufacturing_cor, scales = list(at = c(10, 20, 30,40)))
plot(SSA_Manufacturing, type = "vectors", idx = 1:20)
plot(SSA_Manufacturing, type = "paired", idx = 1:20, plot.contrib = FALSE)
dev.off()

# Consumer confidence
SSA_ConsConf = ssa(data$L1_CSCICP02,L,neig = 40,kind = "1d-ssa")
SSA_ConsConf_cor = wcor(SSA_ConsConf)
pdf("D:/EUR/Master/Seminar Case studies in Applied Econometrics/SSA Graphs/Consumer_Confidence_ssa.pdf")
plot(data$L1_CSCICP02, type = 'l', main = "Consumer Confidence")
plot(SSA_ConsConf, ylab = "Contribution", xlab = "Eigentriple", main = "Consumer Confidence")
plot(SSA_ConsConf_cor, scales = list(at = c(10, 20, 30,40)))
plot(SSA_ConsConf, type = "vectors", idx = 1:20)
plot(SSA_ConsConf, type = "paired", idx = 1:20, plot.contrib = FALSE)
dev.off()

# Export 
SSA_Export = ssa(data$L3_XTEXVA01,L,neig = 40,kind = "1d-ssa")
SSA_Export_cor = wcor(SSA_Export)
pdf("D:/EUR/Master/Seminar Case studies in Applied Econometrics/SSA Graphs/Export_ssa.pdf")
plot(data$L3_XTEXVA01, type = 'l', main = "Export")
plot(SSA_Export, ylab = "Contribution", xlab = "Eigentriple", main = "Export")
plot(SSA_Export_cor, scales = list(at = c(10, 20, 30,40)))
plot(SSA_Export, type = "vectors", idx = 1:20)
plot(SSA_Export, type = "paired", idx = 1:20, plot.contrib = FALSE)
dev.off()

# Import
SSA_Import = ssa(data$L3_XTIMVA01,L,neig = 40,kind = "1d-ssa")
SSA_Import_cor = wcor(SSA_Import)
pdf("D:/EUR/Master/Seminar Case studies in Applied Econometrics/SSA Graphs/Import_ssa.pdf")
plot(data$L3_XTIMVA01, type = 'l', main = "Import")
plot(SSA_Import, ylab = "Contribution", xlab = "Eigentriple", main = "Import")
plot(SSA_Import_cor, scales = list(at = c(10, 20, 30,40)))
plot(SSA_Import, type = "vectors", idx = 1:20)
plot(SSA_Import, type = "paired", idx = 1:20, plot.contrib = FALSE)
dev.off()

# AEX
SSA_AEX = ssa(data$L1_AEX,L,neig = 40,kind = "1d-ssa")
SSA_AEX_cor = wcor(SSA_AEX)
pdf("D:/EUR/Master/Seminar Case studies in Applied Econometrics/SSA Graphs/AEX_ssa.pdf")
plot(data$L1_AEX, type = 'l', main = "AEX")
plot(SSA_AEX, ylab = "Contribution", xlab = "Eigentriple", main = "AEX")
plot(SSA_AEX_cor, scales = list(at = c(10, 20, 30,40)))
plot(SSA_AEX, type = "vectors", idx = 1:20)
plot(SSA_AEX, type = "paired", idx = 1:20, plot.contrib = FALSE)
dev.off()

# AEX Volatility
SSA_AEXVOL = ssa(data$L1_AEXVLT,L,neig = 40,kind = "1d-ssa")
SSA_AEXVOL_cor = wcor(SSA_AEXVOL)
pdf("D:/EUR/Master/Seminar Case studies in Applied Econometrics/SSA Graphs/AEXVOL_ssa.pdf")
plot(data$L1_AEXVLT, type = 'l', main = "AEXVOL")
plot(SSA_AEXVOL, ylab = "Contribution", xlab = "Eigentriple", main = "AEXVOL")
plot(SSA_AEXVOL_cor, scales = list(at = c(10, 20, 30,40)))
plot(SSA_AEXVOL, type = "vectors", idx = 1:20)
plot(SSA_AEXVOL, type = "paired", idx = 1:20, plot.contrib = FALSE)
dev.off()

SSA_transformation = function(df,nrgroups,L,automaticgrouping = TRUE) { # Dates need to be the first column
  ssa_transformed <- df[,1]
  for (i in 2:ncol(df)) {
    Y <- ssa(df[,i],L,neig = 50 ,kind = "1d-ssa")
    g <- grouping.auto(Y, grouping.method = "wcor", method = "average", nclust = nrgroups)
    recon = reconstruct(Y, groups = g)
    for (j in 1:length(g)) {
      newdf = data.frame(recon[j])
      colnames(newdf) = paste('SSA',colnames(df)[i],j,sep='_')
      ssa_transformed = cbind(ssa_transformed,newdf)
    }
    # print(paste("Total variance of group", colnames(df)[i],":",(sum(Y$sigma[g[[1]]])+sum(Y$sigma[g[[2]]])+sum(Y$sigma[g[[3]]]))/sum(Y$sigma),sep = ' '))
  }
  return(ssa_transformed)
}

sum(U$sigma[1:3])
g <- grouping.auto(X, grouping.method = "wcor", method = "average", nclust = 15)
for (i in 1:15) {print(g[[i]])}

for (k in 0:119){
  testeru = SSA_transformation(data[1:315+k,],10,L)
  if (k==0) {
    newset = testeru
  } else {
    newset = rbind(newset, testeru[315+k,])
    }
}
testeru = SSA_transformation(data[1:316,],10,L)

# compare with principal components
varlag = as.data.frame(shift(data$L2_LRHUTTTT,n=0:12, type = 'lag', give.names=TRUE))
pcacheck <- prcomp(na.omit(varlag), center = TRUE, scale. = TRUE)$x
par(mfrow = c(2,2))
plot(pcacheck[,1], type = 'l')
plot(testeru$SSA_L2_LRHUTTTT_1, type = 'l')
plot(testeru$SSA_L2_LRHUTTTT_2, type = 'l')
plot(rcon[1:315,])

for (i in 2:ncol(data)) {
U = ssa(data[1:315,i],L,neig = 50 ,kind = "1d-ssa")
g <-  
recon = reconstruct(U, groups = list(1, 2, c(3, 4), c(5,6)))
plot(recon,
     type = c("raw", "cumsum"),
     plot.method = c("native", "matplot", "xyplot"),
     base.series = NULL,
     add.original = TRUE,
     add.residuals = TRUE)
}


