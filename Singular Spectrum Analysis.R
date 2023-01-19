# Singular Spectrum Analysis file

library(Rssa)
library(ggplot2)
library(gridExtra)
library(cowplot)
#library(Rfssa)

data = read.csv("D:/EUR/Master/Seminar Case studies in Applied Econometrics/data-eur2023.csv", sep=';',row.names = 1)

plot(data$L1_BCCI)
L = sqrt(nrow(data))
L = 50
L = 120

U = ssa(data$L1_BCCI,L,neig = 40,kind = "1d-ssa")
X = ssa(data$L1_BRCI,L,kind = "1d-ssa")
Z = ssa(data$L1_BSCI,L,kind = "1d-ssa")
plot(log(U$sigma))

plot(U, main = "BCCI")
plot(X, main = "BRCI")
plot(Z, main = "BSCI")

U_cor = wcor(U)
g <- grouping.auto(U, grouping.method = "wcor", 
                   method = "average", nclust = 5)
print(g[[1]])
print(g[[2]])
print(g[[3]])


plot(U, type = "vectors", vectors = "factor", idx = 1:20)

# Unemployment
SSA_un = ssa(data$L2_LRHUTTTT,L,neig = 40,kind = "1d-ssa")
U_cor = wcor(U)
pdf("D:/EUR/Master/Seminar Case studies in Applied Econometrics/SSA Graphs/Unemployment_ssa.pdf")
plot(U, ylab = "Contribution", xlab = "Eigentriple", main = "Unemployment")
plot(U_cor, scales = list(at = c(10, 20, 30,40)))
plot(U, type = "vectors", idx = 1:20)
plot(U, type = "paired", idx = 1:20, plot.contrib = FALSE)
dev.off()

recon = reconstruct(U, group = list(1,2,c(4,6),c(3,5)))
plot(recon)

# Producer confidence: Construction
SSA_un = ssa(data$L1_BCCI,L,neig = 40,kind = "1d-ssa")
U_cor = wcor(U)
pdf("D:/EUR/Master/Seminar Case studies in Applied Econometrics/SSA Graphs/Construction_ssa.pdf")
plot(U, ylab = "Contribution", xlab = "Eigentriple", main = "Unemployment")
plot(U_cor, scales = list(at = c(10, 20, 30,40)))
plot(U, type = "vectors", idx = 1:20)
plot(U, type = "paired", idx = 1:20, plot.contrib = FALSE)
dev.off()

recon = reconstruct(U, group = list(1,2,c(4,6),c(3,5)))
plot(recon)


# Producer confidence: Retail



# Producer confidence: Manufacturing



# Consumer confidence



# Export 




# Import




# AEX




# AEX Volatility








for (i in 2:ncol(data)) {
U = ssa(data[1:315,i],100,neig = 50 ,kind = "1d-ssa")
recon = reconstruct(U, groups = list(1, 2, c(3, 4), c(5,6)))
plot(recon,
     type = c("raw", "cumsum"),
     plot.method = c("native", "matplot", "xyplot"),
     base.series = NULL,
     add.original = TRUE,
     add.residuals = TRUE)
}


