#Clearing Environment
#bliepbloep
#test2
rm(list=ls())

#Importing Data
df <- read.csv("~/Desktop/data-eur2023.csv", row.names=1)

#Creating Series
unempl_L2 <- df$L2_LRHUTTTT
constr.conf_L1 <- df$L1_BCCI
retail.conf_L1 <- df$L1_BRCI
manuf.conf_L1 <- df$L1_BSCI
cons.conf_L1 <- df$L1_CSCICP02
export.logs_L3 <- df$L3_XTEXVA01
import.logs_L3 <- df$L3_XTIMVA01
AEX.val.logs_L1 <- df$L1_AEX
AEX.vol_L1 <-df$L1_AEXVLT 

#Creating DataFrame Levels 
pub.date <- as.Date(df$pub_date)
unemploy <- c(df$L2_LRHUTTTT[3:434],'NA','NA')
constr.conf <- c(df$L1_BCCI[2:434],'NA')
retail.conf <- c(df$L1_BRCI[2:434],'NA')
manuf.conf <- c(df$L1_BSCI[2:434],'NA')
cons.conf <- c(df$L1_CSCICP02[2:434],'NA')
export.logs <- c(df$L3_XTEXVA01[4:434],'NA','NA','NA')
import.logs <- c(df$L3_XTIMVA01[4:434],'NA','NA','NA')
AEX.val.logs <- c(df$L1_AEX[2:434],'NA')
AEX.vol <- c(df$L1_AEXVLT[2:434],'NA')

df.levels <- data.frame(pub.date,unemploy,constr.conf,retail.conf,
                        manuf.conf,cons.conf,export.logs,import.logs,
                        AEX.val.logs, AEX.vol)

#Graph for Producer Confidence
pdf("producer_confidence.pdf", height = 8, width = 16)
plot(pub.date[1:431],df.levels[1:431,3], lwd = 2, type = "l", col = "steelblue", xlab = "", ylab = "", main = "Producer Confidence")
lines(pub.date[1:431],df.levels[1:431,4], lwd = 2, col = "red3")
lines(pub.date[1:431],df.levels[1:431,5], lwd = 2, col = "green4")
legend("bottomright", c("Construction", "Retail", "Manufacturing"), fill = c("steelblue","red3", "green4"), cex =1)
dev.off()

#Graph for Unemployment
pdf("unemployment.pdf", height = 8, width = 16)
plot(pub.date[1:431],df.levels[1:431,2], lwd = 2, type = "l", col = "steelblue", xlab = "", ylab = "", main = "Unemployment Rate")
dev.off()

#Graph for Consumer Confidence
pdf("cons_conf.pdf", height = 8, width = 16)
plot(pub.date[1:431],df.levels[1:431,6], lwd = 2, type = "l", col = "steelblue", xlab = "", ylab = "", main = "Consumer Confidence")
dev.off()

#Graph for Export & Import
pdf("export_import.pdf", height = 8, width = 16)
plot(pub.date[1:431],df.levels[1:431,7], lwd = 2, type = "l", col = "steelblue", xlab = "", ylab = "", main = "Export and Import (logs)")
lines(pub.date[1:431],df.levels[1:431,8], lwd = 2, col = "red3")
legend("bottomright", c("Export", "Import"), fill = c("steelblue","red3"), cex =1)
dev.off()

#Graph AEX
pdf("AEX_value.pdf", height = 8, width = 16)
plot(pub.date[1:431],df.levels[1:431,9], lwd = 2, type = "l", col = "steelblue", xlab = "", ylab = "", main = "AEX Value")
dev.off()

#Graph AEX
pdf("AEX_vol.pdf", height = 8, width = 16)
plot(pub.date[1:431],df.levels[1:431,10], lwd = 2, type = "l", col = "steelblue", xlab = "", ylab = "", main = "AEX Volatility")
dev.off()


