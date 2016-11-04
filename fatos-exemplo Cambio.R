###############################################################################
## Exemplo do capítulo sobre EVT de medidas condicionais de risco utilizando
## o método processo pontual auto-excitavel.
## Dados da GS entre 05/05/1999 a 04/10/2016
###############################################################################
library(xts)
library(PerformanceAnalytics)
library(fBasics)
library(xtable)
library(plotly)

setwd("C:\\Users\\Rafael\\Documents\\UDESC\\TCC\\TCC Latex svn\\TCC-R-codes")

irate <- as.xts(read.zoo(read.csv2("JuroFut.csv"), format = "%d/%m/%Y", FUN = as.Date))
xrate <- as.xts(read.zoo(read.csv2("Cambio.csv"), format = "%d/%m/%Y", FUN = as.Date))
ibov <- as.xts(read.zoo(read.csv("Ibovespa.csv"), format = "%Y-%m-%d", FUN = as.Date))

prices <- cbind(1000/(1+irate[,1]/100), 1000/(1+irate[,2]/100), 1000/(1+irate[,3]/100))
iretornos <- na.omit(Return.calculate(prices, method = "log"))
xretornos <- na.omit(Return.calculate(xrate, method = "log"))
ibovretornos <- na.omit(Return.calculate(ibov[,"Adj.Close"], method = "log"))
names(iretornos) <- c("Retornos 30d", "Retornos 180d", "Retornos 360d")
names(xretornos) <- "Retornos Câmbio"
names(ibovretornos) <- "Retornos Ibovespa"

op <- par(mfrow=c(3,1))
acf(iretornos[,3], lag.max=15, ylim=c(-0.1, 0.2), main="Juros Futuros", lwd=2, xlab="")
grid(nx=NA, ny=NULL, col = "black")
acf(xretornos, lag.max=15, ylim=c(-0.1, 0.2), main="Câmbio", lwd=2, xlab="")
grid(nx=NA, ny=NULL, col = "black")
acf(ibovretornos, lag.max = 15, ylim=c(-0.1, 0.2), main = "Ibovespa", lwd = 2)
grid(nx=NA, ny=NULL, col = "black")
par(op)

op <- par(mfrow=c(3,1))
acf(abs(iretornos[,3]), lag.max=15, main="Juros Futuros", lwd=2, xlab="")
grid(nx=NA, ny=NULL, col = "black")
acf(abs(xretornos), lag.max=15, main="Câmbio", lwd=2, xlab="")
grid(nx=NA, ny=NULL, col = "black")
acf(abs(ibovretornos), lag.max = 15, main = "Ibovespa", lwd = 2)
grid(nx=NA, ny=NULL, col = "black")
par(op)