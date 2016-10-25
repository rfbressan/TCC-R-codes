###############################################################################
## Exemplo do capítulo sobre EVT de medidas condicionais de risco utilizando
## o método processo pontual auto-excitavel.
## Dados da GS entre 05/05/1999 a 04/10/2016
## Utilizacao do pacote SAPP
###############################################################################
library(xts)
library(PerformanceAnalytics)
library(xtable)
library(SAPP)
library(PtProcess)
library(fExtremes)
library(gPdtest)
library(plotly)
library(boot)

setwd("C:\\Users\\Rafael\\Documents\\UDESC\\TCC\\TCC Latex svn\\TCC-R-codes")

prices <- as.xts(read.zoo(read.csv("evt-exemplo GS.csv"), format = "%Y-%m-%d", FUN = as.Date))
returns <- na.omit(Return.calculate(prices, method = "log"))
losses <- -1*returns
losses <- losses[,"Adj.Close"]
names(losses) <- "losses"
u <- quantile(losses, 0.9)
indextimes <- as.numeric(index(losses)-index(losses[1,]))

## Vamos primeiro estimar os parametros da GPD
gpd <- gpdFit(as.timeSeries(losses[,"losses"]), u=u, type = "mle")
xi <- gpd@fit$par.ests[1]
beta <- gpd@fit$par.ests[2]

parameters <- c(0.05, 3.1, 1.5, 0.1, 1.1) # Valores iniciais dos parametros para lambdag
stime <- indextimes[1]
etime <- indextimes[length(indextimes)]

## Estimando o modelo ETAS
etasmodel <- etasap(indextimes, coredata(losses), threshold = u, parami = parameters, tstart = stime,
                    zte = etime, approx = 1, plot = TRUE)

############################################################################################################
## Com os parametros estimados mu, K, c, alpha e p em etasmodel$param, calcular lambdag através do pacote
## PtProcess

dados <- data.frame(indextimes, coredata(losses))
names(dados) <- c("time", "magnitude")
dados <- dados[which(dados$magnitude>u),]-u
params <- c(etasmodel$param[[1]], etasmodel$param[[2]], etasmodel$param[[4]], etasmodel$param[[3]],
            etasmodel$param[[5]])
lambdag <- etas_gif(dados, evalpts = indextimes, params = params)

op <- par(mfrow=c(2,1))
plot(dados, type="h")
plot(indextimes, lambdag, type = "l")
par(op)
