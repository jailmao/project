#libraries
library(shiny)
library(fGarch)
library(stats)
library(quantmod)
library(forecast)
#getuserinput
input=("^FTSE")
VaR_input = 500
VaR_level = 5
input_date = "2020-03-29"

#import the historical data
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading historical data using quantmod

price <- getSymbols(input, from = input_date,
           to = Sys.Date(),warnings = FALSE,
           auto.assign = FALSE, periodicity="daily")
returns <- as.numeric(diff(log(price[,6])))
returns[1]=0
returns<-na.omit(returns)

#fit a model
armaCoef1 <- as.numeric(auto.arima(returns)$arma[1])

ag <- as.formula(sprintf("~arma(%d,0) + garch(1,1)", armaCoef1))

distributions <- c("norm", "snorm", "std", "sstd", "ged", "sged")
min_bic=0
selectedDistribution <- NULL

for (dist in distributions) {
  modelTemp <- garchFit(formula = ag, 
                    data = returns, 
                    cond.dist = dist, 
                    trace = FALSE)
  k<-length(coef(modelTemp))
  logLik <- modelTemp@fit$llh
  bic <- -k*log(length(returns)) + 2*logLik
  if (bic < min_bic) {
    min_bic=bic
    cond_dist=dist
    model=modelTemp
    
  }
}

#simulate values

modelCoef<- coef(model)

simulate_distribution <- function(distribution, model, N) {
  set.seed(42)  
  
  if (distribution == "norm") {
    simulated_values <- rnorm(N, 0, 1)
  } else if (distribution == "snorm") {
    simulated_values <- rsnorm(N, 0, 1, xi=model["skew"])
  } else if (distribution == "std") {
    simulated_values <- rstd(N, 0, 1, nu = model["shape"])
  } else if (distribution =="sstd"){
    simulated_values <- rsstd(N, 0, 1, nu = model["shape"], xi = model ["skew"])
  } else if (distribution == "ged"){
    simulated_values <- rged (N, 0, 1, nu=model["shape"])
  } else if (distribution == "sged"){
    simulated_values <- rsged (N, 0, 1, nu=model["shape"], xi= model["skew"])
  }
  else {
    stop("Unsupported distribution")
  }
  
  return(simulated_values)
}
#reintroduce garch

sigma2 <- modelCoef["omega"]+modelCoef["alpha1"]*tail(model@residuals,1)^2+modelCoef["beta1"]*tail(model@sigma.t, 1)^2

modelOut=modelCoef["mu"]+sqrt(sigma2)*simulate_distribution(cond_dist, modelCoef, 10000)
if (armaCoef1 != 0){
  for (i in 1:armaCoef1){
    modelOut=modelOut+modelCoef[i+1]*rev(tail(returns, armaCoef1))[i]
    
  }
}
par(mfrow=c(1,1))
plot(price[,6])


VaR=quantile(modelOut, VaR_level/100)
investment = VaR_input/-VaR
expected_return=quantile(modelOut, 0.5)*investment
model
investment
expected_return
