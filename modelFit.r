library(fGarch)
library(forecast)
script_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_directory)
source("main.R")
head(returns)
distributions<-c("norm", "snorm", "std", "sstd", "ged", "sged")
print(auto.arima(returns))

