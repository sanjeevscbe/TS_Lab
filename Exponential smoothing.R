library(ggplot2)
library(fpp2)
oil
oildata = window(oil,start = 1996)
autoplot(oildata)+ylab("oil (millions of tonnes)")+xlab("Year")
# simple exponential - No trend
#the simplest of the smoothing method is called Simple Exponential Smoothing.This method is suitable when there is no clear trend or seasonal pattern is found.
#SES as a flat forecasting equation i.e., all forecasts take the same value equal to the last level component
fc=ses(oildata,h=5)
fc
round(accuracy(fc),2)
autoplot(fc)+autolayer(fitted(fc),series="Fitted")+ylab("oil (millions of tonnes)")+xlab("Year")
air=window(ausair,start=1990)
fc=holt(air,h=5)
summary(fc)
#very small value of beta means that the slope hardly changes over time.
fc2=holt(air,damped=TRUE,phi=0.9,h=15)
fc2=holt(air,damped=TRUE,phi=0.9,h=15) 
autoplot(air)+
  autolayer(fc,series="Holt's method",PI = FALSE)+
  autolayer(fc2,series = "Damped Holt's method",PI=FALSE)+
  ggtitle("Forecasts from Holt's method")+xlab("Year")+
  ylab("Air passengers in Australia (Millions)")+
  guides(colour=guide_legend(title="Forecast"))
fc2=holt(air, damped=TRUE)
summary(fc2)

aust = window(austourists,start=2005)
fit1=hw(aust,seasonal="additive")
fit2=hw(aust,seasonal="multiplicative")
summary(fit1)
summary(fit2)
autoplot(aust)+
  autolayer(fit1,series="Holt's winter method(Additive)",PI = FALSE)+
  autolayer(fit2,series = "Holt's winter method(Multiplicative)",PI=FALSE)+
  ggtitle("Forecasts from Holt's winter method")+xlab("Year")+
  ylab("Tourists in Australia (Millions)")+
  guides(colour=guide_legend(title="Forecast"))
f1=hw(aust,seasonal="additive",damped=TRUE,phi=0.9,h=15)
f2=hw(aust,seasonal="multiplicative",damped=TRUE,phi=0.9,h=15)
summary(f1)
summary(f2)
autoplot(aust)+
  autolayer(f1,series="Damped Holt's winter method(Additive)",PI = FALSE)+
  autolayer(f2,series = "Damped Holt's winter method(Multiplicative)",PI=FALSE)+
  ggtitle("Forecasts from Damped Holt's winter method")+xlab("Year")+
  ylab("Tourists in Australia (Millions)")+
  guides(colour=guide_legend(title="Forecast"))
#error can be additive, multiplicative
fit=ets(aust)
summary(fit)
autoplot(fit)
