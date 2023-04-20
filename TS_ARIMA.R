autoplot(uschange[,"Consumption"])+xlab("year")+ylab("Quarterly percentage change")
fit=auto.arima(uschange[,"Consumption"],seasonal = FALSE,trace = TRUE)
fit
fit %>% forecast(h=10) %>% autoplot(include=80)#forecasts for next 10 periods with 80% confidence interval
ggAcf(uschange[,"Consumption"])
ggPacf(uschange[,"Consumption"])
#if the data are from ARIMA(p,d,0) or ARIMA(0,d,q) model then the ACF and PACF plots can be helpful in determining the value of p or q
#if p and q are both positive then the plots donot help in finding suitable values of p and q
#The data may follow ARIMA(p,d,0) if ACF anf PACF plots of the differenced data show the followin pattern
#1) ACF is exponentially decaying or sinusoidal
#2) there is a significant spike at lag p in the PACF but none beyond lag p

#The data may follow ARIMA(0,d,q) if the ACF nd PACF plots of the differenced data show the following patterns
#1) the PACF is exponentially decaying or sinusoidal
#2) there is a Significant spike at lag q in the ACF but none beyond lag q
fit1=Arima(uschange[,"Consumption"],order=c(3,0,0))
fit1
fit1 %>% forecast(h=10) %>% autoplot(include=80)
fit3=auto.arima(uschange[,"Consumption"],seasonal = FALSE,stepwise=FALSE,approximation = FALSE)
fit3
#ARIMA(3,0,0) -> so it is AR(3)model
elecequip %>% stl(s.window="periodic") %>% seasadj()->eeadj
autoplot(eeadj)
ggAcf(eeadj)
ggPacf(eeadj)
eeadj %>% diff() %>% ggtsdisplay(main="")
fit4=Arima(eeadj,order=c(3,1,1))
fit4
checkresiduals(fit4)
#unit root falls outside the circle then stationary and viceversa
#The stationarity condition for the model ARMA(p,q) is that p complex root of phi(B) lie outside the unit circle and the invertibility condition are that
# the q complex roots of theta(B) lie outside the unit circle.
autoplot(fit4)

#Drawback of arima
#As its name suggest it supports both AR and MA elements, the integrated elements refers to differencing allowing the method to support time series data with trend
# a problem with ARIMA is that it does not suppot seasonal data

#In case of SARIMA it adds 3 new hyperparameters to specify AR, differencing(I) and MA for the seasonal component of the time series.
    
    #SARIMA
library(astsa)
prodn
plot(prodn)
acf2(prodn)
acf2(diff(diff(prodn),12),48)#diff((),no of data points in a time period)  this is for seasonal differencing
nsdiffs(prodn)
#Model SARIMA(2,1,0)(0,1,3)12
sarima(prodn,20,2,1,0,0,1,3,12)
library(fpp2)
library(ggplot2)
autoplot(euretail)+ylab("Retail Index")+xlab("Year")
nsdiffs(euretail)
ndiffs(euretail)
euretail
euretail %>% diff(lag=4) %>% ggtsdisplay()
euretail %>% diff(lag=4) %>% diff() %>% ggtsdisplay()
euretail %>% Arima(order=c(0,1,1),seasonal=c(0,1,1)) %>% residuals() %>% ggtsdisplay()
euretail %>% arima(order=c(0,1,2),seasonal=c(0,1,1)) %>% residuals() %>% ggtsdisplay()
euretail %>% arima(order=c(0,1,3),seasonal=c(0,1,1)) %>% residuals() %>% ggtsdisplay()
#Now it becomes white noise
auto.arima(euretail)
