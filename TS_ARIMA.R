autoplot(uschange[,"Consumption"])+xlab("year")+ylab("Quarterly percentage change")
fit=auto.arima(uschange[,"Consumption"],seasonal = FALSE,trace = TRUE)
fit
fit %>% forecast(h=10) %>% autoplot(include=80)#forecasts for next 10 periods with 80% confidence interval
ggAcf(uschange[,"Consumption"])
ggPacf(uschange[,"Consumption"])
#if the data are from ARIMA(p,d,0) or ARIMA(0,d,q) model then the ACF and PACF plots can be helpful in determining the value of p or q
#if p and q are both positive then the plots donot help in finding suitable values of p and q
#The data may follow ARIMA(p,d,0) if ACF anf PACF plots of the differenced data show thw followin pattern
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
