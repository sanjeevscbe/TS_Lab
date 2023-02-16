library(fpp2)
library(forecast)
library(ggplot2)
ausbeer
beer2<-window(ausbeer,star=1992,end=c(2007,4))
beer2
y<-meanf(beer2,h=10)#Forecast using Average method
y
beer2<-window(ausbeer,star=1992,end=c(2007,4))
autoplot(beer2)+
autoplot(meanf(beer2,h=11),series="Mean",PI=FALSE)+
autolayer(naive(beer2,h=11),series = "Naive",PI=FALSE)+
autolayer(snaive(beer2,h=11),series="Seasonal naive",PI=FALSE)+
ggtitle("Forecasts for quarterly beer production")+
xlab("year")+ylab("Megalitres")+guides(colour=guide_legend(title="Forecast"))

goog200
milk
#Calender Adjustment
plot(milk)
dframe<-cbind(Monthly=milk,DailyAverage=milk/monthdays(milk))
autoplot(dframe,facet=TRUE)+xlab("Years")+ylab("Pounds")+ggtitle("Milk production per cow")
lambda<-BoxCox.lambda(elec)
autoplot(BoxCox(elec,lambda))
autoplot(goog200)+xlab("Day")+ylab("closing Price (US$)")+ggtitle("Google Stock(daily ending 6 December 2013)")
res<- residuals(naive(goog200))
plot(res,main="residuals")
gghistogram(res)+ggtitle("Histogram of residulas")
ggAcf(res)+ggtitle("ACF of residuals")
Box.test(res,lag=10,fitdf=0)
Box.test(res,lag=10,fitdf=0,type="Lj")
#for both q1 and q2 the results are not significant, thus we can conclude that the residuals are not distinguishable from a white noise series
#time series with no autocorrelation is called a white noise series
checkresiduals(naive(goog200))
