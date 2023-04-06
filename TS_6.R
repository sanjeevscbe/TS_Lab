goog200 # Daily google stock price
plot(goog200)
acf(goog200)
Box.test(goog200)#Test statistic used to check for autocorrelation,Ho = No Autocorrelation
# since P-value < 0.05 reject Ho.
goog200diff1 = diff(goog200)
plot(goog200diff1)
acf(goog200diff1)
Box.test(goog200diff1)
#here H0 is not rejeted.The data is not having autocorrelation and it is stationary.
plot(a10)
cbind("Sales ($million)" = a10,"Monthly log sales" = log(a10),
      "Annual Change in log sales"=diff(log(a10),12)) %>% autoplot(facets=TRUE)+xlab("Year")+ylab("")+ggtitle("Antidiabetic dru sales")
loga10 = log(a10)
test = ur.kpss(loga10)
summary(test)
test1 = ur.kpss(diff(loga10))
summary(test1)
kings = scan("https://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
plot(kings)
kingstimeseries = ts(kings)
plot(kingstimeseries)
ur.kpss(kingstimeseries)
summary(ur.kpss(kingstimeseries))
#the process of using a sequence of KPSS test to determine the appropriate no. of first difference is carried out by the function ndiffs.
#A similar funtion for determining whether seasonal differencing is required is nsdiffs.
ndiffs(kingstimeseries) # for trend
nsdiffs(kingstimeseries) # for seasonality
kingstimeseries1 = diff(kingstimeseries)
test3=ur.kpss(kingstimeseries1)
acf(kingstimeseries1)
pacf(kingstimeseries1)
auto.arima(kingstimeseries)
