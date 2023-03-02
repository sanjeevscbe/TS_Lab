# EVALUATING FORECAST ACCURACY

#the size of the test set is typically about 20% of the total sample.
#Although this value depends on how long the sample is and how far ahead you want to forecast.
#the test set should ideally be at least as large as the maximum forecast horizon required.
#The following point should be noted 
#A perfect fit can always be obtained by using a model with enough parameters.
#A model which fits training data well, will not necessarily forecast well.
#Forecast Error = eT+h = yT+h - y^ T+h/T
library(ggplot2)
library(forecast)
library(fpp2)
beer2
beer3 = window(ausbeer,start=1992,end=c(2007,4))
beer3
beerfit1=meanf(beer3,h=10)
beerfit1
beer4=window(ausbeer,start=2008)
accuracy(beerfit1,beer4)

beerfit2=snaive(beer3,h=10)
accuracy(beerfit2,beer4)

beerfit3= rwf(beer3,h=10)
accuracy(beerfit3,beer4)

# forecasting using regression
library(ggplot2)
uschange
autoplot(uschange[,c("Consumption","Income")])+ylab("%change")+xlab("year")
tslm(Consumption ~ Income, data=uschange)
uschange %>% as.data.frame() %>% GGally::ggpairs()
fit.consMR <- tslm(Consumption ~ Income+Production+Savings+Unemployment,data=uschange)
summary(fit.consMR)
checkresiduals(fit.consMR)
#for forecasting purpose the final tow columns are of limited interest(i.e., p-value and t-value)
#this is useful when studying the effect of each predictor but is not particularly useful of forecasting.
#the figure shows a timeplot, ACF and histogram from Multiple regression model fitted to the US quarterly data.
# as well as biyush godflay test for jointly testing upto 8th order of the autocorrelation.
#the time plot shows changing variation over time.But is otherwise relatively small.
# this heteroscadicity will potentially make the prediction interval coverage inaccurate.
#the histogram shows that the residuals seems to be slightly skewed which may also affect the coverage probability of the prediction interval
#the autocorrelation plot shows that a significant spike is there at lag 7.But it is not quiet enough 
#for biyush godflay to be significant at 5% level.In any case the autocorrelation is not practically large and
#at lag 7 it is unlikely to have any noticeable impact on the forecast or on the prediction interval