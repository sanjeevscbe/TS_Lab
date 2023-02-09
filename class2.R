library(fpp2)
library(ggplot2)
?elecdemand
elecdemand
autoplot(elecdemand[,c("Demand","Temperature")],facets=TRUE) + xlab("Year: 2014") + ylab("") + ggtitle("Half-houry electricity demand: Victoria,Australia")
qplot(Temperature,Demand,data=as.data.frame(elecdemand))+ylab("Demand (GW)")+xlab("Temperature (celcius)")      
cor(elecdemand)         
visnights
?visnights
autoplot(visnights)
autoplot(visnights[,1:5],facets = TRUE)+ylab("Number of visitor nights each quarter (millions)")
GGally::ggpairs(as.data.frame(visnights[,1:5]))
#from the figure we can observe that the second column of plot shows that there is a strong positive relationship between visitors to the NSWNthCo and NSWSthCo.
#But no detectable relationship between the visitors NSWNthCo and NSWSthIn.
ausbeer
beer2<-window(ausbeer,start=1992)
gglagplot(beer2)
#here the colour indicates the quarter of the variable .the relationship is strongly positive between lag 4 and lag 8, reflecting the strong seasonality in the data.
#the negative relationship is also seen for lag 2 and lag 6,because peaks are plotted against troughs.
#autocorrelation
acf(beer2)
#r4 is higher than other lags, this is due to the seasonal patter in the data.
#r2 is more negative than for the other lags because troughs tends to be 2 quarters behind peaks.
aelec<-window(elec, start=1980)
autoplot(aelec)+xlab("Year")+ylab("GWH")
ggAcf(aelec,lag=48)
#the slow decrease in ACF as lag increases is due to the trend, while scalloped shape is due to the seasonality
set.seed(30)
y<-ts(rnorm(50))
autoplot(y)+ggtitle("White noise")
ggAcf(y)
#The time series data that shows no autocorrelation is called White Noise.
#also it is found to be within confidence interval(i.e., blue dotted lines)