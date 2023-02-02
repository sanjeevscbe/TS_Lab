library(ggfortify)
library(zoo)
library(tseries)
library(astsa)
library(forecast)
library(ggplot2)
library(fpp2)
data("AirPassengers")
View(AirPassengers)
plot(AirPassengers,main ="Air Passengers Dataset")#main is for title
decomposed_air_passengers<-decompose(AirPassengers,type = "multiplicative")
autoplot(decomposed_air_passengers)
ggseasonplot(AirPassengers)
decomposed_air_passengers$figure# seasonality Index value for all months in a year(highest peak is found in july)
ggseasonplot(decomposed_air_passengers$seasonal)# Average Seasonality index for all years (peaks may vary for each year)
ggseasonplot(AirPassengers,polar = TRUE)
ggsubseriesplot(AirPassengers,main = "Seasonal subseries of AirPassengers")

births<-scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births
plot(births)
birthstimeseries<-ts(births,frequency=12,start=c(1946,1))
birthstimeseries
plot(birthstimeseries,main="birthstimeseries dataset")
decomposed_birthstimeseries <-decompose(birthstimeseries,type = "additive")
autoplot(decomposed_birthstimeseries)
decomposed_birthstimeseries$seasonal
birthstimeseries_adjusted<- birthstimeseries - (decomposed_birthstimeseries$seasonal)#removing seasonality 
birthstimeseries_adjusted
plot(birthstimeseries_adjusted)
rm(decomposed_birthstimeseries_adjusted)
