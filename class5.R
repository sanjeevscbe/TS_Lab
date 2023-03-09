library(ggplot2)
library(fpp2)
library(forecast)
beer2<-window(ausbeer,start=1992)
plot(beer2)
fit.beer=tslm(beer2 ~ trend + season)
summary(fit.beer)
#we can conclude that there is an average downward trend of -0.34 mega liters per quarter
#on average the 2nd quarter has production of 34.7 mega liters lower than the 1st quarter.Here 1st quarter is the intercept 441.8.
#similarly in the 4th quarter has 72.79 mega liters of production higher than the 1st quarter.
autoplot(beer2, series="Data")+autolayer(fitted(fit.beer),series="Fitted")+xlab("year")+ylab("Megaliters")+ggtitle("Quarterly beer production")
#while there are many possible predictors we need some strategy for selecting the best predictors to use in the regression model
#A common approach which is invalid to use in a multiple linear regression on all predictors and disregard all variables whose p-value are greater than 0.05
#To start with statistical significance does not always indicate the predictive value,even if forecasting is not the goal this is not a good strategy because
#the p-value can be misleading when 2 or more variables are correlated to each other.
#instead we use measure of predictive accuracy
#Types1 - Cross Valitation(CV)
#The time series cross validation is a general tool for determining the predictive ability of the model
#1)remove observation t from the dataset and fit the model using the remaining data,then compute the error for the omitted observation.
#2)repeat step 1 for t=1,2,...T 
#3)compute the MSE from e1*,e2*,e3*,..,eT* and we will call this a cross validation.Although this looks like a time consuming procedure
#there are fast methods of calculating CV so that it takes no longer than fitting one model to the full dataset.
fit.consMR <- tslm(Consumption ~ Income+Production+Savings+Unemployment,data=uschange)
summary(fit.consMR)
CV(fit.consMR)
fit.consMR1 <- tslm(Consumption ~ Income+Savings+Unemployment,data=uschange)
summary(fit.consMR1)
CV(fit.consMR1)
#try it of 16 combination and find which combination has the least CV and BIC value.
#EX-ante forecasts are those that are made using only the information that is available in advance.
#eg: % change in US consumption for quarters following the end of the sample.

#Ex-post forecasts are those that are made using the later information on the predictors.It is backward looking and it looks the result
#after they have already occured.eg: in investment company analyst can use hystorical return to forecast the probability of making profit or loss on an investment.
#suppose a US policy maker is interested in comparing the predicted change in consumption when there is a constant growth of 1% and 0.5% respectively
#for income and savings with no change in employement rate vs a respective decline of 1% and 0.5% for each of the 4 quarters following the end of the sample
fit.consBest <- tslm(Consumption ~ Income+Savings+Unemployment,data=uschange)
h = 4
newdata = data.frame(Income = c(1,1,1,1),
                     Savings = c(0.5,0.5,0.5,0.5),
                     Unemployment = c(0,0,0,0))
fcast.up <- forecast(fit.consBest,newdata = newdata)
newdata = data.frame(Income=rep(-1,h),
                     Savings = rep(-0.5,h),
                     Unemployment = rep(0,h))
fcast.down =  forecast(fit.consBest,newdata = newdata)
fcast.down
autoplot(uschange[,1])+ylab("% change in US consumption")+autolayer(fcast.up, PI=TRUE,series="increase")+autolayer(fcast.down, PI=TRUE,series="decrease")+guides(colour=guide_legend(title="scenario"))

