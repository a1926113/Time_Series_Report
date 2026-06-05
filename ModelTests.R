#load in data and clean

BirthsAndFertilityRatesAnnual <- 
  read_csv("C:/Users/ebony/OneDrive - University of Adelaide/Desktop/2026 Time Series/BirthsAndFertilityRatesAnnual (1).csv")


BirthsAndFertilityRatesAnnual <- BirthsAndFertilityRatesAnnual |>
  mutate(across(2:67, as.character)) |>
  mutate(across(2:67, ~na_if(., "na"))) |>
  mutate(across(2:67, as.double))

BirthsAndFertilityRatesAnnual <- BirthsAndFertilityRatesAnnual |>
  pivot_longer(cols = -DataSeries,
               names_to = "Year",
               values_to = "Value") |>
  pivot_wider(names_from = DataSeries,
              values_from = "Value")

BirthsAndFertilityRatesAnnual$Year <-as.integer(BirthsAndFertilityRatesAnnual$"Year")

BirthsAndFertilityRatesAnnual

#make TFR
TFR <- BirthsAndFertilityRatesAnnual[, -c(3:18)]


TFR  <-
  as_tsibble(TFR,
             index = Year) |>
  rename(year = Year, TFR = "Total Fertility Rate (TFR)")

TFR

#make TLB

TLB <- BirthsAndFertilityRatesAnnual[, -c(2:15,17,18)]

TLB  <-
  as_tsibble(TLB,
             index = Year) |>
  rename(year = Year, TLB = "Total Live-Births")

TLB

#make them training

T.TFR <- TFR[1:52, ]

T.TLB <- TLB[1:52, ]


#first plot the data

autoplot(T.TFR)
#looks like there is a nonlinear decreasing trend with a potential seasonal
#component

autoplot(T.TLB)
#looks like there is a nonlinear decreasing trend with a potential seasonal
#component


#will try to plot these with a log transform to try and linearise the trends
autoplot(log(T.TFR))
#had an effect on linearising the data
autoplot(log(T.TLB))
#does not look like it effectively linearised the data

#we will now difference the data to try and detrend it

T.diffTFR <- T.TFR |> mutate(D.TFR = difference(TFR, lag = 1))

T.diffTFR

T.diffTFR|> autoplot(D.TFR)

T.diffTLB <- T.TLB |> mutate(D.TLB = difference(TLB, lag = 1))

T.diffTLB|> autoplot(D.TLB)
#these look more stationary now 

acf(na.omit(T.diffTFR$D.TFR), lag.max = 40)
pacf(na.omit(T.diffTFR$D.TFR), lag.max = 40)
#the pacf shows the last spike at lag 13
#so we can try fitting an ARIMA(13,1,0) 

TFR.arima1 <- arima(T.TFR$TFR, order=c(13,1,0))

acf(TFR.arima1$resid, lag.max = 45)
pacf(TFR.arima1$resid, lag.max = 45)

#there is a significant lag at 15 in the pacf try adding a ma component
TFR.arima2 <- arima(T.TFR$TFR, order=c(13,1,15))

acf(TFR.arima2$resid, lag.max = 45)
pacf(TFR.arima2$resid, lag.max = 45)
#the pacf and acf shows white noise residuals
#this is bad though because of the principle of parsimony
#Warning message:
#In arima(T.TFR$TFR, order = c(13, 1, 15)) :
  #possible convergence problem: optim gave code = 1

#this is probably due to a seasonal component in the data. a sarima model 
#will now be fit

TFR.arima3 <- arima(T.TFR$TFR, order=c(8,1,15))

acf(TFR.arima3$resid, lag.max = 45)
pacf(TFR.arima3$resid, lag.max = 45)
#also white noise residuals
summary(TFR.arima2)
summary(TFR.arima3)


T.SdiffTFR <- T.TFR |> mutate(SD.TFR = difference(TFR, lag = 12))

acf(na.omit(T.SdiffTFR$SD.TFR), lag.max = 40)
pacf(na.omit(T.SdiffTFR$SD.TFR), lag.max = 40)
#the acf shows significance to lag 7
#the pacf shows signifiacne for lag 1

#We will try seasonal ARIMA(0, 0, 0)(1, 1, 0)_12 first

TFR.sarima1 <- arima(T.TFR$TFR,order=c(0,0,0),
                     season=list(order=c(1,1,0),period=12))
acf(TFR.sarima1$resid, lag.max = 45)
pacf(TFR.sarima1$resid, lag.max = 45)
#the acf shows significant lags until lag 7 and from lag 10 until lag 19
#the pacf shows a significant lag at 1 and 13

#try with regular differencing
TFR.sarima2 <- arima(T.TFR$TFR,order=c(0,1,0),
                     season=list(order=c(1,1,0),period=12))
acf(TFR.sarima2$resid, lag.max = 45)
pacf(TFR.sarima2$resid, lag.max = 45)
#this shows the last significant lag in the pacf at 5 so we will try that

TFR.sarima3 <- arima(T.TFR$TFR,order=c(0,1,5),
                     season=list(order=c(1,1,0),period=12))
acf(TFR.sarima3$resid, lag.max = 45)
pacf(TFR.sarima3$resid, lag.max = 45)
#there is a lag at 15 in pacf so we will try


TFR.sarima4 <- arima(T.TFR$TFR,order=c(0,1,8),
                     season=list(order=c(1,1,0),period=12))
acf(TFR.sarima4$resid, lag.max = 45)
pacf(TFR.sarima4$resid, lag.max = 45)
#this has white noise residuals
#q value is still to high probably

#will try it with a moving average component
TFR.sarima5 <- arima(T.TFR$TFR,order=c(1,1,0),
                     season=list(order=c(1,1,0),period=12))
acf(TFR.sarima5$resid, lag.max = 45)
pacf(TFR.sarima5$resid, lag.max = 45)
#the pacf has residuals up to 15

TFR.sarima6 <- arima(T.TFR$TFR,order=c(0,1,15),
                     season=list(order=c(0,1,0),period=12))
acf(TFR.sarima6$resid, lag.max = 45)
pacf(TFR.sarima6$resid, lag.max = 45)
#this has white noise residuals

TFR.sarima7 <- arima(T.TFR$TFR,order=c(0,1,8),
                     season=list(order=c(0,1,1),period=12))
acf(TFR.sarima7$resid, lag.max = 45)
pacf(TFR.sarima7$resid, lag.max = 45)
#this has white noise residuals

TFR.sarima8 <- arima(T.TFR$TFR,order=c(0,1,3),
                     season=list(order=c(0,1,2),period=12))
acf(TFR.sarima8$resid, lag.max = 45)
pacf(TFR.sarima8$resid, lag.max = 45)
#this is white noise residuals
summary(TFR.sarima8)

TFR.sarima9 <- arima(T.TFR$TFR,order=c(0,1,8),
                     season=list(order=c(2,1,0),period=12))
acf(TFR.sarima9$resid, lag.max = 45)
pacf(TFR.sarima9$resid, lag.max = 45)
summary(TFR.sarima9)

#i will now try fitting an arima for the log transformed TFR data
T.logTFR <- T.TFR
T.logTFR$TFR <- log(T.TFR$TFR)


#i will create the acf and pacf plots for the differenced log data


T.difflogTFR <- T.logTFR |> mutate(D.logTFR = difference(TFR, lag = 1))

T.difflogTFR

T.diffTFR|> autoplot(D.TFR)

acf(na.omit(T.difflogTFR$D.logTFR), lag.max = 40)
pacf(na.omit(T.diffTFR$D.TFR), lag.max = 40)
#the last significant lag is at lag 13 like the untransformed one

logTFR.arima1 <- arima(T.logTFR$TFR, order=c(13,1,0))
acf(logTFR.arima1$resid, lag.max = 45)
pacf(TFR.arima1$resid, lag.max = 45)
#the last signifiact lag is at 15 like the untrasnfored one
logTFR.arima2 <- arima(T.logTFR$TFR, order=c(13,1,15))
acf(logTFR.arima2$resid, lag.max = 45)
pacf(TFR.arima2$resid, lag.max = 45)
#white noise residuals but gave the same errors as before

logTFR.arima3 <- arima(T.logTFR$TFR, order=c(8,1,15))
acf(logTFR.arima3$resid, lag.max = 45)
pacf(logTFR.arima3$resid, lag.max = 45)
summary(logTFR.arima3)
#white noise residuals

#i will try sarima

T.SdifflogTFR <- T.logTFR |> mutate(SD.logTFR = difference(TFR, lag = 12))

acf(na.omit(T.SdifflogTFR$SD.logTFR), lag.max = 40)
pacf(na.omit(T.SdifflogTFR$SD.logTFR), lag.max = 40)

#We will try seasonal ARIMA(0, 1, 0)(0, 1, 0)_12 first

logTFR.sarima1 <- arima(T.logTFR$TFR,order=c(0,1,0),
                     season=list(order=c(0,1,0),period=12))
acf(logTFR.sarima1$resid, lag.max = 45)
pacf(logTFR.sarima1$resid, lag.max = 45)
#these plots are the same as the untransformed ones


#i will decide that the untransformed is better for simplicity. and then i will
#forecast using the data and whichever behaves better will be the one i use 
#and it will be one arima one sarima


#List of white noise residuals:
#tfr.arima2, tfr.sarima4, tfr.sarima6 tfr.sarima7

#i did trials with other 