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

TFR.arima1 <- arima(TFR$TFR, order=c(13,1,0))

acf(TFR.arima1$resid, lag.max = 45)
pacf(TFR.arima1$resid, lag.max = 45)

#there is a significant lag at 15 in the pacf try adding a ma component
TFR.arima2 <- arima(TFR$TFR, order=c(13,1,15))

acf(TFR.arima2$resid, lag.max = 45)
pacf(TFR.arima2$resid, lag.max = 45)
#the pacf shows white noise residuals, but acf still shows a big spike at lag 1

#this is probably due to a seasonal component in the data. a sarima model 
#will now be fit


T.SdiffTFR <- T.TFR |> mutate(SD.TFR = difference(TFR, lag = 12))

acf(na.omit(T.SdiffTFR$SD.TFR), lag.max = 40)
pacf(na.omit(T.SdiffTFR$SD.TFR), lag.max = 40)
#the acf shows significance to lag 7
#the pacf shows signifiacne for lag 1

#We will try seasonal ARIMA(0, 0, 0)(1, 1, 0)_12 first

TFR.sarima1 <- arima(TFR$TFR,order=c(0,0,0),
                     season=list(order=c(1,1,0),period=12))
acf(TFR.sarima1$resid, lag.max = 45)
pacf(TFR.sarima1$resid, lag.max = 45)
#this is still not good, we will try it using the values from arima2

