T.diffTLB <- T.TLB |> mutate(D.TLB = difference(TLB, lag = 1))

T.diffTLB|> autoplot(D.TLB)
#this looks more stationary now 

acf(na.omit(T.diffTLB$D.TLB), lag.max = 40)
pacf(na.omit(T.diffTLB$D.TLB), lag.max = 40)
#the pacf shows the last spike at lag 13
#so we can try fitting an ARIMA(13,1,0) 

TLB.arima1 <- arima(T.TLB$TLB, order=c(13,1,0))

acf(TLB.arima1$resid, lag.max = 45)
pacf(TLB.arima1$resid, lag.max = 45)
#there is a significant lag at 15 in the pacf try adding a ma component

TLB.arima2 <- arima(T.TLB$TLB, order=c(13,1,15))

acf(TLB.arima2$resid, lag.max = 45)
pacf(TLB.arima2$resid, lag.max = 45)
#these are white noise resudulas but there is a possible convergence problem

TLB.arima3 <- arima(T.TLB$TLB, order=c(13,1,1))

acf(TLB.arima3$resid, lag.max = 45)
pacf(TLB.arima3$resid, lag.max = 45)
#these are white noise


#we will now try fitting some sarima models

T.SdiffTLB <- T.TLB |> mutate(SD.TLB = difference(TLB, lag = 12))

acf(na.omit(T.SdiffTLB$SD.TLB), lag.max = 40)
pacf(na.omit(T.SdiffTLB$SD.TLB), lag.max = 40)
#acf shows signifiance up to lag 5 and pacf shows signifiance up to lag 5

#We will try seasonal ARIMA(0, 1, 0)(0, 1, 0)_12 first

TLB.sarima1 <- arima(T.TLB$TLB,order=c(0,1,0),
                     season=list(order=c(1,1,0),period=12))

acf(TLB.sarima1$resid, lag.max = 45)
pacf(TLB.sarima1$resid, lag.max = 45)

