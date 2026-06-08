TFR.arima <- T.TFR |>
  model(
    arima1 = ARIMA(TFR ~ 1 + pdq(13,1,15) +  PDQ(0,0,0))
  )

TFR.arima.FC <- TFR.arima |> forecast(h=13)

TFR |> 
  autoplot(TFR) +
  autolayer(TFR.arima.FC) +
  autolayer(Test.TFR, colour = "red") +
  labs(x = "Year",
       y = "Total Fertility Rate",
       title = "Total Fertility Rate Per Year in Singapore ARIMA Prediction")



TFR.sarima <- T.TFR |>
  model(
    arima = ARIMA(TFR ~ 1 + pdq(2,1,3) +  PDQ(0,1,0))
  )

TFR.sarima.FC <- TFR.sarima |> forecast(h=13)

TFR |> 
  autoplot(TFR) +
  autolayer(TFR.sarima.FC) +
  autolayer(Test.TFR, colour = "red") +
  labs(x = "Year",
       y = "Total Fertility Rate",
       title = "Total Fertility Rate Per Year in Singapore SARIMA Prediction")

###################################

TLB.arima <- T.TLB |>
  model(
    arima1 = ARIMA(TLB ~ 1 + pdq(13,1,1) +  PDQ(0,0,0))
  )

TLB.arima.FC <- TLB.arima |> forecast(h=13)

Test.TLB <- TLB[54:66,]

Test.TLB

TLB |> 
  autoplot(TLB) +
  autolayer(TLB.arima.FC) +
  autolayer(Test.TLB, colour = "red") +
  labs(x = "Year",
       y = "Total Live-Births",
       title = "Total Live Births Per Year in Singapore ARIMA Prediction")

Test.TLB
Test.TFR

TLB.sarima <- T.TLB |>
  model(
    arima = ARIMA(TLB ~ 1 + pdq(0,1,3) +  PDQ(0,1,0))
  )

TLB.sarima.FC <- TLB.sarima |> forecast(h=13)
TLB.sarima.FC

TLB |> 
  autoplot(TLB) +
  autolayer(TLB.sarima.FC) +
  autolayer(Test.TLB, colour = "red") +
  labs(x = "Year",
       y = "Total Live-Births",
       title = "Total Live-Births Per Year in Singapore SARIMA Prediction")

