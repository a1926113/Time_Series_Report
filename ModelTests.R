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

T.diffTFR|> autoplot(D.TFR)

T.diffTLB <- T.TLB |> mutate(D.TLB = difference(TLB, lag = 1))

T.diffTLB|> autoplot(D.TLB)
#these look more stationary now 


#first we will try to fit a model to the TFR data
#we will look at the acf and pacf plots

