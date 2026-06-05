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

T.TFR <- TFR[14:66, ]

T.TLB <- TLB[14:66, ]



