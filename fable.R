library(tidyverse)
library(tsibble)
library(lubridate)
library(fable)
library(tsibble)
library(tsibbledata)

###################################################

global_economy %>%
  autoplot(GDP/Population, alpha = 0.3) +
  guides(colour = FALSE)

avg_gdp_pc <- global_economy %>%
  as_tibble() %>%
  group_by(Country) %>%
  summarise(
    # Average GDP per capita for each country
    gdp_pc = mean(GDP/Population, na.rm = TRUE),
    # Most recent GDP per capita for each country
    last = last((GDP/Population)[!is.na(GDP/Population)])
  )
top_n(avg_gdp_pc, 5, gdp_pc)

max_gdp_pc <- global_economy %>%
  semi_join(
    avg_gdp_pc %>%
      filter(gdp_pc == max(gdp_pc, na.rm = TRUE)),
    by = "Country"
  )

# install.packages("ggrepel")
# Using goem_label_repel() gives nicer label positions than geom_label()
# If the ggrepel package is not available, you can use geom_label() instead
global_economy %>%
  ggplot(aes(x = Year, y = GDP / Population, group = Country)) +
  geom_line(alpha = 0.3) +
  geom_line(colour = "red", data = max_gdp_pc) + 
  geom_label_repel(
    aes(label = Country, x = 2020, y = last),
    data = top_n(avg_gdp_pc, 5, last),
  )

#source code: https://github.com/rstudio-conf-2020/time-series-forecasting/blob/master/materials/labs.R

#########################################

#Forecasting a univariate time series (the annual GDP for Saudi Arabia).


sa <- global_economy %>%
  filter(Country == "Saudi Arabia")


sa_gdp_model <- sa %>%
  model(
    arima = ARIMA(GDP)
  )

sa_gdp_model
glance(sa_gdp_model)


sa_gdp_model %>%
  select(Country,arima) %>%
  forecast(h = "10 years") %>%
  autoplot(sa)

####################################

#Modelling many series (the global GDP).

fit_global_economy <- global_economy %>% model(arima = ARIMA(GDP))
fit_global_economy


#Fitting and forecasting the US & UK GDP.


US_UK_GDP <-global_economy %>%
  filter(
      Country == "United States"|
      Country == "United Kingdom") %>%
  select(GDP)


fit_US_UK_GDP <- US_UK_GDP %>%
  model(arima = ARIMA(GDP),
        ets = ETS(GDP))

fit_US_UK_GDP %>%
  forecast(h = 10) %>%
  filter(Country == "United States"|
           Country == "United Kingdom") %>%
  autoplot(global_economy)

####################################


