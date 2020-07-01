require(tidyverse)
require(parsnip)
require(tidymodels)

dat <- rio::import(here::here("data", "african_crises.csv"))

# caseA number which denotes a specific country
# cc3A three letter country code
# countryThe name of the country
# yearThe year of the observation
# systemic_crisis"0" means that no systemic crisis occurred in the year and "1" means that a systemic crisis occurred in the year.
# exch_usdThe exchange rate of the country vis-a-vis the USD
# domestic_debt_in_default"0" means that no sovereign domestic debt default occurred in the year and "1" means that a sovereign domestic debt default occurred in the year
# sovereign_external_debt_default"0" means that no sovereign external debt default occurred in the year and "1" means that a sovereign external debt default occurred in the year
# gdp_weighted_defaultThe total debt in default vis-a-vis the GDP
# inflation_annual_cpiThe annual CPI Inflation rate
# independence"0" means "no independence" and "1" means "independence"
# currency_crises"0" means that no currency crisis occurred in the year and "1" means that a currency crisis occurred in the year
# inflation_crises"0" means that no inflation crisis occurred in the year and "1" means that an inflation crisis occurred in the year
# banking_crisis"no_crisis" means that no banking crisis occurred in the year and "crisis" means that a banking crisis occurred in the year

