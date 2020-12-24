
# Angus Watters
# ENVS 193
# LAB 1 --- Data wrangling + manipulation + organizing + cleaning
# 12/23/2020


# STEP 1 --- Load libraries
# Libraries
install.packages('tidyverse')
library(tidyverse)
library(readr)

usgs = read_csv('data/usco2015v2.0.csv')
tmp1 = usgs %>% rename(pop =  'TP-TotPop', yr = 'YEAR', irrigation = 'IG-IrSpr') %>%
  rename(county = COUNTY)


pop_irrigation_df = tmp1 %>% select(STATE, COUNTY, YEAR, pop, irrigation)
pop_
az = readRDS('data/az/az-join-time.rds')
x = c('Alaska', 'California', 'New York')
df = data.frame(water = 1:3, state = x)

x = 4
y = 'c'
z = '4'
class(x)
class(z)

lst = c(4, 'b', 3)
y = as.character(3)
y = as.numeric(3)













library(ggplot2)

library(tidyverse)
library(tigris)
library(readxl)
USAboundariesData::

# STEP 2 --- READ IN 2 DIFFERENT TYPES DATA SETS (Population = excel file & COVID =CSV from a URL)
pop = read_excel('data/PopulationEstimates.xls', skip = 2) %>%
  select(pop2019 = POP_ESTIMATE_2019, fips = FIPStxt)

covid_url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(covid_url)

# STEP 2 --- PERFORM AN INNER JOIN BY COUNTY 'FIPS' SO THAT EACH COUNTY IS ASSIGNED POPULATION
az_groundwater = readRDS('data/az/az-join-time.rds') %>%
  group_by(wellid) %>%
  head(2)
install.packages('tidycensus')
