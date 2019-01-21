library(tidyverse)
library(data.table)
library(surveillance)
library(writexl)
library(rgeos)
library(maptools)
library(broom)
library(lubridate)
library(ISOweek)

# You will need to change your working directory
#setwd("/git/xx_01")

fileSources = file.path("code", list.files("code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)


# Reading in raw data
#CreateFakeData()
raw_data <- readRDS("data_raw/individual_level_data.RDS")
municipalities <- readRDS("data_raw/norwayLocations.RDS")

# Grouping data by location and date. We want to make sure every date appears
data_by_location_date = groupByDate(raw_data)
  
# Add isoWeek and iso Year
data_by_location_date = addIsoWeekYear(data_by_location_date)

# Group by municipality and year and week
data_by_location_week = data_by_location_date[, list(observed=sum(observed)) , by=.(location, year, week)]

# Find outbreaks and create overview plots for each municipality
# Used the CreateFakeData.R code to understand the regression model. Will use a poisson regression model on the day level where:
# lambda = (year - 2000) + month_i I_i, where I_i is a indicator if the date is in month 1:12.
# In the create fake data the dependence on year is given by exp(1.2 + 0.01(year - 2000)), but using a taylor expansion
# of the exponential which leads to very little error for years up to 2010 this reduces to exp(1.2)(1 + 0.01(year - 2000))
# Will fit for each day and then aggregate to the weekly level as the generative model was on a daily level.

createMunicipalityOverviewOutbreak(data_by_location_date, municipalities)

# Creative task
# For this create task I aim to present two different aspects of Disease X
# 1. How the number of cases of the disease changes over time
# 2. How the number of cases of the disease is distributed geographically

# Country level overview

createCountryOverview(data_by_location_date, "results/creative_assignment/temporal_overview.png")

# Country map and breakdown by region

data_since_49 = prepareEnrichLastWeeksbyMunicipality(data_by_location_date, municipalities, 49)

createCountryTable(data_since_49,"results/creative_assignment/county_table_last_3_weeks.xlsx")

createMunicipalityMap(data_since_49, "results/creative_assignment/municipality_map.png", "49-52")

  
