library(dplyr)
library(tidyverse)
library(ggplot2)

income_df  = read.csv(file="datasets/income.csv")
crime_df  = read.csv(file="datasets/crime.csv")
service_df = read.csv(file="datasets/service.csv")

# glimpse(income_df)
# glimpse(crime_df)
# glimpse(service_df)

glimpse(income_df)
summary(income_df)
summary(crime_df)
summary(service_df)

income_df[order(income_df$Median.Household.Income),]
# Basically, fed hill inner harbor and canton are good places to live(90k+)
#
# Bad places are oldtown, middle east, upton, druid heights
