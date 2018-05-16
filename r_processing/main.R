library(dplyr)
library(tidyverse)
library(ggplot2)

income_df = read.csv(file="../datasets/processed_data/income.csv")
crime_df = read.csv(file="../datasets/processed_data/crime.csv")
service_df = read.csv(file="../datasets/processed_data/service.csv")

income_df[order(income_df$Median.Household.Income),]
# High Income: Canton, Federal Hill, Inner Harbor
# good places to live(90k+) (mhhi)
canton.crime = crime_df[crime_df$Neighborhood=="canton",]
canton.income = income_df[income_df$Neighborhood=="canton",]
canton.service = service_df[service_df$Neighborhood=="canton",]
canton.ratio = nrow(canton.crime) / canton.income$Total.Population # 71 per 1000 crimes
canton.average_wait_days = mean(canton.service$Time.Delta.in.secs) / 60 / 60 / 24

canton.ratio

plot(count(canton.crime, Day.of.the.Year))
plot(count(canton.crime, Week.of.the.Year))


druid_heights.crime = crime_df[crime_df$Neighborhood=="druid heights",]
druid_heights.income = income_df[income_df$Neighborhood=="druid heights",]
druid_heights.service = service_df[service_df$Neighborhood=="druid heights",]
druid_heights.ratio = nrow(druid_heights.crime) / druid_heights.income$Total.Population
druid_heights.average_wait_days = mean(druid_heights.service$Time.Delta.in.secs) / 60 / 60 / 24

druid_heights.ratio
druid_heights.average_wait_days

plot(count(druid_heights.crime, Day.of.the.Year))
plot(count(druid_heights.crime, Week.of.the.Year))
