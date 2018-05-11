library(dplyr)
library(tidyverse)
library(ggplot2)

income_df  = read.csv(file="../datasets/processed_data/income.csv")
crime_df  = read.csv(file="../datasets/processed_data/crime.csv")
service_df = read.csv(file="../datasets/processed_data/service.csv")

income_df[order(income_df$Median.Household.Income),]
# High Income: Canton, Federal Hill, Inner Harbor
# good places to live(90k+) (mhhi)
neigh.high.canton = crime_df[crime_df$Neighborhood=="canton",]

plot(count(neigh.high.canton, Day.of.the.Year))




neigh.high.fed_hill = crime_df[crime_df$Neighborhood=="federal hill",]

# Low Income: Druid Heights, Harlem Park, Barclay
neigh.low.druid_heights = crime_df[crime_df$Neighborhood=="druid heights",]
neigh.low.harlem_park = crime_df[crime_df$Neighborhood=="harlem park",]



# breaks = cut(seq(0,365, by=2), Inf)
# neigh.high.canton.table = table(neigh.high.canton$Day.of.the.Year)

# class(neigh.high.canton.table)
# plot.table(neigh.low.canton.table)
#
# table.cont(neigh.high.canton$Day.of.the.Year)
# freq(neigh.high.canton, )
# count(neigh.high.canton, 'Day.of.the.Year')

# neigh.high.canton =

glimpse(income_df)
glimpse(crime_df)
glimpse(service_df)

# summary(income_df)
# summary(crime_df)
# summary(service_df)
