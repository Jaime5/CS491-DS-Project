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

plot(count(canton.crime, Week.of.the.Year))
# plot()

# brooklyn.crime

# canton.service$

# neigh.high.fed_hill = crime_df[crime_df$Neighborhood=="federal hill",]
#
# num_of_weeks = seq(1, 53, 1)
# #
# # neigh.high.canton %>% group_by(num_of_weeks) %>% summarise(sum=sum(Week.of.the.Year))
# neigh.high.canton.count = count(neigh.high.canton, Week.of.the.Year)
# neigh.high.fed_hill.count = count(neigh.high.fed_hill, Week.of.the.Year)
#
# # cut(neigh.high.canton$Week.of.the.Year, breaks=53)
# cut(neigh.high.canton$Week.of.the.Year, breaks=num_of_weeks, right=TRUE)
#
# x = as.data.frame(table(neigh.high.canton$Week.of.the.Year))
# # num_of_weeks = neigh.high.canton.count$Week.of.the.Year
# #
#
# x2 = as.data.frame(table(neigh.high.fed_hill$Week.of.the.Year))
#
# # neigh.high.canton.count
#
# plot_df = data.frame(num_of_weeks,
#                      neigh.high.canton.count$n,
#                      neigh.high.fed_hill.count$n)
#
# plot(neigh.high.canton.count)
# lines(neigh.high.canton.count)
# # lines()
# #
# neigh.high.canton$
#
# # count_x_y = count(neigh.high.canton, Week.of.the.Year)
# # count_x_y
# # plot(count_x_y)
# # lines()
# # lines(count(neigh.high.canton, Week.of.the.Year))
# # plot(count(neigh.high.canton, Day.of.the.Year))
# # plot(count(neigh.high.canton, Week.of.the.Year))
# #
# # # plot(count(neigh.high.canton, Day.of.the.Year))
# # plot(count(neigh.high.fed_hill, Week.of.the.Year))
#
# # Low Income: Druid Heights, Harlem Park, Barclay
# neigh.low.druid_heights = crime_df[crime_df$Neighborhood=="druid heights",]
#
# neigh.high.druid_heights.count = count(neigh.low.druid_heights, Week.of.the.Year)
#
# plot(neigh.high.druid_heights.count)
# lines(neigh.high.druid_heights.count)
# plot(neigh.high.canton.count)
#



# neigh.low.harlem_park = crime_df[crime_df$Neighborhood=="harlem park",]
#
# neigh.low.harlem_park.count = count(neigh.low.harlem_park, Week.of.the.Year)
#
# plot(neigh.low.harlem_park.count)
# lines(neigh.high.druid_heights.count)

# plot(neigh.high.druid_heights.count)


# breaks = cut(seq(0,365, by=2), Inf)
# neigh.high.canton.table = table(neigh.high.canton$Day.of.the.Year)

# class(neigh.high.canton.table)
# plot.table(neigh.low.canton.table)
#
# table.cont(neigh.high.canton$Day.of.the.Year)
# freq(neigh.high.canton, )
# count(neigh.high.canton, 'Day.of.the.Year')
