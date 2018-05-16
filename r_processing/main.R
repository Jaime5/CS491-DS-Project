library(dplyr)
library(tidyverse)
library(ggplot2)

# Within a district, is there a correlation between the time taken to complete
# a service request and the total crime in that area?

# If so, is it possible to predict “crime” based on the district’s service requests?¶

income_df = read.csv(file="../datasets/processed_data/income.csv")
crime_df = read.csv(file="../datasets/processed_data/crime.csv")
service_df = read.csv(file="../datasets/processed_data/service.csv")

# income_df[order(income_df$Median.Household.Income),]
# # High Income: Canton, Federal Hill, Inner Harbor
# # good places to live(90k+) (mhhi)
# canton.crime = crime_df[crime_df$Neighborhood=="canton",]
# canton.income = income_df[income_df$Neighborhood=="canton",]
# canton.service = service_df[service_df$Neighborhood=="canton",]
# canton.ratio = nrow(canton.crime) / canton.income$Total.Population # 71 per 1000 crimes
# canton.average_wait_days = mean(canton.service$Time.Delta.in.secs) / 60 / 60 / 24
#
# canton.ratio
#
# plot(count(canton.crime, Day.of.the.Year))
# plot(count(canton.crime, Week.of.the.Year))
#
#
# druid_heights.crime = crime_df[crime_df$Neighborhood=="druid heights",]
# druid_heights.income = income_df[income_df$Neighborhood=="druid heights",]
# druid_heights.service = service_df[service_df$Neighborhood=="druid heights",]
# druid_heights.ratio = nrow(druid_heights.crime) / druid_heights.income$Total.Population
# druid_heights.average_wait_days = mean(druid_heights.service$Time.Delta.in.secs) / 60 / 60 / 24
#
# druid_heights.ratio
# druid_heights.average_wait_days
#
# plot(count(druid_heights.crime, Day.of.the.Year))
# plot(count(druid_heights.crime, Week.of.the.Year))

districts = c()
crime_ratios = c()
avg_svc_wait_times = c()
med_svc_wait_times = c()

for (dist in unique(income_df$Neighborhood)) {
    crime = crime_df[crime_df$Neighborhood==dist,]
    income = income_df[income_df$Neighborhood==dist,]
    service = service_df[service_df$Neighborhood==dist,]
    ratio = nrow(crime) / income$Total.Population
    avg_wait_day = mean(service$Time.Delta.in.secs) / 60 / 60 / 24

    med_wait_day = median(service$Time.Delta.in.secs) / 60 / 60 / 24

    districts = c(districts, dist)
    crime_ratios = c(crime_ratios, ratio)
    avg_svc_wait_times = c(avg_svc_wait_times, avg_wait_day)
    med_svc_wait_times = c(med_svc_wait_times, med_wait_day)
    # break
}

wow = data.frame(districts, crime_ratios, avg_svc_wait_times, med_svc_wait_times)

plot(wow$avg_svc_wait_times, wow$crime_ratios)
plot(wow$med_svc_wait_times, wow$crime_ratios)
# plot(wow$ratios, wow$avg_waits_day)
