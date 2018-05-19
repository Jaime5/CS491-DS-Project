library(dplyr)
library(tidyverse)
library(ggplot2)
library(gam)

set.seed(10201)

# Within a district, is there a correlation between the time taken to complete
# a service request and the total crime in that area?
# If so, is it possible to predict “crime” based on the district’s service requests?¶

income_df = read.csv(file="../datasets/processed_data/income.csv")
crime_df = read.csv(file="../datasets/processed_data/crime.csv")
service_df = read.csv(file="../datasets/processed_data/service.csv")

# income_df[order(income_df$Median.Household.Income),]
# # High Income: Canton, Federal Hill, Inner Harbor
# # good places to live(90k+) (mhhi)

canton.crime = crime_df[crime_df$Neighborhood=="canton",]
canton.income = income_df[income_df$Neighborhood=="canton",]
canton.service = service_df[service_df$Neighborhood=="canton",]
canton.ratio = nrow(canton.crime) / canton.income$Total.Population # 71 per 1000 crimes
canton.avg_wait_days = mean(canton.service$Time.Delta.in.secs) / 60 / 60 / 24

canton.med_wait_days = median(canton.service$Time.Delta.in.secs) / 60 / 60 / 24

plot(count(canton.crime, Day.of.the.Year))
lines(count(canton.crime, Day.of.the.Year))
plot(count(canton.crime, Week.of.the.Year))

plot(canton.crime)

druid_heights.crime = crime_df[crime_df$Neighborhood=="druid heights",]
druid_heights.income = income_df[income_df$Neighborhood=="druid heights",]
druid_heights.service = service_df[service_df$Neighborhood=="druid heights",]
druid_heights.ratio = nrow(druid_heights.crime) / druid_heights.income$Total.Population
druid_heights.average_wait_days = mean(druid_heights.service$Time.Delta.in.secs) / 60 / 60 / 24

druid_heights.ratio
druid_heights.average_wait_days

plot(count(druid_heights.crime, Day.of.the.Year))
plot(count(druid_heights.crime, Week.of.the.Year))

neighborhoods = c()
crime_ratios = c()
avg_svc_wait_times = c()
med_svc_wait_times = c()
n_incomes = c()

# is there a correlation between crime rate
# and

for (n in unique(income_df$Neighborhood)) {
    crime = crime_df[crime_df$Neighborhood==n,]
    income = income_df[income_df$Neighborhood==n,]
    service = service_df[service_df$Neighborhood==n,]

    ratio = nrow(crime) / income$Total.Population
    avg_wait_day = mean(service$Time.Delta.in.secs) / 60 / 60 / 24
    med_wait_day = median(service$Time.Delta.in.secs) / 60 / 60 / 24

    neighborhoods = c(neighborhoods, n)
    crime_ratios = c(crime_ratios, ratio)
    avg_svc_wait_times = c(avg_svc_wait_times, avg_wait_day)
    med_svc_wait_times = c(med_svc_wait_times, med_wait_day)
    n_incomes = c(n_incomes, income$Median.Household.Income)
    # break
}

results = data.frame(neighborhoods, crime_ratios, avg_svc_wait_times, med_svc_wait_times, n_incomes)



# plot(wow$avg_svc_wait_times, wow$crime_ratios)
# plot(wow$med_svc_wait_times, wow$crime_ratios)
# plot(wow$ratios, wow$avg_waits_day)

train = sample(nrow(results), nrow(results) * .7)

# crime rate, with service time to completion
crime.fit_1 = lm(crime_ratios ~ avg_svc_wait_times + med_svc_wait_times, data=results, subset=train)

summary(crime.fit_1)


crime.fit_2 = lm(crime_ratios ~ avg_svc_wait_times, data=results, subset=train)

summary(crime.fit_2)

crime.fit_3 = lm(crime_ratios ~ med_svc_wait_times, data=results, subset=train)

summary(crime.fit_2)

# crime rate reported, with income per area
crime.fit_4 = lm(crime_ratios ~ n_incomes, data=results, subset=train)

summary(crime.fit_4)

crime.fit_5 = lm(crime_ratios ~ n_incomes + med_svc_wait_times + avg_svc_wait_times, data=results, subset=train)

summary(crime.fit_5)

anova(crime.fit_1, crime.fit_2, crime.fit_3, crime.fit_4, test="F")

plot(results$n_incomes, results$crime_ratios, xlab="Incomes of Neighborhoods ($)", ylab="Crime Rate of Neighborhoods (ratio)")

# wow = mean((crime_ratios - predict(crime_fit, results))[-train]^2)
#
# wow.max = max(results$crime_ratios[train])
# wow.max
# wow.min = min(results$crime_ratios[train])
# wow.min
crime.fit_4 = gam(crime_ratios ~ s(n_incomes, 10), data=results, subset=train)

par(mfrow=c(1,1))
plot(crime.fit_3, se=TRUE, col="blue")
