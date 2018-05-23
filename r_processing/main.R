library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gam)
library(glmnet)

set.seed(10201)

# Within a district, is there a correlation between the time taken to complete
# a service request and the total crime in that area?
# If so, is it possible to predict “crime” based on the district’s service requests?¶

# load datasets

income_df = read.csv(file="../datasets/processed_data/income.csv")
crime_df = read.csv(file="../datasets/processed_data/crime.csv")
service_df = read.csv(file="../datasets/processed_data/service.csv")

SECS_TO_DAYS = 86400

# Sort neighborhoods based on income below.
income_df[order(income_df$Median.Household.Income),]

# # High Income: Canton, Federal Hill, Inner Harbor
# # good places to live(90k+) (mhhi)

# WE DO IT HIGH

high_neigh = "canton"
high.crime = crime_df[crime_df$Neighborhood==high_neigh,]
high.income = income_df[income_df$Neighborhood==high_neigh,]
high.service = service_df[service_df$Neighborhood==high_neigh,]
high.ratio = nrow(high.crime) / high.income$Total.Population # 71 per 1000 crimes
high.avg_wait_days = mean(high.service$Time.Delta.in.secs) / SECS_TO_DAYS
high.med_wait_days = median(high.service$Time.Delta.in.secs) / SECS_TO_DAYS

high.ratio
high.med_wait_days

# NOW WE DO LOW

plot(count(high.crime, Day.of.the.Year))
lines(count(high.crime, Day.of.the.Year))
plot(count(high.crime, Week.of.the.Year))

low_neigh = "druid_heights"
low.crime = crime_df[crime_df$Neighborhood==low_neigh,]
low.crime = income_df[income_df$Neighborhood==low_neigh,]
low.service = service_df[service_df$Neighborhood==low_neigh,]
low.ratio = nrow(low.crime) / low.income$Total.Population
low.average_wait_days = mean(low.service$Time.Delta.in.secs) / 86400

low.ratio
low.average_wait_days

plot(count(low.crime, Day.of.the.Year))
plot(count(low.crime, Week.of.the.Year))

# ====Begin processing data for all neighborhoods ===========

neighborhoods = c()
crime_ratios = c()
avg_svc_wait_times = c()
med_svc_wait_times = c()
n_incomes = c()

# is there a correlation between crime rate

for (n in unique(income_df$Neighborhood)) {
    crime = crime_df[crime_df$Neighborhood==n,]
    income = income_df[income_df$Neighborhood==n,]
    service = service_df[service_df$Neighborhood==n,]

    ratio = nrow(crime) / income$Total.Population
    avg_wait_day = mean(service$Time.Delta.in.secs) / SECS_TO_DAYS
    med_wait_day = median(service$Time.Delta.in.secs) / SECS_TO_DAYS

    neighborhoods = c(neighborhoods, n)
    crime_ratios = c(crime_ratios, ratio)
    avg_svc_wait_times = c(avg_svc_wait_times, avg_wait_day)
    med_svc_wait_times = c(med_svc_wait_times, med_wait_day)
    n_incomes = c(n_incomes, income$Median.Household.Income)
}

results = data.frame(neighborhoods, crime_ratios, avg_svc_wait_times, med_svc_wait_times, n_incomes)

# ==== Creating linear regression models for neighborhoods ===========
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

# note that there is NO correlation between income and crime

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

# ==== Using heirarchical clusting for neighborhoods ===========

results.copy = cbind(results)
drops = c("neighborhoods", "med_svc_wait_times", "avg_svc_wait_times")
# drops = c("neighborhoods", "avg_svc_wait_times")
results.copy = results.copy[,!(names(results.copy) %in% drops)]
results.scale = scale(results.copy)
results.dist = dist(results.scale)

hc.com = hclust(results.dist, method="complete")

plot(results.copy$n_incomes, results.copy$crime_ratios)

ggplot(data=results, mapping=aes(x=n_incomes, y=crime_ratios)) +
    geom_point(color = "#006EA1") +
    labs(title="Crime Ratio x Avg Median Income",
         x="Average Median Income per Household ($)",
         y="Crime Ratio") +
    theme_light() + theme(axis.text.x=element_text(angle=90, hjust=1))

plot(hc.com, main="Complete Linkage", labels=results$neighborhoods)


# plot(hclust(results.dist), labels=results$neighborhoods, main="Complete Linkage")
plot(hclust(results.dist), labels=results$neighborhoods, main="Average Linkage")
plot(hclust(results.dist), labels=results$neighborhoods, main="Single Linkage")

