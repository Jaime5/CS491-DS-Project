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

serviceSummary = service_df %>% group_by(Method.Received) %>% summarize(count = n())
print(serviceSummary)

crimeSummary = crime_df %>% group_by(Description) %>% summarize(count = n())
print(crimeSummary)

neighborhoods = c()
crime_ratios = c()
avg_svc_wait_times = c()
med_svc_wait_times = c()
n_incomes = c()

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

ggplot(data = results, mapping = aes(x = neighborhoods, y = crime_ratios)) +
    geom_col(color = "#006EA1") +
    labs(title = "Median Time to Complete Service Requests in Baltimore City",
         x = "Neighborhood",
         y = "Median Time (Days)") +
    theme_light() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = results, mapping = aes(x = neighborhoods, y = avg_svc_wait_times)) +
    geom_col(color = "#006EA1") +
    labs(title = "Median Time to Complete Service Requests in Baltimore City",
         x = "Neighborhood",
         y = "Median Time (Days)") +
    theme_light() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = results, mapping = aes(x = neighborhoods, y = med_svc_wait_times)) +
    geom_col(color = "#006EA1") +
    labs(title = "Median Time to Complete Service Requests in Baltimore City",
         x = "Neighborhood",
         y = "Median Time (Days)") +
    theme_light() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = results, mapping = aes(x = neighborhoods, y = n_incomes)) +
    geom_col(color = "#006EA1") +
    labs(title = "Median Time to Complete Service Requests in Baltimore City",
         x = "Neighborhood",
         y = "Median Time (Days)") +
    theme_light() + theme(axis.text.x = element_text(angle = 90, hjust = 1))



train = sample(nrow(results), nrow(results) * .7)

# crime rate, with service time to completion
crime.fit_1 = lm(crime_ratios ~ avg_svc_wait_times + med_svc_wait_times, data=results, subset=train)

summary(crime.fit_1)


crime.fit_2 = lm(crime_ratios ~ avg_svc_wait_times, data=results, subset=train)

summary(crime.fit_2)

crime.fit_3 = lm(crime_ratios ~ med_svc_wait_times, data=results, subset=train)

summary(crime.fit_3)

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


# ================== BEGIN PROCESSING CELESTES DATA ==========
new_crime_df = read.csv(file="../datasets/processed_data/myData.csv")

new_incomes = c()

for(i in 1:nrow(new_crime_df)) {
    row = new_crime_df[i,]
    n = row$neighborhood
    income = income_df[income_df$Neighborhood==n,]
    new_incomes = c(new_incomes, income$Median.Household.Income)
}

new_crime_df$incomes = new_incomes

glimpse(new_crime_df)

crime.fit_6 = lm(crime_count ~ service_request_count, data=new_crime_df)

plot(new_crime_df$service_request_count,new_crime_df$crime_count)
#
crime.fit_7 = lm(crime_count ~ service_request_count + doty + incomes, data=new_crime_df)

plot(new_crime_df$incomes, new_crime_df$crime_count)
points(new_crime_df$service_request_count, new_crime_df$crime_count)
points(new_crime_df$doty, new_crime_df$crime_count)
abline(crime.fit_7)
# plot(new_crime_df$incomes, new_crime_df$crime_count)

summary(crime.fit_7)
#
# crime.fit_6 = lm(crime_count ~ service_request_count, data=celestes_crime_df)
# #
# summary(crime.fit_6)
#
# plot(celestes_crime_df$doty, celestes_crime_df$crime_count)
# abline(crime.fit_6)
#
# crime.fit_7 = lm(crime_count ~ service_request_count + doty, data=celestes_crime_df)


#
# plot(celestes_crime_df$service_request_count, celestes_crime_df$crime_count)
#
# plot(celestes_crime_df$doty, celestes_crime_df$crime_count)
# abline(crime.fit_6)


# Neeed to get rid of outliers to fix data.
