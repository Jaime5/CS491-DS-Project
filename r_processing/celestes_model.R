library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gam)
library(glmnet)

set.seed(10201)

income_df = read.csv(file="../datasets/processed_data/income.csv")
compact_df = read.csv(file="../datasets/processed_data/myData.csv")

incomes = c()

for(i in 1:nrow(compact_df)) {
    row = compact_df[i,]
    n = row$neighborhood
    income = income_df[income_df$Neighborhood==n,]
    incomes = c(incomes, income$Median.Household.Income)
}

compact_df$incomes = incomes

train = sample(nrow(compact_df), nrow(compact_df) * .7)
test = (-train)
compact.test = compact_df[test]

lm.fit = lm(crime_count ~ doty + service_request_count + incomes, data=compact_df, subset=train)

# lm.fit = lm(crime_count ~ doty, data=compact_df, subset=train)
lm.fit = lm(crime_count ~ incomes, data=compact_df, subset=train)

lm.pred = predict(lm.fit, newx=compact_df[test,])

mean((lm.pred - compact.test)^ 2)

plot(lm.fit)
summary(lm.fit)

plot(compact_df$incomes, compact_df$crime_count)
abline(lm.fit)

# ============== Begin doing clustering of data ==============

sample_df = cbind(compact_df)

sample_df.sample = sample_n(sample_df, 35)
neighborhoods = sample_df.sample$neighborhood
drops = c("X", "neighborhood")
sample_df.sample = sample_df.sample[,!(names(sample_df.sample) %in% drops)]
sample_df.sample = scale(sample_df.sample)
sample_df.dist = dist(sample_df.sample)

hc.complete = hclust(sample_df.dist, method="complete")
# neighcompact_df$neighborhood

# hclust(sample_df.dist), labels=neighborhoods, main="Complete Linkage")
