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

lm.pred = predict(lm.fit, newx=compact_df[test,])

mean((lm.pred - compact.test)^ 2)


summary(lm.fit)

plot(compact_df$incomes, compact_df$crime_count, xlab="Income", ylab="Crime Count")
abline(lm.fit)

summary(lm.fit)

ggplot(data=compact_df,
       mapping=aes(x=incomes, y=crime_count)) +
                   geom_point(color = "#006EA1") +
                   labs(title="Crime Ratio x Median Income for ALL neighborhoods, with all data points.",
        x="Income Per Area ($)",
        y="Crime Ratio (ratio)") +
    theme_light() + theme(axis.text.x=element_text(angle=90, hjust=1))



