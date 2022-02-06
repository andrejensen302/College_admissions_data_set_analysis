#Load libraries

#test123

library(arules)
library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(rpart.plot)

#Load datasets
setwd("~/Documents/RStudio_Uploads/College_admissions_data_set_analysis")
colleges_df = read.delim("colleges.tsv", sep = '\t',  header = TRUE)
colleges_data_dictionary_df = read.delim("colleges_data_dictionary.tsv", sep = '\t',  header = TRUE)

#Discretize variables

colleges_df$cost_quartiles = discretize(colleges_df$cost, 
                                        method = 'frequency',
                                        categories = 4,
                                        labels = c('cost_Q1', 'cost_Q2', 'cost_Q3', 'cost_Q4'))

colleges_df$median_debt_quartiles = discretize(colleges_df$median_debt, 
                                        method = 'frequency',
                                        categories = 4,
                                        labels = c('median_debt_Q1', 'median_debt_Q2', 'median_debt_Q3', 'median_debt_Q4'))

colleges_df$family_income_median_quartiles = discretize(colleges_df$family_income_median, 
                                               method = 'frequency',
                                               categories = 4,
                                               labels = c('median_family_income_Q1', 'median_family_income_Q2', 
                                                           'median_family_income_Q3', 'median_family_income_Q4'))

colleges_df$poverty_rate_quartiles = discretize(colleges_df$poverty_rate, 
                                                        method = 'frequency',
                                                        categories = 4,
                                                        labels = c('poverty_rate_Q1', 'poverty_rate_Q2', 
                                                                    'poverty_rate_Q3', 'poverty_rate_Q4'))

colleges_df$earnings_quartiles = discretize(colleges_df$median_earnings,
                                            method = 'frequency',
                                            categories = 4,
                                            labels = c('earnings_Q1', 'earnings_Q2',
                                                       'earnings_Q3', 'earnings_Q4'))

table(colleges_df$median_debt_quartiles)
table(colleges_df$cost_quartiles)
table(colleges_df$family_income_median_quartiles)
table(colleges_df$poverty_rate_quartiles)
table(colleges_df$earnings_quartiles)

summary(colleges_df$cost)
summary(colleges_df$median_debt)
summary(colleges_df$family_income_median)
summary(colleges_df$poverty_rate)

#Feature Engineering

#High poverty rate schools
help("mutate")

colleges_df = colleges_df %>% mutate(high_poverty_rate = ifelse(poverty_rate >= 10.0, TRUE, FALSE))

#High Family income schools
colleges_df = colleges_df %>% mutate(high_family_income = ifelse(family_income_median >= 80000, TRUE, FALSE))

#High student debt schools
colleges_df = colleges_df %>% mutate(high_median_debt = ifelse(median_debt >= 25000, TRUE, FALSE))


avg_earnings_poverty = colleges_df %>% na.omit() %>% 
  group_by(high_poverty_rate) %>% 
  summarize(avg_earnings = mean(median_earnings))

avg_earnings_high_family_income = colleges_df %>% na.omit() %>% 
  group_by(high_family_income) %>% 
  summarize(avg_earnings = mean(median_earnings))

avg_earnings_median_debt = colleges_df %>% na.omit() %>% 
  group_by(high_median_debt) %>% 
  summarize(avg_earnings = mean(median_earnings))

avg_earnings_poverty
avg_earnings_high_family_income
avg_earnings_median_debt

ggplot(na.omit(colleges_df), aes(x = median_earnings, fill = high_poverty_rate)) + 
  geom_histogram(alpha = 0.60) + 
  geom_vline(data = avg_earnings_poverty, aes(xintercept = avg_earnings, color = high_poverty_rate))

ggplot(na.omit(colleges_df), aes(x = median_earnings, fill = high_family_income)) + 
  geom_histogram(alpha = 0.60) + 
  geom_vline(data = avg_earnings_high_family_income, aes(xintercept = avg_earnings, color = high_family_income))

ggplot(na.omit(colleges_df), aes(x = median_earnings, fill = high_median_debt)) + 
  geom_histogram(alpha = 0.60) + 
  geom_vline(data = avg_earnings_median_debt, aes(xintercept = avg_earnings, color = high_median_debt))

#Create transaction

colleges_df_features = colleges_df %>% select(locale, control, pred_deg, historically_black, men_only,
                                           women_only, religious, online_only, high_family_income,
                                           top_ten, high_poverty_rate, high_median_debt, earnings_quartiles)

colleges_df_transactions = as(colleges_df_features, 'transactions')

inspect(head(colleges_df_transactions))
summary(colleges_df_transactions)


#Plot item frequency of transactions
is(colleges_df_transactions)
itemFrequencyPlot(colleges_df_transactions, topN = 15, cex = 0.70)

#Generate rules

#Support = 0.01, confidence = 0.60
rules_1  = apriori(colleges_df_transactions,
                 parameter = list(sup = 0.01, conf = 0.6, target = 'rules'))

#Support = 0.01, confidence = 0.10
rules_3 = apriori(colleges_df_transactions,
                  parameter = list(sup = 0.01, conf = 0.10, target = 'rules'))

#Support = 0.10, confidence  = 0.60
rules_2 = apriori(colleges_df_transactions,
                   parameter = list(sup = 0.10, conf = 0.6, target = 'rules'))

inspect(head(rules_1))
inspect(head(rules_3))
inspect(head(rules_2))


summary(rules_2)
summary(rules_3)

#Filtering and inspecting patterns for bottom quarter of earners

inspect(rules_1)

low_earners = subset(rules_1, subset = lhs %in% 'earnings_quartiles=earnings_Q1' & lift > 1.0)

inspect(head(low_earners, by = 'lift'))
inspect(low_earners)
subset(low_earners)


