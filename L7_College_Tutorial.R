#Tutorial - Introduction

library(arules)
library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(rpart.plot)

#Load file
setwd("~/Documents/RStudio_Uploads/College_admissions_data_set_analysis")
colleges_df = read.delim("colleges.tsv", sep = '\t',  header = TRUE)

#Tutorial - Discretize Featuress

#Dicretize function (make continuous variables discrete)

help("discretize")

colleges_df$cost_quartiles = discretize(colleges_df$cost, 
                                     method = 'frequency',
                                     categories = 4,
                                     labels = c ('cost_Q1', 'cost_Q2', 'cost_Q3', 'cost_Q4'))

#Show quartiles for cost
table(colleges_df$cost_quartiles)
summary(colleges_df$cost)

#tutorial - associatio rules 3: Featuring Engineering STEM schools

#Feature engineering

#High STEM schools

colleges_df = colleges_df %>% mutate(stem_perc =  architecture_major_perc + comm_tech_major_perc + 
                                       computer_science_major_perc + engineering_major_perc + 
                                       eng_tech_major_perc + bio_science_major_perc + 
                                       math_stats_major_perc,
                                     high_stem = ifelse(stem_perc >= 0.30, TRUE, FALSE))

avg = colleges_df %>% na.omit() %>% 
  group_by(high_stem) %>% 
  summarize(avg_earnings = mean(median_earnings))

avg

ggplot(na.omit(colleges_df), aes(x = median_earnings, fill = high_stem)) + 
  geom_histogram(alpha = 0.60) + 
  geom_vline(data = avg, aes(xintercept = avg_earnings, color = high_stem))

#Tutorial: Constuct Rules

colleges_features = colleges_df %>% select(locale, control, pred_deg, historically_black, men_only,
                                           women_only, religious, online_only, cost_quartiles, high_stem,
                                           top_ten)

head(colleges_features)

#Make transactions

colleges_trans = as(colleges_features, 'transactions')
help("inspect")
help("itemFrequencyPlot")

is(colleges_trans)

inspect(colleges_trans[1])
inspect(colleges_trans[1:4])

itemFrequencyPlot(colleges_trans, topN = 10, cex = 0.70)

#Create Rules 

rules = apriori(colleges_trans,
                parameter = list(sup = 0.01, conf = 0.6, target = 'rules'))

inspect(head(rules))

#Tutorial - Filtering Rules

#filtering and inspecting the rules that you create

high_cost = subset(rules, subset = rhs %in% 'cost_quartiles=cost_Q4' & lift > 1)

inspect(head(high_cost, by = 'lift'))





