#Load libraries

library(dplyr)
library(ggplot2)
library(ggdendro)
library(stringr)

#Set seed for consistent output
set.seed(1234)

#Load datasets
setwd("~/Documents/UW Data Analytics")
colleges_df = read.delim("colleges.tsv", sep = '\t',  header = TRUE)
colleges_data_dictionary_df = read.delim("colleges_data_dictionary.tsv", sep = '\t',  header = TRUE)

colleges_df %>% glimpse()

############R Script for section 1############

#Cluster to identity colleges to receive grant money (high numbers of low-income, fist generation college attendees)
#3 clusters

college_features = colleges_df %>% select(institution_name, first_gen_share, poverty_rate, 
                                          family_income_median, median_earnings, pell_grant_rate, top_ten) %>% na.omit() %>% distinct()

kmeans_cluster_3 = kmeans(select(college_features, -institution_name), 3)

attributes(kmeans_cluster_3)

kmeans_cluster_3$centers

college_features = college_features %>% mutate(cluster_number = kmeans_cluster_3$cluster)

college_features$cluster_number = factor(college_features$cluster_number)

is.factor(college_features$cluster_number)

grant_candidates = college_features %>% filter(cluster_number == '2')

help('distinct')

#Distinct universities (original dataset)
distinct_universities_college_features <- distinct(college_features, institution_name, keep_all = TRUE)

#distinct universities (grant receivers)
distinct_universities_grant_receivers <- distinct(grant_candidates, institution_name, keep_all = TRUE)

#Visualize Data

ggplot(college_features, aes(x = family_income_median,
                              y = median_earnings,
                              color = factor(cluster_number))) + geom_point(alpha = 0.50)

help('ggdendrogram')
help('subset')

############R Script for section 2############


college_features2 = colleges_df %>% select(institution_name, first_gen_share, poverty_rate, 
                                           family_income_median, median_earnings, pell_grant_rate, top_ten) %>% na.omit() %>% distinct()

kmeans_cluster_5 = kmeans(select(college_features2, -institution_name), 5)

college_features2 = college_features2 %>% mutate(cluster_number = kmeans_cluster_5$cluster)

attributes(kmeans_cluster_5)

kmeans_cluster_5$centers

college_features2$cluster_number = factor(college_features2$cluster_number)

#Confirm that cluster number converted to a factor variable type.
is.factor(college_features2$cluster_number)

grant_candidates2 = college_features2 %>% filter(cluster_number == '1')

#Distinct universities (original dataset)
distinct_universities_college_feature2 <- distinct(college_features2, institution_name, keep_all = TRUE)

#distinct universities (grant receivers)
distinct_universities_grant_receivers2 <- distinct(grant_candidates2, institution_name, keep_all = TRUE)

#Visualize five cluster data

ggplot(college_features2, aes(x = family_income_median,
                             y = median_earnings,
                             color = factor(cluster_number))) + geom_point(alpha = 0.50)

############R Script for section 3############

#The !is.na(sat_verbal_quartile_1) removes universities that do not have SAT admission criteria, 
#so we are looking at similar degree-granting universities.

grant_colleges = colleges_df %>% filter((!is.na(sat_verbal_quartile_1) & family_income_median < 40000 & median_earnings < 30000))

top_ten_schools = colleges_df %>% filter(top_ten == TRUE)

heir_analysis_data = rbind(grant_colleges, top_ten_schools)

major_percentage_data = heir_analysis_data %>% select(institution_name, top_ten, ends_with('_major_perc')) %>% na.omit()
                                         
#Heirarchical clustering

help(dist)

#Compute the distance between each of the vectors

euclidean = dist(select(major_percentage_data, -institution_name, -top_ten),
                 method = 'euclidean')

euclidean

hier = hclust(euclidean)

attributes(hier)

#Wrute over label #'s in hier with the institution name
hier$labels = major_percentage_data$institution_name

#Plot the dendogram

ggdendrogram(hier, rotate = TRUE, size = 2)


#Extract data from hier and replot dendrogram with color coding
dendro_data = dendro_data(hier)

dendro_data$labels = unique(merge(dendro_data$labels, select(college_features, institution_name, top_ten),
                                  by.x = 'label',
                                  by.y = 'institution_name',
                                  all.x = TRUE))

ggplot(segment(dendro_data)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_text(data = label(dendro_data),
            aes(label = label, x = x, y = 0, hjust = 0, color = top_ten),
            size = 2) + coord_flip() + scale_y_reverse(expand = c(0.25, 0)) + 
  theme_minimal() + 
  theme(legend.position = 'bottom')

###Section 3 - evaluating grant colleges vs. top 10 dataset by feature engineered indicator variables###


#Feature Engineering - grant colleges

#High poverty rate schools
help("mutate")

#High poverty rate - grant colleges
grant_colleges = grant_colleges %>% mutate(high_poverty_rate = ifelse(poverty_rate >= 10.0, TRUE, FALSE))

#High Family income schools - grant colleges
grant_colleges = grant_colleges %>% mutate(high_family_income = ifelse(family_income_median >= 70000, TRUE, FALSE))

#High student debt schools - grant colleges
grant_colleges = grant_colleges %>% mutate(high_median_debt = ifelse(median_debt >= 15000, TRUE, FALSE))

avg_earnings_median_debt_grant_colleges = grant_colleges %>% na.omit() %>% 
  group_by(high_median_debt) %>% 
  summarize(avg_earnings = mean(median_earnings))

avg_earnings_family_income_grant_colleges = grant_colleges %>% na.omit() %>% 
  group_by(high_family_income) %>% 
  summarize(avg_earnings = mean(median_earnings))

avg_earnings_poverty_rate_grant_colleges = grant_colleges %>% na.omit() %>% 
  group_by(high_poverty_rate) %>% 
  summarize(avg_earnings = mean(median_earnings))

#Feature engineering - top ten universities

#High poverty rate - top ten schools
top_ten_schools = top_ten_schools %>% mutate(high_poverty_rate = ifelse(poverty_rate >= 10.0, TRUE, FALSE))

#High Family income - top ten schools
top_ten_schools = top_ten_schools %>% mutate(high_family_income = ifelse(family_income_median >= 70000, TRUE, FALSE))

#High student debt schools - top ten schools
top_ten_schools = top_ten_schools %>% mutate(high_median_debt = ifelse(median_debt >= 15000, TRUE, FALSE))

avg_earnings_median_debt_top_ten_schools = top_ten_schools %>% na.omit() %>% 
  group_by(high_median_debt) %>% 
  summarize(avg_earnings = mean(median_earnings))

avg_earnings_family_income_top_ten_schools = top_ten_schools %>% na.omit() %>% 
  group_by(high_family_income) %>% 
  summarize(avg_earnings = mean(median_earnings))

avg_earnings_poverty_rate_top_ten_schools = top_ten_schools %>% na.omit() %>% 
  group_by(high_poverty_rate) %>% 
  summarize(avg_earnings = mean(median_earnings))







