# College admissions data set exploration and analysis #
<a href="https://www.kaggle.com/samsonqian/college-admissions">College Admissions dataset posted on kaggle.com</a>

The following is some of my basic observations and analysis that I found within the College Admissions dataset.

Libraries Used
```
library(dplyr)
library(ggplot2)
library(ggdendro)
library(stringr)
```



- The code snippet below showed that there are 4,494 distinct institution names in the original dataset.
<table><tbody><tr>
<td>Distinct universities (original dataset)
distinct_universities_college_features <- distinct(college_features, institution_name, keep_all = TRUE)</td>
</tr></tbody></table>


I first wanted to analyze the data on a more granular level based on similar attributes (in this case it would be colleges that have a high number of low-income and first generation college college attendees. I broke down the data into three clusters different clusters using Kmeans Clustering as shown in the table below.
 
### Kmeans Clustering Three Cluster Table ###
<img src ="https://github.com/andrejensen302/College_admissions_data_set_analysis/blob/726ffc5151957e5118293b08ddb456865e53210b/misc_images/kmeans_3cluster_family_income.png">

- The chart below shows the same clusters as the table above but plots family income against the median post-graduate earnings for each dataset.

### Kmeans Clustering Three Cluster GGplot ###
<img src ="https://github.com/andrejensen302/College_admissions_data_set_analysis/blob/13295b4fa6a5e57993a504cdff39ec55b197119b/College-admissions-Rmarkdown_files/figure-gfm/unnamed-chunk-4-1.png">

- The chart and table below breaks it down into five different clusters.

### Kmeans Clustering Five Cluster Table ###
<img src="https://github.com/andrejensen302/College_admissions_data_set_analysis/blob/master/misc_images/kmeans_5cluster_family_income.png">

### Kmeans Clustering Five Cluster GGplot ###
<img src="https://github.com/andrejensen302/College_admissions_data_set_analysis/blob/13295b4fa6a5e57993a504cdff39ec55b197119b/College-admissions-Rmarkdown_files/figure-gfm/unnamed-chunk-5-1.png">

- As we can see in both of table and ggplots, the majority of the clusters did not cross the $60,000 threshold for family earnings and $50,000/year post-graduate student earnings.
