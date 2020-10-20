library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

# When writing code, name of the csv file is changed as "reviews.csv" for convinience, data is the same
reviews <- read.csv("reviews.csv", header=TRUE)

#hypothesis test: 1 
#1)	Restaurants, except nightlife & pub types, that have bike parking have higher “stars” on average 
# than the restaurants that does not offer bike parking

#filter out the reviews about Nightlife & Pubs
reviews_wo_pubs <- reviews %>%   
  filter(cuisine != 'Nightlife & Pubs')
View(reviews_wo_pubs)

#reviews_wo_bars <- df_without_outliers %>%   
#  filter(cuisine != 'Nightlife & Pubs')
#View(reviews_wo_bars)

# gather samples for shapiro-wilk test
sample_business_avg_stars <- reviews_wo_pubs[sample(nrow(reviews_wo_pubs), 5000), "business_avg_stars"]
sample_bike_parking <- reviews_wo_pubs[sample(nrow(reviews_wo_pubs), 5000), "bike_parking"]
sample_useful <- reviews_wo_bars[sample(nrow(reviews_wo_bars), 5000), "useful"]
sample_n_friends <- reviews_wo_bars[sample(nrow(reviews_wo_bars), 5000), "n_friends"]

#3. Conduct the Shapiro-Wilk test on the smaller sample
shapiro.test(sample_business_avg_stars)

# In case of two independent samples, need to test the equality of variances
fligner.test(business_avg_stars ~ bike_parking, data = reviews_wo_pubs)
t.test(business_avg_stars ~ bike_parking, data=reviews_wo_pubs, var.equal=FALSE)

#with biking park
reviews_wo_pubs_1_bike <- reviews_wo_pubs %>%   
  filter(bike_parking == 1)
View(reviews_wo_pubs_1_bike)

#without biking park
c <- reviews_wo_pubs %>%   
  filter(bike_parking == 0)
View(reviews_wo_pubs_0_bike)

View(reviews_wo_pubs_1_bike)
View(reviews_wo_pubs_0_bike)

#obtain the same size df to compare ranks
sample_reviews_wo_pubs_0_bike<- reviews_wo_pubs_0_bike[sample(nrow(reviews_wo_pubs_0_bike), 65904), "bike_parking"]
View(sample_reviews_wo_pubs_0_bike)

#rank in asc order for business_avg_stars
reviews_wo_pubs_1_bike <-  reviews_wo_pubs_1_bike %>%
  arrange(business_avg_stars)


sample_reviews_wo_pubs_0_bike <-
  arrange(reviews_wo_pubs_0_bike)


res <- wilcox.test(sample_reviews_wo_pubs_0_bike$business_avg_stars, reviews_wo_pubs_1_bike$business_avg_stars)
res

#hypothesis test: 2 
#2)	Wheelchair accessible restaurants, except nightlife & pub types, 
#recieve higher “stars” on average than the restaurants that are not wheelchair accessible.

fligner.test(business_avg_stars ~ tenure, data = reviews_wo_pubs)
t.test(business_review_count ~ bike_parking, data=reviews_wo_pubs, var.equal=TRUE)

require(graphics)

plot(business_review_count ~ bike_parking, data = reviews_wo_pubs)

plot(business_avg_stars ~ wheelchair_accessible, data = reviews_wo_pubs)
fligner.test(business_avg_stars ~ wheelchair_accessible, data = reviews_wo_pubs)
t.test(business_avg_stars ~ wheelchair_accessible, data=reviews_wo_pubs, var.equal=FALSE)

#with wheelchair 
reviews_wo_pubs_1_wheelchair <- reviews_wo_pubs %>%   
  filter(wheelchair_accessible == 1)
View(reviews_wo_pubs_1_wheelchair)

#without wheelchair 
reviews_wo_pubs_0_wheelchair <- reviews_wo_pubs %>%   
  filter(wheelchair_accessible == 0)
View(reviews_wo_pubs_0_wheelchair)



sample_wheelchair_accessible<- reviews_wo_pubs_0_wheelchair[sample(nrow(reviews_wo_pubs_0_wheelchair), 65904), "wheelchair_accessible"]
sample_reviews_wo_pubs_0_bike <-   arrange(reviews_wo_pubs_0_bike)


res2 <- wilcox.test(reviews_wo_pubs_0_wheelchair$business_avg_stars, reviews_wo_pubs_1_wheelchair$business_avg_stars)
res2

#hypothesis test: 4
# 4)	Users with lower average stars receive more useful votes for their reviews.
cor.test(reviews$user_avg_stars, reviews$useful, method="pearson")

# Check if the two variables follow normal distributions
sample_useful <- reviews[sample(nrow(reviews), 5000), "useful"]
sample_user_avg_stars <- reviews[sample(nrow(reviews), 5000), "user_avg_stars"]
#sample_tenure <- df_without_outliers[sample(nrow(df_without_outliers), 5000), "tenure"]


shapiro.test(sample_useful)
shapiro.test(sample_user_avg_stars)

# Spearman correlation coefficient and 
# corresponding statsitical test
cor.test(reviews$user_avg_stars, reviews$useful, method="spearman")


#hypothesis test: 6
# 6)	Reviews that are written by users that have higher tenure are found more useful.
cor.test(reviews$tenure, reviews$useful, method="pearson")

# Check if the two variables follow normal distributions
sample_useful <- reviews[sample(nrow(reviews), 5000), "useful"]
sample_tenure <- reviews[sample(nrow(reviews), 5000), "tenure"]
#sample_tenure <- df_without_outliers[sample(nrow(df_without_outliers), 5000), "tenure"]

shapiro.test(sample_useful)
shapiro.test(sample_tenure)

# Spearman correlation coefficient and 
# corresponding statsitical test
res <- cor.test(df_without_outliers$tenure, df_without_outliers$useful, method="spearman")
res

#7	Reviews that are written by users that have elite badge receive useful votes more. 
cor.test(reviews$tenure, reviews$useful, method="pearson")

# Check if the two variables follow normal distributions
sample_useful <- reviews[sample(nrow(reviews), 5000), "useful"]
sample_tenure <- reviews[sample(nrow(reviews), 5000), "tenure"]
#sample_tenure <- df_without_outliers[sample(nrow(df_without_outliers), 5000), "tenure"]

shapiro.test(sample_useful)
shapiro.test(sample_tenure)

# Spearman correlation coefficient and 
# corresponding statsitical test
res <- cor.test(df_without_outliers$tenure, df_without_outliers$useful, method="spearman")
res

# gather samples for shapiro-wilk test
sample_business_avg_stars <- reviews_wo_pubs[sample(nrow(reviews_wo_pubs), 5000), "business_avg_stars"]
sample_bike_parking <- reviews_wo_pubs[sample(nrow(reviews_wo_pubs), 5000), "bike_parking"]
sample_useful <- reviews_wo_bars[sample(nrow(reviews_wo_bars), 5000), "useful"]
sample_n_friends <- reviews_wo_bars[sample(nrow(reviews_wo_bars), 5000), "n_friends"]


# Pearson correlation coefficient and 
# corresponding statsitical test
cor.test(reviews$business_review_count, reviews$user_avg_stars, method="spearman")
cor.test(df_without_outliers$business_review_count, df_without_outliers$user_avg_stars, method="spearman")

df_without_outliers
# relation does not seem as if it is following a normal distr.
qplot(fill = groups, x = y, data = obs.data, geom = "density", 
      alpha = I(0.5),
      adjust = 1.5, 
      xlim = c(-4, 6))

plot(reviews$business_avg_stars, reviews$business_review_count, main='Avg Rev. Count vs. Avg Stars for Businesses', 
     pch=19, ylab='Average Business Review Count', xlab='Average Business Stars')

# Check if the two variables follow normal distributions
shapiro.test(reviews$business_review_count)
shapiro.test(reviews$user_avg_stars)

# cor= 0.04550767 



#hypothesis test: 8 : t-test

# In case of two independent samples, need to test the equality of variances
fligner.test(reviews$useful, reviews$has_been_elite)

require(graphics)

plot(useful ~ has_been_elite, data = reviews)
fligner.test(useful ~ has_been_elite, data = reviews)





#filter reviews about Nightlife & Pubs
reviews_from_pubs <- reviews %>%   
  filter(cuisine == 'Nightlife & Pubs')
View(reviews_from_pubs)
