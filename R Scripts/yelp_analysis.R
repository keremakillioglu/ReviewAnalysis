library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

# When writing code, name of the csv file is changed as "reviews.csv" for convinience, data is the same
reviews <- read.csv("reviews.csv", header=TRUE)

# Look at the incomplete cases: Missing values represented by 'NA's
dt.missing <- reviews[!complete.cases(reviews), ]
View(dt.missing) 
# No incomplete cases found

# Look at the complete cases
result_complete_cases <- complete.cases(reviews)
View(result_complete_cases)
# All cases are complete

# Duplicated rows are checked, if any exist
duplicates= duplicated(reviews) 
View(duplicates)

# Observed that all data is distinct by the code below
dist =distinct(reviews)
View(dist)

# Print number of data which has complete ratings
sprintf("Number of data rows which has complete ratings: %s",  sum(complete.cases(reviews)))

# Print number of data which has incomplete ratings
sprintf("Number of data rows which has incomplete ratings: %s",  sum(!complete.cases(reviews)))

# No missing data found, general data information is obtained
colnames(reviews)
head(reviews)
summary(reviews)
View(reviews)

#Summary statistics of the numerical variables: Using built-in function 'summary'
summary(reviews[, c("useful", "business_review_count", "user_review_count", "n_friends","tenure", 
                       "stars", "business_avg_stars", "user_avg_stars")])

#Subset all numerical variables into another data frame
reviews.numerical <- reviews[, c("useful", "business_review_count", "user_review_count", "n_friends","tenure", 
                                 "stars", "business_avg_stars", "user_avg_stars")]

# Distinct number of restaurants in the dataset
unique_restaurants <- unique(reviews$business_id)
length(unique_restaurants)
View(unique_restaurants)

# Distinct number of users in the dataset
unique_users <- unique(reviews$user_id)
length(unique_users)

library(stargazer)
#Get a descriptive table with all numerical variables
stargazer(reviews.numerical, type="text")

# Printout the summary in word format to add it our report
stargazer(reviews.numerical, type="html", median=TRUE, out="reviews.numerical.summary.table.doc")

# for each unique restaurant, retrieve cuisines
unique_restaurants_with_cuisines <- distinct(reviews, business_id, cuisine)
nrow(unique_restaurants_with_cuisines)
View(unique_restaurants_with_cuisines)

#Tabluate cuisine variable
table(reviews$cuisine)

library(forcats)
# generate plot the see distribution of reviews to different cuisine 
ggplot(reviews, aes(x = fct_infreq(cuisine), fill = fct_infreq(cuisine))) +
  geom_bar(width = 1, colour = "black", show.legend = FALSE) +
  xlab("Cuisine Type") + ylab("Reviews Recieved")

#Tabluate bike parking variable
table(reviews$bike_parking)

# Pie Chart with Percentages for Bike Parking
slices <- c(145158  , 73689)
answer <- c("No", "Yes")
pct <- round(slices/sum(slices)*100)
answer <- paste(answer, pct) # add percents to labels
answer <- paste(answer,"%",sep="") # ad % to labels
pie(slices,labels = answer, col=rainbow(length(lbls)),
    main="Is Bike Parking Allowed")

# Nightlife type restaurants & bike parking relationship
nightlife_and_parking <- table(reviews$cuisine=="Nightlife & Pubs", reviews$bike_parking)
nightlife_and_parking

#Wheelchair Accessibility
table(reviews$wheelchair_accessible)

# Pie Chart with Percentages for Wheelchair Accessibility
slices <- c(168169  , 50678)
answer <- c("No", "Yes")
pct <- round(slices/sum(slices)*100)
answer <- paste(answer, pct) # add percents to labels
answer <- paste(answer,"%",sep="") # ad % to labels
pie(table(reviews$wheelchair_accessible), main='Is Wheelchair Accessible', labels = answer, 
    col=c('darkgrey', 'darkgreen'))

# Being Elite
table(reviews$has_been_elite)

# Avg Rev. Count vs. Avg Stars for Businesses
plot(reviews$business_avg_stars, reviews$business_review_count, main='Avg Rev. Count vs. Avg Stars for Businesses', 
     pch=19, ylab='Average Business Review Count', xlab='Average Business Stars')

# remove Nightlife & Pub containing rows since some of our hypotheses need this information
reviews_wo_pubs <- reviews %>%   
                  filter(cuisine != 'Nightlife & Pubs')
View(reviews_wo_pubs)

#Encode wheelchair_accessible, cuisine and bike_parking as factors for original df
reviews$cuisine <- factor(reviews$cuisine)
reviews$wheelchair_accessible <- factor(reviews$wheelchair_accessible)
reviews$bike_parking <- factor(reviews$bike_parking)
reviews$has_been_elite <- factor(reviews$has_been_elite)

#Encode wheelchair_accessible, cuisine and bike_parking as factors for df without Nightlife & Pub
reviews_wo_pubs$cuisine <- factor(reviews_wo_pubs$cuisine)
reviews_wo_pubs$wheelchair_accessible <- factor(reviews_wo_pubs$wheelchair_accessible)
reviews_wo_pubs$bike_parking <- factor(reviews_wo_pubs$bike_parking)
reviews_wo_pubs$has_been_elite <- factor(reviews_wo_pubs$has_been_elite)


qplot(x = reviews_wo_pubs$bike_parking, y = reviews_wo_pubs$business_review_count,
      geom = "boxplot", data = reviews_wo_pubs,
      xlab = "Bike Parking", 
      ylab = "Business Review Count",
      fill = I("lightblue"))

qplot(x = reviews$bike_parking, y = reviews$business_avg_count,
      geom = "boxplot", data = reviews,
      xlab = "Bike Parking", 
      ylab = "Business Review Count",
      fill = I("lightblue"))

qplot(x = reviews$wheelchair_accessible, y = reviews$business_review_count,
      geom = "boxplot", data = reviews,
      xlab = "wheelchair_accessible", 
      ylab = "business_review_count",
      fill = I("lightblue"))

qplot(x = reviews$has_been_elite, y = reviews$useful,
      geom = "boxplot", data = reviews,
      xlab = "has_been_elite", 
      ylab = "useful",
      fill = I("lightblue"))

#Create Boxplot for every numerical variable
#boxplot for stars
boxplot(reviews$stars, main="Boxplot of Reviews' Stars", 
        ylab="Stars")
#boxplot for useful votes received 
boxplot(reviews$useful, main="Boxplot of Reviews' Useful Votes", 
        ylab="Useful Votes")
##################################### REMOVING OUTLIERS
outlier_useful <- boxplot(reviews$useful)$out
# Print out the outlier values
outlier_useful
reviews_without_useful_outliers <- reviews[!(reviews$useful %in% outlier_useful), ]
View(reviews_without_useful_outliers)
boxplot(reviews_without_useful_outliers$useful, main="Boxplot of Reviews' Useful Votes (-out)", 
        ylab="Useful Votes")

df_without_outliers <- reviews[!(reviews$useful %in% outlier_useful), ]
View(df_without_outliers)
##################################### REMOVING OUTLIERS


#boxplot for business review count
boxplot(reviews$business_review_count, main="Reviews' Business Review Count", 
        ylab="Business Review Count")
##################################### REMOVING OUTLIERS
outlier_business_review_count <- boxplot(reviews$business_review_count)$out
# Print out the outlier values
outlier_business_review_count

df_without_outliers <- df_without_outliers[!(df_without_outliers$business_review_count %in% outlier_business_review_count), ]
View(df_without_outliers)
#one more iteration

outlier_business_review_count <- boxplot(df_without_outliers$business_review_count)$out
# Print out the outlier values
df_without_outliers
df_without_outliers <- df_without_outliers[!(df_without_outliers$business_review_count %in% outlier_business_review_count), ]
View(df_without_outliers)
##################################### REMOVING OUTLIERS
boxplot(df_without_outliers$business_review_count, main="Reviews' Business Review Count", 
        ylab="Business Review Count")
##################################### NEW BOXPLOT PRINTED


#boxplot for average business stars 
boxplot(reviews$business_avg_stars, main="Average Business Stars for Reviews", 
        ylab="Average Business Stars")
##################################### REMOVING OUTLIERS
outlier_business_avg_stars <- boxplot(df_without_outliers$business_avg_stars)$out
# Print out the outlier values
outlier_business_avg_stars
df_without_outliers <- df_without_outliers[!(df_without_outliers$business_avg_stars %in% outlier_business_avg_stars), ]
View(df_without_outliers)
##################################### REMOVING OUTLIERS

boxplot(df_without_outliers$business_avg_stars, main="Average Business Stars for Reviews", 
        ylab="Average Business Stars")
##################################### NEW BOXPLOT PRINTED

#boxplot for average user stars 
boxplot(reviews$user_avg_stars, main="Average Business Stars for Reviews", 
        ylab="Average Business Stars")
##################################### REMOVING OUTLIERS
outlier_user_avg_stars<- boxplot(reviews$user_avg_stars)$out
# Print out the outlier values
outlier_user_avg_stars
df_without_outliers <- df_without_outliers[!(df_without_outliers$user_avg_stars %in% outlier_user_avg_stars), ]
View(df_without_outliers)
##################################### REMOVING OUTLIERS
#boxplot for average user stars 
boxplot(df_without_outliers$user_avg_stars, main="Average Business Stars for Reviews", 
        ylab="Average Business Stars")
##################################### NEW BOXPLOT PRINTED
#one more iteration

outlier_user_avg_stars<- boxplot(df_without_outliers$user_avg_stars)$out
# Print out the outlier values
outlier_user_avg_stars
df_without_outliers <- df_without_outliers[!(df_without_outliers$user_avg_stars %in% outlier_user_avg_stars), ]
View(df_without_outliers)

#boxplot for number of friends 
boxplot(reviews$n_friends, main="Reviewers' Number of Friends ", 
        ylab="number of friends ")
##################################### REMOVING OUTLIERS
outlier_n_friends<- boxplot(reviews$n_friends)$out
# Print out the outlier values
outlier_n_friends
df_without_outliers <- df_without_outliers[!(df_without_outliers$n_friends %in% outlier_n_friends), ]
View(df_without_outliers)
##################################### REMOVING OUTLIERS

#boxplot for reviewers' tenure 
boxplot(reviews$tenure, main="Reviewers' Tenure", 
        ylab="Number of Friends ")
outlier_tenure<- boxplot(reviews$tenure)$out
df_without_outliers <- df_without_outliers[!(df_without_outliers$tenure %in% outlier_tenure), ]
View(df_without_outliers)

boxplot(df_without_outliers$tenure, main="Reviewers' Tenure", 
        ylab="Number of Friends ")

# Printout the summary in word format to add it our report
stargazer(df_without_outliers, type="html", median=TRUE, out="df_without_outliers.summary.table.doc")
