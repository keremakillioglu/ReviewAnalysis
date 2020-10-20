library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

# When writing code, name of the csv file is changed as "reviews.csv" for convinience, data is the same
reviews <- read.csv("reviews.csv", header=TRUE)
reviews_for_2_anova <- read.csv("reviews.csv", header=TRUE)

colnames(reviews)
# runnin 2  factor anova on the following hypothesis
# The positive relationship between having a bike park for nightclub & pub type of restaurants 
#is weaker when review stars are higher.



#Encode wheelchair_accessible, cuisine is already factored, has_elite and bike_parking as factors for original df
reviews$wheelchair_accessible <- factor(reviews$wheelchair_accessible)
reviews$bike_parking <- factor(reviews$bike_parking)
reviews$has_been_elite <- factor(reviews$has_been_elite)

result.aov <- aov(stars ~ cuisine, data=reviews)
summary(result.aov)
ggline(reviews, x="cuisine", y="stars", add="mean", ylim = c(1, 5))

TukeyHSD(result.aov)


#filter reviews about Nightlife & Pubs
reviews_for_2_anova$cuisine[which(reviews_for_2_anova$cuisine != "Nightlife & Pubs")] = "Others"

result.2aov <- aov(stars ~ cuisine + bike_parking + 
                     cuisine:bike_parking, data = reviews_for_2_anova)
summary(result.2aov)
library("ggpubr")

ggline(reviews_for_2_anova, x="cuisine", y="stars", linetype="cuisine", 
       add="mean", ylim = c(1, 5))

ggline(reviews, x="cuisine", y="stars", linetype="bike_parking", 
       add="mean", ylim = c(1, 5))

#### LINEAR REGRESSION
#hyp5 
# The positive relationship between number of friends and user review count 
#is stronger when reviews receive useful votes more. 

# Define a linear model and store the formula in the object 'modelA'
modelA <- (stars ~ bike_parking + wheelchair_accessible + has_been_elite + tenure + user_avg_stars
           + business_avg_stars + business_review_count + cuisine + n_friends + user_review_count )

# Estimate the above model by OLS using the sample data
# We call the function 'lm()' and store the results in 'resultA'
resultA <- lm(formula=modelA, data=reviews)
summary(resultA)

hist(residuals(resultA), breaks=50, col = "darkblue")
# Obtain the scatter plot of residuals against the fitted values of y
# 'pch' controls marker type; 'cex' controls marker size
plot(resultA, 1, pch=19, cex =.3)

# Two Formal tests of the residuals:
sample_residuals <- resultA[sample(nrow(resultA), 5000), "residuals"]

resultA$residuals

# Shapiro-Wilk normality test of the residuals
shapiro.test(resultA$residuals)

install.packages("car")

# Load the 'car' package
library("car")

# Use the 'ncvTest()' to conduct the Breusch-Pagan Test
ncvTest(resultA)

#--------------------------------------
# Multicollinearity
#--------------------------------------
# Use the 'vif()' function in the 'car' package to obtain 
# variance inflation factors (VIFs) for the independent 
# variables based on the estimation results

# Load the 'car' package
library("car")

# Use the 'vif()' function to obtain VIFs
vif(resultA)


# Define a linear model and store the formula in the object 'modelA'
modelB <- (useful ~ user_review_count+  + I(user_review_count^2) + bike_parking + wheelchair_accessible + has_been_elite + tenure + user_avg_stars
           + business_avg_stars + business_review_count + cuisine + n_friends)

resultB <- lm(formula=modelB, data=reviews)
summary(resultB)

#hyp5 
# The positive relationship between number of friends and user review count 
#is stronger when reviews receive useful votes more. 
modelC <- (useful ~ user_review_count+ n_friends + user_review_count: n_friends+ bike_parking + wheelchair_accessible + has_been_elite + tenure + user_avg_stars
           + business_avg_stars + business_review_count + cuisine)

resultC <- lm(formula=modelC, data=reviews)

# Print out the estimation results
summary(resultC)


modelD <- (stars ~ user_review_count+ n_friends + business_review_count: business_avg_stars+ bike_parking + wheelchair_accessible + has_been_elite + tenure + user_avg_stars
           + business_avg_stars + business_review_count + cuisine)

resultD <- lm(formula=modelD, data=reviews)

# Print out the estimation results
summary(resultD)

hist(residuals(resultC), breaks=50, col = "darkblue")
# Obtain the scatter plot of residuals against the fitted values of y
# 'pch' controls marker type; 'cex' controls marker size
plot(resultC, 1, pch=19, cex =.3)
vif(resultC)


#--------------------------------------
# Integrating regression results
#--------------------------------------
# Use the 'stargazer' package again
library(stargazer)

# If you work with Word, you can output table in the 'html' type and specify the output 
# file with a .doc extension
stargazer(resultC, resultA, type="html", no.space=TRUE,
          out="2.linear.regression.table.doc")
