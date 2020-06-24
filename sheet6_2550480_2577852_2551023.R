### Stats with R Exercise sheet 6

##########################
#Week 7: Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, December 9. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.


## Please write below your (and your teammates') name, matriculation number. 
## Name 1:                       AMOUH Tete Modeste Romeo
## Matriculation number1:        2550480

## Name 2:                       Md Jonybul Islam (2577852)
## Matriculation number2:        2577852

## Name 3:                       Sonja Marie Jeanne Persch
## Matriculation number3:        2551023

## Change the name of the file by adding your matriculation numbers
## (sheet06_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################

library('car')
library(reshape)
library(languageR)

#######################
### Exercise 1: Correlation
#######################

# a) Get some data - access the ratings data set in languageR and name it "data".
# The data set contains subjective frequency ratings and their length averaged over 
# subjects, for 81 concrete English nouns.

data(ratings)
data <- ratings

# b) Take a look at the data frame.
head(data)
str(data)
summary(data)


# c) Let's say you're interested in whether there is a linear relationship between 
# the word frequency of the 81 nouns and their length.
# Take a look at the relationship between the frequency and word length data by 
# means of a scatterplot (use the ggplot library for this).
library(ggplot2)

ggplot(data, aes(x = Frequency, y = Length)) + 
  geom_point()
  

# d) Judging from the graphs, do you think that word frequency and word length are 
# in any way correlated with one another?

# Judging from the graphs, yes we can say that word frequency and word length are 
# in any way correlated with one another

# e) Compute the Pearson correlation coefficient for the two variables by means 
# of cor().
# Tell R to only include complete pairs of observations.
# As a reminder: Pearson coefficient denotes the covariance of the two variables 
# divided by the product of their respective variance. 
# It is scaled between 1 (for a perfect positive correlation) to -1 (for a perfect 
# negative correlation).

cor(data$Frequency, y = data$Length, use = "everything",
    method = c("pearson"))

# f) Does the correlation coefficient suggest a small, medium or large effect?
# What about the direction of the effect?

#It suggest a small effect with a negative direction

# g) Note that we have a large number of tied ranks in word length data 
# (since there are multiple words with the length of e.g. 5).
# Thus, we might draw more accurate conclusions by setting the method to 
# Kendall's tau instead of the Pearson correlation coefficient (which is the default).
# How do you interpret the difference between these 2 correlation coefficients?

cor(data$Frequency, y = data$Length, use = "everything",
    method = c("kendall"))

#Kendall correlation measures the strength of dependence between two variables
#but the pearson correlation measure the degree of the relationship between linearly related variables

# h) What about significance? Use the more user-friendly cor.test()!
# Take a look at the output and describe what's in there.
# What do you conclude?

cor.test(data$Frequency, data$Length,
         alternative = c("two.sided", "less", "greater"),
         method = c("pearson"),
         exact = NULL, conf.level = 0.95, continuity = FALSE)


#Pearson's product-moment correlation

#data:  data$Frequency and data$Length
#t = -4.2109, df = 79, p-value = 6.685e-05
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5912247 -0.2314324
#sample estimates:
#       cor 
#-0.4281462 


# i) Finally, also calculate Spearman's rank correlation for the same data.

cor(data$Frequency, y = data$Length, use = "everything",
    method = c("spearman"))

#######################
### Exercise 2: Regression
#######################

# a) Fit a linear regression model to the data frame "data" from exercise 1 
# for the variables Frequency (outcome variable) and Length (predictor variable).
# General form: 
# "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"

mod <- lm(Length ~ Frequency, data = data)

# b) How do you interpret the output? Is the relationship between the two variables 
# positive or negative?
# Plot the data points and the regression line.

#Call:
#  lm(formula = Length ~ Frequency, data = data)

#Coefficients:
#  (Intercept)    Frequency  
#8.8793      -0.6229 
# The coefficient ß0 of the regression ist 8.8793
# The coeficient ß1 of the regression ist -0.6229
# The regression is then Length = 8.8793 - 0.6229*Frequency
# Slope: Increasing 1 Frequency ---> decreasing Length 0.6229
# Intercept: Frequency is 0 ---> Length is 8.8793
# The relationship between the two variables is negativ

# regression line plotting
ggplot(data, aes(x = Frequency, y = Length)) + 
  geom_point() + geom_abline(slope = -0.6229, intercept = 8.8793) + 
  geom_smooth(method = "lm", se = FALSE)


# c) Run the plotting command again and have R display the actual words that belong 
# to each point. 
# (Don't worry about readability of overlapping words.)

# Sorry but i don't realy understand what you are asking to do. 
# So i did like i understood it. But i think it is false. 
# if it is realy false can you explain me a little bit the wright answer in your feedback

ggplot(data, aes(x = Word, y = Length)) + 
  geom_point() + geom_abline(slope = -0.6229, intercept = 8.8793) + 
  geom_smooth(method = "lm", se = FALSE)




#######################
### Exercise 3: Regression
#######################


# a) Try this again for another example:
# Let's go back to our digsym data set.
# Set your wd and load the data frame digsym_clean.csv and store it in a variable.
# You can download this data frame from the material of week 6: t-test and friends. 

setwd('C:/Users/ra/Desktop/Romeo_doc/Uni/statistic with R')
data <- read.csv('digsym_clean.csv')
data


# b) Suppose you want to predict reaction times in the digit symbol task by 
# people's age.
# Fit a linear regression model to the data frame for the variables 
# correct_RT_2.5sd (outcome variable) and Age (predictor variable).
# General form: 
# "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
# But first we need to cast the data to compute an RT mean (use correct_RT_2.5sd) 
# for each subject, so that we have only one Age observation per Subject.
# Store the result in a new dataframe called "cast".
# In case you're wondering why we still have to do this - like the t-test, 
# linear regression assumes independence of observations.
# In other words, one row should correspond to one subject or item only.

cast <-cast(data, Subject + Age ~ ., fun.aggregate = mean, value = "correct_RT_2.5sd", na.rm = TRUE)
colnames(cast)[colnames(cast)=="(all)"] <- "correct_RT_2.5sd"
head(cast)

# c) Now fit the regression model.

mod_digsym <- lm(correct_RT_2.5sd ~ Age, data = cast)


# d) Let's go over the output - what's in there?
# How do you interpret the output?
mod_digsym

#Call:
#  lm(formula = correct_RT_2.5sd ~ Age, data = Cdata)

#Coefficients:
#  (Intercept)          Age  
#   637.93            21.22 

# The Regression function is then correct_RT_2.5sd = 21.22* Age + 637.93
# Slope: If the age increase of one year the correct_RT_2.5sd will be increased by 21.22
# Intercept: for age equal 0 the correct_RT_2.5sd will be 637.93

# e) Plot the data points and the regression line. 
ggplot(cast, aes(x = Age, y = correct_RT_2.5sd)) + 
  geom_point() + geom_abline(slope = 21.22, intercept = 637.93)

# f) Plot a histogram and qq-plot of the residuals. 
# Does their distribution look like a normal distribution?
#histogram
hist(residuals(mod_digsym))
# qq plot
qqPlot(residuals(mod_digsym))

# yes their distributions look like a normal distribution


# g) Plot Cook's distance for the regression model from c) which estimates the 
# residuals (i.e. distance between the actual values and the  predicted value on 
# the regression line) for individual data points in the model.
plot(cooks.distance(mod_digsym))

# h) Judging from the plot in g) it actually looks like we have 1 influential 
# observation in there that has potential to distort (and pull up) our regression 
# line.
# The last observation (row 37) in cast has a very high Cook's distance 
# (greater than 0.6).
# In other words, the entire regression function would change by more than 
# 0.6 when this particular case would be deleted.
# What is the problem with observation 37?
# Run the plotting command again and have R display the subjects that belong to 
# each point.

# Data points with large residuals (outliers) and/or high leverage may distort the outcome 
# and accuracy of a regression. Cook's distance measures the effect of deleting a given observation.
# So the Observation 37 has a great influence on the regression

# So i will replot the cooksdistance again. But i don't realy understand the question
# Please a Feedback will be great

plot(cooks.distance(mod_digsym))


# i) Make a subset of "cast" by excluding the influential subject and name it cast2.

cast2 <- cast[-37,]

# j) Fit the model from c) again, using cast2, and take a good look at the output.

mod_digsym2 <- lm(correct_RT_2.5sd ~ Age, data = cast2)

# k) What's different about the output?
# How does that change your interpretation of whether age is predictive of RTs?

# output
#Call:
#  lm(formula = correct_RT_2.5sd ~ Age, data = cast2)
#Coefficients:
#  (Intercept)          Age  
#    862.05           11.98

# the slope and intercept are different.
# The Regression function is then correct_RT_2.5sd = 11.98* Age + 862.05
# Slope: If the age increase of one year the correct_RT_2.5sd will be increased by 11.98
# Intercept: for age equal 0 the correct_RT_2.5sd will be 862.05

# l) Plot the regression line again - notice the difference in slope in 
# comparison to our earlier model fit?
ggplot(cast2, aes(x = Age, y = correct_RT_2.5sd)) + 
  geom_point() + geom_abline(slope = 11.98, intercept = 862.05)

# we have different regression line because of different slope

# m) Display the two plots side by side to better see what's going on.
library(cowplot)

plot1 <- ggplot(cast, aes(x = Age, y = correct_RT_2.5sd)) + 
  geom_point() + geom_abline(slope = 21.22, intercept = 637.93)

plot2 <- ggplot(cast2, aes(x = Age, y = correct_RT_2.5sd)) + 
  geom_point() + geom_abline(slope = 11.98, intercept = 862.05)

cowplot::plot_grid(plot1, plot2, labels = "AUTO")


# n) Compute the proportion of variance in RT that can be accounted for by Age.
# In other words: Compute R Squared.
# Take a look at the Navarro book (Chapter on regression) if you have trouble 
# doing this.

summary(mod_digsym2)$r.squared

# o) How do you interpret R Squared?

# The calculated R squared indicates that the model explains only 3.49% of the variability of the response data around its mean.
