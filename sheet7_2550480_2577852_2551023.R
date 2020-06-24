### Stats with R Exercise sheet 7

##########################
#Week 8: ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, December 16. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates) name, matriculation number. 
## Name 1:                       AMOUH Tete Modeste Romeo
## Matriculation number1:        2550480

## Name 2:                       Md Jonybul Islam (2577852)
## Matriculation number2:        2577852

## Name 3:                       Sonja Marie Jeanne Persch
## Matriculation number3:        2551023

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################



#######################
### Exercise 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, 
# Cambridgeshire County Council considered 14 pairs of locations. The locations were 
# paired to account for factors such as traffic, volume and type of road. One site in 
# each pair had a sign erected warning of the dangers of speeding and asking drivers 
# to slow down. No action was taken at the second site. Three sets of measurements 
# were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the 
# erection of the sign, shortly after the erection of the sign, and again after 
# the sign had been in place for some time.

# a) For the further reference please use ?amis. 
# It may take some time to understand the dataset. 

?amis

# b) Load the dataset, store it into a variable called "data", and briefly inspect it. 
# Feel free to make some plots and calculate some statistics in order to understand 
# the data.
head(amis)
str(amis)
summary(amis)
ggplot(amis, aes(x=period,y=speed, col=as.factor(warning))) + geom_point()
ggplot(amis, aes(x=as.factor(pair),y=speed)) + geom_point()
ggplot(amis, aes(x=as.factor(period),y=speed)) + geom_boxplot()


# c) All our columns have numeric type. Convert the categorial columns to factors.
amis$warning<-as.factor(amis$warning)
amis$period<-as.factor(amis$period)
amis$pair<-as.factor(amis$pair)

# d) Plot boxplots for the distribution of `speed` for each of the `period` values 
# (before, immediately after and after some time). Build 2 plots (each containing 3 
# boxplots) side by side depending on the `warning` variable.
# (For all plots here and below please use ggplot)

install.packages("gridExtra")
library(gridExtra)
plot1_data<-subset(amis, warning==1)
plot2_data<-subset(amis, warning==2)
summary(plot1_data)
summary(plot2_data)
boxplot1<-ggplot(plot1_data, aes(x=factor(period),y=speed)) + geom_boxplot()
boxplot2<-ggplot(plot2_data, aes(x=factor(period),y=speed)) + geom_boxplot()
grid.arrange(boxplot1, boxplot2, ncol=2)

# e) What can you conclude looking at the plots? What can you say about people's 
# behaviour in different periods: before, immediately after and after some time?
# Boxplot1
# Immediately after the warning sign there is a reduction in the vehicle speed. 
# However  this  increases when recorded after some period of time (at period 3)
# Boxplot2
# in the absence of warning signs at the 3 periods we don't have much difference between the speeds  

# f) What are your ideas about why the data with warning==2 (sites where no sign was 
# erected) was collected?
# The data recorded when there are no warning signs placed helps to understand the natural behavior and compare it against 
# that of the driver when the warnings are placed

#######################
### Exercise 2: 1-way ANOVA
#######################

# a) First let's create a new data frame which will be used for all exercise 2.
# For the 1-way ANOVA we will be working with a subset of `amis` using only the 
# data for sites where warning signs were erected, which corresponds to warning==1. 
# Therefore first subset your data to filter out warning==2 and then apply cast() 
# to average "speed" over each "pair" and "period". 
# Assign this new data frame to the variable casted_data.

amis_sub<-subset(amis, warning==1)
head(amis_sub)
casted_data<-cast(amis_sub, period + pair ~., fun.aggregate = 'mean', value = 'speed', na.rm = TRUE)
colnames(casted_data)[colnames(casted_data)=="(all)"] <- "AverageSpeed"
head(casted_data)
summary(casted_data)
# b) Build boxplots of the average speed depending on "period".
ggplot(data=casted_data, aes(x=factor(period), y=AverageSpeed)) + 
  geom_boxplot() + 
  labs(title="The average speed depending on period")

# c) Looking at the boxplots, is there a difference between the periods?
#Yes. the Average speeds recorded for period 3 is the highest, followed by that for Period 1 and then 2. 

# Now, let's check the ANOVA assumptions and whether they are violated or not 
# and why.

# d) Independence assumption
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)

perm_data<-casted_data
perm_data<- perm_data[sample(1:nrow(perm_data)), ]
head(perm_data)
summary(aov(casted_data$AverageSpeed~casted_data$period))
summary(aov(perm_data$AverageSpeed~perm_data$period))
# The variance does not change for the permuted data. 
# This confirms the independence assumption

# e) Normality of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)
mod <- lm(casted_data$AverageSpeed~casted_data$period)
re <- residuals(mod)
head(re)
model <- aov(casted_data$AverageSpeed~casted_data$period) 
res=model$residuals
head(res)
shapiro.test(res)

# output
#Shapiro-Wilk normality
#test
#data:  res
#W = 0.96239, p-value = 0.1798

# p value is large so the assumped hypothesis that the residual is normal cannot be rejected

# f) Homogeneity of variance of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)

prd<-as.factor(casted_data$period)
leveneTest(casted_data$AverageSpeed~prd)

# output
#Levene's Test for Homogeneity of Variance (center = median)
#      Df F value Pr(>F)
#group  2  0.1772 0.8383
#      39
# Large value of p indicates Homogeneity of variance of residuals


# g) Now we are ready to perform 1-way ANOVA: please use the function aov() on the speed
# depending on the period,report p-value and interpret the result in details
help("aov")
anova<-aov(casted_data$AverageSpeed ~ casted_data$period)
summary(anova)
# p>0.05 (p=0.382) There is no significant relationship between The Average speed and period
# The F value is 0.986, The ratio of Variance between groups to the variance within groups is small

# h) Please do pairwise t-tests of the same variables as in g) using pairwise.t.test().
pairwise.t.test(casted_data$AverageSpeed,casted_data$period)


# i) Report the pairwise p-values and interpret the result in detail.
#output
#Pairwise comparisons using t tests with pooled SD 
#data:  casted_data$AverageSpeed and casted_data$period 
#1    2   
#2 0.81 -   
#  3 0.81 0.51
#P value adjustment method: holm
# None of the interactions between the 3 period groups are statistically significant
# since all values are greater than 0.05

# j) Try to use no adjustment for pairwise testing and then the Bonferroni correction.
# Does the result change?
pairwise.t.test(x=casted_data$AverageSpeed, g=casted_data$period, p.adjust.method = "none")
# Output
#1    2   
#2 0.59 -   
#  3 0.40 0.17
#P value adjustment method: none

pairwise.t.test(x=casted_data$AverageSpeed, g=casted_data$period, p.adjust.method = "bonferroni")
# Output
#1    2   
#2 1.00 -   
#  3 1.00 0.51
#P value adjustment method: bonferroni
# Yes,There is an increase in the p values when bonferroni is used in comparison to no adjustment

#######################
### Exercise 3: 2-way ANOVA
#######################
# a) Now we want to analyze the influence of 2 categorial variables 
# (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1).
# First, we need to average the speed over each `pair`, `warning` and `period
# Cast your data again and assign the resuts to casted_data2.
casted_data2<-cast(amis, pair+period+warning ~., fun.aggregate = mean, value = "speed", na.rm = TRUE)
colnames(casted_data2)[colnames(casted_data2)=="(all)"] <- "AverageSpeed"
head(casted_data2)
# b) Calculate the mean for each of the 6 possible pairs of `period` and `warning`.
mea<-aggregate(AverageSpeed ~ period + warning, casted_data2, mean)
mea

# c) Do you think there is a significant difference between some of the groups?
var(mea[,3])
# The average speeds have a variance of 2.367826.

# d) Now apply the 2-way ANOVA: please use the function aov() on the speed depending 
# on the period and warning.
# Report the p-value and interpret the result in detail.
anova_2<-summary(aov(casted_data2$AverageSpeed~casted_data2$period*casted_data2$warning,casted_data2))
anova_2

# The p values between speed and warning = 0.00488 with F value = 8.396. The ratio of Variance between groups to the variance within groups is sufficiently large and the p value impleis statistical significance in their relationship
# The p values between speed and period = 0.33507 imply that the interactions between these two entities are statistically insignificant though the F value > 1
# The p values between perid and warning =  0.69975 imply that the interactions between these two entities are statistically insignificant

# e) What do you conclude about the behaviour of drivers based on the 2-way ANOVA?
# There is no significant interaction between Period~Speed, Period and warning
# The speed however, is affected by the presence/absence of warnings (statistically significant)


