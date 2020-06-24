### Stats with R Exercise sheet 4

##########################
#Week5: Tests for Categorical Data
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 25th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.


## Please write below your (and your teammates) name, matriculation number. 
## Name 1:                       AMOUH Tete Modeste Romeo
## Matriculation number1:        2550480

## Name 2:                       Md Jonybul Islam (2577852)
## Matriculation number2:        2577852

## Name 3:                       Sonja Marie Jeanne Persch
## Matriculation number3:        2551023

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

#################################################################################
#################################################################################

##########
##Exercise 1. Binomial distribution
##########
## Suppose thprobability of answering 4 or less questions 
##    correctly by chance. ere are 12 multiple choice questions in a quiz. 
## Each question has 5 possible answers, and only one of them is correct. 

## a) Please calculate the probability of getting exactly 4 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.
p <- 1/5

dbinom(4,12,p)

## b) Next please calculate the 
pbinom(4,12,p, lower.tail = TRUE)

##########
##Exercise 2. Chi-square test
##########
## a) Consider the dataset dutchSpeakersDistMeta from our first tutorial again. 
##    Load the package (languageR) and look at the summary of the variables, 
##    as well as their classes. Which variables are factors?
library(languageR)

# load data
data(dutchSpeakersDistMeta)
# head
head(dutchSpeakersDistMeta,5)
#summary
summary(dutchSpeakersDistMeta)
#class
str(dutchSpeakersDistMeta)
# or
sapply(dutchSpeakersDistMeta,class)

# variables which are factor: Speaker, Sex, AgeGroup, ConversationType, Edulevel

## b) We want to find out whether there is a difference between males and females 
##    with respect to the age groups they are in.
##	  First use the function 'table()' to get the counts and create 
##    a contingency table of AgeGroup by Sex.

contingency_age_sex <- table(dutchSpeakersDistMeta$Sex, dutchSpeakersDistMeta$AgeGroup)
contingency_age_sex

##    Visualize your data with a single bar plot (use ggplot) that represents the counts with 
##    respect to each age group and each sex.
install.packages("ggplot2")
library("ggplot2")
myFrame <- as.data.frame(contingency_age_sex)
myFrame$category <- paste(myFrame$Var1,myFrame$Var2)
ggplot(myFrame, aes(x= category , y= Freq) ) + geom_bar(stat = "identity")

## c) Inspect the table you created in b). Does it look like there could be a significant 
##    difference between the sexes?

#Yes, there are significant difference between Sex with respect to age group.

## d) We are going to calculate whether there's a difference between males and females 
##    regarding their age group using the function chisq.test. 
##    Look at the help of this function. 
##    Then use the  function to calculate whether there's a difference in our table from b). 
##    Is there a significant difference in age group?

?chisq.test
chi_test<- chisq.test(contingency_age_sex)
chi_test

# As p values exceeded .5( usual level of significance), it will keep the 
#null hypothesis. So there are no significant difference in age group.


## e) What are the degrees of freedom for our data? How are they derived?
#degree of freedom of our data is 4. It is mainly derived (number of independet piece of info 
#of catagory, n - 1).Because if we can get info about n-1 catagory, than we can easily get n.


##########
##Exercise 3. Binomial versus chi-square
##########
##    In this exercise, we'll consider a paper on therapeutic touch 
##    (google it, if you want to know what that is...) that was published in the Journal 
##    of the American Medical Association (Rosa et al., 1996).
##    The experimenters investigated whether therapeutic touch is real by using the 
##    following method:
##    Several practitioners of therapeutic touch were blindfolded. The experimenter 
##    placed her hand over one of their hands. If therapeutic touch is a real 
##    phenomenon, the principles behind it suggest that the participant should 
##    be able to identify which of their hands is below the experimenter's hand. 
##    There were a total of 280 trials, of which the therapeutic touch therapists 
##    correctly indicated when a hand was placed over one of their hands 123 times.

## a) What is the null hypothesis, i.e. how often would we expect the participants to 
##    be correct by chance (in raw number and in percentage)?

#    H0:  140 is the number we expect the participants to be correct by chance (randomly) 
#    which is 50% in percentage.

## b) Using a chisquare test, what do you conclude about whether therapeutic touch 
##    works? 
obsfreq <- c(123,160)
h_null_proba <- c(0.5,0.5)
chisq.test(obsfreq,p=h_null_proba)
# p value is less than 5. we will reject the null hypothesis.Therapeutic touch doesn't work. 

## c) Now calculate significance using the binomial test as we used it in exercise 1.

dbinom(123,size = 280,prob =1/2)
pbinom(123,size = 280, prob =1/2, lower.tail=FALSE)
# With this resultats we can conclude that the Therapeutic touch work

## d) The results from these two tests are slightly different. Which test do you think 
##    is better for our data, and why?

## binomial test is better beacaus the binomial test is an exact test to compare the 
## observed distribution to the expected distribution when there are only two categories 
## (so only two rows of data were entered). In this situation, the chi-square is only an 
## approximation.For more rows or more than one sample chi -square test is better.

## we are not really 100 percent sure about our answers. So we expected a feedback.

##########
##Exercise 4.
##########
## Describe a situation where you would choose McNemar's test over the ChiSquare test. 
## What would be the problem of using the normal ChiSquare test in a case where 
## McNemar's test would be more appropriate?

#Suppose we want to find out whether a gastric surgery decreases the appetite for chocolate more in men than in women.
#The sample group could be 50 obese men and 50 obese women, the nominal variable would be yes/no when asked if they have an appetite for chocolate.
#After they receive gastric surgery, the same groups of people would be asked the same question to see if the surgery made any difference.
#The McNemar test is a within-subject test. That means the samples for both test are paired, like in this example. 
#The samples for chiSquare tests must be independent, which means they can't be paired.