### Stats with R Exercise Sheet 3

##########################
#Week4: Hypothesis Testing
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 18th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete. 


## Please write below your (and all of your teammates') name, matriculation number. 
## Name 1:                       AMOUH Tete Modeste Romeo
## Matriculation number1:        2550480

## Name 2:                       Md Jonybul Islam (2577852)
## Matriculation number2:        2577852

## Name 3:                       Sonja Marie Jeanne Persch
## Matriculation number3:        2551023

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###############
### Exercise 1: Deriving sampling distributions
###############
## In this exercise, we're going to derive sampling distributions with 
## different sizes.

## a) Load the package languageR. We're going to work with the dataset 'dative'. 
## Look at the help and summary for this dataset.
library(languageR)
data("dative")
help(dative)
summary(dative)

## The term dative alternation is used to refer to the alternation between 
## a prepositional indirect-object construction
## (The girl gave milk (NP) to the cat (PP)) and a double-object construction 
## (The girl gave the cat (NP) milk (NP)).
## The variable 'LenghtOfTheme' codes the number of words comprising the theme.

## b) Create a contingency table of 'LenghtOfTheme' using table(). 
##    What does this table show you?
table(dative$LengthOfTheme)
#a table for numeric vector coding with the number of words comprising every theme.
## c) Look at the distribution of 'LenghtOfTheme' by plotting a histogram and a boxplot. 
##    Do there appear to be outliers? Is the data skewed?
hist(dative$LengthOfTheme)
boxplot(dative$LengthOfTheme, ylab='Length of theme', horizontal = TRUE)
#Yes, there are appear to be have outliers. The data has right skewed. From box plot we can see there are multiple outliers. 

## d) Now we're going to derive sampling distributions of means for different 
##    sample sizes. 
##    What's the difference between a distribution and a sampling distribution?

#A distribution is the whole set of values, or individuals which we are interested in where's
#distribution of sample statistics(Sample taken from population(mean, Standard Deviation)) is sampling distribution.

## e) We are going to need a random sample of the variable 'LengthOfTime'. 
##    First create a random sample of 5 numbers using sample(). 
##    Assign the outcome to 'randomsampleoflengths'
randomsampleoflengths<-sample(dative$LengthOfTheme,size = 5)
randomsampleoflengths

## f) Do this again, but assign the outcome to 'randomsampleoflengths2'. 
randomsampleoflengths2<-sample(dative$LengthOfTheme,size = 5)
randomsampleoflengths2

## g) Now calculate the mean of both vectors, and combine these means 
##    into another vector called 'means5'.
mean1=mean(randomsampleoflengths)
mean2=mean(randomsampleoflengths2)
means5<-merge(x=mean1,y=mean2)

## h) In order to draw a distribution of such a sample, we want means of 
##    1000 samples. However, we don't want to repeat question e and f 
##    1000 times. We can do this in an easier way: 
##    by using a for-loop. See dataCamp or the course books for 
##    how to write loops in R.
list1<-list()
for (sample in 1:1000){
  means55<-mean(sample(dative$LengthOfTheme,size = 5))
  list1<-append(list1,means55)
}
means5=as.numeric(as.character(unlist(list1)))
## i) Repeat the for-loop in question h, but use a sample size of 50. 
##    Assign this to 'means50' instead of 'means5'.
list2<-list()
for (sample in 1:1000){
  means550<-mean(sample(dative$LengthOfTheme,size = 50))
  list2<-append(list2,means550)
}
means50=as.numeric(as.character(unlist(list2)))



## j) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?
#Means5 and Means50 are containg 1000 mean of 5 and 50 random sample data.  

## k) Look at the histograms for means5 and means50. Set the number of breaks to 15.
##    Does means5 have a positive or negative skew?
hist(means5,breaks=15)
hist(means50,breaks=15)
#Means5 has a positive skew.
## l) What causes this skew? In other words, why does means5 have bigger 
##    maximum numbers than means50?
#Means5 have a lower bound. That's why it is skewed right. Mean of the Lenght of Theme
#comprising the theme of means5 has lower bound/has more long where means50 has almost mid size. 

###############
### Exercise 2: Confidence interval
###############

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.

## a) What does a confidence interval mean from the perspective of experiment replication?
#Confidence interval mean is the mean inference interval of sample means from the population.
#It shows us the range of samples means of population. As sample can't give the perfect derivation
#of population, experimental replication is the use of enough subject for the reduction of variation
#for the confidence interval.

## b) Let's calculate the confidence interval for our means from the previous 
##    exercise.
##    First, install and load the packages 'lsr' and 'sciplot'
install.packages("lsr")
install.packages("sciplot")
library(lsr)
library(sciplot)
## c) Look at the description of the function ciMean to see which arguments it takes.
?ciMean
#Aruments
#x-A numeric vector, data frame or matrix containing the observations.
#conf-The level of confidence desired. Defaults to a 95% confidence interval
#na.rm-Logical value indicating whether missing values are to be removed. Defaults to FALSE.
## d) Use ciMean to calculate the confidence interval of the dataset dative from
##    the previous exercise.
##    Also calculate the mean for the variable LengthOfTheme.
cMean<-ciMean(dative)
cMean
mean(dative$LengthOfTheme)

## e) Does the mean of the sample fall within the obtained interval? 
##    What does this mean?
#Yes, it does. It means the mean of sample data "Lenth of Theme" falls into the sample Confidence  
#Interval which is more viable to represent samples from a populatin data than a more specific
#representation of mean from sample which changes from sample to sample.


## f) As the description of dative mentions, the dataset describes the 
##    realization of the dative as NP or PP in two corpora.
##    The dative case is a grammatical case used in some languages 
##    (like German) to indicate the noun to which something is given.
##    This dataset shows us, among other things, how often the theme is 
##    animate (AnimacyOfTheme) and how long the theme is (LengthOfTheme).
##    Plot this using the function bargraph.CI(). Look at the help for this function. 
##    Use the arguments 'x.factor' and 'response'.
bargraph.CI(dative$AnimacyOfTheme,dative$LengthOfTheme)

## g) Expand the plot from question f with the ci.fun argument 
##    (this argument takes 'ciMean'). 
##    Why does the ci differ in this new plot compared to the previous plot?
bargraph.CI(dative$AnimacyOfTheme,dative$LengthOfTheme,ci.fun= function(x) c(ciMean(x)-se(x), ciMean(x)+se(x)))

# ci differ in this new plot because it uses the values of the confidence interval of dative$LengthOfTheme
# to calculate the function ci.fun. But in the previous plot only the mean was been used.

###############
### Exercise 3: Plotting graphs using ggplot.
###############
# There are many ways of making graphs in R, and each has their own advantages 
# and disadvantages. One popular package for making plots is ggplot2. 
# The graphs produced with ggplot2 look professional and the code is quite easy 
# to manipulate.
# In this exercise, we'll plot a few graphs with ggplot2 to show its functionalities.
# You'll find all the information you'll need about plotting with ggplot2 here: 
# http://www.cookbook-r.com/Graphs/
# Also, you have been assigned the ggplot2 course in DataCamp. Please work through 
# this course.

## a) First install and load the ggplot2 package. Look at the help for ggplot.
library(ggplot2)
help(ggplot2)

## b) We're going to be plotting data from the dataframe 'ratings' 
##    (included in languageR). 
##    Look at the description of the dataset and the summary.
library(languageR)
data('ratings')
help(ratings)
summary(ratings) 
str(ratings)
## For each word, we have three ratings (averaged over subjects), one for the 
## weight of the word's referent, one for its size, and one for the words' 
## subjective familiarity. Class is a factor specifying whether the word's 
## referent is an animal or a plant. 
## Furthermore, we have variables specifying various linguistic properties, 
## such as word's frequency, its length in letters, the number of synsets 
## (synonym sets) in which it is listed in WordNet [Miller, 1990], its 
## morphological family size (the number of complex words in which 
## the word occurs as a constituent), and its derivational entropy (an 
## information theoretic variant of the family size measure). 
## Don't worry, you don't have to know what all this means yet in order to 
## be able to plot it in this exercise!

## c) Let's look at the relationship between the class of words and the length. 
##    In order to plot this, we need a dataframe with the means.
##    Below you'll find the code to create a new dataframe based on the existing 
##    dataset ratings.
##    Plot a barplot of ratings.2 using ggplot. Map the two classes to two 
##    different colours. 
##    Remove the legend.
summary(ratings)
condition <- c("animal", "plant")
frequency <- c(mean(subset(ratings, Class == "animal")$Frequency), mean(subset(ratings, Class == "plant")$Frequency))
length <- c(mean(subset(ratings, Class == "animal")$Length), mean(subset(ratings, Class == "plant")$Length))
ratings.2 <- data.frame(condition, frequency, length)
ratings.2

# Barplot code
ggplot(ratings.2, aes (x = condition, y = length, fill = condition))+ geom_bar(stat =  'identity', show.legend = FALSE)

## d) Let's assume that we have additional data on the ratings of words. 
##    This data divides the conditions up into exotic and common animals 
##    and plants.
##    Below you'll find the code to update the dataframe with this additional data.
##    Draw a line graph with multiple lines to show the relationship between 
##    the frequency of the animals and plants and their occurrence.
##    Map occurrence to different point shapes and increase the size 
##    of these point shapes.
condition <- c("animal", "plant")
frequency <- c(7.4328978, 3.5864538)
length <- c(5.15678625, 7.81536584)
ratings.add <- data.frame(condition, frequency, length)
ratings.3 <- rbind(ratings.2, ratings.add)
occurrence <- c("common", "common", "exotic", "exotic")
ratings.3 <- cbind(ratings.3, occurrence)
ratings.3

ggplot(ratings.3, aes (x = occurrence, y = frequency, group= condition))+ 
  geom_point (aes(shape = condition, size = occurrence)) + 
  geom_line(linetype = 'solid', color="red")

## e) Based on the graph you produced in question d, 
##    what can you conclude about how frequently 
##    people talk about plants versus animals, 
##    with regards to how common they are?

# Poeple talk more frequently about exotic animals than common animals. But by plants people talk more frequently
# about common plants than exotic plants.
