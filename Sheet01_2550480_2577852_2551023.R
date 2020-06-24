### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 4. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates') name and matriculation number. 
## Name 1:                       AMOUH Tete Modeste Romeo
## Matriculation number1:        2550480

## Name 2:                       Md Jonybul Islam (2577852)
## Matriculation number2:        2577852

## Name 3:                       Sonja Marie Jeanne Persch
## Matriculation number3:        2551023

## Change the name of the file by adding your matriculation numbers
## (sheet01_firstID_secondID_thirdID.R)



## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## moodle discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
dirctory_path1 <- getwd()
dirctory_path1

## b) Get help with this function.

help(getwd)

## c) Change your working directory to another directory.

setwd('C:/Users/RA/Desktop/Romeo_Doc/UNI_MASTER/WS19_20/Statistik_mit_R')
changed_path <- getwd()
changed_path


###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages('languageR')
library(languageR)

## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?

        # load data
data(dutchSpeakersDistMeta)
        # head
head(dutchSpeakersDistMeta,5)
## head shows the first rows of the dataset, hier the first 5 speakers and their informations.
        # tail
tail(dutchSpeakersDistMeta,5)
## tail shows the last rows of the dataset, hier the lasr 5 speakers and their informations.
summary(dutchSpeakersDistMeta)

## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.

number_of_speaker = nrow(dutchSpeakersDistMeta)
number_of_speaker
#  the number of the speaker in the dataset is 165

## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.

boxplot(dutchSpeakersDistMeta$AgeYear~dutchSpeakersDistMeta$Sex)

## e) Does it seem as if either of the two groups has more variability in age?
## no

## f) Do you see any outliers in either of the two groups?

## yes we have outliers in the group of males

## g) Now calculate the mean and standard deviation of the AgeYear per group. 
##    Do this by creating a subset for each group.
##    Do the groups seem to differ much in age?

## Subset 'males'

library(dplyr)

subset_males <- dutchSpeakersDistMeta %>%
                   filter(Sex == 'male')
# mean of the AgeYear of the group 'male'
mean_subset_male <- mean(subset_males$AgeYear)
mean_subset_male
## the mean of the AgeYear of the group 'male' is 1967

stdeviation_subset_male <- sd(subset_males$AgeYear)
stdeviation_subset_male

## Subset 'females'

subset_females <- dutchSpeakersDistMeta %>%
  filter(Sex == 'female')
# mean of the AgeYear of the group 'female'
mean_subset_female <- mean(subset_females$AgeYear)
mean_subset_female
## the mean of the AgeYear of the group 'male' is 1967

stdeviation_subset_female <- sd(subset_females$AgeYear)
stdeviation_subset_female

## h) What do the whiskers of a boxplot mean?

# the whiskers are the two lines outside the box that extend to the highest and lowest observations.

###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)

# scale: ratio
# discrete or continuous: discrete
# why: Because  there can't be anything in the middle between two values. 
 

## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?

## Dataframes are generally refers to tabular data, consist of rows and columns (contains numbers of observation or measurement).
## It is used for storing data tables and a list of vectors of equal length.It is the case in this experiment. 
## But a matrix is a collection of data elements arranged in a two-dimensional rectangular layout. 

## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25

pps <- c(1:25)
pps

## d) Next, create a vector containing all the observations. Name this vector 'obs'.

obs <- c(18, 15, 22, 19, 18, 17, 18, 20, 17, 12, 16, 16, 17, 21, 25, 18, 20, 21, 20, 20, 15, 18, 17, 19, 20)
obs
## e) Create a dataframe for this data. Assign this to 'stories'. 

stories <- data.frame(pps,obs)
stories

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?
summary(stories)
lapply(stories,class)

# pps is numerical 

## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?
stories$pps <- as.factor(stories$pps)
lapply(stories,class)

# factor is better class for pps because, pps is only the ID of each participant. Its only to identificate the children.  
# It can be also the name of each children. numerical class means also that we can order the values from the largest to the smallest. 
# And it will have meaning. But it has no meaning for pps analysis.


## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.
hist(stories$obs, breaks = 8)

## i) Create a kernel density plot using density().
plot(density(stories$obs))

## j) What is the difference between a histogram and a kernel density plot?

## Histogram a diagram consisting of rectangles whose area is proportional to the frequency of a variable and whose width is equal to the class interval
## A Density Plot visualises the distribution of data over a continuous interval or time period. 
## An advantage Density Plots have over Histograms is that they're better at determining the distribution 
## shape because they're not affected by the number of bins used (each bar used in a typical histogram).

## This is a difficult one, remember you just need to provide a serious attempt at solving each 
## exercise in order to pass. 
## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)

library(ggplot2)
ggplot(stories, aes(x=obs)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(aes(x=obs),alpha=.1)


###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.

x<-seq(from = -5, to = 5, by= 0.1)
x

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.

y<- dnorm(x, mean = 0, sd = 1, log = FALSE)
y

## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(x, y,xlab="z values of x",
          ylab="Density")

## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.

plot(x, y,type = "l",ylim=c(0,0.8),xlab="z values of x",
     ylab="Density")

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.
abline(x,y,v= mean(x),lty=2)

## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".

beaver1
b1temp<- beaver1$temp
b1temp

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
# mean
b1temp_mean = mean(b1temp)
b1temp_mean
# Standard deviation
b1temp_sd = sd(b1temp)
b1temp_sd
# plotting of normal distribution
plot(b1temp,dnorm(b1temp, mean = b1temp_mean, sd = b1temp_sd, log = FALSE))


## h) We observe two temparatures (36.91 and 38.13). What's the likelihood that
##    these temperatures (or more extreme ones) respectively come 
##    from the normal distribution from g)?
#The temparature 36.91 is almost the center point of normal distribution. Its likelyhood is almost 100%
#wit respect to 38.13 which has almost 0 % likelyhood from the normal distribution.
#N.B: Assumption are given from calculating Normal Distribution theorem.

## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. What do you observe?
b1temp_random<-rnorm(20, mean = b1temp_mean, sd = b1temp_sd)
hist(b1temp_random, breaks = 6)
#As everytime random sample is changing(as rnorm function creates different sample everytime),
#frequency of temparature changes respectively.

