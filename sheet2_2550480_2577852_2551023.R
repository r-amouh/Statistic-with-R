###############
### Cleaning Data
###############

# Please do the "Cleaning Data with R" exercise that was assigned in DataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercises below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a tutorial on how to further work with this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory. 

#setwd('C:/Users/RA/Desktop/Romeo_Doc/UNI_MASTER/WS19_20/Statistik_mit_R')



# 2. Read in the data into a variable called "dat".
dat <- read.csv('digsym.csv')
dat
# 3. Load the libraries languageR, stringr, dplyr and tidyr.
library(languageR)
library(stringr)
library(dplyr)
library(tidyr)

# 4. How many rows, how many columns does that data have?

dim(dat)
# the data has 3700 rows and 11 columns

# 5. Take a look at the structure of the data frame using "glimpse".

glimpse(dat)

# 6. View the first 20 rows, view the last 20 rows.

# first 20 rows
head(dat,20)

# last 20 rows
tail(dat,20)

# 7. Is there any missing data in any of the columns?
anyNA(dat)

# yes there is missing data

# 8. Get rid of the row number column.

dat$X <- NULL
dat

# 9. Put the Sub_Age column second.
dat<- dat[c(1,10,2,3,4,5,6,7,8,9)]
dat

# 10. Replace the values of the "ExperimentName" column with something shorter, more legible.
dat$ExperimentName<- as.factor('digit symbol')


# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.

data2 <- dat %>% filter(List == 'Trial:2')
dat <- data2
rm(data2)
head(dat)
# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate".
dat <- separate(dat,Sub_Age,c("Subject" , "Age"), sep = '_')
head(dat)

# 13. Make subject a factor.
dat$Subject<- as.factor(dat$Subject)
class(dat$Subject)

# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".
right_wrong_function <- function(x){ if (str_sub(x, 3, 3) =='w'){return('wrong')} else {return('right')}}
dat$File <- mapply(right_wrong_function,dat$File)
head(dat$File)

# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 at the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc).

# we use dat$File of number 14 to do the number 15. So we get like value in File ( example: 'wrong000', 'right000')
dat$File <- str_pad(dat$File, width = 8, side = 'right', pad = '0')
head(dat$File)

# 16. Remove the column "List".
dat$List <- NULL
dat

# 17. Change the data type of "Age" to integer.
dat$Age <- as.integer(dat$Age)

# 18. Missing values, outliers:
# Do we have any NAs in the data, and if so, how many and where are they?
anyNA(dat)

# We don't have any NAs

# 19. Create an "accuracy" column using ifelse-statement.
# If actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0.

# hier we use the ifelse-statement in a function that we will use to create "accuracy" column 
comparison_function <- function(x,y){if ( x == y){ return(1) } else {return (0)}}

dat$accuracy <- mapply(comparison_function, dat$StimulDS1.RESP, dat$StimulDS1.CRESP)

head(dat$accuracy)

# 20. How many wrong answers do we have in total?
nmbers_wrong = sum(dat$accuracy == 0)
nmbers_wrong
# we have in total 185 wrong answers

# 21. What's the percentage of wrong responses?

percentage_wrong = nmbers_wrong/length(dat$accuracy)*100
percentage_wrong
#the percentage of wrong responses is 5.555556%

# 22. Create a subset "correctResponses" that only contains those data points where subjects 
# responded correctly. 

correctResponses <- dat %>% filter(accuracy == 1)
head(correctResponses)

# 23. Create a boxplot of StimulDS1.RT - any outliers?

boxplot(correctResponses$StimulDS1.RT, horizontal = TRUE)

# Yes we have several outliers, because the are values outside of the whiskers of boxblot

# 24. Create a histogram of StimulDS1.RT with bins set to 50.

hist(correctResponses$StimulDS1.RT, breaks = 50)

# 25. Describe the two plots - any tails? any suspiciously large values?

# histogram
# most of the value of StimulDS1.RT, which occured more tha 100 times are under 2000.The value greater than 2000 have 
# a frequency less than 100. The shape of the distribution looks like positively skewed.
# Boxplot
# According to our Boxplot we have several outliers. the most extreme point inside
# 1,5 time inter-quatile range are 486 and 2100.Our first quantile is 891, the median 1088
# and the 3rd quantile is 1399


# 26. View summary of correct_RT.
summary(correctResponses$StimulDS1.RT)

# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named 
# "cleaned".

cleaned <- correctResponses %>% filter(StimulDS1.RT != 13852 )
cleaned

## EXTRA Exercises:
##You can stop here for your submission of this week's assignment,
##but you are encouraged to try it now. 
##All these exercises will be discussed and solved in the tutorial!

# 28. Dealing with the tail of the distribution: outlier removal
# Now we want to define a cutoff value for the StimulDS1.RT variable in the correctResp dataset.
# Values should not differ more than 2.5 standard deviations from the grand mean of this variable.
# This condition should be applied in a new variable called "correct_RT_2.5sd", which prints NA 
# if an RT value is below/above the cutoff. 


# 29. Take a look at the outlier observations.
# Any subjects who performed especially poorly?


# 30. How many RT outliers are there in total?


# 31. Plot a histogram and boxplot of the correct_RT_2.5sd column again - nice and clean eh?


# 32. Next, we'd like to take a look at the average accuracy per subject.
# Using the "cast" function from the library "reshape", create a new data.frame which shows the 
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".


# 33. Sort in ascending order or plot of the average accuracies per subject.


# 34. Would you exclude any subjects, based on their avrg_accuracy performance?


# 35. Congrats! Your data are now ready for analysis. Please save the data frame you created 
# into a new file called "digsym_clean.csv".