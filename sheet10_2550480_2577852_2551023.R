##########################
#Week 11: Model Families and Logistic Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 20. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name 1:                       AMOUH Tete Modeste Romeo
## Matriculation number1:        2550480

## Name 2:                       Md Jonybul Islam (2577852)
## Matriculation number2:        2577852

## Name 3:                       Sonja Marie Jeanne Persch
## Matriculation number3:        2551023

######################################################################################################################


####
#Part 1
####
# The folder speed.dating
# contains data from an experiment on a few hundred students that randomly assigned
# each participant to 10 short dates with participants of the opposite sex
# (Fisman et al., 2006). For each date, each person recorded several subjective
# numerical ratings of the other person (attractiveness, compatibility, and some 
# other characteristics) and also wrote down whether he or she would like to meet
# the other person again. Label and rij1, . . . , rij6 as person i’s numerical ratings of person j 
# on the dimensions of attractiveness, compatibility, and so forth.
setwd("E:/Study/MSc/Winter 19/Stat With R/Assignment 10")
mydata = read.csv("Speed Dating Data.csv")
mydata
summary(mydata)
str(mydata)


#(1) Fit a classical logistic regression predicting Pr(yij = 1) given person i's 
#    ratings of person j. For ratings, use the features attr, sinc, intel, fun; see the documentation for what exactly these
#    abbreviations stand for.
#    Also, please plot the data in order to inspect it, and discuss the importance of attractiveness, compatibility, and so 
#    forth in this predictive model.

mod<-lm(dec ~ attr +  sinc + intel + fun, mydata)
mod
## We can say that one unit increase in fun value will increase decision of choosing a partner
## by 0.33711 while keeping other things constant and by increasing one unit increase in attr, 
## decision of choosing partner will increase by 0.5623 while keeping other things constant.

#library(broom)
#augmented_mod<-augment(mod)s
#install.packages("ggplot2")
#library("ggplot2")
#data_space <- ggplot(augmented_mod, aes(x = iid, y = pid, color = pid+attr+sinc+intel+fun)) + 
#    geom_point()
#data_space + 
#   geom_line(aes(y = .fitted))
install.packages("ggplot2")
library(ggplot2)
ggplot(mydata , aes(x= factor(gender), y = attr_o )) + geom_boxplot()
ggplot(mydata , aes(x= factor(gender), y = sinc_o )) + geom_boxplot()
ggplot(mydata , aes(x= factor(gender), y = intel_o  )) + geom_boxplot()
ggplot(mydata , aes(x= factor(gender), y = fun_o )) + geom_boxplot()
## Here we can see that females give more importance to attractiveness and fun than the males,
## both give equal importance to sincereity and males give more importance to intelligent than
##females.


#(2) Expand this model to allow varying intercepts for the persons making the
#    evaluation; that is, some people are more likely than others to want to meet
#    someone again. Discuss the fitted model.

mod2 <- lm(dec ~ dec ~ fun + attr + intel + sinc + id, mydata)
summary(mod2)

## Here attr, sync and fun are most important attribute for dating decisions. So these are 
## significant in the model at a significance level of 0.05 , so we will reject the null 
##hypothesis.


#(3) Expand further to allow varying intercepts for the persons being rated. Discuss
#    the fitted model.

mod3 <- lm(dec ~ fun + attr + intel + sinc + id + pid, mydata)
summary(mod)
## Most of the matches done on the basis of fun, attr values, sinc. Decsion
## between the partners is also affected by id no but not by partner id(pid).
## Fun, attr,sinc, id are significant in the model at a significance level of 0.05 ,
## so we will reject null hypothesis.


#(4) Now fit some models that allow the coefficients for attractiveness, compatibility, and the 
#    other attributes to vary by person.  Fit a multilevel model, allowing the intercept and the 
#    coefficients for the 6 ratings to vary by the rater i. (Hint: The model will not converge when you 
#    include many predictors as random slopes; see with how many predictors you can get the model to converge;
#    and try out some of the tricks we have seen to see whether they affect convergence for this dataset.)

mod4 <- lm(dec ~ fun + id , mydata)
summary(mod)
mod5 <- lm(dec ~ fun  + attr + id, mydata)
summary(mod5)
mod6 <- lm(dec ~ fun + attr + sinc + id, mydata)
summary(mod6)
mod7 <- lm(dec~fun + attr + sinc + intel + fun + (1 + attr + sinc + intel + fun|pid) + (1+ attr + sinc + intel + fun|iid), mydata)
summary(mod7)

#(5) compare the output for the different models that you calculated - did the model design 
#affect your conclusions?

summary(mod4)# AIC 9422
summary(mod5)# AIC 8433
summary(mod6)# AIC 8376
summary(mod7)# AIC 8329
# Yes, the model design affect our conclusions, as all of them have different AIC. Model 7 is
#best model with lower AIC


####
#Part 2
####

# In this example, num_awards is the outcome variable and indicates the number of awards earned by students at
# a high school in a year, math is a continuous predictor variable and represents students' scores on their 
# math final exam, and prog is a categorical predictor variable with three levels indicating the type of program 
# in which the students were enrolled. It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". 
# Let's start with loading the data and looking at some descriptive statistics.

p = read.csv("poisson_sim.csv", sep=";")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)
})
summary(p)

#(6) Plot the data to see whether program type and math final exam score seem to affect the number of awards.

ggplot(p, aes(x = math, y = num_awards, color = prog)) + geom_point() 
## We can see that students who have high math scores belong  mostly to Academic program
## type and also recieves more no of awards. 

#(7) Run a generalized linear model to test for significance of effects.

Pm1<-lm(num_awards ~ math + prog , data = p)
summary(Pm1)
## Interception is if math score is zero and if there is no program type for which student is 
## enrolled, the number of awards will decrease by 2.19550. slope is if we increase maths
## score by one unit , no of awards increase by 0.04789 keeping all other things constant. 
## If we increase program Academic type by one unit, the no of awards increase by 0.47861 
## keeping all other things constant.
## Both math score predictor with p value 4.03e-09 is significant at a 0.05 alpha and 
## prog Academic 0.0051 is also significant at 0.05 alpha. So we will reject the null hypothesis. 

#(8) Do model comparisons do find out whether the predictors significantly improve model fit.

Pm2<-lm(num_awards ~ math , data = p)
summary(Pm2)

## If we comapre R square values of  both these models, Pm0 has high R square value of  0.2662
## than model Pm1 (0.2433). This concludes that after including predictor prog,  model Pm0
## explains 26.62 % of the variance in the outcome variable. Model Pm0 gets improved by including
## predictor prog

#(9) Compare to a model that uses a gaussian distribution (normal lm model) for this data.

Pm3<-glm(num_awards ~ math + prog, data = p)
summary(Pm3)
