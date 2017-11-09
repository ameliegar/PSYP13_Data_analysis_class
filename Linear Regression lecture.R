# Data collection form: https://goo.gl/forms/nNXWwCPTbdHxGrBO2


###########################################################
#                                                         #
#                 Loading packages                        #
#                                                         #
###########################################################

library(psych) # for describe
library(lm.beta) # for lm.beta
library(dplyr)
library(gsheet)
library(car) # for scatter3d, residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot
library(rgl) # for scatter3d

###########################################################
#                                                         #
#                 Custom functions                        #
#                                                         #
###########################################################

# function to plot distance from regression line (residual error)
# this is used for demonstration, not essential to statistical analysis

error_plotter <- function(mod, col = "black"){
  mod_vars = as.character(mod$call[2])
  data = eval(parse(text = as.character(mod$call[3])))
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern ='~',mod_vars))-2)
  x = substr(mod_vars, as.numeric(gregexpr(pattern ='~',mod_vars))+2, nchar(mod_vars))
  
  data$pred = predict(mod)
  
  if(x == "1"){x = "response_ID"
  data$response_ID = 1:nrow(data)}
  
  plot(data[,y] ~ data[,x], ylab = y, xlab = x)
  abline(mod)
  
  for(i in 1:nrow(data)){
    clip(min(data[,x]), max(data[,x]), min(data[i,c(y,"pred")]), max(data[i,c(y,"pred")]))
    abline(v = data[i,x], lty = 2, col = col)
  }
  
}



###### Simple regression


newdata = as.data.frame(gsheet2tbl("https://docs.google.com/spreadsheets/d/1RSGUhjNpDH4HQHIyqTHH-yHXx3X4YfFH4tzN51Q_hRA/edit?usp=sharing"))
mydata = as.data.frame(gsheet2tbl("https://docs.google.com/spreadsheets/d/1C07SRvPJzftZaFApiI5YgptyymvyYQpIUi9v6gFbLHw/edit?usp=sharing"))

mydata

### check data set for invalid data (e.g. coding errors)
# descriptive statistics
describe(mydata)

# histograms
hist(mydata$height, breaks = 20)
hist(mydata$shoe_size, breaks = 20)
# scatterplot
plot(shoe_size ~ height, data = mydata)


## how can I predict the outcome?

# find a pattern in the data instead of focusing on the individual datapoints

# run a regression analysis
mod1 <- lm(shoe_size ~ height, data = mydata)

# the regression model identifies the underlying pattern of the data
# and gives a prediction of the outcome value for
# each possible value of the predictors(s) 
# this is given in the for of a regression equation
# Y = b0 + b1*X

mod1




# you can make predictions by applying this equation by hand
# or just use the predict() function 
# (without specifying the data file, the predictions will be for 
# the data file on which the model was fit)
newdata$pred = predict(mod1, newdata = newdata)

# regression identifies the pattern by fitting a straight line to the data 
# that is the closest to all data points
# visalize regression line on scatterplot
abline(mod1)


# predicted values all fall on the regression line
points(predict(mod1) ~ height, data = mydata, col = "red")


## How to measure model performance? (Is this model prediction any good?)
# plot the residual error
error_plotter(mod1)

# sum of absolute differences between actual value and prediction
RAD = sum(abs(mydata$shoe_size - predict(mod1)))
RAD

# sum of squared differences between actual value and prediction
RSS = sum((mydata$shoe_size - predict(mod1))^2)
RSS



## How to measure if substantial information was gained with taking int account the predictor?
# Compare to best guess without the predictor

# model only using the mean of the outcome variable as a predictor
mod_mean <- lm(shoe_size ~ 1, data = mydata)
# plot the residual error
error_plotter(mod_mean, col = "red")
# sum of absolute differences between actual value and prediction
TAD = sum(abs(mydata$shoe_size - predict(mod_mean)))
TAD
# sum of squared differences between actual value and prediction
TSS = sum((mydata$shoe_size - predict(mod_mean))^2)
TSS








# the total amount of information gained about the variability of the 
# outcome is shown by the R squared statistic
1-(RSS/TSS)


# R^2 = 1 means all variablility of the outcome is perfectly predicted by the predictor(s)
plot(mydata$shoe_size, mydata$shoe_size)

# R^2 = 0 means no variablility  of the outcome is predicted by the predictor(s)
plot(sample(1:nrow(mydata)), mydata$shoe_size)




# Is the model with the predictor significantly better than a model without the predictor?
# do an anova to find out, comparing the amount of variance explained by the two models
anova(mod_mean, mod1)



## you can get all this information from the summary
summary(mod1)

# significance test for the predictor
cor.test(mydata$shoe_size, mydata$height)

# confidence interval of the regression coefficients
confint(mod1)

# plot the confidence intervals

ggplot(mydata, aes(x = height, y = shoe_size))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x)



###### Multiple regression
# using multiple predictors in linear regression models

# Housing sales dataset from Kaggel
data_house = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/data_house_small_sub.csv")

### Predicting apartment sales prices using sqfootage and house grade

### check data set for invalid data (e.g. coding errors)
# descriptive statistics
describe(data_house)

# histograms
hist(data_house$price, breaks = 30)
hist(data_house$sqft_living, breaks = 30)
hist(data_house$grade, breaks = 30)

# scatterplots
plot(price ~ sqft_living, data = data_house)
plot(price ~ grade, data = data_house)

# fit the regression model
mod_house1 = lm(price ~ sqft_living + grade, data = data_house)

# plotting the scatterplots for each predictor separately
# with the simple regression regression lines
plot(price ~ sqft_living, data = data_house)
abline(lm(price ~ sqft_living, data = data_house))
plot(price ~ grade, data = data_house)
abline(lm(price ~ grade, data = data_house))

# plot the regression plane (3D scatterplot with regression plane)
scatter3d(price ~ sqft_living + grade, data = data_house)

# the regression equation: Y = b0 + b1*X1 + b2*X2


mod_house1

predict(mod_house1, newdata = data_house)


### What to report in a publication or and home assignment
# interpreting the summary
summary(mod_house1)

AIC(mod_house1)

confint(mod_house1)

lm.beta(mod_house1)




### Which predictor had the most unique information added to the model?
# we need to calculate the standardized coefficents to be able to compare them
lm.beta(mod_house1)


### categorical variables can be predictors

mod_cat <- lm(shoe_size ~ height + gender, data = mydata)

mod_cat

summary(mod_cat)
# plot the relationship of shoe size and gender
# (you would have to specify that gender is a factor 
# type vector and not a character type vector for this plot to work well)
plot(mydata$shoe_size ~ factor(mydata$gender))


### higher order terms can be added as predictors
# but unless you know what you are doing, always
# add the first order term in the model as well, like here:
mod_house_quad <- lm(price ~ grade + I(grade^2), data = data_house)
summary(mod_house_quad)
plot(price ~ grade, data = data_house)

ggplot(data_house, aes(x = grade, y = price))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x+I(x^2))



### interactions can be predictors

# the effect of goegraphic location added to the model
mod_house_geolocation = lm(price ~ sqft_living + grade + long + lat, data = data_house)
summary(mod_house_geolocation)

# the effect of the interaction of latitude and longitude added to the model
mod_house_geolocation_inter1 = lm(price ~ sqft_living + grade + long + lat + I(long * lat), data = data_house)
summary(mod_house_geolocation_inter1)
# this will result in the same output:
mod_house_geolocation_inter2 = lm(price ~ sqft_living + grade + long * lat, data = data_house)
summary(mod_house_geolocation_inter2)




### Comparing models

# Hierarchical regression
# Quantify the amount of information gained about the variability of the outcome
# by adding pedictors in block certain predictors
mod_house2 <- lm(price ~ sqft_living + grade, data = data_house)
summary(mod_house2)

mod_house_geolocation_inter2 = lm(price ~ sqft_living + grade + long * lat, data = data_house)
summary(mod_house_geolocation_inter2)

# compare models with the anova function
# can only be used if the models are nested! (all of the predictor used in the
# smaller model are also used in the larger model)
anova(mod_house2, mod_house_geolocation_inter2)

# compare with AIC
# smaller AIC means more information
# if the difference in AIC of the two models is smaller than 2, they contain
# roughly the same information about the outcome
AIC(mod_house2)
AIC(mod_house_geolocation_inter2)


mod_house_geolocation_inter_neig = lm(price ~ sqft_living + grade + long * lat + sqft_living15 + sqft_lot15, data = data_house)
summary(mod_house_geolocation_inter_neig)

# you can enter multiple models into anova
anova(mod_house2, mod_house_geolocation_inter2, mod_house_geolocation_inter_neig)

AIC(mod_house2)
AIC(mod_house_geolocation_inter2)
AIC(mod_house_geolocation_inter_neig)
# these results indicate that we did not gain substantial new information about
# the variability of the outcome by entering information about the neighborhood
# into the model






### Model selection
# first rule of model selection:
# always go with the model that is grounded in theory and prior research
# because automatic model selection can lead to overfitting


# "predicting" variability of the outcome in your original data is easy

# if you fit a model that is too flexible, you will get perfect fit on your intitial data
plot(price ~ sqft_living, data = data_house[10:30,])
lines(price[order(sqft_living)] ~ sqft_living[order(sqft_living)], data = data_house[10:30,])

# but your model will not fit reality well
# this can be tested by collecting more data, and seeing how 
# well the model predicts the new data
# or, by designating a part of your data as a "training set", fitting the 
# model on that, and testing model performance on the other part, the "test set"

# training set with a felxible model
plot(price ~ sqft_living, data = data_house[10:30,], xlim = range(data_house[10:30,"sqft_living"]), ylim = range(data_house[10:30,"price"]))
lines(price[order(sqft_living)] ~ sqft_living[order(sqft_living)], data = data_house[10:30,])
# testing model performance on test set (fits poorly)
plot(price ~ sqft_living, data = data_house[30:50,], xlim = range(data_house[10:30,"sqft_living"]), ylim = range(data_house[10:30,"price"]))
lines(price[order(sqft_living)] ~ sqft_living[order(sqft_living)], data = data_house[10:30,])

# linear regression is very inflexible, so it is less prone to overfitting

# training set with a felxible model
plot(price ~ sqft_living, data = data_house[10:30,], xlim = range(data_house[10:30,"sqft_living"]), ylim = range(data_house[10:30,"price"]))
mod_for_plot <- lm(price ~ sqft_living, data = data_house[10:30,])
pred <- predict(mod_for_plot)
lines(pred[order(data_house[10:30,"sqft_living"])] ~ data_house[10:30,"sqft_living"][order(data_house[10:30,"sqft_living"])])
# testing model performance on test set (fits OK)
plot(price ~ sqft_living, data = data_house[30:50,], xlim = range(data_house[10:30,"sqft_living"]), ylim = range(data_house[10:30,"price"]))
lines(pred[order(data_house[10:30,"sqft_living"])] ~ data_house[10:30,"sqft_living"][order(data_house[10:30,"sqft_living"])])

# but the more predictors you include in the model, the more flexible it gets
# thus, overfitting becomes a problem again

# a model with the obvious predictors of house price
# NOTE that we only use the first hundred observation (half) of the dataset
# here, as a training set, so later we can test the performance 
# of our model on the other half
mod_house2_train <- lm(price ~ sqft_living + grade, data = data_house[1:100,])
summary(mod_house2_train)

# add 50 random variables to the list of predictors
rand_vars = as.data.frame(matrix(rnorm(mean = 0, sd = 1, n = 50*nrow(data_house)), ncol = 50))
data_house_withrandomvars = cbind(data_house, rand_vars)
lm_formula = as.formula(paste("price ~ sqft_living + grade + ", paste(names(rand_vars), collapse = " + "), sep = ""))

mod_house2_rand_train = lm(lm_formula, data = data_house_withrandomvars[1:100,])

# R squared increased a lot
summary(mod_house2_rand_train)

pred_train <- predict(mod_house2_train)
pred_train_rand <- predict(mod_house2_rand_train)
RSS_train = sum((data_house_withrandomvars[1:100,"price"] - pred_train)^2)
RSS_train_rand = sum((data_house_withrandomvars[1:100,"price"] - pred_train_rand)^2)
RSS_train
RSS_train_rand

# so is this model better, than the one without the random variablers?

# lets check model performance on the test set (the other half of the dataset)
# we predict the outcome (price) in the test set with the models fitted on 
# the training set
# NOTE that we did not re-fit the models on the test set, we use the models fitted
# on the training set for the prediction
pred_test <- predict(mod_house2_train, data_house_withrandomvars[101:200,])
pred_test_rand <- predict(mod_house2_rand_train, data_house_withrandomvars[101:200,])
# now we calculate the sum of squared residuals 
RSS_test = sum((data_house_withrandomvars[101:200,"price"] - pred_test)^2)
RSS_test_rand = sum((data_house_withrandomvars[101:200,"price"] - pred_test_rand)^2)
RSS_test
RSS_test_rand
# there is more error in the prediction of the model containing the random predictors


## What if I don't have a test-set?
# models can be compared with anova, which is sensitive to the number of predictors
anova(mod_house2_train, mod_house2_rand_train)

# you can compare adjusted R squared in the summary of the two models
# adjusted R squared is also sensitive to the number of predictors,
# but it is still not perfect
summary(mod_house2_train)
summary(mod_house2_rand_train)

# you can compare AIC
# it will usually choose the smaller model
AIC(mod_house2_train)
AIC(mod_house2_rand_train)


# backward regression
mod_back_train = step(mod_house2_rand_train, direction = "backward")

anova(mod_house2_train, mod_back_train)

summary(mod_back_train)

AIC(mod_house2_train)
AIC(mod_house2_rand_train)
AIC(mod_back_train)

# test the performance of backward regression model on the test set
# NOTE that we did not re-fit the models on the test set, we use the models fitted
# on the training set for the prediction
pred_test <- predict(mod_house2_train, data_house_withrandomvars[101:200,])
pred_test_back <- predict(mod_back_train, data_house_withrandomvars[101:200,])
# now we calculate the sum of squared residuals 
RSS_test = sum((data_house_withrandomvars[101:200,"price"] - pred_test)^2)
RSS_test_back = sum((data_house_withrandomvars[101:200,"price"] - pred_test_back)^2)
RSS_test
RSS_test_back
# error is larger in the backward regression model


# the same happens if you pre-test correlation of predictors with the outcome,
# and only include predictors that are significantly correlated with the outcome.
# that way, you are overfitting your model to your training data


# BOTTOM LINE: model selection should be done pre-analysis, based on previous evidence
# post-hoc result-driven model/predictor selection can lead to overfitting
# the only good test of a model's true performance is to test
# the accuracy of its predictions on new data (or a set-asid test set)









###########################################################
#                   Model diagnostics                     #
###########################################################


### Model diagnostics
# Fit the final model (containing all predictors)
mod_house2 = lm(price ~ sqft_living + grade, data = data_house)

# checking for influential outliers

plot(price ~ sqft_living, data = data_house)

plot(mod_house2, which = 4)
plot(mod_house2, which = 5)

## checking assumptions

# normality assumption
# QQ plot
plot(mod_house2, which = 2)
# skew and kurtosis
describe(residuals(mod_house2))
# histogram
hist(residuals(mod_house2), breaks = 20)



# linearity assumption
# predicted values against actual values
pred <- predict( object = mod_house2 )
plot( x = pred, y = data_house$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_house2, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_house2)

# homoscedasticty assumption (homogeneity of variance)
plot(mod_house2, which = 3)
ncvTest(mod_house2)

# multicollinearity (VIF above 5), or a VIF threshold of 3 is recommended in this paper: http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full
# some info about VIF: 
# https://statisticalhorizons.com/multicollinearity
# http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis
vif(mod_house2)
pairs.panels(data_house[,c("price", "sqft_living", "grade")], col = "red", lm = T)





# a more problematic case
# Fit the final model (containing all predictors)
mod_house_geolocation_inter2 = lm(price ~ sqft_living + grade + long * lat, data = data_house[-186,])

# checking for influential outliers

plot(price ~ sqft_living, data = data_house)

plot(mod_house_geolocation_inter2, which = 4)
plot(mod_house_geolocation_inter2, which = 5)

## checking assumptions

# normality assumption
# QQ plot
plot(mod_house_geolocation_inter2, which = 2)
# skew and kurtosis
describe(residuals(mod_house_geolocation_inter2))
# histogram
hist(residuals(mod_house_geolocation_inter2), breaks = 20)



# linearity assumption
# predicted values against actual values
pred <- predict( object = mod_house_geolocation_inter2 )
plot( x = pred, y = data_house$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_house_geolocation_inter2, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_house_geolocation_inter2)

# homoscedasticty assumption (homogeneity of variance)
plot(mod_house_geolocation_inter2, which = 3)
ncvTest(mod_house_geolocation_inter2)

# multicollinearity (VIF above 5), or a VIF threshold of 3 is recommended in this paper: http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full
# some info about VIF: 
# https://statisticalhorizons.com/multicollinearity
# http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis
vif(mod_house_geolocation_inter2)
pairs.panels(data_house[,c("price", "sqft_living", "grade", "long", "lat")], col = "red", lm = T)
