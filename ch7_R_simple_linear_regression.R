# See ch7_R_simple_linear_regression.pdf

# ch7_R_simple_linear_regression.R

# Code from Chapter 7 of Applied Statistics


# Text and code used from:
# Applied Statistics with R
# 2021-07-23

# The license for Applied Statistics with R is given in the line below.
# This work is licensed under a Creative Commons Attribution- NonCommercial-ShareAlike 4.0 International License.

# The most current version of Applied Statistics with R should be available at:
# https://github.com/daviddalpiaz/appliedstats



# Chapter 7 Simple Linear Regression
# 
# “All models are wrong, but some are useful.”
# 
# — George E. P. Box
# 
# After reading this chapter you will be able to:
#   
# Understand the concept of a model.
# Describe two ways in which regression coefficients are derived.
# Estimate and visualize a regression model using R.
# Interpret regression coefficients and statistics in the context of real-world problems.
# Use a regression model to make predictions.
# 
# 7.1 Modeling
# 
# Let’s consider a simple example of how the speed of a car affects its stopping distance, that is, how far it travels before it comes to a stop. To examine this relationship, we will use the cars dataset which, is a default R dataset. Thus, we don’t need to load a package first; it is immediately available.
# 
# To get a first look at the data you can use the View() function inside RStudio.

View(cars)

# We could also take a look at the variable names, the dimension of the data frame, and some sample observations with str().

str(cars)
## 'data.frame':    50 obs. of  2 variables:
##  $ speed: num  4 4 7 7 8 9 10 10 10 11 ...
##  $ dist : num  2 10 4 22 16 10 18 26 34 17 ...

# As we have seen before with data frames, there are a number of additional functions to access some of this information directly.

dim(cars)
## [1] 50  2
nrow(cars)
## [1] 50
ncol(cars)
## [1] 2

#Other than the two variable names and the number of observations, this data is still just a bunch of numbers, so we should probably obtain some context.

?cars

#Reading the documentation we learn that this is data gathered during the 1920s about the speed of cars and the resulting distance it takes for the car to come to a stop. The interesting task here is to determine how far a car travels before stopping, when traveling at a certain speed. So, we will first plot the stopping distance against the speed.

plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey")


# 7.1.1 Simple Linear Regression Model

# 7.2 Least Squares Approach


x = cars$speed
y = cars$dist

# calculate three sums of squares 

Sxy = sum((x - mean(x)) * (y - mean(y)))
Sxx = sum((x - mean(x)) ^ 2)
Syy = sum((y - mean(y)) ^ 2)
c(Sxy, Sxx, Syy)

## [1]  5387.40  1370.00 32538.98

beta_1_hat = Sxy / Sxx
beta_0_hat = mean(y) - beta_1_hat * mean(x)
c(beta_0_hat, beta_1_hat)
## [1] -17.579095   3.932409


# We can now use this line to make predictions. First, let’s see the possible  
# x
# values in the cars dataset. Since some  
# x
# values may appear more than once, we use the unique() to return each unique value only once.

unique(cars$speed)
##  [1]  4  7  8  9 10 11 12 13 14 15 16 17 18 19 20 22 23 24 25

# Let’s make a prediction for the stopping distance of a car traveling at 8 miles per hour.

beta_0_hat + beta_1_hat * 8
## [1] 13.88018

# This tells us that the estimated mean stopping distance of a car traveling at 8 miles per hour is  
# 13.88
# .
# 
# Now let’s make a prediction for the stopping distance of a car traveling at 21 miles per hour. This is considered interpolation as 21 is not an observed value of  
# x
# . (But is in the data range.) We can use the special %in% operator to quickly verify this in R.

8 %in% unique(cars$speed)
## [1] TRUE
21 %in% unique(cars$speed)
## [1] FALSE
min(cars$speed) < 21 & 21 < max(cars$speed)
## [1] TRUE


beta_0_hat + beta_1_hat * 21
## [1] 65.00149

# Lastly, we can make a prediction for the stopping distance of a car traveling at 50 miles per hour. This is considered extrapolation as 50 is not an observed value of  
# x
# and is outside data range. We should be less confident in predictions of this type.

range(cars$speed)
## [1]  4 25
range(cars$speed)[1] < 50 & 50 < range(cars$speed)[2] 
## [1] FALSE


beta_0_hat + beta_1_hat * 50
## [1] 179.0413

# Cars travel 50 miles per hour rather easily today, but not in the 1920s!
  
# This is also an issue we saw when interpreting  
# ^
#   β
# 0
# =
#   −
# 17.58
# , which is equivalent to making a prediction at  
# x
# =
#   0
#  We should not be confident in the estimated linear relationship outside of the range of data we have observed.



# 7.2.2 Residuals

# If we think of our model as “Response = Prediction + Error,” we can then write it as
# 
# y
# =
#   ^
#   y
# +
#   e
# 
# 
# 
# We then define a residual to be the observed value minus the predicted value.
# 
# e
# i
# =
#   y
# i
# −
# ^
#   y
# i


# Let’s calculate the residual for the prediction we made for a car traveling 8 miles per hour. First, we need to obtain the observed value of  


which(cars$speed == 8)
## [1] 5
cars[5, ]
##   speed dist
## 5     8   16
cars[which(cars$speed == 8), ]
##   speed dist
## 5     8   16

# We can then calculate the residual.

16 - (beta_0_hat + beta_1_hat * 8)
## [1] 2.119825

#The positive residual value indicates that the observed stopping distance is actually 2.12 feet more than what was predicted.

# 7.2.3 Variance Estimation
# We’ll now use the residuals for each of the points to create an estimate for the variance


y_hat = beta_0_hat + beta_1_hat * x
e     = y - y_hat
n     = length(e)
s2_e  = sum(e^2) / (n - 2)
s2_e
## [1] 236.5317

# Just as with the univariate measure of variance, this value of 236.53 doesn’t have a practical interpretation in terms of stopping distance. Taking the square root, however, computes the standard deviation of the residuals, also known as residual standard error.

s_e = sqrt(s2_e)
s_e
## [1] 15.37959

# This tells us that our estimates of mean stopping distance are “typically” off by 15.38 feet.

# 7.3 Decomposition of Variation

# The quantity “Sum of Squares Error,”  
# SSE
# , represents the unexplained variation of the observed  
# y
# values. You will often see  
# SSE
# written as  
# RSS
# , or “Residual Sum of Squares.”

SST   = sum((y - mean(y)) ^ 2)
SSReg = sum((y_hat - mean(y)) ^ 2)
SSE   = sum((y - y_hat) ^ 2)
c(SST = SST, SSReg = SSReg, SSE = SSE)
##      SST    SSReg      SSE 
## 32538.98 21185.46 11353.52

SSE / (n - 2)
## [1] 236.5317


s2_e == SSE / (n - 2)
## [1] TRUE


#These three measures also do not have an important practical interpretation individually. But together, they’re about to reveal a new statistic to help measure the strength of a SLR model.

# 7.3.1 Coefficient of Determination R2
# 
# The coefficient of determination is interpreted as the proportion of observed variation in  
# y that can be explained by the simple linear regression model.

R2 = SSReg / SST
R2
## [1] 0.6510794



# 7.4 The lm Function
# 
# So far we have done regression by deriving the least squares estimates, then writing simple R commands to perform the necessary calculations. Since this is such a common task, this is functionality that is built directly into R via the lm() command.
# 
# The lm() command is used to fit linear models which actually account for a broader class of models than simple linear regression, but we will use SLR as our first demonstration of lm(). The lm() function will be one of our most commonly used tools, so you may want to take a look at the documentation by using ?lm. You’ll notice there is a lot of information there, but we will start with just the very basics. This is documentation you will want to return to often.
# 
# We’ll continue using the cars data, and essentially use the lm() function to check the work we had previously done.

stop_dist_model = lm(dist ~ speed, data = cars)

# This line of code fits our very first linear model. The syntax should look somewhat familiar. We use the dist ~ speed syntax to tell R we would like to model the response variable dist as a linear function of the predictor variable speed. In general, you should think of the syntax as response ~ predictor. The data = cars argument then tells R that that dist and speed variables are from the dataset cars. We then store this result in a variable stop_dist_model.
# 
# The variable stop_dist_model now contains a wealth of information, and we will now see how to extract and use that information. The first thing we will do is simply output whatever is stored immediately in the variable stop_dist_model.

stop_dist_model
## 
## Call:
## lm(formula = dist ~ speed, data = cars)
## 
## Coefficients:
## (Intercept)        speed  
##     -17.579        3.932

# We see that it first tells us the formula we input into R, that is lm(formula = dist ~ speed, data = cars). We also see the coefficients of the model. We can check that these are what we had calculated previously. (Minus some rounding that R is doing when displaying the results. They are stored with full precision.)

c(beta_0_hat, beta_1_hat)
## [1] -17.579095   3.932409

# Next, it would be nice to add the fitted line to the scatterplot. To do so we will use the abline() function.

plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey")

abline(stop_dist_model, lwd = 3, col = "darkorange")


# The abline() function is used to add lines of the form  
# a
# +
#   b
# x
# to a plot. (Hence abline.) When we give it stop_dist_model as an argument, it automatically extracts the regression coefficient estimates ( 
#  
#    ^
#     β
#   0
#   and  
#   ^
#     β
#   1
# ) and uses them as the slope and intercept of the line. Here we also use lwd to modify the width of the line, as well as col to modify the color of the line.
# 
# The “thing” that is returned by the lm() function is actually an object of class lm which is a list. The exact details of this are unimportant unless you are seriously interested in the inner-workings of R, but know that we can determine the names of the elements of the list using the names() command.

names(stop_dist_model)
##  [1] "coefficients"  "residuals"     "effects"       "rank"         
##  [5] "fitted.values" "assign"        "qr"            "df.residual"  
##  [9] "xlevels"       "call"          "terms"         "model"

# We can then use this information to, for example, access the residuals using the $ operator.

stop_dist_model$residuals
##          1          2          3          4          5          6          7 
##   3.849460  11.849460  -5.947766  12.052234   2.119825  -7.812584  -3.744993 
##          8          9         10         11         12         13         14 
##   4.255007  12.255007  -8.677401   2.322599 -15.609810  -9.609810  -5.609810 
##         15         16         17         18         19         20         21 
##  -1.609810  -7.542219   0.457781   0.457781  12.457781 -11.474628  -1.474628 
##         22         23         24         25         26         27         28 
##  22.525372  42.525372 -21.407036 -15.407036  12.592964 -13.339445  -5.339445 
##         29         30         31         32         33         34         35 
## -17.271854  -9.271854   0.728146 -11.204263   2.795737  22.795737  30.795737 
##         36         37         38         39         40         41         42 
## -21.136672 -11.136672  10.863328 -29.069080 -13.069080  -9.069080  -5.069080 
##         43         44         45         46         47         48         49 
##   2.930920  -2.933898 -18.866307  -6.798715  15.201285  16.201285  43.201285 
##         50 
##   4.268876

#Another way to access stored information in stop_dist_model are the coef(), resid(), and fitted() functions. These return the coefficients, residuals, and fitted values, respectively.

coef(stop_dist_model)
## (Intercept)       speed 
##  -17.579095    3.932409
resid(stop_dist_model)
##          1          2          3          4          5          6          7 
##   3.849460  11.849460  -5.947766  12.052234   2.119825  -7.812584  -3.744993 
##          8          9         10         11         12         13         14 
##   4.255007  12.255007  -8.677401   2.322599 -15.609810  -9.609810  -5.609810 
##         15         16         17         18         19         20         21 
##  -1.609810  -7.542219   0.457781   0.457781  12.457781 -11.474628  -1.474628 
##         22         23         24         25         26         27         28 
##  22.525372  42.525372 -21.407036 -15.407036  12.592964 -13.339445  -5.339445 
##         29         30         31         32         33         34         35 
## -17.271854  -9.271854   0.728146 -11.204263   2.795737  22.795737  30.795737 
##         36         37         38         39         40         41         42 
## -21.136672 -11.136672  10.863328 -29.069080 -13.069080  -9.069080  -5.069080 
##         43         44         45         46         47         48         49 
##   2.930920  -2.933898 -18.866307  -6.798715  15.201285  16.201285  43.201285 
##         50 
##   4.268876
fitted(stop_dist_model)
##         1         2         3         4         5         6         7         8 
## -1.849460 -1.849460  9.947766  9.947766 13.880175 17.812584 21.744993 21.744993 
##         9        10        11        12        13        14        15        16 
## 21.744993 25.677401 25.677401 29.609810 29.609810 29.609810 29.609810 33.542219 
##        17        18        19        20        21        22        23        24 
## 33.542219 33.542219 33.542219 37.474628 37.474628 37.474628 37.474628 41.407036 
##        25        26        27        28        29        30        31        32 
## 41.407036 41.407036 45.339445 45.339445 49.271854 49.271854 49.271854 53.204263 
##        33        34        35        36        37        38        39        40 
## 53.204263 53.204263 53.204263 57.136672 57.136672 57.136672 61.069080 61.069080 
##        41        42        43        44        45        46        47        48 
## 61.069080 61.069080 61.069080 68.933898 72.866307 76.798715 76.798715 76.798715 
##        49        50 
## 76.798715 80.731124

# An R function that is useful in many situations is summary(). We see that when it is called on our model, it returns a good deal of information. By the end of the course, you will know what every value here is used for. For now, you should immediately notice the coefficient estimates, and you may recognize the  
# R
# 2
# value we saw earlier.

summary(stop_dist_model)
## 
## Call:
## lm(formula = dist ~ speed, data = cars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -29.069  -9.525  -2.272   9.215  43.201 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -17.5791     6.7584  -2.601   0.0123 *  
## speed         3.9324     0.4155   9.464 1.49e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.38 on 48 degrees of freedom
## Multiple R-squared:  0.6511, Adjusted R-squared:  0.6438 
## F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12

# The summary() command also returns a list, and we can again use names() to learn what about the elements of this list.

names(summary(stop_dist_model))
##  [1] "call"          "terms"         "residuals"     "coefficients" 
##  [5] "aliased"       "sigma"         "df"            "r.squared"    
##  [9] "adj.r.squared" "fstatistic"    "cov.unscaled"

# So, for example, if we wanted to directly access the value of  
# R
# 2
# , instead of copy and pasting it out of the printed statement from summary(), we could do so.

summary(stop_dist_model)$r.squared
## [1] 0.6510794

# Another value we may want to access   

summary(stop_dist_model)$sigma
## [1] 15.37959

# Note that this is the same result seen earlier as s_e. You may also notice that this value was displayed above as a result of the summary() command, which R labeled the “Residual Standard Error.”

# Often it is useful to talk about  
# s
# e
# (or RSE) instead of  
# s
# 2
# e
# because of their units. The units of  
# s
# e
# in the cars example is feet, while the units of  
# s
# 2
# e
# is feet-squared.
# 
# Another useful function, which we will use almost as often as lm() is the predict() function.

predict(stop_dist_model, newdata = data.frame(speed = 8))
##        1 
## 13.88018

# The above code reads “predict the stopping distance of a car traveling 8 miles per hour using the stop_dist_model.” Importantly, the second argument to predict() is a data frame that we make in place. We do this so that we can specify that 8 is a value of speed, so that predict knows how to use it with the model stored in stop_dist_model. We see that this result is what we had calculated “by hand” previously.
# 
# We could also predict multiple values at once.

predict(stop_dist_model, newdata = data.frame(speed = c(8, 21, 50)))
##         1         2         3 
##  13.88018  65.00149 179.04134


# Or we could calculate the fitted value for each of the original data points. We can simply supply the original data frame, cars, since it contains a variables called speed which has the values we would like to predict at.

predict(stop_dist_model, newdata = cars)
##         1         2         3         4         5         6         7         8 
## -1.849460 -1.849460  9.947766  9.947766 13.880175 17.812584 21.744993 21.744993 
##         9        10        11        12        13        14        15        16 
## 21.744993 25.677401 25.677401 29.609810 29.609810 29.609810 29.609810 33.542219 
##        17        18        19        20        21        22        23        24 
## 33.542219 33.542219 33.542219 37.474628 37.474628 37.474628 37.474628 41.407036 
##        25        26        27        28        29        30        31        32 
## 41.407036 41.407036 45.339445 45.339445 49.271854 49.271854 49.271854 53.204263 
##        33        34        35        36        37        38        39        40 
## 53.204263 53.204263 53.204263 57.136672 57.136672 57.136672 61.069080 61.069080 
##        41        42        43        44        45        46        47        48 
## 61.069080 61.069080 61.069080 68.933898 72.866307 76.798715 76.798715 76.798715 
##        49        50 
## 76.798715 80.731124
# predict(stop_dist_model, newdata = data.frame(speed = cars$speed))

# This is actually equivalent to simply calling predict() on stop_dist_model without a second argument.

predict(stop_dist_model)
##         1         2         3         4         5         6         7         8 
## -1.849460 -1.849460  9.947766  9.947766 13.880175 17.812584 21.744993 21.744993 
##         9        10        11        12        13        14        15        16 
## 21.744993 25.677401 25.677401 29.609810 29.609810 29.609810 29.609810 33.542219 
##        17        18        19        20        21        22        23        24 
## 33.542219 33.542219 33.542219 37.474628 37.474628 37.474628 37.474628 41.407036 
##        25        26        27        28        29        30        31        32 
## 41.407036 41.407036 45.339445 45.339445 49.271854 49.271854 49.271854 53.204263 
##        33        34        35        36        37        38        39        40 
## 53.204263 53.204263 53.204263 57.136672 57.136672 57.136672 61.069080 61.069080 
##        41        42        43        44        45        46        47        48 
## 61.069080 61.069080 61.069080 68.933898 72.866307 76.798715 76.798715 76.798715 
##        49        50 
## 76.798715 80.731124

# Note that then in this case, this is the same as using fitted().

fitted(stop_dist_model)
##         1         2         3         4         5         6         7         8 
## -1.849460 -1.849460  9.947766  9.947766 13.880175 17.812584 21.744993 21.744993 
##         9        10        11        12        13        14        15        16 
## 21.744993 25.677401 25.677401 29.609810 29.609810 29.609810 29.609810 33.542219 
##        17        18        19        20        21        22        23        24 
## 33.542219 33.542219 33.542219 37.474628 37.474628 37.474628 37.474628 41.407036 
##        25        26        27        28        29        30        31        32 
## 41.407036 41.407036 45.339445 45.339445 49.271854 49.271854 49.271854 53.204263 
##        33        34        35        36        37        38        39        40 
## 53.204263 53.204263 53.204263 57.136672 57.136672 57.136672 61.069080 61.069080 
##        41        42        43        44        45        46        47        48 
## 61.069080 61.069080 61.069080 68.933898 72.866307 76.798715 76.798715 76.798715 
##        49        50 
## 76.798715 80.731124

#7.5 Maximum Likelihood Estimation (MLE) Approach

# Our goal is to find values of  
# β
# 0
# ,  
# β
# 1
# , and  
# σ
# 2
# which maximize this function, which is a straightforward multivariate calculus problem.



# Note that we use  
# log
# to mean the natural logarithm. 


# 7.6 Simulating SLR
# 
# We return again to more examples of simulation. This will be a common theme!
#   
# In practice you will almost never have a true model, and you will use data to attempt to recover information about the unknown true model. 
# With simulation, we decide the true model and simulate data from it. Then, we apply a method to the data, in this case least squares. 
# Now, since we know the true model, we can assess how well it did.

# For this example, we will simulate  
# n = 21 observations 

# We first set the true parameters of the model to be simulated.

num_obs = 21
beta_0  = 5
beta_1  = -2
sigma   = 3

# Next, we obtain simulated values of  
# ϵi
# after setting a seed for reproducibility.

set.seed(1)
epsilon = rnorm(n = num_obs, mean = 0, sd = sigma)

x_vals = seq(from = 0, to = 10, length.out = num_obs)

# Another common practice is to generate them from a uniform distribution, and then use them for the remainder of the analysis.
# set.seed(1)
# x_vals = runif(num_obs, 0, 10)

y_vals = beta_0 + beta_1 * x_vals + epsilon

# The data,  
# (
#   x
#   i
#   ,
#   y
#   i
# )
# represent a possible sample from the true distribution. Now to check how well the method of least squares works, we use lm() to fit the model to our simulated data, then take a look at the estimated coefficients.

sim_fit = lm(y_vals ~ x_vals)
coef(sim_fit)
## (Intercept)      x_vals 
##    4.832639   -1.831401

# And look at that, they aren’t too far from the true parameters we specified!
  
plot(y_vals ~ x_vals)
abline(sim_fit)

# We should say here, that we’re being sort of lazy, and not the good kinda of lazy that could be considered efficient. Any time you simulate data, you should consider doing two things: writing a function, and storing the data in a data frame.
# 
# The function below, sim_slr(), can be used for the same task as above, but is much more flexible. Notice that we provide x to the function, instead of generating x inside the function. In the SLR model, the  
# x
# i
# are considered known values. That is, they are not random, so we do not assume a distribution for the  
# x
# i
# . Because of this, we will repeatedly use the same x values across all simulations.

sim_slr = function(x, beta_0 = 10, beta_1 = 5, sigma = 1) {
  n = length(x)
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}

# Here, we use the function to repeat the analysis above.

set.seed(1)
sim_data = sim_slr(x = x_vals, beta_0 = 5, beta_1 = -2, sigma = 3)

# This time, the simulated observations are stored in a data frame.

head(sim_data)
##   predictor   response
## 1       0.0  3.1206386
## 2       0.5  4.5509300
## 3       1.0  0.4931142
## 4       1.5  6.7858424
## 5       2.0  1.9885233
## 6       2.5 -2.4614052

# Now when we fit the model with lm() we can use a data argument, a very good practice.

sim_fit = lm(response ~ predictor, data = sim_data)
coef(sim_fit)
## (Intercept)   predictor 
##    4.832639   -1.831401

# And this time, we’ll make the plot look a lot nicer.

plot(response ~ predictor, data = sim_data,
     xlab = "Simulated Predictor Variable",
     ylab = "Simulated Response Variable",
     main = "Simulated Regression Data",
     pch  = 20,
     cex  = 2,
     col  = "grey")

abline(sim_fit, lwd = 3, lty = 1, col = "darkorange")
abline(beta_0, beta_1, lwd = 3, lty = 2, col = "dodgerblue")
legend("topright", c("Estimate", "Truth"), lty = c(1, 2), lwd = 2,
       col = c("darkorange", "dodgerblue"))


# 7.7 History
# For some brief background on the history of linear regression, see “Galton, Pearson, and the Peas: A Brief History of Linear Regression for Statistics Instructors” from the Journal of Statistics Education as well as the Wikipedia page on the history of regression analysis and lastly the article for regression to the mean which details the origins of the term “regression.”







