# See ch8_inference_for_simple_linear_regression.pdf

# ch8_inference_for_simple_linear_regression.R

# Code from Chapter 8 of Applied Statistics

# Text and code used from:
# Applied Statistics with R
# 2021-07-23

# The license for Applied Statistics with R is given in the line below.
# This work is licensed under a Creative Commons Attribution- NonCommercial-ShareAlike 4.0 International License.

# The most current version of Applied Statistics with R should be available at:
# https://github.com/daviddalpiaz/appliedstats


# Chapter 8 Inference for Simple Linear Regression
# 
# After reading this chapter you will be able to:
#   
# Understand the distributions of regression estimates.
# Create interval estimates for regression parameters, mean response, and predictions.
# Test for significance of regression.


# When applied to the cars data, we obtained the following results:
  
stop_dist_model = lm(dist ~ speed, data = cars)

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

# Last chapter, we only discussed the Estimate, Residual standard error, and Multiple R-squared values. In this chapter, we will discuss all of the information under Coefficients as well as F-statistic.

plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey")

abline(stop_dist_model, lwd = 5, col = "darkorange")


# 8.1 Gauss–Markov Theorem

# The Gauss–Markov theorem tells us that when estimating the parameters of the simple linear regression model  
# β
# 0
# and  
# β
# 1
# , the  
# ^
#   β
# 0
# and  
# ^
#   β
# 1
# which we derived are the best linear unbiased estimates, or BLUE for short. (The actual conditions for the Gauss–Markov theorem are more relaxed than the SLR model.)

# 8.2 Sampling Distributions
# 
# Now that we have “redefined” the estimates for  
# ^
#   β
# 0
# and  
# ^
#   β
# 1
# as random variables, we can discuss their sampling distribution, which is the distribution when a statistic is considered a random variable.


# 8.2.1 Simulating Sampling Distributions

# To verify the above results, we will simulate samples of size  
# n = 100

# First we need to decide ahead of time what our  
# x
# values will be for this simulation

set.seed(42)
sample_size = 100 # this is n
x = seq(-1, 1, length = sample_size)
Sxx = sum((x - mean(x)) ^ 2)

# We also fix our parameter values.

beta_0 = 3
beta_1 = 6
sigma  = 2

# With this information, we know the sampling distributions should be:
  
(var_beta_1_hat = sigma ^ 2 / Sxx)
## [1] 0.1176238
(var_beta_0_hat = sigma ^ 2 * (1 / sample_size + mean(x) ^ 2 / Sxx))
## [1] 0.04

# We now simulate data from this model 10,000 times. Note this may not be the most R way of doing the simulation. We perform the simulation in this manner in an attempt at clarity. For example, we could have used the sim_slr() function from the previous chapter. We also simply store variables in the global environment instead of creating a data frame for each new simulated dataset.

num_samples = 10000
beta_0_hats = rep(0, num_samples)
beta_1_hats = rep(0, num_samples)

for (i in 1:num_samples) {
  eps = rnorm(sample_size, mean = 0, sd = sigma)
  y   = beta_0 + beta_1 * x + eps
  
  sim_model = lm(y ~ x)
  
  beta_0_hats[i] = coef(sim_model)[1]
  beta_1_hats[i] = coef(sim_model)[2]
}

# Each time we simulated the data, we obtained values of the estimated coefficiets. The variables beta_0_hats and beta_1_hats now store 10,000 simulated values of  

# We first verify the distribution of  
# 
#   ^β1


mean(beta_1_hats) # empirical mean
## [1] 6.001998
beta_1            # true mean
## [1] 6
var(beta_1_hats)  # empirical variance
## [1] 0.11899
var_beta_1_hat    # true variance
## [1] 0.1176238

# We see that the empirical and true means and variances are very similar. We also verify that the empirical distribution is normal. To do so, we plot a histogram of the beta_1_hats, and add the curve for the true distribution of  
# ^β1
# 
# We use prob = TRUE to put the histogram on the same scale as the normal curve.

# note need to use prob = TRUE

hist(beta_1_hats, prob = TRUE, breaks = 20, 
     xlab = expression(hat(beta)[1]), main = "", border = "dodgerblue")
curve(dnorm(x, mean = beta_1, sd = sqrt(var_beta_1_hat)), 
      col = "darkorange", add = TRUE, lwd = 3)


# We then repeat the process for  
# ^β0


mean(beta_0_hats) # empirical mean
## [1] 3.001147
beta_0            # true mean
## [1] 3
var(beta_0_hats)  # empirical variance
## [1] 0.04017924
var_beta_0_hat    # true variance
## [1] 0.04
hist(beta_0_hats, prob = TRUE, breaks = 25, 
     xlab = expression(hat(beta)[0]), main = "", border = "dodgerblue")
curve(dnorm(x, mean = beta_0, sd = sqrt(var_beta_0_hat)),
      col = "darkorange", add = TRUE, lwd = 3)


#In this simulation study, we have only simulated a finite number of samples. To truly verify the distributional results, we would need to observe an infinite number of samples. However, the following plot should make it clear that if we continued simulating, the empirical results would get closer and closer to what we should expect.

par(mar = c(5, 5, 1, 1)) # adjusted plot margins, otherwise the "hat" does not display
plot(cumsum(beta_1_hats) / (1:length(beta_1_hats)), type = "l", ylim = c(5.95, 6.05),
     xlab = "Number of Simulations",
     ylab = expression("Empirical Mean of " ~ hat(beta)[1]),
     col  = "dodgerblue")
abline(h = 6, col = "darkorange", lwd = 2)


par(mar = c(5, 5, 1, 1)) # adjusted plot margins, otherwise the "hat" does not display
plot(cumsum(beta_0_hats) / (1:length(beta_0_hats)), type = "l", ylim = c(2.95, 3.05),
     xlab = "Number of Simulations",
     ylab = expression("Empirical Mean of " ~ hat(beta)[0]),
     col  = "dodgerblue")
abline(h = 3, col = "darkorange", lwd = 2)


#8.3 Standard Errors

# So now we believe the distributional results

# Then by standardizing these results



# Since we don’t know  
# σ
# in practice, we will have to estimate it using  
# se
# , which we plug into our existing expression for the standard deviations of our estimates.
# 
# These two new expressions are called standard errors which are the estimated standard deviations of the sampling distributions.

# Now if we divide by the standard error, instead of the standard deviation, we obtain the following results which will allow us to make confidence intervals and perform hypothesis testing.

# Recall that a  t distribution is similar to a standard normal, but with heavier tails. As the degrees of freedom increases, the  
# t
# distribution becomes more and more like a standard normal. Below we plot a standard normal distribution as well as two examples of a  
# t distribution with different degrees of freedom. Notice how the  
# t distribution with the larger degrees of freedom is more similar to the standard normal curve.

# define grid of x values
x = seq(-4, 4, length = 100)

# plot curve for standard normal
plot(x, dnorm(x), type = "l", lty = 1, lwd = 2, xlab = "x", ylab = "Density", main = "Normal vs t Distributions")
# add curves for t distributions
lines(x, dt(x, df = 1), lty = 3, lwd = 2, col = "darkorange")
lines(x, dt(x, df = 10), lty = 2, lwd = 2, col = "dodgerblue")

# add legend
legend("topright", title = "Distributions",
       legend = c("t, df = 1", "t, df = 10", "Standard Normal"), 
       lwd = 2, lty = c(3, 2, 1), col = c("darkorange", "dodgerblue", "black"))


# 8.4 Confidence Intervals for Slope and Intercept
# 
# Recall that confidence intervals for means often take the form:
#   
# EST
# ±
# CRIT
# ⋅
# SE
# 
# or
# 
# EST
# ±
# MARGIN
# 
# where  
# EST
# is an estimate for the parameter of interest,  
# SE
# is the standard error of the estimate, and  
# MARGIN
# =
#   CRIT
# ⋅
# SE


# 8.5 Hypothesis Tests
# “We may speak of this hypothesis as the ‘null hypothesis’, and it should be noted that the null hypothesis is never proved or established, but is possibly disproved, in the course of experimentation.”
# 
# — Ronald Aylmer Fisher


# 8.6 cars Example
# We now return to the cars example from last chapter to illustrate these concepts. We first fit the model using lm() then use summary() to view the results in greater detail.

stop_dist_model = lm(dist ~ speed, data = cars)
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

# 8.6.1 Tests in R
# 
# We will now discuss the results displayed called Coefficients. First recall that we can extract this information directly.

names(summary(stop_dist_model))
##  [1] "call"          "terms"         "residuals"     "coefficients" 
##  [5] "aliased"       "sigma"         "df"            "r.squared"    
##  [9] "adj.r.squared" "fstatistic"    "cov.unscaled"
summary(stop_dist_model)$coefficients
##               Estimate Std. Error   t value     Pr(>|t|)
## (Intercept) -17.579095  6.7584402 -2.601058 1.231882e-02
## speed         3.932409  0.4155128  9.463990 1.489836e-12

# The names() function tells us what information is available, and then we use the $ operator and coefficients to extract the information we are interested in. Two values here should be immediately familiar.


summary(stop_dist_model)$coefficients[2,]
##     Estimate   Std. Error      t value     Pr(>|t|) 
## 3.932409e+00 4.155128e-01 9.463990e+00 1.489836e-12

# Pr(>|t|), gives us the p-value of that test.


summary(stop_dist_model)$coefficients[1,]
##     Estimate   Std. Error      t value     Pr(>|t|) 
## -17.57909489   6.75844017  -2.60105800   0.01231882

#In summary, the following code stores the information of summary(stop_dist_model)$coefficients in a new variable stop_dist_model_test_info, then extracts each element into a new variable which describes the information it contains.

stop_dist_model_test_info = summary(stop_dist_model)$coefficients

beta_0_hat      = stop_dist_model_test_info[1, 1] # Estimate
beta_0_hat_se   = stop_dist_model_test_info[1, 2] # Std. Error
beta_0_hat_t    = stop_dist_model_test_info[1, 3] # t value
beta_0_hat_pval = stop_dist_model_test_info[1, 4] # Pr(>|t|)

beta_1_hat      = stop_dist_model_test_info[2, 1] # Estimate
beta_1_hat_se   = stop_dist_model_test_info[2, 2] # Std. Error
beta_1_hat_t    = stop_dist_model_test_info[2, 3] # t value
beta_1_hat_pval = stop_dist_model_test_info[2, 4] # Pr(>|t|)






# This automatically calculates 99% confidence intervals for both  
# β
# 0
# and  
# β
# 1
# 
# 
# For the cars example when interpreting these intervals, we say, we are 99% confident that for an increase in speed of 1 mile per hour, the average increase in stopping distance is between 2.8179187 and 5.0468988 feet, which is the interval for  
# β
# 1
# .
# 
# Note that this 99% confidence interval does not contain the hypothesized value of 0. Since it does not contain 0, it is equivalent to rejecting   

# Note, we can extract specific values from this output a number of ways. This code is not run, and instead, you should check how it relates to the output of the code above.

confint(stop_dist_model, level = 0.99)[1,]
confint(stop_dist_model, level = 0.99)[1, 1]
confint(stop_dist_model, level = 0.99)[1, 2]
confint(stop_dist_model, parm = "(Intercept)", level = 0.99)
confint(stop_dist_model, level = 0.99)[2,]
confint(stop_dist_model, level = 0.99)[2, 1]
confint(stop_dist_model, level = 0.99)[2, 2]
confint(stop_dist_model, parm = "speed", level = 0.99)

# We can also verify that calculations that R is performing for the  
# β
# 1
# interval.

# store estimate
beta_1_hat = coef(stop_dist_model)[2]

# store standard error
beta_1_hat_se = summary(stop_dist_model)$coefficients[2, 2]

# calculate critical value for two-sided 99% CI
crit = qt(0.995, df = length(resid(stop_dist_model)) - 2)

# est - margin, est + margin
c(beta_1_hat - crit * beta_1_hat_se, beta_1_hat + crit * beta_1_hat_se)
##    speed    speed 
## 2.817919 5.046899


# 8.7 Confidence Interval for Mean Response
# 
# In addition to confidence intervals for  
# β
# 0
# and  
# β
# 1
# , there are two other common interval estimates used with regression. The first is called a confidence interval for the mean response. Often, we would like an interval estimate for the mean


# To find confidence intervals for the mean response using R, we use the predict() function. We give the function our fitted model as well as new data, stored as a data frame. (This is important, so that R knows the name of the predictor variable.) Here, we are finding the confidence interval for the mean stopping distance when a car is travelling 5 miles per hour and when a car is travelling 21 miles per hour.

new_speeds = data.frame(speed = c(5, 21))
predict(stop_dist_model, newdata = new_speeds, 
        interval = c("confidence"), level = 0.99)
##         fit       lwr      upr
## 1  2.082949 -10.89309 15.05898
## 2 65.001489  56.45836 73.54462

# 8.8 Prediction Interval for New Observations
# 
# Sometimes we would like an interval estimate for a new observation,  
# Y
# , for a particular value of  
# x
# . This is very similar to an interval for the mean response

# To calculate this for a set of points in R notice there is only a minor change in syntax from finding a confidence interval for the mean response.

predict(stop_dist_model, newdata = new_speeds, 
        interval = c("prediction"), level = 0.99)
##         fit       lwr       upr
## 1  2.082949 -41.16099  45.32689
## 2 65.001489  22.87494 107.12803

# Also notice that these two intervals are wider than the corresponding confidence intervals for the mean response.

# 8.9 Confidence and Prediction Bands

# Often we will like to plot both confidence intervals for the mean response and prediction intervals for all possible values of  
# x
# . We calls these confidence and prediction bands.

speed_grid = seq(min(cars$speed), max(cars$speed), by = 0.01)
dist_ci_band = predict(stop_dist_model, 
                       newdata = data.frame(speed = speed_grid), 
                       interval = "confidence", level = 0.99)
dist_pi_band = predict(stop_dist_model, 
                       newdata = data.frame(speed = speed_grid), 
                       interval = "prediction", level = 0.99) 

plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey",
     ylim = c(min(dist_pi_band), max(dist_pi_band)))
abline(stop_dist_model, lwd = 5, col = "darkorange")

lines(speed_grid, dist_ci_band[,"lwr"], col = "dodgerblue", lwd = 3, lty = 2)
lines(speed_grid, dist_ci_band[,"upr"], col = "dodgerblue", lwd = 3, lty = 2)
lines(speed_grid, dist_pi_band[,"lwr"], col = "dodgerblue", lwd = 3, lty = 3)
lines(speed_grid, dist_pi_band[,"upr"], col = "dodgerblue", lwd = 3, lty = 3)
points(mean(cars$speed), mean(cars$dist), pch = "+", cex = 3)



# 8.10 Significance of Regression, F-Test


# In the case of simple linear regression, the  
# t
# test for the significance of the regression is equivalent to another test, the  
# F
# test for the significance of the regression. This equivalence will only be true for simple linear regression, and in the next chapter we will only use the  
# F
# test for the significance of the regression.


# In particular, we will reject the null when the  
# F
# statistic is large, that is, when there is a low probability that the observations could have come from the null model by chance. We will let R calculate the p-value for us.
# 
# To perform the  
# F
# test in R you can look at the last row of the output from summary() called F-statistic which gives the value of the test statistic, the relevant degrees of freedom, as well as the p-value of the test.

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

# Additionally, you can use the anova() function to display the information in an ANOVA table.

anova(stop_dist_model)
## Analysis of Variance Table
## 
## Response: dist
##           Df Sum Sq Mean Sq F value   Pr(>F)    
## speed      1  21186 21185.5  89.567 1.49e-12 ***
## Residuals 48  11354   236.5                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# This also gives a p-value for the test. You should notice that the p-value from the  
# t
# test was the same. You might also notice that the value of the test statistic for the  
# t
# test,  
# 9.46399
# , can be squared to obtain the value of the  
# F
# statistic,  
# 89.5671065
# .
# 
# Note that there is another equivalent way to do this in R, which we will return to often to compare two models.

anova(lm(dist ~ 1, data = cars), lm(dist ~ speed, data = cars))

## Analysis of Variance Table
## 
## Model 1: dist ~ 1
## Model 2: dist ~ speed
##   Res.Df   RSS Df Sum of Sq      F   Pr(>F)    
## 1     49 32539                                 
## 2     48 11354  1     21186 89.567 1.49e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# We can then think of this usage of anova() as directly comparing the two models. (Notice we get the same p-value again.)


