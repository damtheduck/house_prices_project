library(MASS)
library(car)
library(ggplot2)
library(dplyr)
library(readr)
library(lindia)

# The aim of our project here is to analyse the effect that the number of rooms, bedrooms, baths, showers,
# garages, and location has on house prices. Our data is sampled from in the towns of Farmington and
# Natick, near Boston, USA.

# We first access and sort our table of 138 total responses.

house_prices <- read_table("house_prices.txt") %>% 
    rename(Bedrooms = Bedrms, Shower_Rooms = HaBaths) %>%
    arrange(desc(Price), desc(Rooms), desc(Baths), desc(Shower_Rooms), desc(Garages), desc(Place))
attach(house_prices)
Place <- as.factor(Place)
print(house_prices)

# We apply a multiple linear regression model, Yi = b0 + b1x1i + b2x2i + b3x3i + b4x4i + b5x5i + b6x6i + ei
# for where i ranges from 1 to 138. We initially fit a "full" model. i.e. a model that considers all columns from 
# our table of data as explanatory variables.  

Model_Full <- lm(Price ~ Rooms + Bedrooms + Baths + Shower_Rooms + Garages + Place)
print(Model_Full)
summary(Model_Full)



####    Model Checking    ####

summary(Model_Full)

# We first analyse the global F-test that tests H0: b1 = ... = b6 = 0 against H1: at least one of bq != 0 and
# see our p-value is 0.000..., meaning that we reject our null hypothesis at any feasible significance level
# and see that at least some of our explanatory variables are related to house price. However, we see that the
# p-values of the t-test for H01: b1 = 0 against H11: b1 != 1 and H02: b2 = 0 against H12: b2 != 0 are 64.3%
# and 61.2% respectively, meaning we would reject the null hypotheses at most typical significance levels. We
# will check further for inadequacies in our explanatory variables later.

# Before we begin to move to draw conclusions from our fitted model, we should first analyse whether the fitted 
# linear model has any inadequacies.

# NOTE: If space on our pdf, we can include plots of resid vs each explanatory variable to show no need for
# higher order terms, no autocorrelation.



##  Checking of Residual Plots  ##

# We first turn our attention to the residual plots. We consider the standardised (studentised) residuals, which 
# are assumed to be distributed normally with a mean of 0 and a variance of 1. 

# Here, we produce the normal probabillity (q_q) plot to check our standardised residuals for departures from 
# normality, and therefore, depatures from our assumptions. 

qq_plot_stand <- rstandard(Model_Full) %>%
    data.frame() %>%
    ggplot(aes(sample = .)) + stat_qq() + stat_qq_line(colour = "red", size = 0.7) + labs(title = 
    "Normal Q-Q Plot", y = "Standardised Residuals", x = "Theoretical Quantities", subtitle = 
    "Full Model", caption = "Fig.1")
qq_plot_stand

# This plot does not particuarly exhibit skewness in our data, however it does suggest that there may be 
# high kurtosis, and therefore, departures from normaility, at higher prices.

# We next plot our standardised residuals against their respective fitted values, along with any possible trend.

resid_vs_fitted_plot <- ggplot(Model_Full, aes(x=.fitted, y=.stdresid)) + geom_point() + 
    geom_smooth( formula = y ~ x, colour = "red", se = FALSE, size = 0.7) + labs(title =
    "Standardised Residuals Vs Fitted Values", x = "Fitted Values", y = "Standardised Residuals", subtitle =
    "Full Model", caption = "Fig.2") + 
    geom_hline(yintercept = 0, colour = "black", linetype = "dashed")
resid_vs_fitted_plot

# From analysis of our plot, we see that there may be evidence for there being heteroscedasticity within our 
# residuals due to the "funnel" shape of our plotted values. This means that our variance may be increasing as our 
# expected value increases. We can check this further by plotting the square rooted absolute values of our 
# standardised residuals against their respective fitted values.

sqr_abs_resid_vs_fitted_plot <- ggplot(Model_Full, aes(x = .fitted, y = sqrt(abs(.stdresid)))) +
    geom_point() + geom_smooth( formula = y ~ x, colour = "red", se = FALSE, size = 0.7) + labs(title = 
    "Square Rooted Absolute Standardised Residuals Vs Fitted Values", x = "Fitted Values", y = 
    "Square Rooted Absolute Standardised Residuals", subtitle = "Full Model", caption = "Fig.3")
sqr_abs_resid_vs_fitted_plot

# Here, we are considering standard deviations rather than the variance and see that there is more evidence for 
# the possibility of a non-constant variance as the trend seems to indicate that as our expected value increases,
# our standard deviation, and therefore, variance also increases. (i.e. noteably, there is a positive correlation). 
# Such heteroscedasticity is a problem for our model as it violates our assumption that the residuals are spread 
# constantly. As such, it may be the case that our explanatory variables are less reliable for prediction. 


# When dealing with heteroscedasticity, we should consider a transformation of the response. Let us now plot the
# log-likelihoods of transformations of our expectation against the the Box-Cox transformation parameter, along 
# with a 95% confidence interval.

gg_boxcox(Model_Full, showlambda = FALSE) + labs(title = "Box-Cox", caption = "Fig.4")

# Rounding our transformation parameter with the highest likelihood to the nearest simple rational number (-1/2)
# we see that we may want to transform Yi to 1/sqrt(Yi) in order to better satisfy our assumptions of normality. 
# We will now consider this altered model and check its residual plots for any signs of heteroscedasticity.

Model_Full_ALT <- lm(1/sqrt(Price) ~ Rooms + Bedrooms + Baths + Shower_Rooms + Garages + Place)

qq_plot_stand_ALT <- rstandard(Model_Full_ALT) %>%
    data.frame() %>%
    ggplot(aes(sample = .)) + stat_qq() + stat_qq_line(colour = "red", size = 0.7) + labs(title = 
    "Normal Q-Q Plot", y = "Standardised Residuals", x = "Theoretical Quantities", subtitle = 
    "Altered Full Model", caption = "Fig.5")
qq_plot_stand_ALT

resid_vs_fitted_plot_ALT <- ggplot(Model_Full_ALT, aes(x=.fitted, y=.stdresid)) + geom_point() + 
    geom_smooth( formula = y ~ x, colour = "red", se = FALSE, size = 0.7) + labs(title =
    "Standardised Residuals Vs Fitted Values", x = "Fitted Values", y = "Standardised Residuals", subtitle =
    "Altered Full Model", caption = "Fig.6") + 
    geom_hline(yintercept = 0, colour = "black", linetype = "dashed")
resid_vs_fitted_plot_ALT

sqr_abs_resid_vs_fitted_plot_ALT <- ggplot(Model_Full_ALT, aes(x = .fitted, y = sqrt(abs(.stdresid)))) +
    geom_point() + geom_smooth( formula = y ~ x, colour = "red", se = FALSE, size = 0.7) + labs(title = 
    "Square Rooted Absolute Standardised Residuals Vs Fitted Values", x = "Fitted Values", y = 
    "Square Rooted Absolute Standardised Residuals", subtitle = "Altered Full Model", caption = "Fig.7")
sqr_abs_resid_vs_fitted_plot_ALT

# We now see there are no signs of heteroscedasticity in our plots - our model now better fits our assumptions. 
# We will now continue with this altered model with a transformed response for our analysis.



##  Considering Subsets of Explanatory Variables  ##

# Now that we have transformed our response, we will now compare models that are more parsimonious (i.e. have
# less explanatory variables), to our full model and see if any are as, if not, more, adequate than our full
# model for inference and prediction. 


# First, let us perform a backward elimination of our explanatory variables in order to compare the Akaike's 
# Information Critereon (AIC) of the full model against those of a subset of explanatory variables. The AIC 
# estimates the likelihood of a model to efficiently predict future values. We aim to choose a model with an 
# AIC that is low/near to that of the full model. 

step(Model_Full_ALT)

# We see that our full model has an AIC of -2408.27, whereas the model that does not take into account the number 
# of rooms has an AIC of -2410.27. This reduced model has an AIC that is smaller but still similar to that of the 
# full model. Let us consider the roomless model along with our full model for further checking.

Model_noRooms <- lm(1/sqrt(Price) ~ Bedrooms + Baths + Shower_Rooms + Garages + Place)


# We will now analyse the coefficients of determination, (i.e. our R-squared values) for our models in question.

summary(Model_Full_ALT)
summary(Model_noRooms)

# Considering our full model, we see our coefficient of determination is 70.41%. This value lies (marginally) 
# within a 70% threshold, lending rope to the sufficiency of our model for prediction. We see that our 
# coefficient of determination is also at 70.41% for our roomless model.

# Contextually, we can say that 70.41% of the variation in our select house prices in Farmington and Natick is 
# due to the fitted regeression of our model with respect either model. Here, when considering reduced 
# models, a coefficient of determination closest to that of the maximumn (i.e. to that of the full model) is 
# most desirable. Our reduced model perfectly satisfies this.


# We next take into account the residual mean squares (MSE) for our models. A model with the lowest residual 
# mean square is the most desirable as it indicates a higher accuracy of our explanatory variables. We have that 
# the MSEs (multiplied by 1000 for ease of comaprison) of our two models are 0.1584 and 0.1578 for the 
# full and roomless model respectively. Here, the model that has the lowest residual mean square is the 
# roomless model. 


# From our analysis so far, we see that having the number of rooms included in our explanatory variables may be 
# having a negative effect on our model. Contextually, this could be as a result of the fact that bathrooms, 
# shower rooms and bedrooms all contribute to the number of rooms, thus making the number of rooms correllated to 
# those explanatory variables. 

# If such proves to be the case, it would mean that there is multicollinearity within our explanatory variables. 
# This is undesirable as, consequently, our model may be overly sensitive to small changes in the data and our 
# parameter estimates may have large variances. 


# Let us consider the partial F-tests of linear models of a subset of our explanatory variables: {Xq | X1 :
# q ϵ {2, 3, 4, 5, 6} }. These will test the effect that adding an extra explanatory variable has on the model 
# that only takes into account number of rooms.

partialFtest <- function(variable) {
    lm(1/sqrt(Price) ~ Rooms + variable) %>% anova()
}
partialFtest(Bedrooms)
partialFtest(Baths)
partialFtest(Shower_Rooms)
partialFtest(Garages)
partialFtest(Place)

# These partial F-tests test the hypothesis, H0: Xq = 0, against H1: Hq != 0 given that X1 (i.e. the number 
# rooms) is in our model. The one value that is significant to us comes from the partial F-test containing X2, 
# the number of bedrooms. This test yeilds a p-value of 97.43%, meaning that we would not reject the null 
# hypothesis at any feasilbe significance level. This means that, when our model contains the number of rooms,
# also including the number of bedrooms does not particularly change our model. From this, we can see that our 
# instance of multicollinearity is between the number of rooms and number of bedrooms. 


# Let us now plot the number of rooms against the number of bedrooms for each data point in order to visualise
# this.

rooms_vs_others <- ggplot(data = house_prices, aes(x = Rooms, y = Bedrooms)) +
    geom_smooth(formula = y ~ x, colour = "red", size = 0.7, se = FALSE) + geom_point() + 
    labs(title = "Rooms Vs Bedrooms", x = "Number of Rooms", y = "Number of Bedrooms") 
rooms_vs_others

# This plot further evidences a positive correlation between its respective variables. 


# We will now fit a multiple linear regression model of Rooms against all other explanatory variables in the 
# full model in order to calculate the Variance Inflation Factor of Rooms in our full model. The VIF measures 
# how much of the variance of our model is due to collinearity. 

vif(Model_Full_ALT)

# We see that Rooms has the highest VIF out of all explanatory variables of 5.127 (3 d.p) with respect to the 
# full model. Together with our plot, and our partial F-tests, this sufficiently evidences multicollinearity in 
# the number of rooms with respect to our full model. 

vif(Model_noRooms)

# Considering the VIFs of the variables in the roomless model shows that eliminating rooms significantly 
# reduces any cause for concern with respect to multicollinearity, with the highest VIF at 2.046 (3 d.p) for
# Baths.


# From our analysis of our two potential models, (i.e. the full model and the roomless model), we have seen that 
# our roomless model has an R-squared sufficiently equivalent to that of the maximum and a lower residual mean 
# square and AIC than that of the full model. Paired with the existance of multicollinearity in the number of 
# rooms and bedrooms, we are left with sufficient evidence to support dismissing our full model and continuing 
# our analysis with our roomless model. We will do so.



##  Outliers and Leverage Points  ##

# We will now analyse our model in order to check for significant data points that may be having a negative/
# excessive effect on the fitting of our explanatory variables, and therefore, on our prediction.


# From our residual plots, we see one possible outlier with a standardised residual value of 2.979 (3.d.p). 
# This residual correpsonds to the 19th observation with a fitted value of 0.0008423657 from an original of
# 0.001154778. This observation is the house in Farmington with a price of $749,900 with 26 rooms, 11 bedrooms, 
# 7 baths, 2 shower rooms and 0 garages.

# Assuing that this data point has not been recorded incorrectly we could consider continuing our analysis 
# without this data point.


# Let us now turn our attention to the Cook's distance for our observations, in order to identify any points 
# that have a significantly high influence on our model. Such points are problematic as they play a 
# disproportionate role in determining our explanatory variables. We plot our observation numbers against their 
# respective Cook's distances. 

cooks_distance <- ggplot(Model_noRooms, aes(x = 1:138, y = .cooksd)) + geom_col(width= 0.3, colour = 
    "black", fill = "black") + labs(title = "Cook's Distance for each Observation", x = "Observation Number", y = 
    "Cook's Distance")
cooks_distance

# From our plot, we can see that observation 19 has a Cook's distance significantly higher than the other
# observations. This value is 1.275 (3 d.p). Since this value is greater than 1, this data point should be 
# considered highly influential. From our data set we can see that this observation has a significantly higher 
# amount of rooms and bathrooms than other observations around that price level. We may consider continuing our 
# analysis without this data point in order to increase the reliability of our explanatory variables. 

# Computing the leverage of the 19th observation gives us a value of 0.495 (3 d.p). This value is significantly
# large and lends support to the possibility of this data point having a disproportionate effect on our model.

# We see that the 19th observation is both an outlier and an influential data point as it has significantly more
# bedrooms and bathrooms than the other houses at its price level. This may be caused by a factor not recorded in 
# our data (e.g qualititative factors such as quality of interior/exterior or other factors such as whether the 
# house has a garden, balcony etc.). We will not remove/alter this data point and, instead, when conducting our 
# inference, we will consider our model with and without this point and report on whether there is any 
# significant difference in our findings.

house_prices_ALT <- house_prices %>%
    filter( !(Price %in% c(749900)) )
Model_noRooms_ALT <- lm(1/sqrt(Price) ~ Bedrooms + Baths + Shower_Rooms + Garages + Place, 
data = house_prices_ALT)


# Now that we have checked our model for inadequacies in terms of its accuracy of explanatory variables,
# adhearence to normality and ability to predict as well as indicated any possible detrimental data points, we 
# can now conduct our exploratory analysis.



####    Exploratory Analysis    ####

# (Exploratory analysis conducted in final project pdf)

summary(Model_noRooms)
summary(Model_noRooms_ALT)


# Here we print the retransformed coefficients for the roomless filtered model.

for (i in coefficients(Model_noRooms_ALT)) {
    print((1/i)**2)
}


# Here we create a data frame for the median house (in terms of the explanatory variables) in both Farmington 
# and Natick and use it to predict the its price in Farmington and Natick along with its 99% prediction intervals


row_rep <- function(df, n) {
  df[rep(1:nrow(df), times = n),]
}

averageHouse <- house_prices_ALT %>%
    summarise(Bedrooms = median(Bedrooms), Baths = median(Baths), Shower_Rooms = median(Shower_Rooms),
    Garages = median(Garages)) %>%
    row_rep(2) %>%
    mutate(Place = c(0,1))
averageHouse

lst_index <- 1
for (i in predict(Model_noRooms_ALT, averageHouse, level = 0.99, interval = "prediction")
) {
    lst <- c("Fit_Farmington", "Fit_Natick", "Upper_Farmington", "Upper_Natick", "Lower_Farmington", "Lower_Natick")
    
    cat(lst[lst_index], ": ", i, "->", (1/i)**2, "; ")
    lst_index <- lst_index + 1
}

# Here we print all houses with variables equal to the dataset median values.  

median_data_points <- house_prices_ALT %>%
    filter(Bedrooms == 4, Baths == 2, Shower_Rooms == 1, Garages == 2)
print(median_data_points, n = Inf)


# Here we print how many datapoints we have for Farmington and Natick respectively.

house_prices_ALT %>% filter(Place == 0) %>% nrow()
house_prices_ALT %>% filter(Place == 1) %>% nrow()