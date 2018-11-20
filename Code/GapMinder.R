library(dslabs)
library(dplyr)
library(ggplot2)
library(car)

# The workflow should be something like:
# - Choose a research question and statistical test
# - Choose some scenarios
# - For each scenario, generate lots of possible datasets and evaluate each one using
# your statistical test
# - Evaluate power and size across all of the datasets you generated for that scenario

# Research Question:
# Is life expectancy greater in countries with higher GDPs per capita?

# 1 Parametric test
# Simple Regression

# 1 Non-parametric test
# 



# Data Preparation --------------------------------------------------------

# Load gapminder dataset
data(gapminder)

# Selecting only the useful parameters for the proposed research
subGapMinder <- subset(gapminder, select = c(country, year, life_expectancy,
                                             population, gdp))

# Removing missing data
subGapMinder <- na.omit(subGapMinder)

# Adding the GDP per capita
subGapMinder <- mutate(subGapMinder, gdp_per_capita = gdp / population)

# Calculate the mean GDP and Life Expectancy by country over the years
subGapMinder.summary <- subGapMinder %>% group_by(country) %>%
  summarise(meanGDP = mean(gdp_per_capita), 
            meanLifeExpectancy = mean(life_expectancy))
View(subGapMinder.summary)

# Variation of the Life Expectation with the GDP
subGapMinder.summary %>% select(meanGDP, meanLifeExpectancy) %>%
  ggplot(.) +
  geom_point(aes(x = meanGDP, y = meanLifeExpectancy)) +
  labs(x = "GDP/Capita", y = "Life Expectancy",
       title = "Variation of the Life Expectation with the GDP")
  
# # Variation of the life and gdp over the years
# subGapMinder %>% filter(country == "Algeria") %>%
#   select(life_expectancy, year, gdp_per_capita) %>%
#   ggplot(., aes(x = year)) +
#   geom_point(aes(y = gdp_per_capita, colour = "GDP/Capita")) +
#   geom_point(aes(y = life_expectancy * 20, colour = "Life Expectancy")) +
#   scale_y_continuous(sec.axis = sec_axis(~./20, name = "Life Expectancy")) +
#   scale_colour_manual(values = c("red", "blue")) +
#   labs(x = "Year", y = "GDP Per Capita", colour = "Parameter") +
#   theme(legend.position = c(0.14, 0.9))


# Simple Regression -------------------------------------------------------

# Fit the model
regressionModel <- lm(meanLifeExpectancy ~ meanGDP, data = subGapMinder.summary)
summary(regressionModel)

# Plot data with the best fit line
plot(meanLifeExpectancy ~ meanGDP, data = subGapMinder.summary, pch = 16,
     xlab = "GDP", ylab = "Life Expectancy")
abline(regressionModel, col = "blue", lwd = 2)

# Assumptions -------------------------------------------------------------

residuals.Model <- residuals(regressionModel)

# 1_Normality of Residuals

# Residuals Histogram
hist(residuals.Model)

# QQNorm shows that the residuals are normally distributed
ggplot() +
  geom_qq(aes(sample = residuals.Model)) +
  geom_qq_line(aes(sample = residuals.Model))

# Shapiro Test for normality of residuals
# H0 normally distributed values
# In this case reject the H0
shapiro.test(residuals.Model)

# 2_Homogeneity of variances

# scale-location proves the equality of variance
# In our case not so much
plot(regressionModel, which = 3)

# 3_Auto-Correlation

# Durbin-Watson Test
# H0: Resiuals are not linearly auto-correlated
# 2.1 < 2.5 indicates no auto-correlation
durbinWatsonTest(residuals.Model)


# 4_Linear Relationship

# The residuals are not independent of the fitted values.
# In this case, the model needs to be modified in order to describe the data well.
# There's no linearity between the dependent and independent variables
# We are supposed to have a linear relationship => assumption violated
# Model is not the best fit
plot(fitted(regressionModel), residuals.Model)

View(gapminder)




# Distributions -----------------------------------------------------------

# exponential distribution for the GDP/Capita
randExp <- rexp(1e3, 1/gdp.per.capita.mean)
mean(randExp)
gdp.per.capita.mean
sd(randExp)
gdp.per.capita.sd
hist(randExp, xlim = c(0, 60000), col = "darkorange")

# beta distribution for the life expectancy
xt <- subGapMinder$life_expectancy / ( max( subGapMinder$life_expectancy ) + .0001 )
fit.beta <- fitdistr(xt, "beta", start = list(shape1 = 2, shape2 = 5))
x.beta <- rbeta(1e3,fit.beta$estimate[[1]],fit.beta$estimate[[2]])
x.beta <- x.beta * ( max( subGapMinder$life_expectancy ) + .0001 )
hist(x.beta)
mean(x.beta)
life.expect.mean
sd(x.beta)
life.expect.sd



