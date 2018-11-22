#I confirm that the following report and associated code is my own work, except
# where clearly indicated.

if(!require(dslabs)) install.packages("dslabs")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(car)) install.packages("car")
if(!require(MASS)) install.packages("MASS")

source('./Code/MonteCarlo.R')
source('./Code/SizePower.R')
source('./Code/StatisticalTests.R')
source('./Code/Scenarios.R')


# TODO:
# Display the range of values (min max) V
# Get the mean for rich and poor contries V
# Use them as properties for the ditributions V
# The end result could be a table of the power having as rows the number of samples V
# and as columns the effect size V
# Parallel code V
# Research power and size to check for potential scenarios V
# Check truncnorm package, rtruncnorm V
# Add the power or size type to the returned value in the run scenario func, !don't add it as a table name

# TODO:
# Clean comments
# Check for the assumptions
# do a qqplot durbin whatson for independency, for the spread ncv const error of variance
# Add purpose for the functions
# Rename main code, delete GapMinder.R


# You are correct, size is only calculated when the null hypothesis is true and power
# is calculated when it is false.  And yes, you will have scenarios where one or the
# other is not calculable.  In your report, you should provide at least one result
# describing simulated power and one result for statistical size for each of your statistical tests. 
#
# To extend the example given in the assignment, you might present your results as
# a table of statistical power with columns for effect size (1 cm, 5 cm, 10 cm) and number of samples
# in rows (10, 100, 1000), resulting in 9 estimates of power.
# You could then have a second table where you hold effect size constant at 0 (null hypothesis = true),
# but vary the number of samples and add bias to the reported values (e.g., rounding up),
# leading to 9 values of size.  You would then include versions of these tables
# for both the parametric and non-parametric tests.


# Data Preparation --------------------------------------------------------

# Load gapminder dataset
data(gapminder)

# Selecting only the useful parameters for the proposed research
subGapMinder <- subset(gapminder, select = c(life_expectancy, population, gdp))

# Removing missing data
subGapMinder <- na.omit(subGapMinder)

# Adding the GDP per capita
subGapMinder <- mutate(subGapMinder, gdp_per_capita = gdp / population)

# Get some information about the data intervals to pick the "rich/poor" baseline
gdp_cap_quantiles <- summary(subGapMinder$gdp_per_capita)
rich.baseline <- round(gdp_cap_quantiles[5])
# Adding groups based on the 3rd quantile of GDP/Capita = 6690
# R: Rich contries with GDP/Capita >= 6690
# P: Poor countries with GDP/Capita < 6690
subGapMinder <- mutate(subGapMinder, country_type =
                         ifelse(gdp_per_capita >= rich.baseline, "R", "P"))
View(subGapMinder)

# Convert Country Type to factor
subGapMinder$country_type <- factor(subGapMinder$country_type)

# Preview of the coutry types proportions
# 75-25 since we picked the 3rd quantile as a baseline
table(subGapMinder$country_type)


# Data Analysis -----------------------------------------------------------

par(mfrow = c(2, 2))
# Histogram of the gdp/capita with the normal curve
# Looks like exponentially distributed
gdpCap <- subGapMinder$gdp_per_capita
gdpHist <- hist(gdpCap, main = "GDP/Capita Frequency",
          xlab = "GDP/Capita", ylab = "Frequency", col = "darkgreen")
xfit <- seq(min(gdpCap), max(gdpCap), length = 40) 
yfit <- dnorm(xfit, mean = mean(gdpCap), sd = sd(gdpCap)) 
yfit <- yfit * diff(gdpHist$mids[1:2]) * length(gdpCap) 
lines(xfit, yfit, col = "red", lwd = 2)

# Histogram of the life expectancy with the normal curve
# Beta distributed
lifeExp <- subGapMinder$life_expectancy
lifeExpHist <- hist(subGapMinder$life_expectancy, main = "Life Expectancy Frequency",
     xlab = "Life Expectancy", ylab = "Frequency", col = "blue")
xfit <- seq(min(lifeExp), max(lifeExp), length = 40) 
yfit <- dnorm(xfit, mean = mean(lifeExp), sd = sd(lifeExp)) 
yfit <- yfit * diff(lifeExpHist$mids[1:2]) * length(lifeExp) 
lines(xfit, yfit, col = "red", lwd = 2)

# Histogram of the poor countries' Life Expectancy
poorLifeExp <- subGapMinder %>% filter(country_type == "P")
hist(poorLifeExp$life_expectancy, main = "Life Expectancy Frequency\nin Poor Countries",
                    xlab = "Life Expectancy", ylab = "Frequency", col = "blue")

richLifeExp <- subGapMinder %>% filter(country_type == "R")
hist(richLifeExp$life_expectancy, main = "Life Expectancy Frequency\nin Rich Countries",
     xlab = "Life Expectancy", ylab = "Frequency", col = "blue")
par(mfrow = c(1, 1))

# Mean, variance and range of the life expectancy
life.expect.mean <- mean(subGapMinder$life_expectancy)
life.expect.sd <- sd(subGapMinder$life_expectancy)

# Life Expectancy mean, sd and range for poor countries
life.expect.poor.summary <- subGapMinder %>% filter(country_type == "P") %>%
  summarise(mean = mean(life_expectancy), sd = sd(life_expectancy),
            min = min(life_expectancy), max = max(life_expectancy))

# Life Expectancy mean, sd and range for rich countries
life.expect.rich.summary <- subGapMinder %>% filter(country_type == "R") %>%
  summarise(mean = mean(life_expectancy), sd = sd(life_expectancy),
            min = min(life_expectancy), max = max(life_expectancy))

# BoxPlot showing the differences in mean between the life expectancy
# in rich and poor countries
  ggplot(data = subGapMinder) +
    geom_boxplot(aes(x = country_type, y = life_expectancy,
                     colour = country_type)) +
    scale_colour_manual(values = c("darkgreen", "blue")) +
    labs(x = "Country Type", y = "Life Expectancy") +
    ggtitle("Differences in Life Expectancy\nMeans in the Different Country Types")+
    theme(plot.title = element_text(hjust = 0.5))

# Assumptions -------------------------------------------------------------

# 1_Normality
# Notes:
# H0: Assume the data comes from a normally ditributed data
# p-value < 0.05 => reject the H0 => not normally distributed

# qqplot for poor countries life expectancy
subGapMinder %>% filter(country_type == "P") %>%
  ggplot() +
  geom_qq(aes(sample = life_expectancy)) +
  geom_qq_line(aes(sample = life_expectancy))

# Shapiro test for poor countries
subGapMinder %>% filter(country_type == "P") %>%
  summarise(shapiro.test(life_expectancy[1:5000])$p.value)

# qqplot for rich countries life expectancy
subGapMinder %>% filter(country_type == "R") %>%
  ggplot() +
  geom_qq(aes(sample = life_expectancy)) +
  geom_qq_line(aes(sample = life_expectancy))

# Shapiro test for rich countries
subGapMinder %>% filter(country_type == "R") %>%
  summarise(shapiro.test(life_expectancy[1:5000])$p.value)

# 2_Homogeneity of variances
bartlett.test(subGapMinder$life_expectancy ~ subGapMinder$country_type)
