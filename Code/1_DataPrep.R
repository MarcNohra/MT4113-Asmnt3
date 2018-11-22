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

# Convert Country Type to factor
subGapMinder$country_type <- factor(subGapMinder$country_type)

View(subGapMinder)

# Preview of the coutry types proportions
# 75-25 since we picked the 3rd quantile as a baseline
table(subGapMinder$country_type)


# Data Analysis -----------------------------------------------------------

par(mfrow = c(2, 2))

# Histogram of the gdp/capita with the normal curve
gdpCap <- subGapMinder$gdp_per_capita
gdpHist <- hist(gdpCap, main = "GDP/Capita Frequency",
          xlab = "GDP/Capita", ylab = "Frequency", col = "darkgreen")
# Get the parameters for the normal curve
xfit <- seq(min(gdpCap), max(gdpCap), length = 40) 
yfit <- dnorm(xfit, mean = mean(gdpCap), sd = sd(gdpCap)) 
yfit <- yfit * diff(gdpHist$mids[1:2]) * length(gdpCap)
# Add the normal line
lines(xfit, yfit, col = "red", lwd = 2)

# Histogram of the life expectancy with the normal curve
lifeExp <- subGapMinder$life_expectancy
lifeExpHist <- hist(subGapMinder$life_expectancy, main = "Life Expectancy Frequency",
     xlab = "Life Expectancy", ylab = "Frequency", col = "blue")
# Get the parameters for the normal curve
xfit <- seq(min(lifeExp), max(lifeExp), length = 40) 
yfit <- dnorm(xfit, mean = mean(lifeExp), sd = sd(lifeExp)) 
yfit <- yfit * diff(lifeExpHist$mids[1:2]) * length(lifeExp)
# Add the normal line
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
