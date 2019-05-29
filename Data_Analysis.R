################### Start of header ######################
# Title: CA 3 Data Analysis (Crime in Ireland)
#  
# Description: 
#
# <This code is for CA 3, which is data
#  analysed for Crime in Ireland, particularly
#  drunk driving. It deals with finding the correct
#  statistical method, testing hypothesis and doing
#  Power analysis on dataset. The dataset is from majorly
#  4 data sources which are CSO, HRB Overview, Garda PULSE 
#  and Zanran. It has been cleaned combined and loaded 
#  onto the RStudio Environment.>
#
# Author: <Rituparna Parial>  
# Date: <19-05-2019>
#
################### End of header ########################

# -----LOADING AND EXPLORING DATASET-------

# Since the dataset is in Excel or xlsx format first 
# xlsx package is installed and data is loaded onto
# Drunk_driving dataframe
install.packages("xlsx")
library("xlsx")
drunk.driving <- read.xlsx("Ddriving.xlsx", 1)

# Showing summary of dataset
# Summary gives min, max, quartiles
# mean for numerical values and frequencies
summary(drunk.driving)

# Show the total number of rows
sprintf("Number of Rows in Dataframe: %s", 
        format(nrow(drunk.driving),big.mark = ","))

# Display the structure of the Drunk_driving data frame
str(drunk.driving)

# First 10 rows of the Drunk_driving data frame
head(drunk.driving, n=10)

# Checking for missing values for the data frame
sprintf("Total number of missing valuesin Dataframe: %s"
        , format(sum(is.na(drunk.driving))))

# Modifying the year attribute to be a categorising factor

drunk.driving$year <- factor(drunk.driving$year)
str(drunk.driving)

# --------DESCRIPTIVE STATISTICS---------

install.packages("Hmisc") # note it is case sensitive
library(Hmisc)
# describe() function in the Hmisc package returns 
# the number of variables and observations
# the number of missing and unique values
# the mean, quantiles, and the five highest and lowest values.

describe(drunk.driving)

# ----------VISUAL REPRESENTATION----------
# Exploring the data visually using DataExplorer Library
install.packages("DataExplorer")
library(DataExplorer)

# plot_histogram gives a comparision of the continuous variables
# in a histogram plot
plot_histogram(drunk.driving)

# plot_density gives a comparision of the continuous variables
# in a density plot
plot_density(drunk.driving)

# Representing year-wise unemployment rate
install.packages("ggplot2")
library(ggplot2)
theme_set(theme_bw())

ggplot(drunk.driving, aes(x=year, y=unemployment.rate)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  labs(title="Year-wise Unemployment Rate",
       x="Year",
       y="Unemployment Rate")

# -------CREATING FUNCTION-----------

# Creating a function to see mean, standard deviation,
# kurtosis and skewness of the data 
my_variables <- c("drunk.driving.cases", "total.injuries", "unemployment.rate")
head(drunk.driving[my_variables])

my_variables <- drunk.driving[my_variables]
head(my_variables)
 
# Skewness is a measure of symmetry, or more precisely, the lack of symmetry. 
# Kurtosis is a measure of whether the data are heavy-tailed or 
# light-tailed relative to a normal distribution. That is, data 
# sets with high kurtosis tend to have heavy tails, or outliers. 
# Data sets with low kurtosis tend to have light tails, or lack of outliers.

my_stats <- function(x, na.omit = FALSE){
  if(na.omit)
    x <- x[!is.na(x)] #omits missing values
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m) ^ 3 / s ^ 3)/n
  kurt <- sum((x-m) ^ 4 / s ^ 4)/n - 3
  #skew <- skewness(x)
  #kurt <- kurtosis(x)
  
  return(c(n = n, mean = m, stdev = s, skew = skew,
           kurtosis = kurt))
}

head(my_variables)
sapply(my_variables, my_stats)

# Results show means of drunk.driving.cases as 1698.008,
# total injuries as 3647.991379 and unemployment.rate as 9.5920842.

# The distribution's of first two are skewed to the right (+0.64) and (+0.84)
# But for the unemployment rate it is skewed to the left (-0.32)
# And since it is between 0.5 and 1, the distribution is moderately skewed.

# The kurtosis for drunk driving cases is (-0.525) and
# unemployement rate (-1.374) making it flatter than a normally 
# distributed curve and for the total injuries it
# is (+1.07) making it a positive kurtosis and steeper curve

# ---------GRAPHICALLY REPRESENTING NORMALITY------------

# Graphically representing the data for normality
install.packages("e1071")
library(e1071)

# Fits 3 plots in 1 page
par(mfrow = c(1,3)) 

# Plotting for drunk driving cases
plot(density(drunk.driving$drunk.driving.cases), 
     main = "Density plot : Drunk driving cases",
     ylab = "Frequency",
     sub = paste("Skewness :", 
                 round(e1071::skewness(drunk.driving$drunk.driving.cases), 2)))

# filling in the area under the density plot
polygon(density(drunk.driving$drunk.driving.cases), col = "blue")


# Plotting for total injuries
plot(density(drunk.driving$total.injuries), 
     main = "Density plot : Total injuries",
     ylab = "Frequency",
     sub = paste("Skewness :", 
                 round(e1071::skewness(drunk.driving$total.injuries), 2)))

# filling in the area under the density plot
polygon(density(drunk.driving$total.injuries), col = "red")

# Plotting for Unemployement Rate
plot(density(drunk.driving$unemployment.rate), 
     main = "Density plot : Unemployment Rate",
     ylab = "Frequency",
     sub = paste("Skewness :", 
                 round(e1071::skewness(drunk.driving$unemployment.rate), 2)))

# filling the area under the density plot
polygon(density(drunk.driving$unemployment.rate), col = "green")

# -------Checking using Q-Q Plot--------

# Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation 
# between a given sample and the normal distribution. A 45-degree 
# reference line is also plotted.
library("car")
qqPlot(drunk.driving$drunk.driving.cases, ylab = "Drunk Driving Cases")
qqPlot(drunk.driving$total.injuries, ylab = "Total injuries")
qqPlot(drunk.driving$total.injuries, ylab = "Unemployment Rate")
# From the drunk driving, total injuries and unemployment q-q plot's it 
# can be said that the data in both are over-dispersed relative to a 
# normal distribution.

# ---------SHAPIRO WILK TEST----------

# Formal test is done for normality using the Shaprio-wilks test

normality_test_1 <- shapiro.test(drunk.driving$drunk.driving.cases)
normality_test_1
normality_test_1$p.value

normality_test_2 <- shapiro.test(drunk.driving$total.injuries)
normality_test_2
normality_test_2$p.value

normality_test_3 <- shapiro.test(drunk.driving$unemployment.rate)
normality_test_3
normality_test_3$p.value

# p-value indicates whether the sample comes from a normal distribution 
# p-value is clearly lower than 0.05 so this indicates data is
# not normally distributed .Also the previous tests confirm that data
# is not normally distributed hence it is a Non-parametric test.
# Since both dependent and independent variables are Continuous/scale
# Spearman’s Correlation Coefficient will be used as the statistical test.

# ----HYPOTHESIS TESTING-------

# Hypothesis tests are used to test the validity of a claim that is made
# about a population. In this case the population has drunk driving incidents,
# total injuries and unemployment rate of Ireland. 

# The H0 is the null hypothesis
# The H1 is the alternate hypothesis

# H0: There is no effect of alcohol/drunk driving on total accidents 
# H1: There is a significant effect of alcohol/drunk driving on total 
# accidents.

# H0: μ1 ≠ μ2
# H1: μ1 = μ2 -> μ1 - μ2 = 0

#-------CHOOSING THE APPROPRIATE SAMPLE SIZE---------
# Power Analysis
install.packages("pwr")
library(pwr)

# Give the conventional effect size (small, medium, large) for the 
# tests available in the package
cohen.ES(test = "r", size = "large")

# sample size for a large size effect in the two-sided correlation test
# using the conventional power of 0.95. The alpha level is chosen as 0.5.
power_information <- pwr.r.test(r = 0.5, 
                                sig.level = 0.01,
                                power = 0.95)
power_information
plot(power_information)

# Where r = Linear correlation coefficient,
# sig.level = Significance level (Type I error probability) and
# power = Power of test (1 minus Type II error probability)

# The r is taken as 0.5 as suggested by the cohen.ES test and
# significance level is set as 0.01 with 95% certainity
# The results of the test suggest that a sample size of 62 is 
# needed for the modeling.

#------STATISTICAL TESTING-----------

# Spearman's Correlation Coefficient

cor.test( ~ drunk.driving.cases + total.injuries, 
          data=drunk.driving,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

# Since the p-value = 4.25e-10 < 0.05 So the null hypothesis is rejected.
# The alternate hypothesis is accepted which is There is a significant 
# effect of alcohol/drunk driving on total accidents.

# ------CHECKING CORRELATIONS-------

install.packages("ggcorrplot")
library(ggcorrplot)
corr <- round(cor(my_variables), 1)

ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 4, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Dataset", 
           ggtheme=theme_bw)

# The result suggests there is a  positive correlation between drunk
# driving cases and total injuries (+0.6), negative correlation between 
# unemployment rate v/s drunk driving case (-0.5) and total injuries (-0.3)

# -----FITTING A LINE FOR THE DATA--------

# Plotting a regression line for best fit

par(mfrow = c(1,1)) 

plot(drunk.driving.cases ~ total.injuries, 
     data = drunk.driving, pch = 16, cex = 1.3, col = "blue",
     main = "DRUNK DRIVING CASES PLOTTED AGAINST TOTAL INJURIES", 
     xlab = "TOTAL INJURIES", ylab = "DRUNK DRIVING CASES")
abline(lm(drunk.driving$drunk.driving.cases ~ drunk.driving$total.injuries))

#----------------------------End of Code---------------------------------#
##########################################################################
