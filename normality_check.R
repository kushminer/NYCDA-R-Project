
source("data_cleaning.R")

#####################################################################################################################################

# Visual Analysis for Normal Distribution

#####################################################################################################################################


# ----------------------------> Visual check if variables are normally distributed

# Function to create histogram plots of each variable and fill by group
histogram_by_group <- function(data, variable) {
  p <- ggplot(data, aes(x = value, fill = group)) +
    geom_histogram(binwidth = 5000, alpha = 0.7,position = "dodge") +
    facet_wrap(~variable, ncol = 2, scales = "free") +
    xlab(variable) +
    ylab("Count") +
    theme(legend.position = "top") +
    ggtitle(paste("Histogram Plot by Group"))
  return(p)
}

# Function to create QQ plot plots of each variable and fill by group
qqplot_by_group <- function(data, variable) {
  p <- ggplot(data, aes(sample = value, fill = group, color = group)) +
    stat_qq(alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") + 
    facet_wrap(~variable, ncol = 2, scales = "free") +
    xlab("Theoretical Quantiles") +
    ylab("Sample Quantiles") +
    theme(legend.position = "top") +
    ggtitle(paste("QQ Plot Plot by Group"))
  return(p)
}

# Function to create box plot plots of each variable and fill by group
boxplot_by_group <- function(data, variable) {
  p <- ggplot(data, aes(x = group, y = value, fill = group)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~variable, ncol = 2, scales = "free") +
    xlab("Group") +
    ylab(variable) +
    theme(legend.position = "top") +
    ggtitle(paste("Boxplot by Group"))
  return(p)
}

# Function to create density plot plots of each variable and fill by group
densityplot_by_group <- function(data, variable) {
  p <- ggplot(data, aes(x = value, fill = group)) +
    geom_density(alpha = 0.7) +
    facet_wrap(~variable, ncol = 2, scales = "free") +
    xlab(variable) +
    ylab("Density") +
    theme(legend.position = "top") +
    ggtitle(paste("Density Plot by Group"))
  return(p)
}

# Histogram
## Suggests normal distribution if the histogram has looks like a bell curve 
histogram_by_group(combined_data, "Impressions")

#  QQ plot
## Suggests normal distribution if the data points on the Q-Q plot fall on or close to the diagonal line
qqplot_by_group(combined_data, "Reach")

# Box Plot
## Suggests normal distribution if the plot is roughly symmetrical with the median line in the center and the whiskers are of similar length
boxplot_by_group(combined_data, "Reach")

# Density Plot
## Suggests normal distribution if the density plot looks like a bell curve
densityplot_by_group(combined_data, "Website Clicks")

#####################################################################################################################################

                                        # Statistical Analysis for Normal Distribution

# These methods are not conclusive and can only provide an indication of whether the data may be normally distributed.
# For a more precise conclusion, statistical tests Shapiro-Wilk and Anderson Darling will be used

#####################################################################################################################################


# ----------------------------> Statistically check if variables are normally distributed

# Shapiro Wilk
#  Suggests data is not normally distributed if the p-value is less than the significance level (0.05)

shapiro_wilk <- function(data) {
  # Group the data by variable and group, and calculate the Shapiro-Wilk test statistic and p-value for each group
  sw_results <- data %>%
    group_by(variable, group) %>%
    summarize(test_statistic = shapiro.test(value)$statistic,
              p_value = shapiro.test(value)$p.value) %>%
    arrange(group)
  
  return(sw_results)
}

shapiro_wilk(combined_data)

# Summarize in text
shapiro_wilk_summary <- shapiro_wilk(combined_data) %>%
  filter(p_value >= 0.05) %>%
  group_by(group) %>%
  summarize("Normally Distributed Vars" = paste(variable, collapse = ", "))



# Anderson Darling
#  Suggests data is not normally distributed if the p-value is less than the significance level (0.05)

anderson_darling <- function(data) {
  # Group the data by variable and group, and calculate the Anderson-Darling test statistic and p-value for each group
  ad_results <- data %>%
    group_by(variable, group) %>%
    summarize(test_statistic = ad.test(value)$statistic,
              p_value = ad.test(value)$p.value) %>%
    arrange(group)
  
  return(ad_results)
}

# Summarize in text
anderson_darling_summary <- anderson_darling(combined_data) %>%
  filter(p_value >= 0.05) %>%
  group_by(group) %>%
  arrange(group) %>%
  summarize("Normally Distributed Vars" = paste(variable, collapse = ", "))

shapiro_wilk(combined_data)
anderson_darling(combined_data)
anderson_darling_summary
shapiro_wilk_summary # shapiro_wilk


#####################################################################################################################################

# Shapiro Wilk and Anderso Darling Analysis agree that the following variables have normal distribution
# Average Bidding: Spend (USD), Impressions, Reach, Website Clicks, Searches, Add to Cart, Purchase
# Maximum Bidding: Spend (USD), Impressions, View Content                                                        

# However, our visual analysis concludes otherwise. Therefore, we will trust our visual analysis and use Wilcox Rank Sum hypothesis test

# About the Wilcoxon Rank Sum Test:
# Non Parametric (used for non-normalized data) test used to compare medians of two independent groups
# Concludes there is a significant difference in the median of two groups when p-value is less than 0.05
# The test ranks all the values in both groups together, from lowest to highest, and then calculates the sum of the ranks for each group. It then calculates the test statistic, which is the smaller of the two sums. The p-value of the test is then calculated, which indicates the probability of obtaining the observed test statistic or a more extreme value under the null hypothesis.
# The Wilcoxon rank sum test is a useful alternative to the t-test when the normality assumption of the t-test is violated, or when the sample size is small. It is also appropriate when the data are ordinal, interval, or ratio, but not normally distributed.

#####################################################################################################################################




  