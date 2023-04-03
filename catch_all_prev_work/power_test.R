
# Power Test

# Set the minimum detectable effect (MDE)
mde <- 0.02

# Set the significance level (alpha) and the statistical power
alpha <- 0.05
power <- 0.8

# Estimate the baseline conversion rate for the control group
baseline_cr <- 0.05

# Load the pwr package for power calculations
library(pwr)

# Calculate the necessary sample size
sample_size <- pwr.2p.test(h = mde, sig.level = alpha, power = power, 
                           n = NULL, alt = "greater", 
                           pi = baseline_cr, delta = NULL, 
                           f = NULL, type = "pooled")

# Print the sample size
cat("The necessary sample size is", round(sample_size$n, 0), "impressions.")
