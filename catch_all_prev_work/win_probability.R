
source("data_cleaning.R")


#####################################################################################################################################

# Probability Single Var will remain within 5% of mean (UNDUSED)

#####################################################################################################################################

# Split data by group
grouped_data <- combined_data %>%
  filter(variable == "Add to Cart") %>%
  group_by(group)

# Define function to calculate probability of same spend for a group
prob_same <- function(data) {
  n_sims <- 10000
  spend_sims <- rnorm(n_sims, mean = mean(data$value), sd = 0.05 * mean(data$value))
  same_spend <- sum(abs(spend_sims - mean(data$value)) / mean(data$value) < 0.05) / n_sims
  return(same_spend)
}

# Apply function to each group and summarize results
group_probs <- grouped_data %>%
  summarize(prob_same = prob_same(.))

# Print results
group_probs

#####################################################################################################################################

# Calculating the win probability of a Maximum Bidding campaign compared to an Average Bidding campaign

#####################################################################################################################################

# extract the number of Website Clicks and Impressions for each group:
clicks_impressions <- combined_data %>%
  filter(variable %in% c("Website Clicks", "Impressions")) %>%
  group_by(group, variable) %>%
  summarize(sum = sum(value))

#extract the number of successes (Website Clicks) and trials (Impressions) for each group:
successes_A <- clicks_impressions %>%
  filter(group == "Average Bidding", variable == "Website Clicks") %>%
  pull(sum)

trials_A <- clicks_impressions %>%
  filter(group == "Average Bidding", variable == "Impressions") %>%
  pull(sum)

successes_B <- clicks_impressions %>%
  filter(group == "Maximum Bidding", variable == "Website Clicks") %>%
  pull(sum)

trials_B <- clicks_impressions %>%
  filter(group == "Maximum Bidding", variable == "Impressions") %>%
  pull(sum)

# Set the number of samples
n_samples <- 1000000

# Generate samples from the Beta distribution for each group / Monte Carlo Simulation
samples_A <- rbeta(n_samples, shape1 = successes_A + 1, shape2 = trials_A - successes_A + 1)
samples_B <- rbeta(n_samples, shape1 = successes_B + 1, shape2 = trials_B - successes_B + 1)

# Calculate the win probability for Maximum Bidding
win_probability_B <- mean(samples_B > samples_A)

# Display the win probability
cat("Win probability for Maximum Bidding:", paste0(win_probability_B*100,"%"), "\n")

#####################################################################################################################################

# Check Normalized Metrics for Normalization (USUSED BECAUSE KEY METRICS (RAW METRICS) ALREADY CHECKS)

#####################################################################################################################################


# ------------------------------> check for normal distribution 

# Function to create histogram plots of each variable and fill by group
histogram_by_group <- function(data) {
  p <- ggplot(data, aes(x = CTR, fill = group)) +
    geom_histogram(binwidth = 0.01, alpha = 0.7) +
    xlab("CTR") +
    ylab("Count") +
    theme(legend.position = "top")
  return(p)
}

# Function to create QQ plot plots of each variable and fill by group
qqplot_by_group <- function(data) {
  p <- ggplot(data, aes(sample = CTR, fill = group, color = group)) +
    stat_qq(alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    xlab("Theoretical Quantiles") +
    ylab("Sample Quantiles") +
    theme(legend.position = "top")
  return(p)
}

# Function to create box plot plots of each variable and fill by group
boxplot_by_group <- function(data) {
  p <- ggplot(data, aes(x = group, y = CTR, fill = group)) +
    geom_boxplot(alpha = 0.7) +
    xlab("Group") +
    ylab("CTR") +
    theme(legend.position = "top")
  return(p)
}

# Function to create density plot plots of each variable and fill by group
densityplot_by_group <- function(data) {
  p <- ggplot(data, aes(x = CTR, fill = group)) +
    geom_density(alpha = 0.7) +
    xlab("CTR") +
    ylab("Density") +
    ggtitle("Density Plot of CTR by Group")
  return(p)
}

# Create histogram plots of each variable and fill by group
histogram_by_group(KPI)

# Create QQ plot plots of each variable and fill by group
qqplot_by_group(KPI)

# Create box plot plots of each variable and fill by group
boxplot_by_group(KPI)

# Create density plot plots of each variable and fill by group
densityplot_by_group(KPI)

# ----------------------> Not normal, use Wilcox

# subset the data for the two groups
group1 <- KPI$CTR[KPI$group == "Average Bidding"]
group2 <- KPI$CTR[KPI$group == "Maximum Bidding"]

# perform the Wilcoxon rank-sum test
wilcox.test(group1, group2) 

combined_data %>%
  filter(variable == "Reach")
