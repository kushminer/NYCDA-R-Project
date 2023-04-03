

# Changing the scale of the data so that it is easier to compare values that have different scales. 
# First calculate the total number of purchases for both groups. 
# Then, use a function that calculates the purchase-to-metric ratio tgat gives us a measure of how much each metric contributes to the number of purchases made.
# Next, we use a custom normalization function that takes the variable, value, and group as input and returns a normalized value. 
# The function checks if the variable is "Purchase", in which case it returns the original value. 
# If the variable is any other metric, the function uses the purchase-to-metric ratio for that metric and group to normalize the value.
# Finally, we apply the normalization function to all rows in the combined data frame and add the normalized values to a new column called "normalized_value". 
# This new column contains the values that we can use to compare the performance of the two groups on different metrics, as they are now on the same scale.

# --------------------------- > Normalize the data

# Calculate the total purchases for both groups
total_purchases_average_bidding <- sum(combined_data[combined_data$variable == "Purchase" & combined_data$group == "Average Bidding", "value"])
total_purchases_maximum_bidding <- sum(combined_data[combined_data$variable == "Purchase" & combined_data$group == "Maximum Bidding", "value"])

# Function to calculate Purchase-to-Metric ratios for both groups
calculate_purchase_to_metric_ratio <- function(metric) {
  total_metric_average_bidding <- sum(combined_data[combined_data$variable == metric & combined_data$group == "Average Bidding", "value"])
  total_metric_maximum_bidding <- sum(combined_data[combined_data$variable == metric & combined_data$group == "Maximum Bidding", "value"])
  
  purchase_to_metric_ratio_average_bidding <- total_purchases_average_bidding / total_metric_average_bidding
  purchase_to_metric_ratio_maximum_bidding <- total_purchases_maximum_bidding / total_metric_maximum_bidding
  
  return(list(purchase_to_metric_ratio_average_bidding, purchase_to_metric_ratio_maximum_bidding))
}

# Calculate Purchase-to-Metric ratios for all metrics
ratios <- lapply(c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart"), calculate_purchase_to_metric_ratio)

# Custom normalization function
normalize_values <- function(variable, value, group) {
  if (variable == "Purchase") {
    return(value)
  }
  
  metric_index <- which(c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart") == variable)
  group_index <- ifelse(group == "Average Bidding", 1, 2)
  
  return(value * ratios[[metric_index]][[group_index]])
}

# Normalize the data
combined_data$normalized_value <- mapply(normalize_values, combined_data$variable, combined_data$value, combined_data$group)

# Print the normalized data
combined_data

