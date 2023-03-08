
# -------------------------------------------- import libraries

library(dplyr)
library(ggplot2)
library(reshape2)

# -------------------------------------------- quick view

# View Head
head(control_group)
head(test_group)

# View Structure
str(control_group)
str(test_group)

# Row Number Check
nrow(control_group) - nrow(test_group) # Same number of rows

# Column Number Check
ncol(control_group) - ncol(test_group) # Same number of cols

# Title Values

number_of_days = nrow(control_group)

# -------------------------------------------- import

control_group <- read.csv("control_group.csv", sep = ";", header = F)
test_group <- read.csv("test_group.csv", sep = ";", header = F)

# -------------------------------------------- Wrangling

# --------------------- Column Adjustments

# Column Names

column_names <- c("Campaign Name", "Date", "Spend (USD)",
                  "Impressions", "Reach", "Website Clicks", 
                  "Searches", "View Content", "Add to Cart", 
                  "Purchase")

colnames(control_group) <- column_names
colnames(test_group)    <- column_names

# Change Numeric Columns Type to Integer

numeric_cols <- c("Spend (USD)", "Impressions", "Reach", "Website Clicks", 
                  "Searches", "View Content", "Add to Cart", "Purchase")

control_group[, numeric_cols] <- sapply(control_group[, numeric_cols], as.integer)
test_group[, numeric_cols] <- sapply(test_group[, numeric_cols], as.integer)

# Change Date Column to Date
test_group$Date <- as.Date(test_group$Date, "%d.%m.%Y")
control_group$Date <- as.Date(control_group$Date, "%d.%m.%Y")

# --------------------- Row Adjustments

# Remove Row 1
control_group <- control_group[-1,]
test_group <- test_group[-1,]

# Reset Row Numbers
rownames(control_group) <- 1:nrow(control_group)
rownames(test_group) <- 1:nrow(test_group)

# --------------------- Missing Values

# Check for missing values
sum(is.na(control_group)) # 7 Missing Values
sum(is.na(test_group))    # 0 Missing Values

# Check for missing data
rowSums(is.na(control_group)) # Row 5 Missing Data

# Delete row 5 in both columns --> Opted to delete rather than impute
control_group <- control_group[-5, ]
test_group <- test_group[-5, ]

# Reset Row Numbers Again
rownames(control_group) <- 1:nrow(control_group)
rownames(test_group) <- 1:nrow(test_group)

# --------------------- Add Columns

# Add Day of Week
control_group$Date <- as.Date(control_group$Date, format = "%d.%m.%Y")
control_group$day_of_week <- weekdays(control_group$Date)

# -------------------------------------------- descriptive statistics

# mean, median, and standard deviation
means <- colMeans(control_group[, c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase")], na.rm = TRUE)
medians <- apply(control_group[, c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase")], 2, median, na.rm = TRUE)
stddevs <- apply(control_group[, c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase")], 2, sd, na.rm = TRUE)

# mode
modes <- apply(control_group[, c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase")], 2, function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
})

# quartile ranges
q1 <- apply(control_group[, c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase")], 2, quantile, probs = c(0.25), na.rm = TRUE)
q3 <- apply(control_group[, c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase")], 2, quantile, probs = c(0.75), na.rm = TRUE)
iqr <- q3 - q1

# combine
desc_stats <- data.frame(mean = means, median = medians, mode = modes, stddev = stddevs, q1 = q1, q3 = q3, iqr = iqr)



# -------------------------------------------- line plots

# Plot Spend (USD) column
ggplot(data = control_group) + 
  geom_line(aes(x = Date, y = `Spend (USD)`, color = "Control Group")) +
  geom_line(data = test_group, aes(x = Date, y = `Spend (USD)`, color = "Test Group")) +
  ggtitle("Spend (USD) Over Time") + 
  xlab("Date") + 
  ylab("Spend (USD)") + 
  scale_color_manual(values = c("Control Group" = "blue", "Test Group" = "red"))

# Plot Impressions column
ggplot(data = control_group) + 
  geom_line(aes(x = Date, y = Impressions, color = "Control Group")) +
  geom_line(data = test_group, aes(x = Date, y = Impressions, color = "Test Group")) +
  ggtitle("Impressions Over Time") + 
  xlab("Date") + 
  ylab("Impressions") + 
  scale_color_manual(values = c("Control Group" = "blue", "Test Group" = "red"))

# Plot Reach column
ggplot(data = control_group) + 
  geom_line(aes(x = Date, y = Reach, color = "Control Group")) +
  geom_line(data = test_group, aes(x = Date, y = Reach, color = "Test Group")) +
  ggtitle("Reach Over Time") + 
  xlab("Date") + 
  ylab("Reach") + 
  scale_color_manual(values = c("Control Group" = "blue", "Test Group" = "red"))

# Plot Website Clicks column
ggplot(data = control_group) + 
  geom_line(aes(x = Date, y = `Website Clicks`, color = "Control Group")) +
  geom_line(data = test_group, aes(x = Date, y = `Website Clicks`, color = "Test Group")) +
  ggtitle("Website Clicks Over Time") + 
  xlab("Date") + 
  ylab("Website Clicks") + 
  scale_color_manual(values
                     
# -------------------------------------------- facet wrap bar charts

# Melt the data into long format
columns_to_melt <- c("Spend (USD)", "Impressions", "Reach",
                     "Website Clicks", "Searches", "View Content",
                     "Add to Cart", "Purchase")

control_group_melt <- melt(control_group, id.vars = "Date", measure.vars = columns_to_melt)
test_group_melt <- melt(test_group, id.vars = "Date", measure.vars = columns_to_melt)

# Add a column indicating the group (control or test)
control_group_melt$group <- "control"
test_group_melt$group <- "test"

# Combine the two melted data frames into one data frame
combined_data <- rbind(control_group_melt, test_group_melt)

# Facet bar plot
ggplot(combined_data, aes(x = Date, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Facet line plot
ggplot(combined_data, aes(x = Date, y = value, color = group)) +
  geom_line(aes(group = group)) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Stacked Plots
ggplot(combined_data, aes(x = Date, y = value, fill = group)) +
  geom_area(position = "stack") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~variable, scales = "free_y")

# -------------------------------------------- Totals Comp

library(ggplot2)

# Aggregate data to get sum of each column
agg_data <- combined_data %>%
  group_by(variable, group) %>%
  summarize(sum = sum(value))

# Plot bar plot with facets for each column
ggplot(agg_data, aes(x = group, y = sum, fill = group)) +
  geom_bar(stat = "identity") +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(agg_data, aes(x = group, y = sum, fill = group)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(sum)), position = position_stack(vjust = 0.5)) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# -------------------------------------------- Variable Impact

# Spend Comparison
head(control_group)

ggplot(control_group, aes(x = 'Spend (USD)', y = Impressions)) +
  geom_point() +
  xlab("Spend (USD)") +
  ylab("Impressions") +
  ggtitle("Spend vs Impressions")


