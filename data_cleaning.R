
# Data Cleaning File

# Load Libraries
library(tidyr)
library(purrr)
library(broom)
library(dplyr)
library(scales)
library(plotly)
library(nortest)
library(ggplot2)
library(forcats)
library(reshape2)
library(gridExtra)


# ----------------- Raw Data

average_bidding <- read.csv("/Users/samuelminer/Downloads/7 R.R/*R-Project/R-Shiny-App----A-B-Test-Analysis/control_group.csv", 
                            sep = ";", 
                            header = F)
maximum_bidding <- read.csv("/Users/samuelminer/Downloads/7 R.R/*R-Project/R-Shiny-App----A-B-Test-Analysis/test_group.csv",
                            sep = ";",
                            header = F)


# ----------------- Cleaning

# Column Names Vector
column_names <- c("Campaign Name", 
                  "Date", 
                  "Spend (USD)",
                  "Impressions", 
                  "Reach", 
                  "Website Clicks", 
                  "Searches", 
                  "View Content", 
                  "Add to Cart", 
                  "Purchase")

# Change Column Names
colnames(maximum_bidding) <- column_names
colnames(average_bidding) <- column_names

# Remove the rows with column labels
average_bidding <- average_bidding[-1,] 
maximum_bidding <- maximum_bidding[-1,]       

# Change the data type of columns to integer
numeric_cols <- c("Spend (USD)", "Impressions", "Reach", "Website Clicks", 
                  "Searches", "View Content", "Add to Cart", "Purchase")
average_bidding[, numeric_cols] <- sapply(average_bidding[, numeric_cols], as.integer)
maximum_bidding[, numeric_cols] <- sapply(maximum_bidding[, numeric_cols], as.integer)

# Change the data type of the date column to Date
average_bidding$Date <- as.Date(average_bidding$Date, "%d.%m.%Y")
maximum_bidding$Date <- as.Date(maximum_bidding$Date, "%d.%m.%Y")

# Check for missing values
which(is.na(average_bidding) | average_bidding == "", arr.ind = TRUE)
sum(is.na(average_bidding))
which(is.na(maximum_bidding) | maximum_bidding == "", arr.ind = TRUE)
sum(is.na(maximum_bidding))

# Remove rows with missing values
average_bidding <- average_bidding[-5, ] # 7 Missing Values in this row 
maximum_bidding <- maximum_bidding[-5, ] # Removing to balance data

# Check for missing values again
which(is.na(average_bidding) | average_bidding == "", arr.ind = TRUE)
sum(is.na(average_bidding))
which(is.na(maximum_bidding) | maximum_bidding == "", arr.ind = TRUE)
sum(is.na(maximum_bidding))

# Reset the row numbers
rownames(average_bidding) <- 1:nrow(average_bidding)
rownames(maximum_bidding) <- 1:nrow(maximum_bidding)

# Add Day of Week
average_bidding$Weekday <- weekdays(average_bidding$Date)
maximum_bidding$Weekday <- weekdays(maximum_bidding$Date)

# ---------- Melt data to new DF

# Melt the data into long format
columns_to_melt <- c("Spend (USD)", "Impressions", "Reach",
                     "Website Clicks", "Searches", "View Content",
                     "Add to Cart", "Purchase")

average_bidding_melt <- melt(average_bidding, id.vars = "Date", measure.vars = columns_to_melt)
maximum_bidding_melt <- melt(maximum_bidding, id.vars = "Date", measure.vars = columns_to_melt)

# Add a column indicating the group (average_bidding or maximum_bidding)
average_bidding_melt$group <- "average_bidding"
maximum_bidding_melt$group <- "maximum_bidding"

# Combine the two melted data frames into one data frame
combined_data <- rbind(average_bidding_melt, maximum_bidding_melt)

# Add Day of Week
combined_data$Weekday <- weekdays(combined_data$Date)

# Change Group Names
combined_data$group <- gsub("average_bidding", "Average Bidding", 
                            gsub("maximum_bidding", "Maximum Bidding", combined_data$group))


