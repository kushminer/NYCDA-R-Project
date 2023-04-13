# README: A/B Testing App for Bidding Strategies

This application is designed to perform an A/B test on the efficacy of two bidding strategies: Average Bidding and Maximum Bidding. The dataset used in this analysis was provided by a client, ....com, who wants to understand which bidding strategy brings more conversions.

## Test Introduction
A company recently introduced a new bidding type, “average bidding,” as an alternative to its existing bidding type called “maximum bidding.” The client wants to test this new feature using an A/B test. The context of the dataset is limited, but we will do our best to draw the most meaningful insights possible.

[Link to Test Data](https://www.kaggle.com/datasets/ilkeryildiz/example-dataset-for-ab-test?resource=download)

## Bidding Strategies
### Average Bidding
Also known as: Maximum Clicks or Automated Bid Strategy
Description: Set an average daily budget, and the Google Ads system automatically manages your bids to bring the most clicks possible within your budget.
Example: You have a website that sells a variety of art supplies, and your main goal is to bring more customers to your site. You have a set amount that you want to spend on advertising each month, and there isn't a particular product you want to emphasize most. Maximize Clicks lets you decide the overall amount of your budget, then we'll find you the most customers based on that.
### Maximum Bidding
Also known as: Manual CPC bidding
Description: Manage your maximum cost per click. You can set different bids for each ad group in your campaign, or for individual keywords or placements. If you’ve found certain keywords or placements are more profitable, you can use manual bidding to allocate more of your ad budget to those keywords or placements.
Example: Although your website sells a wide range of art supplies, you're most interested in selling paint brushes. With Manual CPC bidding, even if your ad group has 15 keywords, you can choose to set a higher bid for only the keyword "paint brushes," which will apply whenever that keyword triggers your ad.

[Link to Bidding Strategy Info](https://support.google.com/google-ads/answer/2472725?hl=en)

## Data Housekeeping
### NaN Values
During the analysis, we found that Average bidding was missing values for 8/5/2019. To balance the data, we decided to remove the same data from Maximum bidding. We considered other options, such as imputing the missing data or using a different time period for the analysis, but decided that removing the data was the most balanced option.

### Hypothesis Tests
To assess whether the values of the two groups were significantly different, a hypothesis test was conducted. The appropriate hypothesis test was determined by examining the raw data's normality using various visualizations and statistical analyses. Only one key metric, Website Clicks, appeared to be potentially normally distributed. To confirm this, the Shapiro-Wilk and Anderson-Darling statistical tests were performed.

The non-parametric Wilcoxon Rank Sum Test was used for the remainder of the hypothesis analysis. The analysis can be found in normality_check.R.

### Win Probability
Win Probability analysis was utilized to evaluate the likelihood of success for various conversion and cost metrics such as Clickthrough Rate and Cost per Purchase. Win Probability takes into account factors such as historical performance and uses a Monte Carlo simulation to analyze multiple scenarios and determine the likelihood of achieving success in the future.

Through the Monte Carlo simulation, Win Probability analyzed 10,000 simulations to calculate the likelihood of success for each metric. This resulted in a percentage that indicated the probability of achieving success for each metric in the future.



