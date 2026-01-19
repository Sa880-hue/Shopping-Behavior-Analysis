################################################################################

#Shopping Behavior Analysis

################################################################################

# Section 1: Setup and Data Loading 

################################################################################

install.packages("readxl")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")

# Loading dataset into the R Studio
new_data <- read.csv("shopping_behavior.csv",header=TRUE)
head(new_data)


################################################################################
# Section 2: Data Validation and Final Cleaning
# Note: Initial data inspection and basic cleaning were performed in Excel.
# This section validates and finalizes the dataset for analysis in R.

################################################################################


colSums(is.na(new_data)) # which is equal to 0.

sum(duplicated(new_data)) # which is equal to 0.

library(dplyr)
glimpse(new_data)
summary(new_data)


################################################################################

# Section 3: Feature Engineering

################################################################################


# Age-grouping
new_data <- new_data %>%
  mutate(
    age_group = case_when(
      age < 25 ~ "Under 25",
      age >= 25 & age < 35 ~ "25-34",
      age >= 35 & age < 45 ~ "35-44",
      age >= 45 & age < 55 ~ "45-54",
      age >= 55 ~ "55+"
    )
  )

# Convert purchase_amount_usd into spending levels.
new_data <- new_data %>%
  mutate(
    spending_level = case_when(
      purchase_amount_usd < 40 ~ "Low",
      purchase_amount_usd >= 40 & purchase_amount_usd < 70 ~ "Medium",
      purchase_amount_usd >= 70 ~ "High"
    )
  )

# Converting subscription_status into binary
new_data <- new_data %>%
  mutate(
    subscription_flag = ifelse(subscription_status == "Yes", 1, 0)
  )

# Converting discount_applied & promo_code_used into binary
new_data <- new_data %>%
  mutate(
    discount_flag = ifelse(discount_applied == "Yes", 1, 0),
    promo_flag = ifelse(promo_code_used == "Yes", 1, 0)
  )

# Previous purchase level
new_data <- new_data %>%
  mutate(
    customer_type = case_when(
      previous_purchases < 10 ~ "New",
      previous_purchases >= 10 & previous_purchases < 30 ~ "Returning",
      previous_purchases >= 30 ~ "Loyal"
    )
  )

# Turning review_ratings into labels
new_data <- new_data %>%
  mutate(
    rating_category = case_when(
      review_rating < 3 ~ "Low",
      review_rating >= 3 & review_rating < 4 ~ "Medium",
      review_rating >= 4 ~ "High"
    )
  )

#Updated dataset
glimpse(new_data)


################################################################################

# Section 4: Exploratory Data Analysis (using ggplot2 and dplyr libraries)

################################################################################


library(ggplot2)
library(dplyr)


# Question 1: How much do customers generally spend?
ggplot(new_data, aes(x = purchase_amount_usd)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Purchase Amounts (USD)",
    x = "Purchase Amount (USD)",
    y = "Number of Purchases"
  )
# Insights acquired: 
# Most customers spend between $39 and $81, with an average spending of around $60. 
# Spending is fairly evenly distributed within this range, and there are no extreme outliers.


# Question 2: What items are purchased most frequently?
top_items <- new_data %>%
  count(item_purchased, sort = TRUE) %>%
  slice_head(n = 10)

ggplot(top_items, aes(x = reorder(item_purchased, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 10 Most Purchased Items",
    x = "Item",
    y = "Number of Purchases"
  )
# Insights acquired:
# Apparel items such as pants, shirts, and dresses are the most frequently purchased.
# Accessories like belts and sunglasses appear less frequently, indicating they are secondary purchases.


# Question 3: Do discounts lead to higher purchase amounts?
ggplot(new_data, aes(x = discount_applied, y = purchase_amount_usd)) +
  geom_boxplot(fill = "orange") +
  labs(
    title = "Impact of Discounts on Purchase Amount",
    x = "Discount Applied",
    y = "Purchase Amount (USD)"
  )
# Insights acquired:
# The median purchase amount remains consistent (~$60) regardless of whether a discount is applied.
# This suggests that discounts do not lead to higher spending per transaction.


# Question 4: Do subscribers spend more?
ggplot(new_data, aes(x = subscription_status, y = purchase_amount_usd)) +
  geom_boxplot(fill = "purple") +
  labs(
    title = "Purchase Amount by Subscription Status",
    x = "Subscription Status",
    y = "Purchase Amount (USD)"
  )
# Insights acquired:
# The median purchase amount for subscribers and non-subscribers is nearly identical (~$60).
# This indicates that having a subscription does not significantly impact the amount spent per purchase.
# However, subscribers show slightly more consistent spending behavior


# Question 5: Which categories generate higher spending?
ggplot(new_data, aes(x = category, y = purchase_amount_usd)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Purchase Amount by Product Category",
    x = "Category",
    y = "Purchase Amount (USD)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Insights acquired:
# Clothing and footwear categories show higher median purchase amounts,
# while accessories tend to be lower-value purchases.
# Outerwear exhibits high variability, suggesting diverse pricing ranges.


# Question 6: How does loyalty affect spending?
ggplot(new_data, aes(x = customer_type, y = purchase_amount_usd)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Purchase Amount by Customer Type",
    x = "Customer Type",
    y = "Purchase Amount (USD)"
  )
# Insights acquired:
# Purchase amount remains relatively consistent across customer types.
# New customers show a slightly higher median spending, possibly due to
# initial purchase behavior or onboarding offers.
# Loyalty appears to influence purchase frequency rather than order value.


# Question 7: Is higher spending linked to better reviews?
ggplot(new_data, aes(x = rating_category, y = purchase_amount_usd)) +
  geom_boxplot(fill = "lightcoral") +
  labs(
    title = "Purchase Amount by Review Rating",
    x = "Review Rating",
    y = "Purchase Amount (USD)"
  )
# Insights acquired:
# Purchase amounts remain relatively consistent across low, medium,
# and high review rating groups. This suggests that higher customer
# satisfaction (as measured by reviews) does not strongly influence
# how much customers spend per transaction.


# Question 8: How often do customers buy?
ggplot(new_data, aes(x = frequency_of_purchases)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Frequency of Customer Purchases",
    x = "Purchase Frequency",
    y = "Number of Customers"
  )
# Insights acquired: Customers generally buy items once in 3 months or in a year


################################################################################

# End of Project

################################################################################








