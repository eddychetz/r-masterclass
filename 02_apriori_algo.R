# APRIORI ALGORITHM ----

library(tidyverse)
library(janitor)
library(arules)

transactions_tbl <- read_csv('./data/transactions.csv')

transactions_tbl %>% glimpse()

# Create a transaction dataset from your data. Since your data appears to be in a wide format, 
# you'll need to reshape it into a transaction dataset where each row represents a transaction, 
# and each column represents an item

transactions <- transactions_tbl %>%
    pivot_longer(-...1, names_to = "item", values_to = "value") %>%
    filter(!is.na(value)) %>%
    select(...1, item) %>%
    unique() %>%
    droplevels()

transactions

# Convert the transaction data to a format suitable for the Apriori algorithm

transactions <- as(transactions, "transactions")

# Explore the resulting association rules

min_support <- 0.01  # Adjust the support threshold as needed
min_confidence <- 0.3  # Adjust the confidence threshold as needed

rules <- apriori(
    transactions,
    parameter = list(support = min_support, confidence = min_confidence)
)

# Explore the resulting association rules

inspect(rules)
