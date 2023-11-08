# APRIORI ALGORITHM ----

library(tidyverse)
library(janitor)
library(arules)

transactions_tbl <- read_csv('./data/transactions.csv') %>%
    
    clean_names()

transactions_tbl %>% glimpse()

# Create a transaction dataset from your data. Since your data appears to be in a wide format, 
# you'll need to reshape it into a transaction dataset where each row represents a transaction, 
# and each column represents an item

transactions <- transactions_tbl %>%
    pivot_longer(!x1, names_to = "item", values_to = "count") %>%
    filter(!is.na(count)) %>%
    select(x1, item) %>%
    unique() %>%
    droplevels()

transactions

# Convert the transaction data to a format suitable for the Apriori algorithm

transactions <- as(transactions, "transactions")

# Run the Apriori algorithm

min_support <- 0.01  # Adjust the support threshold as needed
min_confidence <- 0.3  # Adjust the confidence threshold as needed

rules <- apriori(
    transactions,
    parameter = list(
        support = min_support, 
        confidence = min_confidence
        ))

# Explore the resulting association rules

inspect(rules)
