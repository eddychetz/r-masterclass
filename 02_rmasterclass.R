 # Classification
 # Clustering
 # Apriori algorithm
 # Decision trees

# GOAL: Improve Your Ability to Cluster Data with K-means

# LIBRARIES ----

library(tidymodels)
library(tidyclust)
library(tidyverse) # May need to import library(lubridate)
library(tidyquant)
library(plotly)

# DATA ----

marketing_campaign_tbl <- read_csv("./")

marketing_campaign_tbl %>% glimpse()

# 1.0 DATA PREPARATION ----

data_prep_tbl <- marketing_campaign_tbl %>%
    
    # Remove NA values
    drop_na() %>%
    
    # Convert Dt_Customer datatype to Date
    mutate(Dt_Customer = dmy(Dt_Customer)) %>% 
    
    # Feature: Customer Age - max customer date
    mutate(Dt_Customer_Age = -1*(Dt_Customer - min(Dt_Customer) ) / ddays(1) ) %>%
    select(-Dt_Customer) %>%
    
    # Spent = Sum(Mnt...)
    mutate(Spent = rowSums(across(starts_with("Mnt")))) %>%
    
    # Remove unnecessary features
    select(-Z_CostContact, -Z_Revenue, -Response)

data_prep_tbl %>% glimpse()

# 2.0 RECIPE ----

recipe_kmeans <- recipe(~ ., data = data_prep_tbl) %>%
    
    # encoding cat features
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    
    # normalize all features
    step_normalize(all_numeric_predictors()) %>%
    step_rm("ID")

recipe_kmeans %>% prep() %>% juice() %>% glimpse()

# 3.0 K-MEANS MODEL ----

model_kmeans <- k_means(num_clusters = 4) %>%
    set_engine("stats")

set.seed(123)
wflw_fit_kmeans <- workflow() %>%
    add_model(model_kmeans) %>%
    add_recipe(recipe_kmeans) %>%
    fit(data_prep_tbl)

# 4.0 PREDICT NEW DATA ----

wflw_fit_kmeans %>% predict(data_prep_tbl)

extract_cluster_assignment(wflw_fit_kmeans)

extract_centroids(wflw_fit_kmeans)

# BONUS: VISUALIZE CLUSTERS ----

g <- data_prep_tbl %>%
    bind_cols(extract_cluster_assignment(wflw_fit_kmeans), .) %>%
    ggplot(aes(Spent, Income)) +
    geom_point(
        aes(fill = .cluster),
        shape = 21,
        alpha = 0.3,
        size  = 5
    ) +
    geom_smooth(color = "blue", se = FALSE) +
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_y_continuous(
        labels = scales::dollar_format(),
        limits = c(0, 200000)
    ) +
    labs(title = "Customer Clusters: Spent vs Income") +
    scale_fill_tq() +
    theme_tq()

ggplotly(g)