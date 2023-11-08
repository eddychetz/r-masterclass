 # Classification
 # Clustering
 # Apriori algorithm
 # Decision trees

# GOAL: Improve Your Ability to Cluster Data with K-means

# LIBRARIES ----

library(tidymodels)
library(tidyclust) # For clustering
library(tidyverse) # May need to import library(lubridate)
library(tidyquant)
library(plotly)
library(janitor)
# DATA ----

marketing_campaign_tbl <- read_csv("./data/marketing_campaign.csv")

marketing_campaign_tbl <- marketing_campaign_tbl %>%
    clean_names() %>% 
    glimpse()

# Checking missing values

skimr::skim_without_charts(marketing_campaign_tbl)

# 1.0 DATA PREPARATION ----

data_prep_tbl <- marketing_campaign_tbl %>%

   
    # Remove NA values
    drop_na() %>%
    
    # Convert Dt_Customer datatype to Date
    mutate(dt_customer = dmy(dt_customer)) %>%
    
    # Feature: Customer Age - max customer date
    mutate(dt_customer_age = -1*(dt_customer - min(dt_customer)) / ddays(1)) %>%
    select(-dt_customer) %>%
    
    # Spent = Sum(mnt...)
    mutate(Spent = rowSums(across(starts_with("mnt")))) %>%
    
    # Remove unnecessary features
    select(-c(z_cost_contact, z_revenue, response))

data_prep_tbl %>% glimpse()

# 2.0 RECIPE ----

recipe_kmeans <- recipe(~ ., data = data_prep_tbl) %>%
    
    # encoding cat features
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    
    # normalize all features
    step_normalize(all_numeric_predictors()) %>%
    step_rm("id")

recipe_kmeans %>% prep() %>% juice() %>% glimpse()

# 3.0 K-MEANS MODEL ----

model_kmeans <- k_means(num_clusters = 3) %>%
    set_engine("stats")

set.seed(123)
wflw_fit_kmeans <- workflow() %>%

    add_recipe(recipe_kmeans) %>%
    add_model(model_kmeans) %>%
    fit(data_prep_tbl)

# 4.0 PREDICT NEW DATA ----

wflw_fit_kmeans %>% predict(data_prep_tbl)

extract_cluster_assignment(wflw_fit_kmeans)

extract_centroids(wflw_fit_kmeans)

# BONUS: VISUALIZE CLUSTERS ----

g <- data_prep_tbl %>%
    bind_cols(extract_cluster_assignment(wflw_fit_kmeans), .) %>%
    ggplot(aes(Spent, income)) +
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
