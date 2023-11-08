# CLASSIFICATION ----

# * Libraries ----

library(tidyverse)
library(tidymodels)
library(janitor)

# * Read data ----

credit_tbl <- read_csv("./data/credit_loans.csv")

credit_tbl <- credit_tbl %>% 
    
    clean_names() %>%
    
    glimpse()

# * Data Preparation ----

credit_prep_tbl <- credit_tbl %>% 
    mutate_if(is.character, as_factor) %>%
    select(-loan_id) %>%
    mutate(credit_history = as_factor(credit_history)) %>%
    glimpse()

# Check missing values ----

skimr::skim_without_charts(credit_prep_tbl)

# * Split ----

set.seed(123)
split <- credit_prep_tbl %>% initial_split(prop = 0.8)

split

train_tbl <- training(split)
test_tbl <- testing(split)

# PREPROCESSING ----

# * Define the recipe ----

credit_recipe <- recipe(
    loan_status ~ ., 
    data = train_tbl
    ) %>%
    
    step_naomit(all_predictors()) %>%
    
# Encoding cat features
    step_dummy(
        all_nominal(), 
        one_hot = TRUE
        ) %>%
    step_normalize(all_predictors())

credit_recipe %>%
    prep() %>%
    bake(new_data = train_tbl) %>%
    glimpse()

# MODELING ----

model_spec <- rand_forest(trees = 100) %>%
    set_mode('classification') %>%
    set_engine('ranger')

# * Combine recipe and model in a workflow

loan_credit_wf <- workflow() %>%
    
    # Add preprocessing recipe
    add_recipe(credit_recipe) %>%
    
    # Add model spec
    add_model(model_spec) 

# Fit the model ----

credit_fit <- fit(loan_credit_wf, data = train_tbl)

# * Make predictions ----

predictions <- credit_fit %>%
    predict(test_tbl) %>%
    bind_cols(test_tbl)

# EVALUATE ----

# * Evaluate the model ----

credit_metrics <- predictions %>%
    metrics(truth = loan_status, estimate = .pred_class)

credit_metrics
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    