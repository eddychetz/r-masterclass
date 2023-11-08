# DATA PREPROCESSING ----

# * Using prep(), recipe(), bake(), juice()
library(recipes)

mtcars

cars_train <- mtcars[1:20,]
cars_test <- mtcars[21:32,]

# Prepare recipe ----

cars_rec <- recipe(mpg ~ ., data = cars_train) %>%
    step_log(disp) %>%
    step_center(all_predictors())

cars_rec

# Execute the recipe steps ----

cars_prep <- prep(cars_rec)
cars_prep

# juice() and bake() performs the same task

juice(cars_prep)

bake(cars_prep, cars_train)
