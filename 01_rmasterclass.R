library(tidyverse)

data_tbl <- data.frame(
    Name = c("Alice", "Robert", "Eddy"),
    Age = c(34, 36, 25),
    Score = c(78, 69, 85)
)

data_tbl %>% glimpse()

data2_tbl <- tibble(
    Name = c("Alice", "Robert", "Eddy"),
    Age = c(34, 36, 25),
    Score = c(78, 69, 85)
)

data2_tbl %>% glimpse()

# FS library ----

library(fs)

fs::dir_create("./day_01/")
fs::dir_create("./data/")

# Save data ----

saveRDS(data_tbl, "./day_01/table.rds")

write.csv(data_tbl, "./day_01/table.csv")


# Read data ----
library(janitor)

credit_tbl <- read_csv("./credit_loans.csv")

credit_tbl <- credit_tbl %>% 
    
    clean_names() %>%
    
    glimpse()

# Filtering ---

credit_tbl %>%
    
    filter(between(loan_amount, 500, 1000))

credit_tbl %>%
    
    filter(education %in% c('Graduate'))


cleaned_credit_tbl <- credit_tbl %>% 
    mutate_if(is.character, as_factor) %>%
    select(-loan_id) %>%
    mutate(credit_history = as_factor(credit_history)) %>%
    
    glimpse()

# Quick summary ----

library(skimr)

skimr::skim_without_charts(cleaned_credit_tbl)


library(DataExplorer)

cleaned_credit_tbl %>%
    
    plot_missing()

# Correlation Analysis ----

library(correlationfunnel)

cleaned_credit_tbl %>%
    
    na.omit() %>%
    
   binarize(n_bins = 5) %>%
    
    correlate(target = "loan_status__Y") %>%
    
    plot_correlation_funnel(interactive = F) %>%
    
    glimpse()






