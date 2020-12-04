# https://www.youtube.com/watch?v=BgWCuyrwD1s&t=557s

library(tidytuesdayR)
library(tidyverse)
theme_set(theme_light())

ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

ikea %>% 
  ggplot(aes(width, price)) +
  geom_point(alpha = 0.4) +
  scale_y_log10()

ikea %>% 
  select(X1, price, depth:width) %>% 
  pivot_longer(depth:width, names_to = "dim") %>% 
  ggplot(aes(value, price, color = dim)) +
  geom_point(alpha = 0.4) +
  scale_y_log10() +
  facet_wrap(~ dim, scales = "free_x") +
  labs(x = "",
       y = "Price")

# make df for modeling
ikea_df <- ikea %>% 
  select(price, name, category, depth, height, width) %>% 
  mutate(price = log10(price)) %>% 
  mutate(across(where(is.character), factor))

###--- build a model ---###
library(tidymodels)
set.seed(123)

ikea_split <- initial_split(ikea_df, strata = price)
ikea_train <- training(ikea_split)
ikea_test <- testing(ikea_split)

set.seed(234)
ikea_boots <- bootstraps(ikea_train, strata = price)

library(usemodels)
use_ranger(price ~., data - ikea_train)

library(textrecipes)
ranger_recipe <- 
  recipe(formula = price ~ ., data = ikea_train) %>% 
  # like fct_lump
  step_other(name, category, threshold = 0.01) %>% 
  # level name cleanup
  step_clean_levels(name, category) %>% 
  # impute missings
  step_knnimpute(depth, height, width)

ranger_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_mode("regression") %>% 
  set_engine("ranger") 

ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

set.seed(8577)
doParallel::registerDoParallel()
ranger_tune <-
  tune_grid(ranger_workflow, 
            resamples = ikea_boots, 
            grid = 11)

# show best in terms of specific metrics
show_best(ranger_tune, metric = "rmse")
show_best(ranger_tune, metric = "rsq")

autoplot(ranger_tune)

# finalize workflow with best model in terms of rmse, can use metric = "rsq" as well
final_rf <- ranger_workflow %>% 
  finalize_workflow(select_best(ranger_tune))

final_rf

# last fit with test_data and finalized workflow
ikea_fit <- last_fit(final_rf, ikea_split)
ikea_fit

###--- collect metrics ---###
collect_metrics(ikea_fit)

###--- collect predictions ---###
collect_predictions(ikea_fit)

collect_predictions(ikea_fit) %>% 
  ggplot(aes(price, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(size = 3, alpha = 0.5, color = "midnightblue") +
  coord_fixed()

# predict new obervation's price
predict(ikea_fit$.workflow[[1]], ikea_test[15,])

###--- variable importance ###---
library(vip)
imp_spec <- ranger_spec %>% 
  # re-run single model (with best model parameters) to include permutation vip, expensive to do for all models
  finalize_model(select_best(ranger_tune)) %>% 
  set_engine("ranger", importance = "permutation")

workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(imp_spec) %>% 
  fit(ikea_train) %>% 
  pull_workflow_fit() %>% 
  vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))




