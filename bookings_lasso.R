require(tidyverse)
require(tidymodels)

bookings <- read_csv("hotel_bookings.csv")

glimpse(bookings)


# predict lead time (days between initial booking and arrival date)

num_cols <- bookings %>% 
  select_if(is.numeric) %>% 
  # select(-c(is_canceled)) %>% 
  names()

mod_dat <- bookings %>% 
  mutate_if(is.character, ~as.factor(.)) %>% 
  mutate(id = row_number()) %>% 
  select(-reservation_status_date) %>% 
  filter(!is.na(children)) %>% 
  select(all_of(num_cols), country) %>% 
  # mutate(id = as.character(row_number())) %>% 
  select(-c(arrival_date_year)) %>% 
  mutate(country = as.character(fct_explicit_na(fct_lump(country, 4))))
  # select(-country)

summary(mod_dat)

glimpse(mod_dat)

book_split <- initial_split(mod_dat, strata = country)
book_train <- training(book_split)
book_test <- testing(book_split)

book_rec <- recipe(lead_time ~., data = book_train) %>% 
  # update_role(id, new_role = "ID") %>% 
  update_role(country, new_role = "ID") %>% 
  step_zv(all_numeric(), -all_outcomes()) %>% 
  step_normalize(all_numeric(), -all_outcomes())

book_prep <- book_rec %>% 
  prep()

lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>% 
  set_engine("glmnet")

wf <- workflow() %>% 
  add_recipe(book_rec)

lasso_fit <- wf %>% 
  add_model(lasso_spec) %>% 
  fit(data = book_train)

lasso_fit %>% 
  pull_workflow_fit() %>% 
  tidy()

set.seed(1234)

book_boot <- bootstraps(book_train)

tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

lambda_grid <- grid_regular(penalty(),
                            levels = 50)

doParallel::registerDoParallel()

set.seed(4321)

lasso_grid <- tune_grid(
  wf %>% add_model(tune_spec),
  resamples = book_boot,
  grid = lambda_grid
)

lasso_grid %>% 
  collect_metrics() %>% 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(ymin = mean - std_err,
                    ymax = mean + std_err),
                alpha = 0.5) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")

lowest_rmse <- lasso_grid %>% 
  select_best("rmse", maximize = FALSE)

final_lasso <- finalize_workflow(wf %>% add_model(tune_spec),
                                 lowest_rmse)

require(vip)

# variable importance with sign
final_lasso %>% 
  fit(book_train) %>% 
  pull_workflow_fit() %>% 
  vi(lambda = lowest_rmse$penalty) %>% 
  mutate(Importance = abs(Importance),
         Variable = fct_reorder(Variable, Importance)) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0,0)) +
  labs(y = NULL)


last_fit(final_lasso,
         book_split) %>% 
  collect_metrics()

predict(
  final_lasso %>% fit(book_train),
  book_test
)
