require(tidyverse)

sparrows <- rio::import("https://raw.githubusercontent.com/StirlingCodingClub/Manuscripts_in_Rmarkdown/master/data/Bumpus_data.csv") %>% 
  mutate(surv = factor(surv),
         sex = factor(sex)) %>% 
  mutate(id = row_number()) %>% 
  select(-sex)

head(sparrows)
glimpse(sparrows)

sparrows %>% 
  ggplot(aes(surv, totlen)) +
  geom_boxplot()

# GGally::ggpairs(sparrows)
# GGally::ggscatmat(sparrows)  # faster, but excludes factors

sparrows %>% 
  count(surv, sex, sort = TRUE)

skimr::skim(sparrows)

mod <- glm(surv ~., family = "binomial", data = sparrows) %>% 
  broom::tidy(conf.int = TRUE)

# coefficient plot
mod %>% 
  mutate(term = fct_reorder(term, desc(estimate))) %>% 
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high))


require(tidymodels)

spl <- initial_split(sparrows, strata = "surv")
train_df <- training(spl)
test_df <- testing(spl)

spar_rec <- recipe(surv ~., data = sparrows) %>% 
  update_role(id, new_role = "id") %>% 
  step_normalize(all_numeric())

# prep recipe
prepped <- prep(spar_rec)

# see transformed data
head(juice(prepped))

log_spec <- logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

log_wf <- workflow() %>% 
  add_recipe(spar_rec) %>% 
  add_model(log_spec)

set.seed(9876)

# create grid of tuning parameter values
lambda_grid <- grid_regular(
  penalty(),
  levels = 30
)

# create bootstrap resamples with stratification
boots <- bootstraps(train_df, strata = "surv")

doParallel::registerDoParallel()

log_grid <- tune_grid(
  log_wf,
  resamples = boots,
  grid = lambda_grid,
  metrics = metric_set(roc_auc, ppv, npv)
)

# visualize
log_grid %>% 
  collect_metrics()

log_grid %>% 
  collect_metrics() %>% 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  facet_wrap(~.metric) +
  scale_x_log10()


# choose best model
best_auc <- log_grid %>% 
  select_best("roc_auc")

best_auc

final_log <- finalize_workflow(log_wf, best_auc)

final_log

# look at variable importance
require(vip)

fit_vars <- final_log %>% 
  fit(train_df) %>% 
  pull_workflow_fit() %>% 
  vi(lambda = best_auc$penalty)

fit_vars %>% 
  mutate(Importance = abs(Importance),
         Variable = fct_reorder(Variable, Importance)) %>% 
  ggplot(aes(Importance, Variable, fill = Sign)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Sign, scales = "free_y")


# fit to train_df and evaluate
final_log %>% 
  fit(test_df) %>% 
  pull_workflow_fit() %>% 
  vi(lambda = best_auc$penalty) %>% 
  mutate(Importance = abs(Importance),
         Variable = fct_reorder(Variable, Importance)) %>% 
  ggplot(aes(Importance, Variable, fill = Sign)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Sign, scales = "free_y")

final <- last_fit(final_log, spl)

final %>% 
  collect_metrics()

final %>% 
  collect_predictions()

final %>% 
  collect_predictions() %>% 
  conf_mat(surv, .pred_class)

