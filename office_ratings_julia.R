require(tidyverse)
require(schrute)

# https://www.youtube.com/watch?time_continue=14&v=R32AsuKICAY&feature=emb_logo

theme_set(theme_light())

ratings_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv") 

schrute::theoffice

# seasons and episodes not counted the same way

remove_regex <- "[:punct:]|[:digit:]|parts |part |the |land"

office_ratings <- ratings_raw %>% 
  transmute(episode_name = str_to_lower(title),
            episode_name = str_remove_all(episode_name, remove_regex),
            episode_name = str_trim(episode_name),
            imdb_rating)

office_info <- schrute::theoffice %>% 
  mutate(seaosn = as.numeric(season),
         episode = as.numeric(episode),
         episode_name = str_to_lower(episode_name),
         episode_name = str_remove_all(episode_name, remove_regex),
         episode_name = str_trim(episode_name)) %>% 
  select(season, episode, episode_name, director, writer, character)

# episodes not captured by name cleaning
office_ratings %>% 
  distinct(episode_name) %>% 
  anti_join(office_info %>% 
              distinct(episode_name))

office_info %>% 
  distinct(episode_name) %>% 
  anti_join(office_ratings %>% 
              distinct(episode_name))


# build dataset for modeling
characters <- office_info %>% 
  count(episode_name, character) %>% 
  add_count(character, wt = n, name = "character_count") %>% 
  filter(character_count > 800) %>% 
  select(-character_count) %>% 
  pivot_wider(names_from = character,
              values_from = n,
              values_fill = list(n = 0)) # fill empties with 0

creators <- office_info %>% 
  distinct(episode_name, director, writer) %>% 
  pivot_longer(director:writer, names_to = "role", values_to = "person") %>% 
  separate_rows(person, sep = ";") %>% 
  add_count(person) %>% 
  filter(n > 10) %>% 
  distinct(episode_name, person) %>% 
  mutate(person_value = 1) %>% # place holder for pivot_wider
  # column for each person, row for each episode
  pivot_wider(names_from = person,
              values_from = person_value,
              values_fill = list(person_value = 0))

office <- office_info %>% 
  distinct(season, episode, episode_name) %>% 
  inner_join(characters, by = "episode_name") %>% 
  inner_join(creators, by = "episode_name") %>% 
  inner_join(office_ratings, by = "episode_name") %>% 
  janitor::clean_names()


office %>% 
  ggplot(aes(season, imdb_rating, fill = as.factor(season))) +
  geom_boxplot(show.legend = FALSE)

office %>% 
  ggplot(aes(episode, imdb_rating, fill = as.factor(episode))) +
  geom_boxplot(show.legend = FALSE)

# train a model

require(tidymodels)

office_split <- initial_split(office, strata = season)
office_train <- training(office_split)
office_test <- testing(office_split)

office_rec <- recipe(imdb_rating ~., data = office_train) %>% 
  update_role(episode_name, new_role = "ID") %>% # dont use episode name to predict 
  step_zv(all_numeric(), -all_outcomes()) %>% # remove zero variance predictors
  step_normalize(all_numeric(), -all_outcomes())

office_prep <- office_rec %>% 
  prep(strings_as_factors = FALSE) # dont want id string var to be a factor

lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>% 
  set_engine("glmnet")

wf <- workflow() %>% 
  add_recipe(office_rec)

lasso_fit <- wf %>% 
  add_model(lasso_spec) %>% 
  fit(data = office_train)

lasso_fit %>% 
  pull_workflow_fit() %>% 
  tidy()


# tune lasso parameters
set.seed(1234)
office_boot <- bootstraps(office_train, strata = season)

# use same workflow

tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

lambda_grid <- grid_regular(penalty(),
                            levels = 50)

doParallel::registerDoParallel()

set.seed(2020)

# 32:21
lasso_grid <- tune_grid(
  wf %>% add_model(tune_spec),
  resamples = office_boot,
  grid = lambda_grid
)

lasso_grid %>% 
  pull(.notes)

lasso_grid %>% 
  collect_metrics() %>% 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(ymin = mean - std_err,
                    yamx = mean + std_err),
                alpha = 0.5) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")

# model selection
lowest_rmse <- lasso_grid %>% 
  select_best("rmse", maximize = FALSE) # get penalty value at lowest rmse

final_lasso <- finalize_workflow(wf %>% add_model(tune_spec),
                                 lowest_rmse)

require(vip)

final_lasso %>% 
  fit(office_train) %>% 
  pull_workflow_fit() %>% 
  vi(lambda = lowest_rmse$penalty) %>% 
  mutate(Importance = abs(Importance),
         Variable = fct_reorder(Variable, Importance)) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = sign)) +
  geom_col() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = NULL)


# final model split
last_fit(final_lasso,
         office_split) %>% 
  collect_metrics()
  





