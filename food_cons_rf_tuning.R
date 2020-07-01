#https://r-tastic.co.uk/post/from-messy-to-tidy/
# need to scrape data from html table

require(rvest)
require(tidyverse)

url <- "https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018"

# scrape
url_html <- read_html(url)

####---- get table ----####
whole_table <- url_html %>% 
  html_nodes('table') %>% 
  html_table(fill = TRUE) %>% 
  .[[1]]

str(whole_table)

# remove unneeded column and rows
table_content <- whole_table %>% 
  select(-X1) %>% 
  filter(!row_number() %in% 1:3)

# When inspecting the HTML code behind the table
# , you will notice that all the “icon headers” 
# belong to thead-icon class that has an attribute 
# title with the very column names that we’re interested in.

####---- get all headers ----####
raw_headers <- url_html %>%
  #  column names are in .thead-icon attribute
  html_nodes(".thead-icon") %>%
  html_attr('title')

# we selected a few extra titles, get the bottom most which are
# the actual column names
tidy_bottom_header <- raw_headers[28:length(raw_headers)]

# based on column names in table, we want "Pork":"Nuts inc. Peanut Butter"
raw_middle_header <- raw_headers[17:27]

# organise them in a way that the column names reflect the variable order
tidy_headers <- c(
  rep(raw_middle_header[1:7], each = 2),
  "animal_total",
  rep(raw_middle_header[8:length(raw_middle_header)], each = 2),
  "non_animal_total",
  "country_total")

# we want to retain the information about both, the type of food 
# that the value refers too, as well as the metric that it describes 
# (consumption or CO2 emmissions). Let’s combine the two and make 
# them column names:
combined_colnames <- paste(tidy_headers, tidy_bottom_header, sep = ';')

colnames(table_content) <- c("Country", combined_colnames)


long_table <- table_content %>%
  # make column names observations of Category variable
  tidyr::pivot_longer(cols = -Country, names_to = "Category", values_to = "Values") %>%
  # separate food-related information from the metric
  tidyr::separate(col = Category, into = c("Food Category", "Metric"), sep = ';')

# Metric variable that ideally should be split into two separate columns for an 
# easier comparison of consumption and CO2 emissions between the countries:
tidy_table <- long_table %>%
  # names of new columns from Metric, corresponding values are in Values
  tidyr::pivot_wider(names_from = Metric, values_from = Values) %>% 
  janitor::clean_names()

# rename and get rid of observations with total in food_category.
# they can be easily calculated later
food_consumption <- tidy_table %>%
  rename(consumption = 3,
         co2_emmission = 4) %>%
  filter(!stringr::str_detect(food_category, "total"))

####---- modeling ----####

# https://juliasilge.com/blog/food-hyperparameter-tune/

require(countrycode)

?codelist

# add continent column
food <- food_consumption %>% 
  mutate(continent = countrycode(country,
                                 origin = "country.name",
                                 destination = "continent")) %>% 
  select(-co2_emmission) %>% 
  # convert to wide data, one row per country
  pivot_wider(names_from = food_category,
              values_from = consumption) %>% 
  janitor::clean_names() %>% 
  mutate(asia = case_when(
    continent == "Asia" ~ "Asia",
    TRUE                ~ "Other"
  )) %>% 
  select(-c(country, continent)) %>% 
  mutate_at(vars(everything(), -asia), ~as.numeric(.)) %>% 
  mutate_if(is.character, ~as.factor(.))

require(GGally)
# for only numeric values, color by asia
ggscatmat(food, columns = 1:11, color = "asia")

## Tune hyperparameters

require(tidymodels)
set.seed(1234)

food_split <- initial_split(food, strata = asia)
train_df <- training(food_split)
test_df <- testing(food_split)

food_rec <- recipe(asia ~., data = train_df) %>% 
  step_zv(all_numeric(), -all_outcomes()) %>% 
  step_normalize(all_numeric(), -all_outcomes())

food_prep <- food_rec %>% 
  prep()


# create workflow object
wf <- workflow() %>% 
  add_recipe(food_rec)

# make bootstrap (with replacement) resamples
food_boot <- bootstraps(food, times = 30)
food_boot
# /numbers go into asessment data (didn't make it into the sample)

# hyperparameters: parameter you can't learn while training the model
# mtry: number of predictors randomly sampled at each split
# trees: numberof trees, doesn't make huge impact, just make sure there's emough
# min_n: number of data points in node before further splitting
rf_spec <- rand_forest(mode = "classification", # asia or not
                       mtry = tune(),
                       trees = 1000,
                       min_n = tune()) %>% 
  set_engine("ranger")

doParallel::registerDoParallel()

# try whole bunch (grid) of hyperparameters
# fit on every bootstrap and eval on asessment data
rf_grid <- tune_grid(
  wf %>% add_model(rf_spec), 
  resamples = food_boot
)

# accuracy and area under the curve
rf_grid %>% 
  collect_metrics()

# plot mean accuracy by mtry and min_n
mtry_grid_plot <- rf_grid %>% 
  collect_metrics() %>% 
  ggplot(aes(mtry, mean, color = .metric)) +
  geom_errorbar(aes(ymin = mean - std_err,
                    ymax = mean + std_err),
                alpha = 0.5) +
  geom_line(size = 1.5)

min_n_grid_plot <- rf_grid %>% 
  collect_metrics() %>% 
  ggplot(aes(min_n, mean, color = .metric)) +
  geom_errorbar(aes(ymin = mean - std_err,
                    ymax = mean + std_err),
                alpha = 0.5) +
  geom_line(size = 1.5)

require(patchwork)

mtry_grid_plot / min_n_grid_plot

# show 'best' hyperparameter specifications and compare to graphs
rf_grid %>% 
  show_best("roc_auc", maximize = FALSE)

# choose 'best' hyperparameters
best_hyper <- rf_grid %>% 
  show_best("roc_auc", maximize = FALSE) %>% 
  slice(5)

# best specification
best_spec <- rand_forest(mode = "classification", # asia or not
                         mtry = 5,
                         trees = 1000,
                         min_n = 26) %>% 
  set_engine("ranger")

# finalize workflow with model specification and chosen hypers
final_rf <- finalize_workflow(wf %>% 
                                add_model(best_spec), 
                              best_hyper)

# fit final model on original test data
final_fit <- final_rf %>% 
  fit(test_df)

# get predicted classes
predictions <- predict(final_fit, test_df)

# see predictions on test df
test_df %>% 
  cbind(predictions) %>% 
  select(asia, .pred_class, everything())



















