require(tidyverse)
require(tidytuesdayR)

dat <- tt_load("2020-06-30")

characters <- dat$characters

characters %>% 
  count(rendered_unconcious, sort = TRUE)

# predict whether or not there was at least one 'rendered_unconscious'
# huge class imbalance

by_issue <- characters %>% 
  group_by(issue) %>% 
  summarize(num_uncs = sum(rendered_unconcious)) %>% 
  mutate(any_unc = if_else(num_uncs >= 1, "yes", "no")) %>% 
  select(issue, any_unc, everything())

d <- characters %>% 
  left_join(by_issue, by = "issue") %>% 
  mutate(any_unc = factor(any_unc))

c <- d %>% 
  select(any_unc, c(sample(1:length(.), 5)))

c %>% 
  filter_all(~!is.na(.)) %>% 
  mutate_all(~factor(.)) %>% 
  glm(any_unc~., family = "binomial", data = .) %>% 
  broom::tidy() -> tidy_coefs

# no significant coefs
tidy_coefs %>% 
  filter(p.value <= .05)

###########--------- tidymodels ---------############
require(tidymodels)

comic_bechdel <- dat$comic_bechdel %>% 
  mutate(writer = fct_lump(writer, 4)) %>% 
  mutate(series = as.factor(series)) %>% 
  select(-artist) %>% 
  mutate(cover_artist = fct_lump(cover_artist, 6)) %>% 
  mutate(pass_bechdel = as.factor(pass_bechdel)) %>% 
  select(-page_number) %>% 
  select(-notes)

pca_rec <- recipe(~., data = comic_bechdel) %>% 
  step_pca(all_predictors())

prepped <- prep(pca_rec)

juice(pca_rec)

#######

dat <- comic_bechdel %>% 
  mutate(pass_bechdel = as.integer(if_else(pass_bechdel == "yes", 1, 0)))

glm(pass_bechdel ~ series + issue + writer + issue:writer, data = dat, family = "binomial") %>% 
  broom::tidy()

#########

comic_bechdel %>% 
  group_by(series) %>% 
  summarize(n_writ = n_distinct(writer))

comic_bechdel %>% 
  count(series, writer, sort = TRUE) %>% 
  ggplot(aes(series, writer, fill = n)) +
  geom_tile()

