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

tidy_coefs %>% 
  filter(p.value <= .05)

###### code for repo ml_projects is at https://bitbucket.polarisalpha.com/projects/RP/repos/ml_projects/browse