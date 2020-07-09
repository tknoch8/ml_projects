require(tidyverse)
require(tidymodels)

sparrows <- rio::import("https://raw.githubusercontent.com/StirlingCodingClub/Manuscripts_in_Rmarkdown/master/data/Bumpus_data.csv") %>% 
  mutate(surv = factor(surv),
         sex = factor(sex)) %>% 
  mutate(id = row_number()) %>% 
  mutate(id = paste0(sex, "_", surv, "_", id)) %>% 
  mutate(sex = if_else(sex == "male", 1, 0)) %>% 
  mutate(surv = if_else(surv == "alive", 1, 0))

pca_rec <- recipe(~., data = sparrows) %>% 
  update_role(id, surv, sex, new_role = "id") %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)

# how many PCs to keep?
# maybe just 2?
summ <- helpRs::get_pca_summary(pca_prep)

require(broom)

tidy_pca <- tidy(pca_prep, 2)

tidy_pca %>% 
  filter(component %in% paste0("PC", 1:5)) %>% 
  mutate(component = fct_inorder(component)) %>% 
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

require(tidytext)

tidy_pca %>% 
  filter(component %in% paste0("PC", 1:4)) %>% 
  group_by(component) %>% 
  top_n(8, abs(value)) %>% 
  ungroup() %>% 
  mutate(terms = tidytext::reorder_within(terms, abs(value), component)) %>% 
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  tidytext::scale_y_reordered() +
  facet_wrap(~component, nrow = 1, scales = "free_y") +
  labs(y = NULL, fill = "Positive?")

juice(pca_prep) %>% 
  ggplot(aes(PC1, PC2, label = id)) +
  geom_point(aes(color = surv), size = 2) +
  # geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = NULL)
  # scale_color_manual(aes(color), values = c("blue", "red"))

juice(pca_prep) %>% 
  ggplot(aes(PC1, 0, label = id)) +
  geom_point(aes(color = surv), size = 2) +
  # geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = NULL)
# scale_color_manual(aes(color), values = c("blue", "red"))


glm(
  juice(pca_prep)$sex ~ juice(pca_prep)$PC1, family = "binomial"
)

sparrows %>% 
  mutate(surv = factor(surv)) %>% 
  ggplot(aes(surv, femur)) +
  geom_boxplot()

alive_femurs <- sparrows %>% 
  filter(surv == "1") %>% 
  pull(femur)

dead_femurs <- sparrows %>% 
  filter(surv == "0") %>% 
  pull(femur)

t.test(alive_femurs, dead_femurs, alternative = "two.sided", var.equal = TRUE)

