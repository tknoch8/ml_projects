require(janeaustenr)
require(tidytext)
require(tidyverse)
require(broom)
require(stm)

# unnest tokens
words <- janeaustenr::austen_books() %>% 
  filter(book == "Sense & Sensibility") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word")

# create sparse matrix for topic modeling
word_matrix <- words %>% 
  mutate(id = row_number()) %>% 
  group_by(word) %>% 
  add_count(word) %>% 
  filter(n() >= 20) %>% 
  cast_sparse(id, word, n)

# create topic_model
topic_model <- stm(
  word_matrix,
  K = 6,
  verbose = TRUE,
  init.type = "Spectral"
)

# visualize the top words that define each topic
topic_model %>% 
  tidy() %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  mutate(term = tidytext::reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term)) +
  geom_col() +
  facet_wrap(~topic, scales = "free")
