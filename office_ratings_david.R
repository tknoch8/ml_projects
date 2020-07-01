require(tidyverse)
require(schrute)

theme_set(theme_light())

transcripts <- as_tibble(theoffice) %>% 
  mutate(season = as.integer(season),
         episode = as.integer(episode)) %>% 
  mutate(character = str_remove_all(character, '"')) %>% 
  mutate(name = str_to_lower(str_remove_all(episode_name, "\\.| \\(Part.*")))

office_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv") %>% 
  mutate(name = str_to_lower(str_remove_all(title, "\\.| \\(Part.*|\\: Part.*")))

office_ratings %>% 
  group_by(season) %>% 
  summarize(avg_rating = mean(imdb_rating)) %>% 
  ggplot(aes(season, avg_rating)) +
  geom_line() +
  scale_x_continuous(breaks = 1:9)

office_ratings %>% 
  mutate(title = fct_inorder(title),
         episode_number = row_number()) %>% 
  ggplot(aes(episode_number, imdb_rating)) +
  geom_line(group = 1) +
  geom_smooth(group = 1) +
  geom_point(aes(color = factor(season), size = total_votes)) +
  geom_text(aes(label = title), check_overlap = TRUE, hjust = 1) +
  expand_limits(x = -10) + # to see all titles
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  labs(x = "Episode number",
       y = "IMDB Rating",
       title = "Popularity of The Office episodes over time",
       subtitle = "Color represents season, size represents number of ratings")


office_ratings %>% 
  arrange(desc(imdb_rating)) %>% 
  mutate(title = paste0(season, ". ", episode, " ", title),
         title = fct_reorder(title, imdb_rating)) %>% 
  head(20) %>% 
  ggplot(aes(title, imdb_rating, color = factor(season), size = total_votes)) +
  geom_point() +
  coord_flip() +
  labs(color = "Season",
       title = "Most popular episodes of The Office")

# Transcripts

require(tidytext)

blacklist <- c("yeah", "hey", "uh", "gonna")
blacklist_characters <- c("Everyone", "All", "Both", "Guy", "Girl", "Group")

transcript_words <- transcripts %>% 
  group_by(character) %>% 
  filter(n() >= 30,
         n_distinct(episode_name) > 2) %>% 
  ungroup() %>% 
  select(-text_w_direction) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(tidytext::stop_words, by = "word") %>% 
  filter(!word %in% blacklist,
         !character %in% blacklist_characters)

transcript_words %>% 
  count(character, sort = TRUE)

character_tf_idf <- transcript_words %>% 
  add_count(word) %>% 
  filter(n >= 20) %>% 
  count(word, character) %>% 
  bind_tf_idf(word, character, n) %>% 
  arrange(desc(tf_idf))

character_tf_idf %>% 
  filter(character %in% c("Dwight", "Jim", "Michael", "Darryl", "Jan", "Holly")) %>% 
  group_by(character) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>% 
  mutate(word = reorder_within(word, tf_idf, character)) %>% 
  head(20) %>% 
  ggplot(aes(word, tf_idf)) +
  geom_col() +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ character, scales = "free_y") +
  labs(x = "",
       y = "TF-IDF of character-word pairs")

# combine datasets

# machine learning - what affects popularity of an episode:
# * season/time
# * director
# * writer
# * lines per character

#### fucked up ### 45:20 https://www.youtube.com/watch?v=_IvAubTDQME
# season episode pairings
ratings_summarized <- office_ratings %>% 
  group_by(name) %>% 
  summarize(imdb_rating = mean(imdb_rating))

character_lines_ratings <- transcripts %>%  
  count(character, name) %>% 
  group_by(character) %>% 
  filter(sum(n) >= 100) %>% 
  inner_join(ratings_summarized, by = "name") %>% 
  summarize(avg_rating = mean(imdb_rating),
            nb_episodes = n()) %>% 
  arrange(desc(avg_rating))
  


