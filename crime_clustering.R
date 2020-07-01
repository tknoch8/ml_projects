# https://uc-r.github.io/kmeans_clustering

require(tidyverse)
require(cluster)
require(factoextra)

crime <- rio::import(here::here("data", "crime.csv"))

row.names(crime) <- NULL

# prepare
crime %>% 
  janitor::clean_names() %>% 
  mutate_if(is.numeric, ~scale(.)) %>% 
  column_to_rownames(var = "city") -> dat

distance <- get_dist(dat)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k4 <- kmeans(dat, centers = 4, nstart = 25)
k4$withinss
k4$cluster
k4$tot.withinss/k4$totss
k4$tot.withinss/k4$betweenss

k3 <- kmeans(dat, centers = 3, nstart = 100)
k3$cluster

k3$withinss/k3$totss
k3$tot.withinss/k3$totss
k3$tot.withinss/k3$betweenss

fviz_cluster(k3, data = dat)
fviz_cluster(k4, data = dat)

require(furrr)

clusty <- tibble(k = c(2:6)) %>% 
  mutate(clust = map(k, kmeans, x = dat, nstart = 100)) %>% 
  mutate(viz = map(clust, fviz_cluster, data = dat)) %>% 
  mutate(bet_ss_tot = map_dbl(clust, function(x) x$betweenss / x$totss))
  # mutate(viz = future_map2(.x = viz, .y = bet_ss_tot, .f = ~function(.x, .y) .x + labs(subtitle = paste0(round(.y, 2)))))
  # mutate(viz = map2(viz, bet_ss_tot, function(viz) viz + labs(subtitle = as.character(round(.y, 2)))))

# pain in the ass
for (i in seq_along(clusty)) {
  clusty$viz[[i]] <- clusty$viz[[i]] + labs(subtitle = paste0("prop WSS: ", round(clusty$bet_ss_tot[i], 2), "\n",
                                                              "k: ", clusty$k[i]))
}

require(patchwork)

# view plot of each value of k
clusty$viz[[1]] + clusty$viz[[2]] + clusty$viz[[3]] / clusty$viz[[4]] + clusty$viz[[5]]
### subtitle not showing for k = 6 plot








