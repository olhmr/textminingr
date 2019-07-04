### Chapter 8

library(jsonlite)
metadata <- fromJSON("https://data.nasa.gov/data.json")
names(metadata$dataset)
# This dataset has changed from when the book was written, so the instructions
# are no longer valid. The below is an attempt to follow the general principle,
# but it won't match the results exactly.

class(metadata$dataset$title)
class(metadata$dataset$description)
class(metadata$dataset$keyword)

library(dplyr)
nasa_title <- tibble(id = metadata$dataset$identifier, # The version in the text is not working
                     title = metadata$dataset$title)
nasa_title

nasa_desc <- tibble(id = metadata$dataset$identifier,
                    desc = metadata$dataset$description)
nasa_desc %>%
  select(desc) %>%
  sample_n(5)

library(tidyr)
nasa_keyword <- tibble(id = metadata$dataset$identifier,
                       keyword = metadata$dataset$keyword) %>% 
  unnest(keyword)
nasa_keyword

library(tidytext)
nasa_title <- nasa_title %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words)
nasa_desc <- nasa_desc %>%
  unnest_tokens(word, desc) %>%
  anti_join(stop_words)

nasa_title
nasa_desc

nasa_title %>%
  count(word, sort = TRUE)
# as.data.table(nasa_title)[, .(count = .N), by = .(word)][order(-count), ]

my_stopwords <- tibble(word = c(as.character(1:10),
                                "v1", "v03", "l2", "l3", "l4", "v5.2.0",
                                "v003", "v004", "v006", "v7", "ii", "v1.0",
                                "0.5", "0.667", "v001"))
nasa_title <- nasa_title %>%
  anti_join(my_stopwords)
nasa_desc <- nasa_desc %>%
  anti_join(my_stopwords)

nasa_keyword %>%
  group_by(keyword) %>%
  count(sort = TRUE)

nasa_keyword <- nasa_keyword %>%
  mutate(keyword = tolower(keyword))

library(widyr)
title_word_pairs <- nasa_title %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE)
title_word_pairs

desc_word_pairs <- nasa_desc %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE)
desc_word_pairs

library(ggplot2)
library(igraph)
library(ggraph)

set.seed(1234)
title_word_pairs %>%
  filter(n >= 150) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") + 
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) + 
  theme_void()
# Lots of talk of phases in the titles

set.seed(1234)
desc_word_pairs %>%
  filter(n >= 1500) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) + 
  theme_void()

keyword_pairs <- nasa_keyword %>%
  pairwise_count(keyword, id, sort = TRUE, upper = FALSE) %>%
  filter(item1 != "ngda") %>% # Just an acronym for National Geospatial Data Asset
  filter(item2 != "ngda")
keyword_pairs

set.seed(1234)
keyword_pairs %>%
  filter(n >= 300) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") + 
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) + 
  theme_void()
# National Geospatial Data Asset is pretty much congruent with Earth Science.
# Centers and laboratories are referred to as completed.

keyword_cors <- nasa_keyword %>%
  filter(!keyword %in% c("ngda")) %>%
  group_by(keyword) %>%
  filter(n() >= 50) %>%
  pairwise_cor(keyword, id, sort = TRUE, upper = FALSE)
keyword_cors

set.seed(1234)
keyword_cors %>%
  filter(correlation > 0.6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  theme_void()
