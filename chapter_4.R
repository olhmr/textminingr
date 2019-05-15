### Chapter 4

library(dplyr)
library(tidytext)
library(janeaustenr)
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
austen_bigrams
# The ngram is bidirectional / overlapping: e.g. for "sense and sensibility" it
# extracts "sense and" as well as "and sensibility".

austen_bigrams %>%
  count(bigram, sort = TRUE)

library(tidyr)
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts
# When we remove the stop words, we're left with titles at the top. Converting
# to lowercase might obscure analysis here: we could use capitalisation to
# determine proper nouns and hence influence the sentiment of words such as
# "miss". If "miss" is before a proper noun, it should have no sentiment - in
# fact, it should probably be treated as a stop word in those cases.

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united 

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)
# This is useful, but you really need to analyse the whole sentence to extract
# complex meaning. Trigrams of "it's not like I am happy" will include "I am
# happy".

AFINN <- get_sentiments("afinn")
AFINN

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  # Named vector is dplyr's way of joining by different variables
  # This matches word2 in bigrams_separated with word in AFINN
  count(word2, score, sort = TRUE)
not_words

library(ggplot2)
not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>% # Descending absolute contribution
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>% # Arrange call before looked at absolute contribution
  # This orders from positive to negative
  # ggplot(aes(x = word2, y = n * score, fill = n * score > 0)) + # Why not use contribution?
  ggplot(aes(x = word2, y = contribution, fill = contribution > 0)) + # Yep, same result
  geom_col(show.legend = FALSE) + 
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurences") + 
  coord_flip()

negation_words <- c("not", "no", "never", "without")
negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE)

library(igraph)
bigram_counts
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph

library(ggraph)
set.seed(2017)
ggraph(bigram_graph, layout = "fr") + # Unclear from help page what layout "fr" is
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(0.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(0.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
  theme_void()

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}
visualise_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(0.15, "inches"))
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

library(gutenbergr)
kjv <- gutenberg_download(10)
library(stringr)
kjv_bigrams <- kjv %>%
  count_bigrams()
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualise_bigrams()

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>% # Only look at Pride & Prejudice
  mutate(section = row_number() %/% 10) %>% # Divide into 10-row sections
  filter(section > 0) %>% # Skip first section
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)
austen_section_words

library(widyr)
word_pairs <- austen_section_words %>%
  pairwise_count(item = word, feature = section, sort = TRUE) 
# For each possible pair of words, count the number of sections which those
# words co-occur
word_pairs

word_pairs %>%
  filter(item1 == "darcy")
