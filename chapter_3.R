### Chapter 3

library(dplyr)
library(janeaustenr)
library(tidytext)
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)
total_words <- book_words %>%
  group_by(book) %>%
  summarise(total = sum(n))
book_words <- left_join(book_words, total_words)
book_words

library(ggplot2)
ggplot(book_words, aes(x = n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) + 
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")
# Most words are on the rarer side, with a long tail of extremely common words.

