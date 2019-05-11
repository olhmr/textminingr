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

freq_by_rank <- book_words %>% 
  group_by(book) %>%
  mutate(rank = row_number(), # The list is sorted by number of occurences, so rank = row number
         `term frequency` = n/total)
freq_by_rank

freq_by_rank %>%
  ggplot(aes(x = rank, y = `term frequency`, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()
# Assuming a inversely proportional relationship, we expect a function of the
# form `frequency = constant / rank` => f = c^(-r)
# Taking the logarithm of both sides yield log(f) = log(c^(-r)) = -rlog(c)
# In other words, log(f) = y = -rlog(c) = -rC, where C is a constant
# Hence we have a linear function with slope -r

rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
# Shows that the 11 to 399 range has a slope of about -1.11

freq_by_rank %>%
  ggplot(aes(x = rank, y = `term frequency`, color = book)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) + # Why not store lm and use the actual model here instaed of approximations?
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() + 
  scale_y_log10()
