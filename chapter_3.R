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

book_words <- book_words %>% 
  bind_tf_idf(word, book, n) # `word` are the tokens, `book` the documents, `n` the count of each token in each document
book_words
# For the extremely common words, idf is near log(1) = 0 

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))
# Proper nouns dominate the list: Austen's language is apparently mostly
# consistent, and what changes between novels are the characters, locations, and
# other names.

book_words %>%
  arrange(desc(tf_idf)) %>% # Order by descending tf_idf
  mutate(word = factor(word, levels = rev(unique(word)))) %>% # Factor tokens by reverse order 
  # If we don't reverse, the highest will be on the bottom after the coordinate flip
  group_by(book) %>%
  top_n(15) %>% # Find top 15 for each book
  ungroup() %>%
  ggplot(aes(x = word, y = tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 5001), 
                              meta_fields = "author") # Download of 5001 (Einstein) failed

physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE)
physics_words

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>% # Not sure this actually does anything
  # The list should already be sorted by arrange call when we created plot_physics
  ggplot(aes(x = word, y = tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") + 
  coord_flip()
# Galilei should probably not have `equall` that high up, since it's just an
# alternate spelling of equal

library(stringr)
physics %>%
  filter(grepl(x = text, pattern = "eq\\.")) %>% # Obviously won't find anything when we failed to download Einstein
  # Why use str_detect from stringr When grepl is in base and works just as
  # well?
  select(text)

physics %>%
  filter(grepl(x = text, pattern = "K1")) %>%
  select(text)

physics %>%
  filter(grepl(pattern = "AK", x = text)) %>%
  select(text)

mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                               "fig", "file", "cg", "cb", "cm"))
physics_words <- physics_words %>%
  anti_join(mystopwords, by = "word")
plot_physics <- physics_words %>% # Same as before
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
ggplot(data = plot_physics, 
       aes(x = word, y = tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()
