### Chapter 2
library(tidytext)
sentiments

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(), 
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy") # Only keep words associated with joy
tidy_books %>% 
  filter(book == "Emma") %>% # Only look at the Emma book
  inner_join(nrc_joy) %>% # Find words associated with joy
  count(word, sort = TRUE) # Count

library(tidyr)
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>% # Only look at words with positive or negative association
  count(book, index = linenumber %/% 80, sentiment) %>% # Divide book into 80-line chunks and count for each - would probably be better to do a rolling window
  spread(sentiment, n, fill = 0) %>% # Spread out sentiments to different columns
  mutate(sentiment = positive - negative) # Calculate difference in count between positive and negative

library(ggplot2)
ggplot(data = jane_austen_sentiment, aes(x = index, y = sentiment, fill = book)) + # Sentiment score across groupings by book
  geom_col(show.legend = FALSE) + # Don't need a legend since we're faceting
  facet_wrap(~book, ncol = 2, scales = "free_x") # Free x to account for different book lengths

pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")
pride_prejudice

afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% # afinn ranks from -5 to 5
  group_by(index = linenumber %/% 80) %>% # Divide inte 80 line chunks
  summarise(sentiment = sum(score)) %>% # Define sentiment as the sum of scores in that chunk
  mutate(method = "AFINN") # Keep track of which method
  
# It feels problematic to only look at matching words, without considering how
# many of the words actually have a match. It's safe if the lexicon can be
# assumed to be mostly exhaustive, but if it isn't, we may be losing a lot of
# context.

bing_and_nrc <- bind_rows(pride_prejudice %>%
                            inner_join(get_sentiments("bing")) %>% # Bing lexicon
                            mutate(method = "Bing et al."), 
                          pride_prejudice %>%
                            inner_join(get_sentiments("nrc") %>% # NRC lexicon
                                         filter(sentiment %in% c("positive",
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>% # Count grouped by method, index, and sentiment
  spread(sentiment, n, fill = 0) %>% # Spread sentiment to columns
  mutate(sentiment = positive - negative) # Calculate difference

# Filtering nrc to only look at positive & negative sentiments initially
# appeared problematic, as it would apparently miss out on a lot of data, but
# after looking at the lexicon it actually has multiple entries for each word,
# so we're not necessarily losing much information.

bind_rows(afinn, bing_and_nrc) %>% 
  ggplot(aes(x = index, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~method, ncol = 1, scales = "free_y")

get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>%
  count(sentiment)

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts
# Clearly miss should not always be a negative word - it's probably mostly used
# in the context of referring to a woman!

bing_word_counts %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

custom_stop_words <- bind_rows(tibble(word = c("miss"), 
                                      lexicon = c("custom")),
                               stop_words)
custom_stop_words

# Below causes freeze for some reason
# library(wordcloud)
# tidy_books %>%
#   anti_join(stop_words) %>%
#   count(word) %>%
#   with(wordcloud(word, n, max_words = 100)) # With evaluates within a new local environment

library(reshape2)
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% # Cast to matrix
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
# Big limitation that the sizes of words are not comparable across sentiments -
# that makes this graph very misleading.

PandP_sentences <- tibble(text = prideprejudice) %>%
  unnest_tokens(sentence, text, token = "sentences")
PandP_sentences$sentence[2]

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>%
  group_by(book) %>%
  summarise(chapters = n()) # Number of rows counted by book: each row is a chapter

bingnegative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")
wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarise(words = n())
tidy_books %>%
  semi_join(bingnegative) %>% # Filter tidy_books to contain only matching in bingnegative, but don't include bingnegative in output
  group_by(book, chapter) %>%
  summarise(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords / words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()
  