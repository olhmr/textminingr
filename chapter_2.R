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
