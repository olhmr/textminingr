### Chapter 5

library(tm)
data("AssociatedPress", package = "topicmodels")
AssociatedPress

terms <- Terms(AssociatedPress)
head(terms)

library(dplyr)
library(tidytext)
ap_td <- tidy(AssociatedPress)
ap_td

ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))
ap_sentiments

library(ggplot2)
ap_sentiments %>%
  count(sentiment, term, wt = count) %>% # Sums the count variable for each sentiment and term; i.e., aggregating documents
  ungroup() %>%
  filter(n >= 200) %>% 
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(x = term, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)
inaug_dfm

inaug_td <- tidy(inaug_dfm)
inaug_td

inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))
inaug_tf_idf

library(tidyr)
year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>% # Match any number of digits
  complete(year, term, fill = list(count = 0)) %>% # Converts missing to 0
  group_by(year) %>%
  mutate(year_total = sum(count))

year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(x = year, y = count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")

ap_td %>%
  cast_dtm(document, term, count)

ap_td %>%
  cast_dfm(document, term, count)

library(Matrix)
m <- ap_td %>%
  cast_sparse(document, term, count)
class(m)

dim(m)

library(janeaustenr)
austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)
austen_dtm

data("acq")
acq
acq[[1]]

acq_td <- tidy(acq)
acq_td

acq_tokens <- acq_td %>%
  select(-places) %>% # Unclear why
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

acq_tokens %>%
  count(word, sort = TRUE)

acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

library(tm.plugin.webmining) # Fails because missing java
library(purrr)

company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", "YHOO", "NFLX")
download_articles <- function(symbol) {
  WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
}
stock_articles <- tibble(company = company, 
                         symbol = symbol) %>%
  mutate(corpus = map(symbol, download_articles)) 
# Apply function download_articles to each symbol, producing a list that gets
# stored in corpus

stock_articles
stock_tokens <- stock_articles %>%
  unnest(map(corpus, tidy)) %>%
  unnest_tokens(word, text) %>%
  select(company, datetimestamp, word, id, heading) 
stock_tokens

library(stringr)
stock_tf_idf <- stock_tokens %>%
  count(company, word) %>%
  filter(!str_detect(word, "\\d+")) %>%
  bind_tf_idf(word, company, n) %>%
  arrange(-tf_idf)

stock_tokens %>%
  anti_join(stop_words, by = "word") %>%
  count(word, id, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarise(contribution = sum(n * score)) %>%
  top_n(12, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(x = word, y = contribution)) +
  geom_col() +
  coord_flip() +
  labs(y = "Frequency of word * AFINN score")

stock_tokens %>%
  count(word) %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(sentiment) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free") +
  ylab("Frequency of this word in the recent financial articles")

stock_sentiment_count <- stock_tokens %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  count(sentiment, company) %>%
  spread(sentiment, n, fill = 0)
stock_sentiment_count

stock_sentiment_count %>%
  mutate(score = (positive - negative) / (positive + negative)) %>% # Relative difference of positive and negative words
  mutate(company = reorder(company, score)) %>%
  ggplot(aes(x = company, y = score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Company",
       y = "Positivity score among 20 recent news articles")

