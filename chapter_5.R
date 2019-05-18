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
