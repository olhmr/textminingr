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
