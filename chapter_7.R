### Chapter 7
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
tweets_julia <- read_csv("data/tweets_julia.csv")
tweets_david <- read_csv("data/tweets_dave.csv")
tweets <- bind_rows(tweets_julia %>%
                      mutate(person = "Julia"),
                    tweets_david %>%
                      mutate(person = "David")) %>%
  mutate(timestamp = ymd_hms(timestamp))
ggplot(data = tweets, aes(x = timestamp, fill = person)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~person, ncol = 1)

library(tidytext)
library(stringr)
remove_reg <- "&amp;|&lt;|&gt;" # I guess this catches formatting artifacts
tidy_tweets <- tweets %>%
  filter(!str_detect(text, "^RT")) %>% # Remove re-tweets
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))
# This doesn't remove links, though

frequency <- tidy_tweets %>%
  group_by(person) %>%
  count(word, sort = TRUE) %>%
  left_join(tidy_tweets %>%
              group_by(person) %>%
              summarise(total = n())) %>%
  mutate(freq = n / total)
frequency

library(tidyr)
frequency <- frequency %>%
  select(person, word, freq) %>%
  spread(person, freq) %>%
  arrange(Julia, David)
frequency

library(scales)
ggplot(data = frequency, aes(x = Julia, y = David)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

tidy_tweets <- tidy_tweets %>% 
  filter(timestamp >= as.Date("2016-01-01"),
         timestamp < as.Date("2017-01-01"))

word_ratios <- tidy_tweets %>%
  filter(!grepl(pattern = "^@", x = word)) %>%
  count(word, person) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(person, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(David / Julia)) %>%
  arrange(desc(logratio))

word_ratios %>% 
  arrange(abs(logratio))

word_ratios %>%
  group_by(logratio < 0) %>% # Group by positive versus negative ratio (with log cases where Julia > David will be negative)
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(x = word, y = logratio, fill = logratio < 0)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (David / Julia)") +
  scale_fill_discrete(name = "", labels = c("David", "Julia"))

words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(timestamp, unit = "1 month")) %>%
  count(time_floor, person, word) %>%
  group_by(person, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(person, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 30)
words_by_time

nested_data <- words_by_time %>%
  nest(-word, -person)
nested_data

library(purrr)
nested_models <- nested_data %>%
  mutate(models = map(data, ~glm(cbind(count, time_total) ~ time_floor, .,
                                 family = "binomial")))
# https://stackoverflow.com/questions/9111628/logistic-regression-cbind-command-in-glm
# count = successes, time_total = trials; essentially computing probability of word showing
# up in a given time period based on the number of times it showed up there and the number
# of times it showed up overall

nested_models

library(broom)
slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>%
  filter(adjusted.p.value < 0.05)
top_slopes

words_by_time %>%
  inner_join(top_slopes, by = c("word", "person")) %>% # Leaves only top slopes
  filter(person == "David") %>%
  ggplot(aes(x = time_floor, y = count / time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

words_by_time %>%
  inner_join(top_slopes, by = c("word", "person")) %>%
  filter(person == "Julia") %>%
  ggplot(aes(x = time_floor, y = count / time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

tweets_julia <- read_csv("data/juliasilge_tweets.csv")
tweets_david <- read_csv("data/drob_tweets.csv")
tweets <- bind_rows(tweets_julia %>%
                      mutate(person = "Julia"),
                    tweets_david %>%
                      mutate(person = "David")) %>%
  mutate(created_at = ymd_hms(created_at))

tidy_tweets <- tweets %>%
  filter(!str_detect(text, "^(RT|@)")) %>% # Remove re-tweets and @ mentions
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"))
tidy_tweets

totals <- tidy_tweets %>%
  group_by(person, id) %>%
  summarise(rts = first(retweets)) %>%
  group_by(person) %>%
  summarise(total_rts = sum(rts))
totals

word_by_rts <- tidy_tweets %>%
  group_by(id, word, person) %>%
  summarise(rts = first(retweets)) %>%
  # Count number of times tweet-word-person combination is retweeted
  group_by(person, word) %>%
  summarise(retweets = median(rts), uses = n()) %>%
  # Median number each person gets each word retweeted + number of times each word was used by each person
  left_join(totals) %>%
  filter(retweets != 0) %>%
  ungroup()
word_by_rts %>%
  filter(uses >= 5) %>%
  arrange(desc(retweets))
