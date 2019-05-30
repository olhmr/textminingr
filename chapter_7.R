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
