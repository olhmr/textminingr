### Chapter 8

library(jsonlite)
metadata <- fromJSON("https://data.nasa.gov/data.json")
names(metadata$dataset)

class(metadata$dataset$title)
class(metadata$dataset$description)
class(metadata$dataset$keyword)

library(dplyr)
nasa_title <- tibble(id = metadata$dataset$identifier, # The version in the text is not working
                     title = metadata$dataset$title)
nasa_title

nasa_desc <- tibble(id = metadata$dataset$identifier,
                    desc = metadata$dataset$description)
nasa_desc %>%
  select(desc) %>%
  sample_n(5)

library(tidyr)
nasa_keyword <- tibble(id = metadata$dataset$identifier,
                       keyword = metadata$dataset$keyword) %>% 
  unnest(keyword)
nasa_keyword

library(tidytext)
nasa_title <- nasa_title %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words)
nasa_desc <- nasa_desc %>%
  unnest_tokens(word, desc) %>%
  anti_join(stop_words)

nasa_title
nasa_desc