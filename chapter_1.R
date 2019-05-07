### Chapter 1
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

library(dplyr)
text_df <- tibble(line = 1:4, text = text)

library(tidytext)
text_df %>%
  unnest_tokens(word, text)

library(janeaustenr)
library(dplyr)
library(stringr)
original_books <- austen_books() %>% # Get the book data
  group_by(book) %>% # Group it for mutate to work for each group independently
  mutate(linenumber = row_number(), # Add line number for each book
         chapter = cumsum(str_detect(string = text, 
                                     pattern = regex("^chapter [\\divxlc]", # Match "chapter" followed by space and a digit, i, v, x, l, or c
                                                     ignore_case = TRUE)))) %>% # The regex ensures str_detect uses regex to match 
  ungroup() # Groups no longer needed

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

data(stop_words) # Contains known stop words in English
tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE)

library(ggplot2)
tidy_books %>% 
  count(word, sort = TRUE) %>% # Count word occurences
  filter(n > 600) %>% # Only show those with more than 600 occurences
  mutate(word = reorder(word, n)) %>% # Order words by occurences
  ggplot(aes(x = word, y = n)) + # Plot data
  geom_col() + # Columns
  xlab(NULL) + # No label on x-axis
  coord_flip() # Flip axes

library(gutenbergr)
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>% 
  count(word, sort = TRUE)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)

# "time" seems to crop up - top 2 words for Bronte, Wells, and Austen

library(tidyr)
frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>% # There are weird leading and trailing underscores in the data: this skips that, but keeps apostrophes
  count(author, word) %>% # Count words by author
  group_by(author) %>% # Group by author
  mutate(proportion = n / sum(n)) %>% # Calculate proportion by author
  select(-n) %>% # Remove unnecessary absolute count
  spread(author, proportion) %>% # Move authors to columns with associated proportion for each word in row
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`) # Gather up Brontë Sisters and H.G. Wells again to enable comparison of them to Jane Austen

library(scales)
ggplot(frequency, aes(x = proportion, # Proportion of usage in Brontë / Wells
                      y = `Jane Austen`, # Proportion of usage in Austen
                      color = abs(`Jane Austen` - proportion))) + # Colour according to difference
  geom_abline(color = "gray40", lty = 2) + # Line with slope 1 to show skew
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + # Avoid overlap with jitter
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + # Skip overlapping items
  scale_x_log10(labels = percent_format()) + # Log-scale to make it easier to read
  scale_y_log10(labels = percent_format()) + # Log-scale to make it easier to read
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") + # Define colour gradient
  facet_wrap(~author, ncol = 2) + # Split the Brontë and Wells plots
  theme(legend.position = "none") + # Remove legend
  labs(y = "Jane Austen", x = NULL) # Add labels

cor.test(data = frequency[frequency$author == "Brontë Sisters", ], 
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells", ], 
         ~ proportion + `Jane Austen`)
