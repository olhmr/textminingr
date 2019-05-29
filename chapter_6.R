### Chapter 6

library(topicmodels)
data("AssociatedPress")
AssociatedPress

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta") 
# Beta is the per-topic-per-word probabilities
ap_topics

library(ggplot2)
library(dplyr)
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

library(tidyr)
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds", 
            "Pride and Prejudice", "Great Expectations")
library(gutenbergr)
books <- gutenberg_works(title %in% titles) %>% # Find info on the titles in the database
  gutenberg_download(meta_fields = "title") # Search based on found info

library(stringr)
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter) # Join title and chapter into column document, separating with _ by default
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()
word_counts

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)
chapters_dtm

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

library(ggplot2)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales = "free") +
  coord_flip()

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) # Converts gives us integers in the chapter column
chapters_gamma

chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(x = factor(topic), y = gamma)) + 
  geom_boxplot() + 
  facet_wrap(~title)

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>% # Find the top association for each chapter: i.e., classification
  ungroup()
chapter_classifications

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic) # Gets a list of books and which topics they're associated with

chapter_classifications %>% 
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus) # Find mismatches
# Despite the worrying boxplot, only two chapters from Great Expecations were misclassified

assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))
assignments

assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(x = consensus, y = title, fill = percent)) + 
  geom_tile() + 
  scale_fill_gradient2(high = "red", label = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) + 
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")

wrong_words <- assignments %>%
  filter(title != consensus)
wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

word_counts %>%
  filter(word == "flopson") # Illustrates the risk with LDA being stochastic

library(mallet)
collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>% # Remove stop words
  mutate(word = str_replace(word, "'", "")) %>% # Remove apostrophes from words
  group_by(document) %>%
  summarise(text = paste(word, collapse = " ")) # Paste all words to one string
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)
mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100) # Train with 100 iterations

tidy(mallet_model)
tidy(mallet_model, matrix = "gamma")
term_counts <- rename(word_counts, term = word) # augment requires "term" as column name
augment(mallet_model, term_counts)
