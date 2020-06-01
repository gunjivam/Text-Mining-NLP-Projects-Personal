# Answer Key for Jane Austen tf-idf Chapter 3 ----

library(gutenbergr)
library(tidyverse)
library(tidytext)

# If you still have your ja_tidy data frame you can skip
# to the "Get Word Frequencies" part.

# Import Austen books ----

ja_books <- gutenberg_download(c(161,1342,141,158,121,105))
hg_books <- gutenberg_download(c(35,36,159,5230))
bs_books <- gutenberg_download(c(767,768,969,1260,9182))

# Tidy Austen books ----

ja_tidy <- ja_books %>%
  unnest_tokens(word,text)

hg_tidy <- hg_books %>%
  unnest_tokens(word,text)

bs_tidy <- bs_books %>%
  unnest_tokens(word,text)


# Get word frequencies for individual Austen books ----

ja_word_freq_by_book <- ja_tidy %>%
  group_by(gutenberg_id) %>%
  count(word,sort=TRUE)

hg_word_freq_by_book <- hg_tidy %>%
  group_by(gutenberg_id) %>%
  count(word,sort=TRUE)

bs_word_freq_by_book <- bs_tidy %>%
  group_by(gutenberg_id) %>%
  count(word,sort=TRUE)

# Add tf_idf column with bind_tf_idf (and arrange big to small)

ja_tf_idf <- ja_word_freq_by_book %>%
  bind_tf_idf(word,gutenberg_id,n) %>%
  arrange(desc(tf_idf))

hg_tf_idf <- hg_word_freq_by_book %>%
  bind_tf_idf(word,gutenberg_id,n) %>%
  arrange(desc(tf_idf))

bs_tf_idf <- bs_word_freq_by_book %>%
  bind_tf_idf(word,gutenberg_id,n) %>%
  arrange(desc(tf_idf))

# Replacing the gutenberg IDs with the titles
# and column heading by "book".

ja_tf_idf <- ja_tf_idf %>%
  ungroup()  %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
    gutenberg_id == '1342', 'Pride & Prejudice')) %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
    gutenberg_id == '141', 'Mansfield Park')) %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
    gutenberg_id == '161', 'Sense & Sensibility')) %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
    gutenberg_id == '105', 'Persuasion')) %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
    gutenberg_id == '121', 'Northanger Abbey')) %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
    gutenberg_id == '158', 'Emma')) %>%
  rename(book = gutenberg_id)

hg_tf_idf <- hg_tf_idf %>%
  ungroup()  %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
                                gutenberg_id == '35', 'The Time Machine')) %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
                                gutenberg_id == '36', 'The War of the Worlds')) %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
                                gutenberg_id == '159', 'The Island of Doctor Moreau')) %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
                                gutenberg_id == '5230', 'The Invisible Man')) %>%
  rename(book = gutenberg_id)

bs_tf_idf <- bs_tf_idf %>%
  ungroup()  %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
                                gutenberg_id == '767', 'Agnes Grey')) %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
                                gutenberg_id == '768', 'Wuthering Heights')) %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
                                gutenberg_id == '969', 'The Tenant of Wildfell Hall')) %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
                                gutenberg_id == '1260', 'Jane Eyre')) %>%
  mutate(gutenberg_id = replace(gutenberg_id, 
                                gutenberg_id == '9182', 'Villette')) %>%
  rename(book = gutenberg_id)

#  Regroup and sort out top 10. 

ja_tf_idf_top10 <- ja_tf_idf %>%
  group_by(book) %>%
  top_n(10) 

hg_tf_idf_top10 <- hg_tf_idf %>%
  group_by(book) %>%
  top_n(10) 

bs_tf_idf_top10 <- bs_tf_idf %>%
  group_by(book) %>%
  top_n(10) 

# Shamelessly stealing plot code from the book

ja_tf_idf_top10 %>%
  ggplot(aes(reorder(word,tf_idf), tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

hg_tf_idf_top10 %>%
  ggplot(aes(reorder(word,tf_idf), tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

bs_tf_idf_top10 %>%
  ggplot(aes(reorder(word,tf_idf), tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()