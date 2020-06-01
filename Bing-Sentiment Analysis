# Necessary libraries----
# We'll use the janeaustenr library for Austen books,
# it's a little easier than gutenbergr.
library(tidyverse)
library(tidytext)
library(janeaustenr)

# Cleaning data----
# First we'll break out chapters in Austen books
# This is just code from Chapter 1 of Silge & Robinson.

ja_chaps <- austen_books() %>%
  group_by(book) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()
  
# Tidy data
ja_tidy_chaps <- ja_chaps %>% unnest_tokens(word,text)

# Get Pos/Neg Sentiments----

# Just need to join with bing data frame

ja_bing_sent <- ja_tidy_chaps %>% 
  inner_join(get_sentiments("bing"))

# That seperates out words that are pos/neg.
# Now tally them by book and chapter.

ja_chap_sent_bing <- ja_bing_sent %>%
  count(book,chapter,sentiment)

# Get pos/neg difference----

# Each chapter has two values now:  pos & neg.
# Use spread to create two columns with these
# (Easier to calculate the difference this way)

ja_chap_sent_bing <- ja_chap_sent_bing %>%
  spread(sentiment,n,fill=0)

# Create difference column

ja_chap_sent_bing <- ja_chap_sent_bing %>%
  mutate(diff = positive - negative)

# Plot----

ggplot(ja_chap_sent_bing,aes(x=chapter,y=diff,fill=book)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~book,ncol=2,scales="free_x")


# Without all the intermediate files----
# Here it is all in one pipe.

ja_chap_sent_bing <- austen_books() %>%
  group_by(book) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>% 
  unnest_tokens(word,text) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(book,chapter,sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(diff = positive - negative)
# Now plot
ggplot(ja_chap_sent_bing,aes(x=chapter,y=diff,fill=book)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~book,ncol=2,scales="free_x")


