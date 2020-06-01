library(wordcloud)
library(tidytext)
library(tidyverse)
library(readr)
library(dplyr)
library(scales)
library(quanteda)
library(quanteda.textmodels)
library(igraph)
library(ggraph)

################################### Question 1 #############################################

pos_reviews <- read_csv("C:/Users/vargheal/OneDrive - Seton Hall University/SHU 2019-2020/Spring Semester/Text Mining/Final Exam/FinalProjectFiles/pos_reviews.csv")

pos_reviews_word_cloud <- pos_reviews %>% unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word,sort=TRUE) %>%
  with(wordcloud(word, n, max.words = 100))

neg_reviews <- read_csv("C:/Users/vargheal/OneDrive - Seton Hall University/SHU 2019-2020/Spring Semester/Text Mining/Final Exam/FinalProjectFiles/neg_reviews.csv")

neg_reviews_word_cloud <- neg_reviews %>% unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word,sort=TRUE) %>%
  with(wordcloud(word, n, max.words = 100))

#################################### Question 2 #############################################

pos_reviews_fancy <- pos_reviews %>% unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(word,sort=TRUE) %>%
  mutate(pos_percent = (n / sum(n)) * 100)

neg_reviews_fancy <- neg_reviews %>% unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(word,sort=TRUE) %>%
  mutate(neg_percent = (n / sum(n)) * 100)

reviews_percents_no_stop <- full_join(pos_reviews_fancy, neg_reviews_fancy, by="word")

ggplot(reviews_percents_no_stop, aes(x = pos_percent, y = neg_percent, color = abs(neg_percent - pos_percent))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "black") +
  theme(legend.position="none") +
  labs(y = "Negative Reviews", x = "Positive Reviews")

#################################### Question 3 #############################################

bing_frame_pos <- pos_reviews %>%
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("bing")) %>%  
  count(val,sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(diff = positive - negative)

bing_frame_neg <- neg_reviews %>%
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("bing")) %>%  
  count(val,sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(diff = positive - negative)

#################################### Question 4 #############################################

reviews_tidy_chaps <- pos_reviews %>% unnest_tokens(word,text)

reviews_nrc_sent <- reviews_tidy_chaps %>% 
  inner_join(get_sentiments("nrc"))

reviews_chap_sent_nrc <- reviews_nrc_sent %>%
  count(val,word,sentiment)

#################################### Question 5 #############################################

pos_reviews_question_5 <- corpus(pos_reviews)

pos_reviews_kwic <- kwic(pos_reviews_question_5, pattern="elaborate", window=2)
View(pos_reviews_kwic)

#################################### Question 6 #############################################

reviews <- rbind(pos_reviews, neg_reviews)

reviews_tidy <- reviews %>%
  unnest_tokens(word,text)

reviews_word_freq_no_stop <- reviews_tidy %>%
  unnest_tokens(word,word) %>%
  anti_join(stop_words) %>%
  count(word,val)

reviews_word_freq_no_stop <- reviews_word_freq_no_stop[complete.cases(reviews_word_freq_no_stop),]

names(reviews_word_freq_no_stop) <- c("term","document","n")

reviews_dtm <- reviews_word_freq_no_stop %>%
  cast_dtm(document,term,n)

reviews_lda <- LDA(reviews_dtm,k=20,control = list(seed = 1234))

reviews_topics <- tidy(reviews_lda,matrix="beta")

reviews_docs <- tidy(reviews_lda,matrix="gamma")

reviews_top_terms <- reviews_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

reviews_top_terms %>%
  ggplot(aes(term, beta, fill = factor(as.factor(topic)))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

reviews_docs %>%
  ggplot(aes(document, topic, fill = gamma)) +
  geom_tile(color="grey") +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  coord_flip()

#################################### Question 7 ############################################

names(reviews) <- c("label","text")

reviews %>% group_by(label) %>% summarise(length(label))

reviews_corpus <- corpus(reviews)

summary(reviews_corpus)

reviews_dfm <- dfm(reviews_corpus, tolower = TRUE)

reviews_dfm_train <- reviews_dfm[1:1500,]
reviews_dfm_test <- reviews_dfm[1501:nrow(reviews_dfm),]

reviews_train <- reviews[1:1500,]
reviews_test <- reviews[1501:nrow(reviews),]

table(reviews_train$label)

reviews_classifier <- textmodel_nb(reviews_dfm_train, reviews_train$label)

reviews_pred <- predict(reviews_classifier,newdata = reviews_dfm_test)

table(reviews_pred, reviews_test$label)

#################################### Question 8 ############################################

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)}

reviews_bigrams <- count_bigrams(reviews)