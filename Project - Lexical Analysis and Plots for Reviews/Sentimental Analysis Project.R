
library(tidytext)
library(tidyverse)
library(readtext)
library(readr)
library(dplyr)
library(scales)
library(quanteda)
library(quanteda.textmodels)
library(igraph)
library(ggraph)

positive_reviews <- read_csv("pos_reviews.csv")
negative_reviews <- read_csv("neg_reviews.csv")

#1 Obtain wordclouds of the positive review text and negative review text.  Based on the wordclouds, 
# what words seem to appear more commonly in positive versus negative reviews?  (You do not have to use R to generate the wordclouds.  In fact as we've seen there are many better options.)

library(wordcloud)

positive_reviews_word_cloud <- positive_reviews %>% unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word,sort=TRUE) %>%
  with(wordcloud(word, n, max.words = 1000))

negative_reviews_word_cloud <- negative_reviews %>% unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word,sort=TRUE) %>%
  with(wordcloud(word, n, max.words = 1000))

#2 Create a "fancy jittered scatterplot" contrasting the word frequencies of the positive versus negative reviews.  
# Comment upon the plot, with particular attention to which words most characterize positive versus negative reviews.

positive_reviews_fancy <- positive_reviews %>% unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(word,sort=TRUE) %>%
  mutate(pos_percent = (n / sum(n)) * 100)

negative_reviews_fancy <- negative_reviews %>% unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(word,sort=TRUE) %>%
  mutate(neg_percent = (n / sum(n)) * 100)

reviews_percents_no_stop <- full_join(positive_reviews_fancy, negative_reviews_fancy, by="word")

ggplot(reviews_percents_no_stop, aes(x = pos_percent, y = neg_percent, color = abs(neg_percent - pos_percent))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "blue") +
  theme(legend.position="none") +
  labs(y = "Negative Reviews", x = "Positive Reviews", title = "Positive vs Negative Jittered Scatterplot")

#3 Use either the bing or AFINN sentiment lexicon (your choice, but choose only one) to measure the 
# positivity/negativity of both the positive and negative reviews.  Comment upon whether or not this matches your expectations, and to what extent.


bing_frame_pos <- positive_reviews %>%
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("bing")) %>%  
  count(val,sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(diff = positive - negative)

ggplot(bing_frame_pos,aes(x=val,y=diff,fill=val)) + 
  geom_col(show.legend = FALSE) +
  geom_text((aes(label = diff, y = diff + 2))) +
  ggtitle("The Positive sentiment for the Reviews")

bing_frame_neg <- negative_reviews %>%
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("bing")) %>%  
  count(val,sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(diff = positive - negative)

ggplot(bing_frame_neg,aes(x=val,y=diff,fill=val)) + 
  geom_col(show.legend = FALSE) +
  geom_text((aes(label = diff, y = diff + 2))) +
  ggtitle("The Negative sentiment for the Reviews")


#4 Use the nrc sentiment lexicon to measure one other attribute other than positivity/negativity (e.g. "sadness" or "joy", etc.)  
# Comment upon whether or not this matches your expectations, and to what extent.

reviews_tidy_chaps_pos <- positive_reviews %>% unnest_tokens(word,text)

reviews_nrc_sent_pos <- reviews_tidy_chaps_pos %>% 
  inner_join(get_sentiments("nrc"))

reviews_chap_sent_nrc_pos <- reviews_nrc_sent_pos %>%
  count(val,word,sentiment)


ggplot(subset(reviews_chap_sent_nrc_pos,sentiment %in% 
                c("anger","fear","sadness")),aes(x=word,y=n,fill=sentiment)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~val,ncol=2,scales="free_x") 


reviews_tidy_chaps_neg <- negative_reviews %>% unnest_tokens(word,text)

reviews_nrc_sent_neg <- reviews_tidy_chaps_neg %>% 
  inner_join(get_sentiments("nrc"))

reviews_chap_sent_nrc_neg <- reviews_nrc_sent_neg %>%
  count(val,word,sentiment)


ggplot(subset(reviews_chap_sent_nrc_neg,sentiment %in% 
                c("anger","fear","sadness")),aes(x=word,y=n,fill=sentiment)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~val,ncol=2,scales="free_x") 

#5 Choose a word that occurs between 20 and 50 times in both the positive and negative reviews and 
# perform a KWIC analysis of it (your choice of window).  Comment upon the different contexts in which the word is found in the positive and negative reviews.

#Creating the Corupus of Negative revivews
negative_reviews_question_5 <- corpus(negative_reviews)

#Finding the word elaborate in the reviews
negative_reviews_kwic <- kwic(negative_reviews_question_5, pattern="elaborate", window=5)
View(negative_reviews_kwic)


textplot_xray(kwic(negative_reviews_question_5, pattern = "elaborate")) 

#########################################################################################################

#Creating the Corupus of positive revivews
positive_reviews_question_5 <- corpus(positive_reviews)

#Finding the word elaborate in the reviews
positive_reviews_kwic <- kwic(positive_reviews_question_5, pattern="elaborate", window=5)
View(positive_reviews_kwic)


textplot_xray(kwic(positive_reviews_question_5, pattern = "elaborate")) 

#6 Obtain a topic model of the combined text of the reviews using 20 topics.  
# How well in your opinion do any of the topics found identify movie "topics" that we (humans) would recognize?

library(topicmodels)

reviews <- rbind(positive_reviews, negative_reviews)

reviews_tidy <- reviews %>%
  unnest_tokens(word,text)

# Get word frequencies and remove stopwords.
reviews_word_freq_no_stop <- reviews_tidy %>%
  unnest_tokens(word,word) %>%
  anti_join(stop_words) %>%
  count(word,val)

# Just in case there are NA cells, eliminate those.
reviews_word_freq_no_stop <- reviews_word_freq_no_stop[complete.cases(reviews_word_freq_no_stop),]

# For consistency with document-term matrix
# we'll rename headings (optional) 
names(reviews_word_freq_no_stop) <- c("term","document","n")

# Cast tidy data frame into document term matrix.
reviews_dtm <- reviews_word_freq_no_stop %>%
  cast_dtm(document,term,n)

# LDA.  Choosing 10 topics just to see.  
# This will take a minute or two.
reviews_lda <- LDA(reviews_dtm,k=20,control = list(seed = 1234))

# Getting beta matrix
reviews_topics <- tidy(reviews_lda,matrix="beta")


reviews_docs <- tidy(reviews_lda,matrix="gamma")

# Looking up the top 10 words in each topic.
reviews_top_terms <- reviews_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Bar Plot For Topic <= 10
reviews_top_terms %>%
  filter(topic <= 10)%>%
  ggplot(aes(term, beta, fill = factor(as.factor(topic)))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  ggtitle("Bar Chart For Topics")
#Bar Plot For Topic > 10
reviews_top_terms %>%
  filter(topic > 10)%>%
  ggplot(aes(term, beta, fill = factor(as.factor(topic)))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  ggtitle("Bar Chart For Topics")

#Tile Plot 
reviews_docs %>%
  ggplot(aes(document, topic, fill = gamma)) +
  geom_tile(color="grey") +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  coord_flip() + 
  ggtitle("Tile Plot For Topics")

#7 Using 75% of the combined text as a training set, create a text classifier that determines whether a review is positive or negative.  
# (Your training set should consist of the first 75% of the positive reviews and the first 75% of the negative reviews.)  Test your classifier on the remaining 25% of reviews and obtain a confusion matrix of the results.  Comment on your classifier's performance.

reviews_1 <- rbind(positive_reviews[1:750,], negative_reviews[1:750,])
reviews_1 <- rbind(reviews_1, positive_reviews[751:nrow(positive_reviews),])
reviews_1 <- rbind(reviews_1, negative_reviews[751:nrow(negative_reviews),])

#

#Create Corpus Object for Quanteda
reviews_corpus <- corpus(reviews_1)

#If you want to view, THen Summarize it.
summary(reviews_corpus)

#Creating DfM matrix for analysis
reviews_dfm <- dfm(reviews_corpus, tolower = TRUE)
reviews_dfm <- dfm_trim(reviews_dfm, min_termfreq = 2, min_docfreq = 2)
reviews_dfm <- dfm_weight(reviews_dfm, scheme = "logave", base = 3)

# Get 75-25 train-test split.  Since 75% of our data
# is 1500, and data randomized, split at 1500 should do it.
reviews_dfm_train <- reviews_dfm[1:1500,]
reviews_dfm_test <- reviews_dfm[1501:nrow(reviews_dfm),]

# Need to split original as well for pos/neg labels.
reviews_train <- reviews_1[1:1500,]
reviews_test <- reviews_1[1501:nrow(reviews_1),]

# Check split to be sure
table(reviews_train$val)

# Ready to produce classifier.
reviews_classifier <- textmodel_nb(reviews_dfm_train, reviews_train$val)

# Let's get its predictions for test set.
reviews_pred <- predict(reviews_classifier,newdata = reviews_dfm_test)

# Now check those predictions against actual labels.
t <- table(reviews_pred, reviews_test$val)

table(reviews_pred, reviews_test$val)

((t[1]+ t[4])/sum(t))*100

#Its 82.4 % accuracy


#8 Obtain bigrams of the combined review text, along with a network visualization of the top bigrams.  
# How many of the top bigrams identify common two-word terms associated with movies?


# The count_bigrams function given to us in Ch. 4.
# Note that it does assume the text is in "text" column.
# It does *not* however need tidy text.
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)}

reviews_bigrams <- count_bigrams(reviews)

# The visualize_bigrams function given in Ch. 4.
# It uses the output of the count_bigrams function above.
# You can adjust top_n number to get more/less bigrams
# but do remember to then rerun the function.

visualize_bigrams <- function(bigrams) {
  set.seed(1234)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    top_n(30) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

