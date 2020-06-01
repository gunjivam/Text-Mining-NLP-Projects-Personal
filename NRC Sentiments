# We will assume that you have run BingLex.R already
# and so have those data frames available as needed.
# This goes pretty much the same as with bing
# just with more categories.

# Get nrc categorization----
# Join with nrc

ja_nrc_sent <- ja_tidy_chaps %>% 
  inner_join(get_sentiments("nrc"))

# That seperates out emotional words of various kinds.
# Tally them:

ja_chap_sent_nrc <- ja_nrc_sent %>%
  count(book,chapter,sentiment)

# Each chapter has many values now. 
# All set up to plot individual qualities, or many.

# Plot-----
# To plot a subset of emotions, can input just an 
# apprpriate subset of the data.

ggplot(subset(ja_chap_sent_nrc,sentiment %in% 
                c("anger","fear","sadness")),aes(x=chapter,y=n,fill=sentiment)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~book,ncol=2,scales="free_x") 

