########################Importing libraries##################################

#install.packages("xgboost")

library(lattice)
library(Matrix)
library(xgboost)
library(caret)
library(dplyr)
library(Hmisc)
library(reshape)
library(magrittr)

#install.packages("tm")
#install.packages("tidytext")

library(twitteR)
library(tm)
library(tidyr)
library(tidytext)

#############################Importing data##################################

#tweet_data<-read.csv("tweets.csv")

#install.packages("rtweet")
library(rtweet)
rt <- search_tweets("#facebook lang:en", n = 18000,  include_rts = TRUE, parse = TRUE, type = "mixed")

names(rt)
tweets_data <- rt %>%
  select(user_id,text, screen_name, created_at)

# Tokenize
tidy_tweets <- tweets_data %>%
  unnest_tokens(word, text)


library(dplyr)
# # Clean
# cleantidytweets <- anti_join(tidy_tweets,get_stopwords())
cleantidytweets<-dplyr::anti_join(tidy_tweets, tidytext::get_stopwords())
str(cleantidytweets)
cleantidytweets$n_char<-nchar(cleantidytweets$word)

cleantidytweets_filtered<-cleantidytweets %>% 
  filter(n_char>2)

cleantidytweets_filtered_2 <-cleantidytweets_filtered %>% 
  filter(word !="http") %>% 
  filter(word !="https") %>% 
  filter(word !="facebook") %>% 
  filter(word !="amp") %>% 
  filter(word !="t.co")

word_count<-count(cleantidytweets_filtered_2, word, sort = TRUE)

word_count$word <- reorder(word_count$word, word_count$n)
word_count_sample <- head(word_count, 20)

ggplot2::ggplot(word_count_sample, ggplot2::aes(x = word, y = n)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggpubr::theme_pubclean()

######## Q1. Which words are most commonly used in the dataset?#############
# May, Social, People, Soviet,community are commonly used words

######## Q2. Word Cloud Fact ###############################################
#install.packages("wordcloud")
library(wordcloud)
wordcloud::wordcloud(word_count$word, word_count$n, min.freq = 1, max.words = 100, random.order=FALSE)
#####
######Wordcloud clearly indicates that the tweets are associated with WW2 Victory day

######## Q3. Tf-idf ######################################################


word_tf.count <- dplyr::count(cleantidytweets, user_id, word, sort = TRUE) 
word_count_tfidf<-word_tf.count
word_count_tfidf$word<-as.character(word_count_tfidf$word)
word_count_tfidf <- word_count_tfidf[which(word_count_tfidf$n > 5 &
                                             word_count_tfidf$word != "facebook" &
                                             nchar(word_count_tfidf$word) > 5),]

head(word_count_tfidf)

tidytext::bind_tf_idf(word_count_tfidf,user_id, word, n)
#########
wordcloud::wordcloud(word_count_tfidf$word, word_count_tfidf$n, min.freq = 1, max.words = 1000, random.order=FALSE)


#########Q4. Join Sentiment and Viz sentiment#######################
#install.packages("textdata")
library(textdata)

fb_df.sen <- dplyr::inner_join(cleantidytweets, tidytext::get_sentiments("nrc"), by = "word")
fb_df.sen <- dplyr::inner_join(fb_df.sen, tidytext::get_sentiments("afinn"), by = "word")
head(fb_df.sen, 10)



fb_df.sen_count <- count(fb_df.sen, sentiment, word, sort = TRUE)
fb_df.sen_count$word <- reorder(fb_df.sen_count$word, fb_df.sen_count$n)
fb_df.sen_count <- by(fb_df.sen_count, fb_df.sen_count["sentiment"], head, n=5)
fb_df.sen_count <- Reduce(rbind, fb_df.sen_count)

ggplot2::ggplot(fb_df.sen_count, ggplot2::aes(x = word, y = n, fill = sentiment)) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~sentiment, scales = "free") +
  ggplot2::labs(y = "Contribution to sentiment", x = NULL) +
  ggplot2::coord_flip() +
  ggpubr::theme_pubclean()
######## The most common sentiments are fear, sadness,anger
######## Q5. Are the posts about facebook rather positive, or rather#####
# negative? (note: feel free to apply a simple mean)
fb_df.sen_response<-fb_df.sen_count %>% 
  group_by(sentiment) %>% 
   summarise(response=sum(n))
####### #The posts about facebook are postive



