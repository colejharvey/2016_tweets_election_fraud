install.packages("ROAuth")
library(ROAuth)
install.packages("twitteR")
library(twitteR)
setup_twitter_oauth("IN SNIPPETS OR CSV", "IN SNIPPETS OR CSV",
                    "IN SNIPPETS OR CSV", "IN SNIPPETS OR CSV")
library(rtweet)
library(tidyverse)
library(tidytext)
library(tweetscores) #Note that tweetscores could only be installed via the menu--Tools, Install packages, file path for the zip file
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "IN SNIPPETS OR CSV" #IN SNIPPETS OR CSV
consumerSecret <- "IN SNIPPETS OR CSV"  #IN SNIPPETS OR CSV
my_oauth <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, 
                             requestURL=requestURL, accessURL=accessURL, authURL=authURL)




library(httr)

# 1. Find OAuth settings for twitter:
#    https://dev.twitter.com/docs/auth/oauth
oauth_endpoints("twitter")

# 2. Register an application at https://apps.twitter.com/
#    Make sure to set callback url to "http://127.0.0.1:1410/"
#
#    Replace key and secret below
myapp <- oauth_app("twitter",
                   key = "IN SNIPPETS OR CSV",
                   secret = "IN SNIPPETS OR CSV"
)

# 3. Get OAuth credentials
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

# 4. Use API
req <- GET(
  "https://api.twitter.com/1.1/statuses/home_timeline.json",
  config(token = twitter_token)
)
stop_for_status(req)
content(req)


###Authorization using rtweet

## store api keys 
api_key <- "IN SNIPPETS OR CSV"
api_secret_key <- "IN SNIPPETS OR CSV"
access_token <- "IN SNIPPETS OR CSV"
access_token_secret <- "IN SNIPPETS OR CSV"

token <- create_token(
  app = "ideologystudy",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

##Testing tweetscores functions

#This uses the rtweet package to get_friends, since tweetscores getFriends does not work for me atm
user <- "colejh"
friends <- get_friends(users = "colejh", token = token)

results <- estimateIdeology(user = "colejh", friends = friends$user_id)
summary(results)
plot(results)


##Faster analysis
results <- estimateIdeology(user = "colejh", friends = friends$user_id, method = "MLE")
summary(results)
plot(results)

##estimation using correspondence analysis
results <- estimateIdeology2(user = "colejh", friends = friends$user_id)


###Testing this with a few rows of the 2016 data

tweets2016 <- read.csv("tweet-ids-001.csv")
tweets2016_top30 <- tweets2016[1:30,]


library(tidyr)

results <- matrix(NA, nrow = 30, ncol=1)

##Create an outer loop here to cycle through batches of users
  users.batch <- as.character(tweets2016_top30$user_screen_name)
  friends <- get_friends(users = users.batch, retryonratelimit = TRUE, token = token) #Maybe get a bunch of friends first, and then separately go through and get ideology
  friends2 <- friends %>% group_by(user) %>% nest()
  
for(i in 1:30){  
  friends.temp <- friends2$data[[i]]
  friends2[i,3] <- estimateIdeology2(user = friends2$user[i], friends = as.numeric(friends.temp$user_id))
  
}


###Moral dictionary using quanteda
library(quanteda)
library(tidytext)
moral.dic <- dictionary(file = "moral foundations dictionary.dic",
             format = "LIWC")

###Getting tweets
tweets2016 <- read.csv("tweet-ids-001.csv")


tweets2016$stripped_text <- gsub("http.*","",  tweets2016$text)
tweets2016$stripped_text <- gsub("https.*","", tweets2016$stripped_text) #Removes URLs

##Getting a corpus

tweets2016.corpus <- corpus(as.character(tweets2016$text))

##Document frequency matrix
moral.dfm <- dfm(tweets2016.corpus, dictionary = moral.dic, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
topfeatures(moral.dfm, 20)
set.seed(100)
textplot_wordcloud(moral.dfm, min_count = 10, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

###Code each tweet for moral terms

#1) Tokenize words from tweets
#1a) Remove stopwords
#2) Count the number of moral words per tweet
#3) Save this value for each tweet

tweets2016 <- read.csv("tweet-ids-001.csv")
tweets2016 <- tweets2016 %>% mutate(text = as.character(text))
tweets2016$stripped_text <- gsub("http.*","",  tweets2016$text)
tweets2016$stripped_text <- gsub("https.*","", tweets2016$stripped_text) #Removes URLs

###Using quanteda corpus methods

tweets2016 <- tweets2016 %>% mutate(docid = id)

tweets2016.corpus <- tweets2016 %>% select(docid, text) %>% corpus(docid_field = "docid", text_field = "text")
moral.dfm <- dfm(tweets2016.corpus, dictionary = moral.dic, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
moral.dfm[1:10,]

##Summing across columns
moral.dfm.tbl <- as_tibble(moral.dfm)
moral.dfm.tbl <- moral.dfm.tbl %>%
  mutate(moral.count = select(., HarmVirtu:MoralityGener) %>% rowSums(na.rm = TRUE))

moral.dfm.tbl <- moral.dfm.tbl %>%
  mutate(moral.neg.count = select(., c(3,5,7,9,11)) %>% rowSums(na.rm = TRUE))

moral.dfm.tbl <- moral.dfm.tbl %>%
  mutate(moral.pos.count = select(., c(2,4,6,8,10)) %>% rowSums(na.rm = TRUE))
###These have the tweet ids, and so can be reconnected with the main file

moral.dfm.tbl <- moral.dfm.tbl %>% mutate(id = as.numeric(document))
tweets2016 <- moral.dfm.tbl %>% select(moral.count, moral.neg.count, moral.pos.count, id) %>%
  left_join(tweets2016, by = "id")




##Emotions using tidytext

emot.dic <- get_sentiments("nrc") #NRC emotions lexicon
emot.dic <- as.dictionary(emot.dic)

emot.dfm <- dfm(tweets2016.corpus, dictionary = emot.dic, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)

emot.dfm.tbl <- as_tibble(emot.dfm)
###These have the tweet ids, and so can be reconnected with the main file

emot.dfm.tbl <- emot.dfm.tbl %>% mutate(id = as.numeric(document))
tweets2016 <- emot.dfm.tbl %>%
  left_join(tweets2016, by = "id")


###Basic dirty analysis
tweets2016.unver <- subset(tweets2016, tweets2016$user_verified == "false")
tweets2016.unver <- tweets2016.unver %>% mutate(incl.media = ifelse(media == "", 0, 1))

model <- lm(retweet_count ~ moral.neg.count + moral.pos.count + anger + anticip + disgust + fear + joy + sad + 
              surpris + trust + user_followers_count + incl.media, data = tweets2016.unver)
summary(model) #Data works (fyi no effect for moral.count)


tweets2016.unver <- tweets2016.unver %>% mutate(anger.bin = ifelse(anger > 0, 1, 0))
model2 <- glm(anger.bin ~ moral.neg.count + moral.pos.count +  user_followers_count + incl.media,
              family = binomial(link="logit"), data = tweets2016.unver)
summary(model2)

library(stargazer)
stargazer(model, model2, type = "html", out = "ssrc appendix models.html")

##Make a sentiment analysis plot?

##Sentiment analysis

library(textdata)
library(tidytext)

tweets2016$stripped_text <- gsub("http.*","",  tweets2016$text)
tweets2016$stripped_text <- gsub("https.*","", tweets2016$stripped_text) #Removes URLs

tweets2016_clean <- tweets2016 %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text) #Using token = "tweets" expands the data so that each row is one word from a tweet
custom_stop_words <- bind_rows(tibble(word = c("trump", "vote", "gore", "election", "fraud"), 
                                      lexicon = c("custom")), 
                               stop_words)

custom_stop_words

# remove stop words from your list of words
cleaned_tweet_words <- tweets2016_clean %>%
  anti_join(custom_stop_words)


# join sentiment classification to the tweet words
nrc_word_counts <- cleaned_tweet_words %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the 2013 flood event.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

###Users' locations
tweets2016 <- tweets2016 %>% mutate(place2 = ifelse(as.character(place) == "", NA, as.character(place)))

tweets2016 %>%
  count(place2, sort = TRUE) %>%
  mutate(place2 = reorder(place2,n)) %>%
  na.omit() %>%
  top_n(30) %>%
  ggplot(aes(x = place2,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter users - unique locations ")

###Bar plot

sentiments <- data.frame(sentiment = c(rep("anger", 4700), rep("anticip", 2310), rep("disgust", 1617), rep("fear", 2839), rep("joy", 1422),
                                       rep("sad", 2760), rep("surpris", 3209), rep("trust", 3122), rep("moral.count", 1434)))

sent.plot <- ggplot(sentiments, aes(sentiment))
sent.plot + geom_bar() + theme_bw()

