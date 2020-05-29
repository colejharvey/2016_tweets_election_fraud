install.packages("ROAuth")
library(ROAuth)
install.packages("twitteR")
library(twitteR)
setup_twitter_oauth("MitZ8CpsxQeeASr14J2mf5kE1", "cXXoOIw3SEx2EV0D5SAZbdDff9jgIhOb8mS4W67nMzcS9F9zVZ",
                    "1264943028478849029-7K7p01qyfBVY1JwdltQ2HQ50mXib2D", "6gWPVfHFqkrHWxndGo0thNhtMnOxQngS8IwudxJjnqbG5")
library(rtweet)
library(tidyverse)
library(tidytext)
library(tweetscores) #Note that tweetscores could only be installed via the menu--Tools, Install packages, file path for the zip file
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "MitZ8CpsxQeeASr14J2mf5kE1" #1264943028478849029-7K7p01qyfBVY1JwdltQ2HQ50mXib2D
consumerSecret <- "cXXoOIw3SEx2EV0D5SAZbdDff9jgIhOb8mS4W67nMzcS9F9zVZ"  #6gWPVfHFqkrHWxndGo0thNhtMnOxQngS8IwudxJjnqbG5
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
                   key = "MitZ8CpsxQeeASr14J2mf5kE1",
                   secret = "cXXoOIw3SEx2EV0D5SAZbdDff9jgIhOb8mS4W67nMzcS9F9zVZ"
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
api_key <- "MitZ8CpsxQeeASr14J2mf5kE1"
api_secret_key <- "cXXoOIw3SEx2EV0D5SAZbdDff9jgIhOb8mS4W67nMzcS9F9zVZ"
access_token <- "1264943028478849029-7K7p01qyfBVY1JwdltQ2HQ50mXib2D"
access_token_secret <- "6gWPVfHFqkrHWxndGo0thNhtMnOxQngS8IwudxJjnqbG5"

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

###Basic dirty analysis
tweets2016.unver <- subset(tweets2016, tweets2016$user_verified == "false")

model <- lm(retweet_count ~ moral.count + user_followers_count, data = tweets2016.unver)
summary(model) #Data works (fyi no effect for moral.count)


