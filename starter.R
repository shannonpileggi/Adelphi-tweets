# load rtweet library ----
library(rtweet)

# create token (insert personal key/secrets/token) ----
create_token(
  app = "AdelphiTweets",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

# obtain tweets from user ----
ar_tweets <- get_timeline(user = "AdelphiResearch", 
                          n = 10000)

# save R object ----
saveRDS(ar_tweets, file = "data/ar_tweets.rds")
