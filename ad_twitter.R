# install.packages("httr")
# install.packages("rtweet")
# install.packages("dplyr")
# install.packages("tokenizers")
# install.packages("lubridate")


library(ggplot2)
library(httr)
library(rtweet)
library(dplyr)
library(tokenizers)
library(lubridate)

#set the WD

setwd("~/Dropbox/Work to do/OpenUp/ADH Audience research/adh_twitter")

# Create Twitter auth using API keys set up .Renviron

token <- create_token(
  consumer_key = Sys.getenv("TWITTERCKEY"),
  consumer_secret = Sys.getenv("TWITTERCS"),
  access_token = Sys.getenv("TWITTERAT"),
  access_secret = Sys.getenv("TWITTERAS"),
)
#Get old data

linksChatter <- read_twitter_csv("links1.csv", unflatten = TRUE)

#get new data

linksChatter1 <- search_tweets("@Africa_DataHub OR africadatahub.org", n=18000)

# Make the retweet columns english

linksChatter1$is_retweet <- gsub("FALSE", "Tweet", linksChatter1$is_retweet)
linksChatter1$is_retweet <- gsub("TRUE", "Retweet", linksChatter1$is_retweet)

#linksChatter1 <- select(linksChatter1, !(followers_count:favourites_count))

#renove first column
#linksChatter <- select(linksChatter, !(user_id))
linksChatter1 <- select(linksChatter1, !(user_id))

write_as_csv(linksChatter1, "links2.csv")
linksChatter1 <- read_twitter_csv("links2.csv", unflatten = TRUE)


#update linksChatter

linksChatter2 <- union(linksChatter,linksChatter1)

#save out the new file

write_as_csv(linksChatter2, "links1.csv")

# let's look at activity

linksChatter2 %>%
  dplyr::group_by(is_retweet) %>%
  ts_plot(by="days") +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses mentioning Africa Data Hub",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet" +
      ggplot2::scale_fill_discrete(labels = c("Tweets", "Retweets"))
  )

# who's talking about us?
whoTweets <- linksChatter2 %>% count(screen_name)
whoTweets <- whoTweets[order(whoTweets$n, na.last=FALSE, decreasing = TRUE),]

write.csv(whoTweets, file = "whoTweets.csv", quote = FALSE)

#Data for GDS retweet chart

whenTweets <- linksChatter2 %>% select(created_at, is_retweet)
whenTweets$created_at <- as_date(whenTweets$created_at)

whenTweets2 <- whenTweets %>%
  count(created_at, is_retweet)
write.csv(whenTweets2, file = "whenTweets.csv", quote = FALSE)

  
# day Tweeted

dayTweets <- linksChatter2 %>% select(created_at)
dayTweets$created_at <- as_date(dayTweets$created_at)
dayTweets <- dayTweets %>% count(created_at)
dayTweets <- dayTweets[order(dayTweets$n, na.last=FALSE, decreasing = TRUE),]

write.csv(dayTweets, file = "dayTweets.csv", quote = FALSE, sep = ",")

retweetSource <- linksChatter2 %>% select(retweet_source)
retweetSource <- retweetSource %>% count(retweet_source)
retweetSource <- retweetSource[order(retweetSource$n,  na.last=FALSE, decreasing = TRUE),]