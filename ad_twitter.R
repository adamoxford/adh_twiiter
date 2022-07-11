# install.packages("httr")
# install.packages("rtweet")
# install.packages("dplyr")
# install.packages("tokenizers")
# install.packages("lubridate")
# install.packages("tidyr")


library(ggplot2)
library(httr)
library(rtweet)
library(dplyr)
library(tokenizers)
library(lubridate)
library(tidyr)

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
  count(created_at, is_retweet) %>%
  pivot_wider(names_from = is_retweet, values_from = n)
  
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

#import Vimeo data

#set date
yesterdayDate <- 2022-07-10

#create url variables
videosURL <- paste0("https://api.vimeo.com/export/csv?jwt_token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjMzMTIxNjYxOTQsInVzZXJfaWQiOjEzNjQyMzAzNiwiYXBwX2lkIjo1ODQ3OSwic2NvcGVzIjoicHVibGljIGludGVyYWN0IHByaXZhdGUgdXBsb2FkIGNyZWF0ZSBlZGl0IGRlbGV0ZSBlbWFpbCBzdGF0cyIsInRlYW1fdXNlcl9pZCI6bnVsbCwidmVyc2lvbiI6My4wM30.TV4e5xEEmZxLJ_sOUVix-2b4ApILNPxcTK6wvjvM88E&export_uri=%2Fusers%2F136423036%2Fvideos%2Fstats%3Fstart_date%3D2021-06-24%26end_date%3D", yesterdayDate, "%26fields%3Dvideo.uri%252Cvideo.created_time%252Cvideo.pictures.sizes%252Cvideo.name%252Cplays%252Cloads%252Cfinishes%252Cdownloads%252Clikes%252Ccomments%252Cunique_viewers%252Cunique_loads%252Cwatched.mean_percent%252Cwatched.mean_seconds%252Cwatched.sum_seconds%26sort%3Dplays%26group_by%3Dvideo%26page%3D1%26per_page%3D15000%26csv%3Dtrue")
dateURL <- paste0("https://api.vimeo.com/export/csv?jwt_token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjMzMTI2MzcyOTEsInVzZXJfaWQiOjEzNjQyMzAzNiwiYXBwX2lkIjo1ODQ3OSwic2NvcGVzIjoicHVibGljIGludGVyYWN0IHByaXZhdGUgdXBsb2FkIGNyZWF0ZSBlZGl0IGRlbGV0ZSBlbWFpbCBzdGF0cyIsInRlYW1fdXNlcl9pZCI6bnVsbCwidmVyc2lvbiI6My4wM30.T4CLyS_7U_OqkjLxF86akRVbLM_T6AJ0SPZbAn_MSHU&export_uri=%2Fusers%2F136423036%2Fvideos%2Fstats%3Fstart_date%3D2021-06-27%26end_date%3D", yesterdayDate, "%26fields%3Drange.start_date%252Crange.end_date%252Cplays%252Cloads%252Cfinishes%252Cdownloads%252Cunique_viewers%252Cunique_loads%252Cwatched.mean_percent%252Cwatched.mean_seconds%252Cwatched.sum_seconds%26sort%3Dplays%26group_by%3Dday%26page%3D1%26per_page%3D15000%26csv%3Dtrue")
sourceURL <- paste0("https://api.vimeo.com/export/csv?jwt_token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjMzMTI2Mzc0MTIsInVzZXJfaWQiOjEzNjQyMzAzNiwiYXBwX2lkIjo1ODQ3OSwic2NvcGVzIjoicHVibGljIGludGVyYWN0IHByaXZhdGUgdXBsb2FkIGNyZWF0ZSBlZGl0IGRlbGV0ZSBlbWFpbCBzdGF0cyIsInRlYW1fdXNlcl9pZCI6bnVsbCwidmVyc2lvbiI6My4wM30.V3GiJvTkxovinHZBC4mRvJ05WW4_yDGqyz4d3hZ3yYk&export_uri=%2Fusers%2F136423036%2Fvideos%2Fstats%3Fstart_date%3D2021-06-27%26end_date%3D", yesterdayDate, "%26fields%3Durl%252Cplays%252Cloads%252Cfinishes%252Cdownloads%252Cunique_viewers%252Cunique_loads%252Cwatched.mean_percent%252Cwatched.mean_seconds%252Cwatched.sum_seconds%26sort%3Dplays%26group_by%3Dembed_domain%26page%3D1%26per_page%3D15000%26csv%3Dtrue")

#get the data
videosData <- read.csv(videosURL)
dateData <- read.csv(dateURL)
sourceData <- read.csv(sourceURL)

#write out
write.csv(videosData, "videoData.csv")
write.csv(dateData, "dateData.csv")
write.csv(sourceData, "sourceData.csv")