# Load packages required for this session into library
library(magrittr)
library(vosonSML)
library(tuber)

# Set up Twitter authentication variables
my_app_name <- "7230ICT_Qin"
my_api_key <- "O2JTa6TiP4RqFTi5AhIjaEO5b"
my_api_secret <- "IIRS6pR8vakdvK7UE8SOCuNa2wW9NBAUdT3kanXYt3J43TCOgq"
my_access_token <- "1501187128038166532-Iwrrqjt1TDszF1MRES9cwNwSTvzN2u"
my_access_token_secret <- "ducOdck8wd3L6UAaKysiMEI3gj8DqFN8YaS7oYVuBZ4Nr"

twitter_data<- Authenticate("twitter",
                                         appName = my_app_name,
                                         apiKey = my_api_key,
                                         apiSecret = my_api_secret,
                                         accessToken = my_access_token,
                                         accessTokenSecret = my_access_token_secret) %>%
    Collect(searchTerm = "#EdSheeran",
            searchType = "recent",
            numTweets = 10000,
            lang = "en",
            includeRetweets = TRUE,
            writeToFile = TRUE)

save_as_csv(twitter_data, "twitter_data.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")






#youtube data
yt_data<- ImportData("YoutubeData.rds", "youtube")

write.csv(yt_data, "yt_data.csv")













# Part 2: Geolocation Tweets ----

# Load packages required for this session into library

library(rtweet)
library(leaflet)
library(dplyr)


# Set up Twitter authentication variables
my_app_name <- "7230ICT_Qin"
my_api_key <- "O2JTa6TiP4RqFTi5AhIjaEO5b"
my_api_secret <- "IIRS6pR8vakdvK7UE8SOCuNa2wW9NBAUdT3kanXYt3J43TCOgq"
my_access_token <- "1501187128038166532-Iwrrqjt1TDszF1MRES9cwNwSTvzN2u"
my_access_token_secret <- "ducOdck8wd3L6UAaKysiMEI3gj8DqFN8YaS7oYVuBZ4Nr"

# Set up bounding box around Gold Coast and draw map 

lat1 <- -28.09469789418109
long1 <- 153.3255386358539
lat2 <- -27.93345069419669
long2 <- 153.46424102783203

map <- leaflet() %>%
    addTiles() %>%
    fitBounds(long1, lat1, long2, lat2)
map


# Find tweets from this area and process them

geo_tweets <- Authenticate("twitter",
                           appName = my_app_name,
                           apiKey = my_api_key,
                           apiSecret = my_api_secret,
                           accessToken = my_access_token,
                           accessTokenSecret = my_access_token_secret) %>%
    Collect(searchTerm = "#auspol",
            searchType = "recent",
            numTweets = 1000,
            lang = "en",
            geocode = "-28.015,153.399,100km", 
            includeRetweets = TRUE,
            writeToFile = TRUE)

geo_tweets_loc <- lat_lng(geo_tweets)

geo_tweets_loc <- subset(geo_tweets_loc, lat != "NA")

geo_tweets_loc <- geo_tweets_loc[c("text", "lat", "lng")]

geo_tweets_grouped <- geo_tweets_loc %>%
    group_by(lat, lng) %>%
    mutate(concat_text = paste0(text, collapse = "<br/> <br/>")) %>%
    select(-text)

geo_tweets_grouped <- unique(geo_tweets_grouped)
View(geo_tweets_grouped)


# Project tweets to map

geo_tweets_grouped$longitude = as.numeric(as.character(geo_tweets_grouped$lng))
geo_tweets_grouped$latitude = as.numeric(as.character(geo_tweets_grouped$lat))

map <- leaflet() %>%
    addTiles() %>%
    fitBounds(long1, lat1, long2, lat2) %>%
    addCircleMarkers(geo_tweets_grouped$longitude, 
                     geo_tweets_grouped$latitude, 
                     popup = geo_tweets_grouped$concat_text)
map
