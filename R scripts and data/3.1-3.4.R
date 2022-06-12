# Load packages required for this session into library
#install.packages("vosonSML")
library(vosonSML)
library(magrittr)
library(igraph)
library(tidyr)
library(tidytext)
library(stopwords)

# Set up Twitter authentication variables
my_app_name <- "7230ICT_Qin"
my_api_key <- "O2JTa6TiP4RqFTi5AhIjaEO5b"
my_api_secret <- "IIRS6pR8vakdvK7UE8SOCuNa2wW9NBAUdT3kanXYt3J43TCOgq"
my_access_token <- "1501187128038166532-Iwrrqjt1TDszF1MRES9cwNwSTvzN2u"
my_access_token_secret <- "ducOdck8wd3L6UAaKysiMEI3gj8DqFN8YaS7oYVuBZ4Nr"

# Read rds data
twitter_data_Ed_retweets  <- ImportData("TwitterData_Retreets.rds", "twitter")
twitter_data_Ed  <- ImportData("TwitterData.rds", "twitter")

# Set up Twitter authentication variables


# Authenticate with Twitter and collect data

twitter_data_Ed_retweets <- Authenticate("twitter",
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

twitter_data_Ed <- Authenticate("twitter",
                                appName = my_app_name,
                                apiKey = my_api_key,
                                apiSecret = my_api_secret,
                                accessToken = my_access_token,
                                accessTokenSecret = my_access_token_secret) %>%
    Collect(searchTerm = "#EdSheeran",
            searchType = "recent",
            numTweets = 10000,
            lang = "en",
            includeRetweets = FALSE,
            writeToFile = TRUE)

#View collected Twitter data


View(twitter_data_Ed_retweets)
View(twitter_data_Ed)


# number of your retrieved tweets are retweets.
numRetweets=nrow(twitter_data_Ed_retweets)-nrow(twitter_data_Ed)
numRetweets
length(twitter_data_Ed_retweets$user_id)-length(twitter_data_Ed$user_id)

length(unique(twitter_data_Ed_retweets$user_id))
length(unique(twitter_data_Ed$user_id))
length(unique(twitter_data_Ed_retweets$user_id))-length(unique(twitter_data_Ed$user_id))
# ----------
# Create actor network and graph from the data
#
twitter_actor_network <- twitter_data_Ed_retweets %>% Create("actor")
twitter_actor_graph <- twitter_actor_network %>% Graph()

# 
# Write graph to file
# Make sure to set your working directory to where you want to save the file
# before you execute the next line

write.graph(twitter_actor_graph, file = "TwitterActor_retweets_Ed.graphml", format = "graphml")


# Run Page Rank algorithm to find important users

rank_twitter_actor_Ed <- sort(page_rank(twitter_actor_graph)$vector, decreasing=TRUE)
head(rank_twitter_actor_Ed, n=5)

#
# Overwrite the 'name' attribute in your graph with the 'screen name' attribute
# to replace twitter IDs with more meaningful names,
# then run the Page Rank algorithm again

V(twitter_actor_graph)$name <- V(twitter_actor_graph)$screen_name

rank_twitter_actor_Ed_retweets<- sort(page_rank(twitter_actor_graph)$vector, decreasing = TRUE)
head(rank_twitter_actor_Ed_retweets, n = 5)

# ----------
# Create semantic network and graph from the data

twitter_semantic_network <- twitter_data_Ed_retweets %>% Create("semantic")
twitter_semantic_graph <- twitter_semantic_network %>% Graph()


# Write graph to file

write.graph(twitter_semantic_graph, file = "TwitterSemantic_Ed_retweets.graphml", format = "graphml")


# Run Page Rank algorithm to find important terms/hashtags

rank_twitter_semantic <- sort(page_rank(twitter_semantic_graph)$vector, decreasing = TRUE)
head(rank_twitter_semantic, n = 10)


# Create the network and graph again, but this time:
# - with 25% of the most frequent terms (before was the default of 5%)
# - with 75% of the most frequent hashtags (before was the default of 50%)
# - removing the actual search term ("#edsheeran")

tw_sem_nw_more_terms <- twitter_data_Ed_retweets %>%
    Create("semantic",
           termFreq = 25,
           hashtagFreq = 75,
           removeTermsOrHashtags = c("#EdSheeran"))

tw_sem_graph_more_terms <- tw_sem_nw_more_terms %>% Graph()


# Write graph to file

write.graph(tw_sem_graph_more_terms,
            file = "TwitterSemanticMoreTerms_Ed_retweets.graphml",
            format = "graphml")
