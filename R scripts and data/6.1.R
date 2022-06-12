
# Load packages required for this session into library
library(vosonSML)
library(magrittr)
library(igraph)
library(tuber)

#import youtuber dataset
#youtubeActor_Ed.graphml
yt_data_Ed <- ImportData("YoutubeData.rds", "youtube")
yt_actor_network <- yt_data_Ed %>% Create("actor")
yt_actor_graph <-yt_actor_network %>% Graph()
write.graph(yt_actor_graph, file = "youtubeActor_Ed.graphml", format = "graphml")
summary(yt_actor_graph)


#import twitter actor dataset
#TwitterSemantic_Ed_retweets.graphml
twitter_data_Ed <- ImportData("TwitterData_Retreets.rds", "twitter")
twitter_actor_network <- twitter_data_Ed %>% Create("actor")
twitter_actor_graph <-twitter_actor_network %>% Graph()
summary(twitter_actor_graph)


#import twitter terms dataset  
#TwitterSemanticMoreTerms_Ed_retweets.graphml
twitter_data_Ed <- ImportData("TwitterData_Retreets.rds", "twitter")
twitter_semantic_network <- twitter_data_Ed %>%
    Create("semantic",
           termFreq = 25,
           hashtagFreq = 75,
           removeTermsOrHashtags = c("#EdSheeran"))
twitter_semantic_graph <- twitter_semantic_network %>% Graph()
summary(twitter_semantic_graph)
