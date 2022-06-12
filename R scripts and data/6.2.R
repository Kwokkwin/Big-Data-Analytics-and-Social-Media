# Part 1: Centrality Analysis ----

# Load packages required for this session into library
library(vosonSML)
library(magrittr)
library(tidytext)
library(igraph)
library(tuber)

# Set up Twitter authentication variables
my_app_name <- "7230ICT_Qin"
my_api_key <- "O2JTa6TiP4RqFTi5AhIjaEO5b"
my_api_secret <- "IIRS6pR8vakdvK7UE8SOCuNa2wW9NBAUdT3kanXYt3J43TCOgq"
my_access_token <- "1501187128038166532-Iwrrqjt1TDszF1MRES9cwNwSTvzN2u"
my_access_token_secret <- "ducOdck8wd3L6UAaKysiMEI3gj8DqFN8YaS7oYVuBZ4Nr"


#import twitter actor dataset
#TwitterSemantic_Ed_retweets.graphml
twitter_data<- ImportData("TwitterData_Retreets.rds", "twitter")
yt_data <- ImportData("YoutubeData.rds", "youtube")
# Create twomode (bimodal) network

twomode_network <- twitter_data %>% Create("twomode", removeTermsOrHashtags = c("#Edsheeran"))
twomode_graph <- twomode_network %>% Graph()

# Write graph to file

write.graph(twomode_graph, file = "TwitterTwomode.graphml", format = "graphml")


# Inspect the graph object

length(V(twomode_graph))
V(twomode_graph)$name


# Find all maximum components that are weakly connected

twomode_comps <- components(twomode_graph, mode = c("weak"))
twomode_comps$no
twomode_comps$csize
head(twomode_comps$membership, n = 30)


# Get sub-graph with most members

largest_comp <- which.max(twomode_comps$csize)

twomode_subgraph <- twomode_graph %>% 
    induced_subgraph(vids = which(twomode_comps$membership == largest_comp))


# Display top 20 nodes from the sub-graph ordered by degree centrality

sort(degree(twomode_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by closeness centrality

sort(closeness(twomode_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph, mode = "out"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by betweenness centrality

sort(betweenness(twomode_subgraph, directed = FALSE), decreasing = TRUE)[1:20]


