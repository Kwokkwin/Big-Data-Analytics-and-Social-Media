
# Load packages required for this session into library

library(tuber)
library(vosonSML)
library(magrittr)
library(igraph)


# import youtube data
yt_data <- ImportData("YoutubeData.rds", "youtube")

yt_actor_network <- yt_data %>% Create("actor")
yt_actor_graph <- Graph(yt_actor_network)


# Transform into an undirected graph

undir_yt_actor_graph <- as.undirected(yt_actor_graph, mode = "collapse")

write.graph(undir_yt_actor_graph, file = "undir_youtubeActor_Ed.graphml", format = "graphml")

# Run Louvain algorithm

louvain_yt_actor <- cluster_louvain(undir_yt_actor_graph)


# See sizes of communities

sizes(louvain_yt_actor)


# Visualise the Louvain communities

plot(louvain_yt_actor, 
     undir_yt_actor_graph, 
     vertex.label = V(undir_yt_actor_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Run Girvan-Newman (edge-betweenness) algorithm

eb_yt_actor <- cluster_edge_betweenness(undir_yt_actor_graph)


# See sizes of communities

sizes(eb_yt_actor)


# Visualise the edge-betweenness communities

plot(eb_yt_actor,
     undir_yt_actor_graph, 
     vertex.label = V(undir_yt_actor_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Visualise the edge-betweenness hierarchy

yt_actor_graph2 <- yt_actor_graph
V(yt_actor_graph2)$name <- V(yt_actor_graph2)$screen_name
undir_yt_actor_graph2 <- as.undirected(yt_actor_graph2, mode = "collapse")
eb_yt_actor2 <- cluster_edge_betweenness(undir_yt_actor_graph2)

is_hierarchical(eb_yt_actor2)
as.dendrogram(eb_yt_actor2)
plot_dendrogram(eb_yt_actor2)

plot_dendrogram(eb_yt_actor2, mode = "dendrogram", xlim = c(1,20))

