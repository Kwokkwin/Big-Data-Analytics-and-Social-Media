# Part 2: Spotify artist analysis ----

# Load packages required for this session into library

library(Rspotify)
library(spotifyr)
library(magrittr)
library(igraph)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggridges)


# Configure application to store Spotify authentication data in cache

options(httr_oauth_cache = TRUE)


# Set up authentication variables

app_id <- "98eb7401a0f242e4a8dd02c28a5c1d87"
app_secret <- "8365ed32f13e47bebf909b431862775d"
token <- "1"


# Authentication for Rspotify package:

keys <- spotifyOAuth(token, app_id, app_secret) 


# Get Spotify data on 'Ed Sheeran'

find_my_artist <- searchArtist("EdSheeran", token = keys)
View(find_my_artist)


# Retrieve information about artist

my_artist <- getArtist("6eUKZXaKkcviH0Ku9w2n3V", token = keys)
View(my_artist)


# Retrieve album data of artist
albums <- getAlbums("6eUKZXaKkcviH0Ku9w2n3V", token = keys) 
View(albums)


# Retrieve songs from all albums
album_ids <- getAlbums("6eUKZXaKkcviH0Ku9w2n3V", token = keys) %>% select(id)

#album_ids<-unlist(album_ids)

for (i in 1:nrow(album_ids))
{
    album_songs <- getAlbum(sapply(album_ids,'[[',i), token = keys) %>%  select(id,name) 
    if(i==1) {
        all_songs<-album_songs
    } else {
        all_songs<-rbind.data.frame(all_songs,album_songs)
    }
}

View(all_songs)
nrow(all_songs)
sum(duplicated(all_songs$name))
length(unique(all_songs$name))
all_songs <- filter(all_songs,!duplicated(all_songs$name))
View(all_songs)

# Retrieve song data. for songs features analyst in the future
# songs=""
# for (song_id in all_songs$id)
# {
#     song <- getFeatures(song_id, token = keys) 
#     if(length(songs)==1) {
#         songs=song
#     } else {
#         songs=bind_rows(songs,song)
#     } 
# }
# 
# View(songs)





# Authentication for spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


# Get audio features for 'Ed Sheeran'

audio_features <- get_artist_audio_features("EdSheeran")

audio_features <- audio_features[!duplicated(audio_features$track_name), ]
View(audio_features)
fivenum(audio_features$tempo)
fivenum(audio_features$liveness)
#summary(audio_features)

# how many songs from audio_features
unique(audio_features$track_name)

# Plot happiness (valence) scores for each album

ggplot(audio_features, aes(x = valence, y = album_name)) +
    geom_density_ridges() +
    theme_ridges() +
    ggtitle("Happiness in Ed Sheeran Albums",
            subtitle = "Based on valence from Spotify's Web API")


# Retrieve information about related artists

related_bm <- getRelated("Ed Sheeran", token = keys)
View(related_bm)


# Create a network of artists related to the Top 100 artists

topsongs <- getPlaylistSongs("spotify", "4hOKQuZbraPDIfaGbM3lKI", token = keys)

edges <- c()
for (artist in topsongs$artist){
    related <- getRelated(artist, token = keys)
    for (relatedartist in related$name){
        edges <- append(edges, artist)
        edges <- append(edges, relatedartist)
    }
}


# Convert network to graph and save as external file

related_artists_graph <- graph(edges)
write.graph(related_artists_graph, file = "RelatedArtists_spotify.graphml", format = "graphml")

# Transform into an undirected graph
undir_related_artists_graph <- as.undirected(related_artists_graph, mode = "collapse")
write.graph(undir_related_artists_graph, file = "undir_RelatedArtists_spotify.graphml", format = "graphml")

