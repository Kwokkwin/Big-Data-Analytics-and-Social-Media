# Load packages required for this session into library
library(tuber)
library(vosonSML)
library(magrittr)
library(igraph)

# Set up YouTube authentication variables 
api_key <- "AIzaSyCx7DXHIoWv2tycuW6D95fBvhEOgKbOPYI"
client_id <- "932099563195-jn3p9h065p0snmn0npe029hu487ihhvk.apps.googleusercontent.com"
client_secret <- "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"


# Authenticate to YouTube using the tuber package
# yt_oauth(app_id = client_id, app_secret = client_secret, token = "")
yt_oauth(app_id = client_id, app_secret = client_secret)


# Search YouTube
video_search <- yt_search("#EdSheeran")

View(video_search)


# Pick all video ids from video_search
video_ids <- as.vector(video_search$video_id)

# some stats without likecomments
stats4=NULL
stats5=NULL
tmp=NULL
num=length(video_ids)
for  (i in 1:num){
    tmp<-get_stats(video_ids[i])
    len<-length(tmp)
    if(len==4){
        stats4<-rbind.data.frame(stats4,tmp)
    } else {
        stats5<-rbind.data.frame(stats5,tmp)
    }
}

#get the max viewCount and likeCount
stats4<-add_column(.data=stats4, likeCount=0, .after = "viewCount")                   
stats<-rbind(stats4,stats5)
    
#get the videoid of max viewCount and likeCount  
videoid_max_likeCount<-stats[stats$likeCount == max(stats$likeCount),]
videoid_max_viewCount<-stats[stats$viewCount == max(stats$viewCount),]

#highest number of likes.
video_search[video_search$video_id == videoid_max_likeCount$id,c("video_id","title")]
#highest number of views 
video_search[video_search$video_id == videoid_max_viewCount$id,c("video_id","title")]

# plot likeCount and viewCount scatter plot
plot(stats$likeCount,stats$viewCount,main="Video Likes vs Views",xlab = "Likes", ylab = "Views", pch = 19, frame = FALSE)
lines(lowess(stats$likeCount, stats$viewCount), col = "blue")

fivenum(as.numeric(stats$likeCount))
fivenum(as.numeric(stats$viewCount))

# Choose some videos band store their video IDs,
# for which we want to collect comments
# and build an actor network

video_ids <- as.vector(video_search$video_id[1:8])

yt_data <- Authenticate("youtube", apiKey = api_key) %>%
    Collect(videoIDs = video_ids,
            writeToFile = TRUE,
            maxComments = 500)

View(yt_data)


