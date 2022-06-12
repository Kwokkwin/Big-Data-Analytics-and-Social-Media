
# Decision Tree -----------------------------------------------------------

library(spotifyr)
library(C50)
library(caret)
library(e1071)
library(dplyr)


# Set up Spotify authentication variables

app_id <- "98eb7401a0f242e4a8dd02c28a5c1d87"
app_secret <- "8365ed32f13e47bebf909b431862775d"
token <- "1"


# Authenticate to Spotify using the spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


# Get songs from EdSheeran and their audio features

EdSheeran_features <- get_artist_audio_features("EdSheeran")
View(EdSheeran_features)

data.frame(colnames(EdSheeran_features))

EdSheeran_features_subset <- EdSheeran_features[ , 9:20]
View(EdSheeran_features_subset)

# Get songs from ShawnMendes and their audio features

ShawnMendes_features <- get_artist_audio_features("ShawnMendes")
View(ShawnMendes_features)

data.frame(colnames(ShawnMendes_features))

ShawnMendes_features_subset <- ShawnMendes_features[ , 9:20]
View(ShawnMendes_features_subset)

# Get top 100 songs and their audio features

top100_features <- get_playlist_audio_features("spotify", "4hOKQuZbraPDIfaGbM3lKI")
View(top100_features)

data.frame(colnames(top100_features))

top100_features_subset <- top100_features[ , 6:17]
View(top100_features_subset)

top100_features_subset <- top100_features_subset %>% rename(track_id = track.id)


# Add the 'Edsheeran' column (class variable) to each data frame
# to indicate which songs are by EdSheeran and which are not

top100_features_subset["isEdSheeran"] <- 0
EdSheeran_features_subset["isEdSheeran"] <- 1
ShawnMendes_features_subset["isEdSheeran"] <- 0


# Remove any songs by EdSheeran that appear in the top 100
# and combine the two data frames into one dataset

top100_features_noEdSheeran <- anti_join(top100_features_subset,
                                     EdSheeran_features_subset,
                                     by = "track_id")
comb_data <- rbind(top100_features_noEdSheeran, EdSheeran_features_subset)
#comb_data <- rbind(top100_features_noEdSheeran, EdSheeran_features_subset,EdSheeran_features_subset)

# combine ShawnMendes and EdSheeran data frames into one dataset
comb_data_singers <- as_tibble(rbind(ShawnMendes_features_subset, EdSheeran_features_subset))

#comb_data_singers <- as_tibble(rbind(ShawnMendes_features_subset, EdSheeran_features_subset,EdSheeran_features_subset))
# Format the dataset so that we can give it as input to a model:
# change the 'isEdSheeran' column into a factor
# and remove the 'track_id' column

comb_data$isEdSheeran <- factor(comb_data$isEdSheeran)
comb_data <- select(comb_data, -track_id)

comb_data_singers$isEdSheeran <- factor(comb_data_singers$isEdSheeran)
comb_data_singers <- select(comb_data_singers,-track_id)
# Randomise the dataset (shuffle the rows)

comb_data <- comb_data[sample(1:nrow(comb_data)), ]
comb_data_singers <- comb_data_singers[sample(1:nrow(comb_data_singers)), ]
comb_data_total<-rbind(comb_data ,comb_data_singers)
# Split the dataset into training and testing sets (80% training, 20% testing)

split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]


split_point_singers <- as.integer(nrow(comb_data_singers)*0.8)
training_set_singers<- comb_data_singers[1:split_point, ]
testing_set_singers <- comb_data_singers[(split_point + 1):nrow(comb_data_singers), ]

split_point_total <- as.integer(nrow(comb_data_total )*0.8)
training_set_total <- comb_data_total [1:split_point, ]
testing_set_total  <- comb_data_total [(split_point + 1):nrow(comb_data_total ), ]

# Train the decision tree model

dt_model <- train(isEdSheeran~ ., data = training_set, method = "C5.0")

dt_model_singers <- train(isEdSheeran~ ., data = training_set_singers, method = "C5.0")

dt_model_total <- train(isEdSheeran~ ., data = training_set_total, method = "C5.0")

# Sample a single prediction (can repeat)

prediction_row <- 1 # MUST be smaller than or equal to training set size

if (tibble(predict(dt_model, testing_set[prediction_row, ])) ==
    testing_set[prediction_row, 12]){
    print("Prediction is correct!")
} else {
    ("Prediction is wrong")
}


if (tibble(predict(dt_model_singers, testing_set_singers[prediction_row, ])) ==
    testing_set_singers[prediction_row, 12]){
    print("Prediction is correct!")
} else {
    ("Prediction is wrong")
}

if (tibble(predict(dt_model_total, testing_set_total[prediction_row, ])) ==
    testing_set_total[prediction_row, 12]){
    print("Prediction is correct!")
} else {
    ("Prediction is wrong")
}


# Analyse the model accuracy with a confusion matrix

group1_situation1<-confusionMatrix(dt_model, reference = testing_set$isEdSheeran)

group1_situation2<-confusionMatrix(dt_model_singers, reference = testing_set_singers$isEdSheeran)

group1_situation3<-confusionMatrix(dt_model_total, reference = testing_set_singers$isEdSheeran)

accuracies<-cbind.data.frame(c(1,2,3,4,5,6),as.numeric(c(unlist(group1_situation1)[[4]],unlist(group1_situation2)[[4]],unlist(group1_situation3)[[4]],unlist(group2_situation1)[[4]],unlist(group2_situation2)[[4]],unlist(group2_situation3)[[4]])))
colnames(accuracies) <- c("Situations", "Accuracies")
plot(accuracies)

