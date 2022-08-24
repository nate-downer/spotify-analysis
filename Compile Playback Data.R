# Call Libraries
library(tidyr)
library(dplyr)
library(lubridate)
library(tibble)
library(ggplot2)
library(rjson)

# Import Data
path <- "data/endsong_0.json"
stream0 <- fromJSON(file = path)

path <- "data/endsong_1.json"
stream1 <- fromJSON(file = path)

path <- "data/endsong_2.json"
stream2 <- fromJSON(file = path)

# Write endsong data to a dataframe
data <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(data) <- c("ts", "ms_played",
                    "track", "artist", 
                    "ended", "song", "podcast")

for(i in seq_len(length(stream0))){
  # Initial Values
  j <- nrow(data) + 1
  
  song <- TRUE
  podcast <- FALSE
  
  # Process for Songs
  tryCatch({
      data[j, 1] <- stream0[[i]][["ts"]]
      data[j, 2] <- stream0[[i]][["ms_played"]]
      data[j, 3] <- stream0[[i]][["master_metadata_track_name"]]
      data[j, 4] <- stream0[[i]][["master_metadata_album_artist_name"]]
      data[j, 5] <- stream0[[i]][["reason_end"]]
      data[j, 6] <- song
      data[j, 7] <- podcast}, 
    error = function(e){
      song <<- FALSE
      podcast <<- TRUE})
  
  # Process for Podcasts
  if (podcast == TRUE){
    tryCatch({
        data[j, 1] <- stream0[[i]][["ts"]]
        data[j, 2] <- stream0[[i]][["ms_played"]]
        data[j, 3] <- stream0[[i]][["episode_name"]]
        data[j, 4] <- stream0[[i]][["episode_show_name"]]
        data[j, 5] <- stream0[[i]][["reason_end"]]
        data[j, 6] <- song
        data[j, 7] <- podcast}, 
      error = function(e){
        podcast <<- FALSE})
  }
}

for(i in seq_len(length(stream1))){
  # Initial Values
  j <- nrow(data) + 1
  
  song <- TRUE
  podcast <- FALSE
  
  # Process for Songs
  tryCatch({
    data[j, 1] <- stream1[[i]][["ts"]]
    data[j, 2] <- stream1[[i]][["ms_played"]]
    data[j, 3] <- stream1[[i]][["master_metadata_track_name"]]
    data[j, 4] <- stream1[[i]][["master_metadata_album_artist_name"]]
    data[j, 5] <- stream1[[i]][["reason_end"]]
    data[j, 6] <- song
    data[j, 7] <- podcast}, 
    error = function(e){
      song <<- FALSE
      podcast <<- TRUE})
  
  # Process for Podcasts
  if (podcast == TRUE){
    tryCatch({
      data[j, 1] <- stream1[[i]][["ts"]]
      data[j, 2] <- stream1[[i]][["ms_played"]]
      data[j, 3] <- stream1[[i]][["episode_name"]]
      data[j, 4] <- stream1[[i]][["episode_show_name"]]
      data[j, 5] <- stream1[[i]][["reason_end"]]
      data[j, 6] <- song
      data[j, 7] <- podcast}, 
      error = function(e){
        podcast <<- FALSE})
  }
}

for(i in seq_len(length(stream2))){
  # Initial Values
  j <- nrow(data) + 1
  
  song <- TRUE
  podcast <- FALSE
  
  # Process for Songs
  tryCatch({
    data[j, 1] <- stream2[[i]][["ts"]]
    data[j, 2] <- stream2[[i]][["ms_played"]]
    data[j, 3] <- stream2[[i]][["master_metadata_track_name"]]
    data[j, 4] <- stream2[[i]][["master_metadata_album_artist_name"]]
    data[j, 5] <- stream2[[i]][["reason_end"]]
    data[j, 6] <- song
    data[j, 7] <- podcast}, 
    error = function(e){
      song <<- FALSE
      podcast <<- TRUE})
  
  # Process for Podcasts
  if (podcast == TRUE){
    tryCatch({
      data[j, 1] <- stream2[[i]][["ts"]]
      data[j, 2] <- stream2[[i]][["ms_played"]]
      data[j, 3] <- stream2[[i]][["episode_name"]]
      data[j, 4] <- stream2[[i]][["episode_show_name"]]
      data[j, 5] <- stream2[[i]][["reason_end"]]
      data[j, 6] <- song
      data[j, 7] <- podcast}, 
      error = function(e){
        podcast <<- FALSE})
  }
}


