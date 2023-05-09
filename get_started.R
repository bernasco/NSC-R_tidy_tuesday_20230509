
# get_started.R ----------------------------------------------------------------

# Load libraries
library(tidyverse)

spotify_songs <- 
  read_csv(file = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")

spotify_songs |>
  glimpse()

# Section added May 9, 2023 -----------------------------------------------

# This shows that songs can appear multiple times in the data
spotify_songs |>
  filter(track_artist == "Ed Sheeran", 
         track_name == "Thinking out Loud") |> 
  arrange(track_id) |> 
  select(track_artist, track_name, 
         track_album_name, playlist_name) 

# Create a clean version with songs un-duplicated and less colums
spotify_songs_clean <-
  # start with the raw data
  spotify_songs |>
  # define groups 
  group_by(track_artist, track_name) |>
  # select only the first instance of each group
  filter(row_number() == 1) |>
  # the grouping is no longer needed
  ungroup() |>
  # remove variables no longer needed
  select(-track_id, -track_album_id, -track_album_name,
         -track_album_release_date, 
         -playlist_name, -playlist_id, -playlist_genre,
         -playlist_subgenre) 

# Specify two columns as factors (nominal / categorical variables) 
spotify_songs_clean_fct <-
  spotify_songs_clean |>
  mutate(mode = factor(mode, 
                       levels = 0:1,
                       labels = c("minor", "major")),
         key = factor(key, 
                      levels = 0:11,
                      labels = c("C", "C#", "D", "D#", "E", "F",
                                 "F#", "G", "G#", "A", "A#", "B")))


# This is what the clean version looks like
spotify_songs_clean_fct |>
  glimpse()


# you can start exploring the data here ----------------------------------------
