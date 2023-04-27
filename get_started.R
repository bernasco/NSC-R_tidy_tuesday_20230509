
# get_started.R ----------------------------------------------------------------

# load libraries
library(tidyverse)

spotify_songs <- 
  read_csv(file = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")

spotify_songs |>
  glimpse()

# you can start exploring the data here ----------------------------------------
