# spotify_popularity

# install if not yet installed
install.packages("tidyverse")
install.packages("broom")
install.packages("lm.beta")


# load libraries
library(tidyverse)
library(broom)
library(lm.beta)

# read data
spotify_songs <- 
  read_csv(file = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")

# data snapshot
spotify_songs |>
  glimpse()

spotify_songs_clean <-
  spotify_songs |>
  group_by(track_id) |>
  filter(row_number() == 1) |>
  ungroup() |>
  select(-track_id, -track_album_id, -track_album_name,
        # -track_album_release_date, 
         -playlist_name, -playlist_id, -playlist_genre,
         -playlist_subgenre)


# data snapshot
spotify_songs_clean |>
  glimpse()


# -------------------------------------------------------------------
#write_csv(spotify_songs, here("spotify_songs.csv"))
#spotify_songs <- read_csv(here("spotify_songs.csv"))
# -------------------------------------------------------------------


# Who are the 10 artists with most tracks on Spotify
spotify_songs_clean |>
  group_by(track_artist) |> 
  summarize(nr_of_tracks = n()) |>
  arrange(desc(nr_of_tracks)) |>
  print(n=10)

spotify_songs_cleaner <-
  spotify_songs_clean |> 
  filter(track_popularity > 0) |> 
  mutate(date = as.Date(track_album_release_date)) 

lm(track_popularity ~ date, data = spotify_songs_cleaner) |>
  summary()

lm(danceability ~ energy + tempo, data = spotify_songs_cleaner) |>
  summary()



# Song popularity is on a scale between 0 and 100
# It is a complex (and not fully documented) function of
# various indicators, including number of times streamed.

# Which are the 10 most popular pop tracks on Spotify
spotify_songs_clean |>
  arrange(desc(track_popularity)) |>
  print(n=100)


# Who are the 10 most popular artists on Spotify
spotify_songs_clean |>
  group_by(track_artist) |> 
  summarize(artist_popularity = mean(track_popularity)) |>
  arrange(desc(artist_popularity)) |>
  print(n=10)

# What does the distribution of track popularity look like?
#   (excluding 0)
spotify_songs_pop |>
  filter(track_popularity > 0) |>
  ggplot() +
  geom_histogram(aes(x=track_popularity), 
                 color = "black", fill = "pink",
                 binwidth = 4)

spotify_songs |> 
  group_by(track_artist) |> 
  summarize(n_tracks = n(),
            mean_popularity = mean(track_popularity)) |>
  arrange(desc(n_tracks))


# What makes for a popular pop song?

spotify_songs_clean |>
  select(#track_popularity,
         danceability,
         energy,
         loudness,
         speechiness,
         acousticness,
         instrumentalness,
         liveness,
         valence,
         tempo,
         duration_ms) |>
  cor() |> round(2)

spotify_songs |>
  ggplot() +
  geom_point(aes(x = energy, y = tempo)) +
  facet_wrap(facets = "playlist_genre")



model_01 <- 
  lm(formula =  danceability ~  energy + loudness + speechiness + 
       tempo,
     data = spotify_songs_clean) 
# The basic results
model_01
# Funny enough, a summary provide more details
model_01 |> summary()


model_02 <- 
  lm(formula = track_popularity ~ danceability + energy + mode, 
     data = spotify_songs_pop) 
# Funny enough, a summary provide more details
model_02 |> summary()


model_03 <- 
  lm(formula = track_popularity ~ danceability + energy + mode, 
     data = spotify_songs_pop) 
model_03 |> summary()

# Add interaction term
model_03 <- 
  lm(formula = track_popularity ~ danceability + energy + mode +
       mode_danceability, 
     data = spotify_songs_pop |>
       mutate(mode_danceability = mode * danceability)) 
model_03 |> summary()

model_03 <- 
  lm(formula = track_popularity ~ danceability + energy + mode +
       mode:danceability, 
     data = spotify_songs_pop) 
model_03 |> summary()

model_03 <- 
  lm(formula = track_popularity ~ danceability*mode + energy, 
     data = spotify_songs_pop) 
model_03 |> summary()

# standardized effects
spotify_songs_pop_std <-
  spotify_songs_pop |>
  mutate(across(.cols = c(track_popularity,
                         danceability,
                         energy,
                         loudness,
                         speechiness,
                         acousticness,
                         instrumentalness,
                         liveness,
                         valence,
                         tempo,
                         duration_ms),
                .fns = ~ (.x - mean(.x)) / sd(.x) ))

model_full_std <- 
  lm(formula = track_popularity ~ danceability + energy + loudness +
       speechiness + acousticness + instrumentalness + liveness +
       valence + tempo + duration_ms, data = spotify_songs_pop_std)  

  model_full_std |> 
  tidy() |> 
  filter(term != "(Intercept)") |>
  ggplot() + 
  geom_point(aes(y = term, x = estimate)) +
  geom_errorbarh(aes(y = term, 
                     xmin = estimate - std.error,
                     xmax = estimate + std.error)) +
  geom_vline(aes(xintercept = 0), color = "red")  

  
  
    
# plotting effects
model_full <- 
  lm(formula = track_popularity ~ danceability + energy + loudness +
       speechiness + acousticness + instrumentalness + liveness +
       valence + tempo + duration_ms, data = spotify_songs) |> 
   tidy()




# Getting standardized coefficient is not standard, you need
#   the lm.beta function from the package with the same name
model_01 |> lm.beta() |> summary()

# As you should never copy andf paste results, how do you get
#   this table (properly formatted) into you text editor?

# The 'broom' package transforms you model output into 
#   dataframe

# glance function for overall results (e.g. R2)
#   1 row
# tidy function for coefficients
#   as many rows as there are variabes in the model
# augment function for residuals and predictions
#  as many rows as there are observations in your data

model_01_glanced <- model_01 |> glance

model_01_tidied <- model_01 |> tidy()

model_01_augmented <- model_01 |> augment()

model_01_tidied

model_full <- 
  lm(formula = track_popularity ~ danceability + energy + loudness +
       speechiness + acousticness + instrumentalness + liveness +
       valence + tempo + duration_ms, data = spotify_songs_pop) 

model_full |> 
  #lm.beta()  |> 
  tidy() |> 
  filter(term != "(Intercept)") |>
  ggplot() + 
  geom_point(aes(y = term, x = estimate)) +
  geom_errorbarh(aes(y = term, 
                     xmin = estimate - std.error,
                     xmax = estimate + std.error))



model_full <- 
  lm(formula = danceability ~ energy + loudness +
       speechiness + acousticness + instrumentalness + liveness +
       valence + tempo + duration_ms, data = spotify_songs) 

model_full |> 
  lm.beta()  |> 
  tidy() |> 
  filter(term != "(Intercept)") |>
  ggplot() + 
  geom_point(aes(y = term, x = std_estimate)) +
  geom_errorbarh(aes(y = term, 
                     xmin = std_estimate - std.error,
                     xmax = std_estimate + std.error))




hist(spotify_songs$mode)
hist(spotify_songs$key)

# What is the distribution of mode (major = 1, minor = 0)
spotify_songs |> count(mode)

# Can we predict whether a song is in a major key from its other
#  attributes?
model_mode <-
  glm(formula = mode ~ energy + loudness +
             speechiness + acousticness + instrumentalness + liveness +
             valence + tempo + duration_ms, 
           data = spotify_songs,
           family = binomial(link = "logit"))
# Results
model_mode |> summary()
# Tidy the coefficients
model_mode |> 
  tidy(exponentiate = TRUE, conf.int = TRUE)
# Overall model statistics (AIC, BIC, ..)
model_mode |>
  glance()
model_mode |> 
  tidy() |> 
  filter(term != "(Intercept)") |>
  ggplot() + 
  geom_point(aes(y = term, x = estimate)) +
  geom_errorbarh(aes(y = term, 
                     xmin = estimate - std.error,
                     xmax = estimate + std.error)) +
  geom_vline(aes(xintercept = 0), color = "red")  



