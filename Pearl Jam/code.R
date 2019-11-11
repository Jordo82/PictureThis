library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)
library(broom)
library(ggrepel)
library(spotifyr)
library(jpeg)
library(grid)
library(gridExtra)

#grab a custom function for captioning plots
source("functions/functions.R")


#First, you'll need a Spotify developer account.  Sign up here
#https://developer.spotify.com/my-applications/#!/applications
#Then, input your client id and secret below
Sys.setenv(SPOTIFY_CLIENT_ID = 'x')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'x')

#get an access token
access_token <- get_spotify_access_token()

#find the artist id for Pearl Jam
artist_id <- search_spotify("Pearl Jam", "artist", authorization = access_token) %>% 
  top_n(1, popularity) %>% 
  pull(id)

#get all of the albums for the artist
albums <- get_artist_albums(artist_id, authorization = access_token) %>% 
  #do some cleanup to limit to the official albums
  filter(name %in% c("Ten", "Vitalogy", "Vs.", "No Code", "Yield", "Binaural",
                     "Riot Act", "Pearl Jam", "Backspacer", "Lightning Bolt"),
         release_date != ymd(20090101))


albums$color <- NA
#for each album, download the album art and extract the mean color
for(i in seq_len(nrow(albums))){
  
  tf <- tempfile(fileext = ".jpg")
  download.file(albums$images[[i]]$url[1], destfile = tf, mode = "wb")
  art <- readJPEG(tf)
  
  #the art file gets stored as an x by y by 3 array, with one dimension each for RGB.
  #By taking the mean value of each RGB dimension and combining them, we get the average color
  albums$color[i] <- rgb(mean(art[,,1]),mean(art[,,2]),mean(art[,,3]))
}
rm(tf, art)


#create a tibble to store data for all tracks on these albums
tracks <- tibble()

#loop through the albums and get all track data
for(i in seq_len(nrow(albums))){
  tracks <- tracks %>% 
    bind_rows({
      get_album_tracks(albums$id[i], authorization = access_token) %>% 
        mutate(album_id = albums$id[i])
    })
}


#create a tibble to store data for all the features of each track
features <- tibble()

#loop through the tracks and get their features
for(i in seq_len(nrow(tracks))){
  features <- features %>% 
    bind_rows(get_track_audio_features(tracks$id[i], authorization = access_token))
}

#join the desireable features onto the track data
tracks <- tracks %>% 
  left_join({
    features %>% 
      select(id, danceability, energy, loudness, speechiness, acousticness, 
             instrumentalness, liveness, valence)
  }, by = "id")



#conduct a principal components analysis of the track features and extract the first 2 PCs
tracks <- tracks %>% 
  bind_cols({
    tracks %>% 
      select(danceability, energy, loudness, speechiness, acousticness, 
             instrumentalness, liveness, valence) %>% 
      prcomp(center = TRUE, scale = TRUE) %>% 
      tidy("x") %>% 
      spread(key = PC, value = value) %>% 
      select(PC1 = `1`, PC2 = `2`)
  }) %>% 
  #do a little cleanup on track names
  mutate(name2 = str_remove(name, " \\(Remastered\\)| - Remastered| - bonus track")) %>% 
  #join on the album color
  left_join(albums %>% select(id, color), by = c("album_id" = "id"))


#set the random seed to ensure reproducibility of the plot
set.seed(20190822)

#plot the tracks by their first two principal components, colored by the album
p <- tracks %>% 
  ggplot(aes(PC1, PC2)) + 
  geom_label_repel(aes(label = name2, fill = color),
                   segment.color = NA, color = "white", fontface = "bold", size = 3) + 
  #need to let ggplot know we're giving it the color hex codes directly
  scale_fill_identity() + 
  #make some general changes to the theme to make everything look nice
  theme_fivethirtyeight() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        panel.grid = element_blank()) + 
  labs(title = "\"I've figured out numbers and what they're for\"",
       subtitle = "The songs of Pearl Jam")

#grab the range of the plot so we can automatically annotate based on them
plot_range <- tibble(x = ggplot_build(p)$layout$panel_scales_x[[1]]$range$range,
                     y = ggplot_build(p)$layout$panel_scales_y[[1]]$range$range)

p <- p +
  #add some explainer text
  annotate("text", 
           x = min(plot_range$x) + 0.3 * abs(diff(plot_range$x)), 
           y = min(plot_range$y) + 0.05 * abs(diff(plot_range$y)),
           label = "Distance between songs is a rough measure of their similarity.\nX and Y position determined using the first two principal components from an analysis on song\ndanceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness & valence.\nLabels are colored by album, using the average color of the album art.",
           color = "grey40", size = 4, fontface = "bold", family = "sans") 



#output plot as png
ggsave("Pearl Jam/Pearl_Jam.png", 
       caption_plot(p, "github.com/jordanRupton/PictureThis", "Source: Spotify"), 
       width = 8, height = 4.5, dpi = 300, scale = 2)
