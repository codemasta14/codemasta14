library(spotifyr)
library(tidyverse)
library(plotly)
codemasta14 <- '223lztjefpjmei42fvtwd3u7a'

#These are system parameters needed to have the authority to access the spotify api

Sys.setenv(SPOTIFY_CLIENT_ID = '0a7bf1a593ad499492291f3a7675aef7')
#Sys.setenv(SPOTIFY_CLIENT_SECRET = 'It\'s a secret')

#Pulls the first playlist from my spotify account, and creates a dataframe of all the track info for the music in that playlist
music <- get_user_playlists(codemasta14)[1,]%>%
  get_playlist_tracks()%>%
  get_track_audio_features()%>%
  left_join(get_user_playlists(codemasta14)[1,]%>%
              get_playlist_tracks(),by = "track_uri")
  
#normalizes the numeric data for distance comparison
scale_music <- music%>%
    select(-(16:21),18,-12)%>%
    select(-c(3,5,12,13,14,15))%>%
    scale()

#clusters data using only the normalize numeric data
#I need to learn more about clustering numeric data and categorical data together
#because different distance methods are used for each, so this is an incomplete clustering.
new_music <- mutate(music, cluster = as.factor(kmeans(scale_music, centers = 6)$cluster))

#Creates an interactive plot used for visualizing groups. Eventually I want to make a shiny app
#That can change the variables from a drop down list. 

new_music%>%
  plot_ly(x=~valence,y=~energy, color = ~cluster,
          
          text = ~paste("Track: ", track_name, '<br>Artist: ',artist_name, '<br>Group: ',cluster))%>%
  layout( legend = list(orientation = 'h'),
    images = list(
      list(source = "https://upload.wikimedia.org/wikipedia/commons/1/19/Spotify_logo_without_text.svg",
           xref = "paper",
           yref = "paper",
           x= .85,
           y= .3,
           sizex = 0.2,
           sizey = 0.2,
           opacity = 0.8
      )))

###This is what I hope to eventually be able to do.

#The add_to_playlist function does not exist yet.
#But in theory it just adds songs from a dataframe, or a list to a playlist that exists on spotify



#The Create_playlist function does not exist. It seems easy though. Look in the source code for the spotifyr 
#Functions, and it seems like you could potentially replace the word 'GET' with the word 'POST' Although
#I have a feeling it might be a bit more intensive than that.



#https://developer.spotify.com/console/playlists/ 
#Use POST with	/v1/users/{user_id}/playlists/{playlist_id}/tracks        to add tracks
#USE POST with 	/v1/users/{user_id}/playlists                             to create a playlist

create_playlist("Name of Playlist")

new_music%>%
  filter(cluster == 1)%>%
  select(track_uri)%>%
  add_to_playlist("Name of Playlist")

