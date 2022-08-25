# Call Libraries
library(tidyr)
library(dplyr)
library(lubridate)
library(tibble)
library(ggplot2)
library(rjson)

# Clean playback data
plb <- data

plb <- plb %>%
  filter(ms_played > 0) %>%
  mutate(dt = strptime(ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
         year = year(dt),
         month = format.Date(dt, "%Y-%m")) %>%
  select(dt, ms_played, track, artist, ended, song, podcast, year, month) %>%
  filter(year >= 2017) %>%
  arrange(dt)

# Find my top 20 Songs of all time
top_20_songs <- plb %>%
  filter(song == TRUE) %>%
  group_by(track, artist) %>%
  summarise(play_count = n()) %>%
  arrange(desc(play_count)) %>%
  head(20) %>%
  ungroup()


# Plot plays of my T0p 20 songs over time
songs_over_time <- plb %>%
  filter(track %in% top_20_songs$track) %>%
  mutate(date = date(dt))

songs_over_time <- merge(songs_over_time, 
                         select(top_20_songs, track, play_count),
                         by = "track")

p_jitter2 <- ggplot(songs_over_time,
    aes(x = reorder(track, play_count),
        y = date,
        color = artist)) +
  geom_jitter(height = .3, width = .2, alpha = 0.4) +
  coord_flip() +
  labs(x = "Songs (In Order of Most Listens)",
       y = "Year",
       title = "Listening Over Time for my Top 20 Songs") +
  theme(legend.position = "none")


# Show Plot
p_jitter2

