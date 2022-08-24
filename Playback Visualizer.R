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
        arrange(dt)

artists_by_month <- plb %>%
  filter(song == TRUE) %>%
    group_by(month, artist) %>%
      summarise(min_played = sum(ms_played) / 60000,
                count_played = n()) %>%
    ungroup()

top_artists <- artists_by_month %>%
  arrange(desc(min_played)) %>%
    distinct(month, .keep_all = TRUE) %>%
      filter(count_played > 5) %>%
        arrange(month)

pods_by_month <- plb %>%
  filter(podcast == TRUE) %>%
    group_by(month, artist) %>%
      summarise(min_played = sum(ms_played) / 60000,
                count_played = n()) %>%
    ungroup()

top_pods <- pods_by_month %>%
  arrange(desc(min_played)) %>%
    distinct(month, .keep_all = TRUE) %>%
      filter(count_played > 5) %>%
        arrange(month)

top_20_artists <- plb %>%
  group_by(artist) %>%
    summarise(total_min_played = sum(ms_played) / 60000,
              discovery = min(dt)) %>%
      arrange(desc(total_min_played)) %>%
        head(20) %>%
  ungroup()

top_20_over_years <- plb %>%
  filter(artist %in% top_20_artists$artist) %>%
    group_by(artist, year, podcast) %>%
      summarise(min_played = sum(ms_played) / 60000) %>%
    ungroup()

top_20_over_years$media_type[top_20_over_years$podcast == TRUE] <- "Podcast"
top_20_over_years$media_type[top_20_over_years$podcast == FALSE] <- "Artist"

top_20_over_years <- merge(top_20_over_years, top_20_artists, by = "artist")

plot1 <- ggplot(top_20_over_years,
  aes(x = year,
      y = reorder(artist, discovery),
      fill = media_type)) +
  geom_raster(aes(alpha = min_played), hjust = 1,
              vjust = 0.5, interpolate = FALSE) +
  scale_fill_manual(values = c("#078935", "#222e7b")) +
  scale_x_continuous(breaks=seq(2012,2022,2)) +
  labs(x = "Year",
       y = "Artists (In Order of When I Started Listening)",
       title = "Listening Over Time for my Top 20 Artists") +
  theme(legend.position = "none")

top_20_over_months <- plb %>%
  filter(artist %in% top_20_artists$artist & year > 2010) %>%
    mutate(date = date(dt))

top_20_over_months$media_type[top_20_over_months$podcast == TRUE] <- "Podcast"
top_20_over_months$media_type[top_20_over_months$podcast == FALSE] <- "Artist"

top_20_over_months <- merge(top_20_over_months, top_20_artists, by = "artist")

plot2 <- ggplot(top_20_over_months,
   aes(y = date,
       x = reorder(artist, total_min_played),
       color = media_type)) +
  geom_jitter(height = .2, width = .2, alpha = 0.05) +
  scale_color_manual(values = c("#078935", "#222e7b")) +
  coord_flip() +
  labs(y = "Year",
       x = "Artists (In Order of When I Started Listening)",
       title = "Listening Over Time for my Top 20 Artists") +
  theme(legend.position = "none")
  
plot2






