# Call Libraries
library(tidyr)
library(dplyr)
library(lubridate)
library(tibble)
library(ggplot2)
library(rjson)
library(cowplot)

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

# Find the top artists and podcasts played each month
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

# Find the top 20 artists + podcasts of all time
top_20_artists <- plb %>%
  group_by(artist) %>%
  summarise(total_min_played = sum(ms_played) / 60000,
            discovery = min(dt)) %>%
  arrange(desc(total_min_played)) %>%
  head(20) %>%
  ungroup()


# Plot heat map of min listened by artist and time
top_20_over_years <- plb %>%
  filter(artist %in% top_20_artists$artist) %>%
  group_by(artist, year, podcast) %>%
  summarise(min_played = sum(ms_played) / 60000) %>%
  mutate(media_type = "") %>%
  ungroup()

top_20_over_years$media_type[top_20_over_years$podcast == TRUE] <- "Podcast"
top_20_over_years$media_type[top_20_over_years$podcast == FALSE] <- "Artist"

top_20_over_years <- merge(top_20_over_years, top_20_artists, by = "artist")

p_heat <- ggplot(top_20_over_years,
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


# Plot jitter plot of plays over time by artist 
top_20_over_time <- plb %>%
  filter(artist %in% top_20_artists$artist & year > 2010) %>%
  mutate(date = date(dt))

top_20_over_time$media_type[top_20_over_time$podcast == TRUE] <- "Podcast"
top_20_over_time$media_type[top_20_over_time$podcast == FALSE] <- "Artist"

top_20_over_time <- merge(top_20_over_time, top_20_artists, by = "artist")

p_jitter <- ggplot(top_20_over_time,
   aes(y = date,
       x = reorder(artist, total_min_played),
       color = media_type,
       alpha = ms_played)) +
  geom_jitter(height = .3, width = .2) +
  scale_color_manual(values = c("#1dab4f", "#566b5c")) +
  scale_y_date(date_breaks = "1 year",
               limits = as.Date(c("2017-03-01", "2022-06-01"))) +
  coord_flip() +
  labs(y = "",
       x = "TOp 20 Artists + Podcasts (In Order of Total Listening Time)") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        plot.margin = margin(0,.3,0, .3, "cm"))

# Plot hist of all songs played by month  
all_over_qtrs <- plb %>%
  mutate(quarter = paste(year(dt), "-", quarter(dt), sep = "")) %>%
  group_by(quarter, podcast) %>%
  summarise(min_played = sum(ms_played / 60000)) %>%
  ungroup()

first_months <- plb %>%
  mutate(quarter = paste(year(dt), "-", quarter(dt), sep = "")) %>%
  arrange(desc(month)) %>%
  group_by(quarter) %>%
  summarise(month = dplyr::first(month)) %>%
  mutate(date = as.Date(paste(month, "-01", sep = ""))) %>%
  ungroup()

all_over_qtrs <- merge(all_over_qtrs, first_months, by = "quarter")

all_over_qtrs <- all_over_qtrs %>%
  mutate(xmin = date,
         xmax = date %m+% months(3),
         ymin = 0,
         ymax = min_played)

for (i in 1:nrow(all_over_qtrs)) {
  if (all_over_qtrs[i, 2] == TRUE) {
    j <- (i - 1)
    all_over_qtrs[i, 8] <- all_over_qtrs[j, 3]
    all_over_qtrs[i, 9] <- (all_over_qtrs[j, 3] + all_over_qtrs[i, 3])
  }
}

p_hist <- ggplot(all_over_qtrs) +
  geom_rect(aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax,
                fill = podcast),
            alpha = 0.7) +
  scale_fill_manual(values = c("#1dab4f", "#566b5c")) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y",
               limits = as.Date(c("2017-03-01", "2022-06-01"))) +
  labs(x = "Year and Quarter",
       y = "Min Played") +
  theme(legend.position = "none",
        plot.margin = margin(-0.3,.3,0.2,3.65, "cm"),
        axis.text.x = element_text(angle = 60, hjust = 1))

# Show plot
title <- ggdraw() + draw_label("Listening Paterns Over Time")
grid1 = plot_grid(title, p_jitter, p_hist,
                  nrow = 3, rel_heights = c(0.07, 1, 0.25))

grid1





