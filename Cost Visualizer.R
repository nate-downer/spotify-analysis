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

# Find how many hours I listened to each month
hours_by_month <- plb %>%
  group_by(month) %>%
  summarise(hours = sum(ms_played / 3600000)) %>%
  mutate(rolling_hours = 0) %>%
  ungroup()

# Find the rolling 3 month average
x = 0
y3 = 0
y2 = 0
y1 = 0

for(i in 1:nrow(hours_by_month)){
  y3 = y2
  y2 = y1
  y1 = hours_by_month[i, 2]
  
  x = x + (y1/3) - (y3/3)
  
  hours_by_month[i, 3] = x
}  

hours_by_month <- hours_by_month %>%
  mutate(dt = as.Date(paste(month, "-01", sep = "")),
         lower = min(hours, rolling_hours),
         upper = max(hours, rolling_hours))

p_hours <- ggplot(hours_by_month,
    aes(x = dt,
        y = rolling_hours)) +
  geom_ribbon(aes(ymin = rolling_hours - 10,
                  ymax = rolling_hours + 10))

p_hours