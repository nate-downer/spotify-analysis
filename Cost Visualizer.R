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
  filter(dt >= as.Date("2017-05-01")) %>%
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

# Format date and add min and max values
hours_by_month <- hours_by_month %>%
  mutate(dt = as.Date(paste(month, "-01", sep = "")),
         rolling_hours = rolling_hours + (mean(hours) - mean(rolling_hours)),
         difference = abs(hours - rolling_hours),
         lower = rolling_hours,
         upper = rolling_hours)

hours_by_month$lower[
    hours_by_month$hours < hours_by_month$rolling_hours
  ] <- hours_by_month$hours[
    hours_by_month$hours < hours_by_month$rolling_hours
  ] - 1

hours_by_month$lower[
    hours_by_month$hours >= hours_by_month$rolling_hours
  ] <- hours_by_month$rolling_hours[
    hours_by_month$hours >= hours_by_month$rolling_hours
  ] - 1

hours_by_month$upper[
    hours_by_month$hours >= hours_by_month$rolling_hours
  ] <- hours_by_month$hours[
    hours_by_month$hours >= hours_by_month$rolling_hours
  ] + 1

hours_by_month$upper[
    hours_by_month$hours < hours_by_month$rolling_hours
  ] <- hours_by_month$rolling_hours[
    hours_by_month$hours < hours_by_month$rolling_hours
  ] + 1

hours_by_month$lower[hours_by_month$lower < 8] <- 8

# Plot hours of listening over time
p_hours <- ggplot(hours_by_month,
    aes(x = dt)) +
  geom_ribbon(aes(x = dt,
                  ymin = lower,
                  ymax = upper),
              alpha = 0.2) +
  geom_line(aes(y = rolling_hours)) +
  labs(x = "Year",
       y = "Hours of Listening per Month") +
  theme(legend.position = "none",
        plot.margin = margin(-0.3,0.3,0.3,0.55, "cm"))

# Plot money paid per hour of listening
hours_by_month$plan <- "Premium"
hours_by_month$plan[hours_by_month$dt < as.Date("2018-06-01")] <- "Student"
hours_by_month$plan[hours_by_month$dt > as.Date("2022-06-01")] <- "Duo"

hours_by_month$price[hours_by_month$plan == "Premium"] <- 9.99
hours_by_month$price[hours_by_month$plan == "Student"] <- 4.99
hours_by_month$price[hours_by_month$plan == "Duo"] <- 6.49

hours_by_month <- hours_by_month %>%
  mutate(per_hour = price / hours,
         per_rolling_hour = price / rolling_hours,
         per_upper = price / upper,
         per_lower = price / lower)

# Add Mean amount payed by plan
hours_by_month$mean[hours_by_month$plan == "Premium"] <- 
  sum(hours_by_month$price[hours_by_month$plan == "Premium"]) /
  sum(hours_by_month$hours[hours_by_month$plan == "Premium"])

hours_by_month$mean[hours_by_month$plan == "Student"] <- 
  sum(hours_by_month$price[hours_by_month$plan == "Student"]) /
  sum(hours_by_month$hours[hours_by_month$plan == "Student"])

hours_by_month$mean[hours_by_month$plan == "Duo"] <- 
  sum(hours_by_month$price[hours_by_month$plan == "Duo"]) /
  sum(hours_by_month$hours[hours_by_month$plan == "Duo"])

p_cost <- ggplot(hours_by_month,
                  aes(x = dt)) +
  geom_ribbon(aes(x = dt,
                  ymin = per_upper,
                  ymax = per_lower),
              alpha = 0.2) +
  geom_line(aes(y = per_rolling_hour)) +
  geom_line(aes(y = mean,
                color = plan),
            linetype = "dashed",
            size = 1,
            alpha = 0.7) +
  scale_color_discrete(labels = c("Spotify  Duo: $0.22",
                                  "Spotify Premium: $0.31",
                                  "Spotify Student: $0.14")) +
  guides(color = guide_legend(title = "Average Cost by Plan")) +
  labs(x = "",
       y = "Cost per Hour of Listening",
       legend = "Aver Cost per Month by Plan") +
    theme(legend.position = "top",
          axis.text.x = element_blank(),
          plot.margin = margin(0,.3,0,0.3, "cm"))


# Show Plots
title = ggdraw() + draw_label("Cost per Hour of Listening (2017-2022)")
grid2 = plot_grid(title, p_cost, p_hours,
                  nrow = 3, rel_heights = c(0.07, 1, 0.6))

grid2




