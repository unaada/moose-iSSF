library(suncalc)
library(chron)
library(lubridate)


df <- read.csv("D:\\Users\\amand\\Documents\\qgis\\masters_qgis\\vectors\\crossing_time\\intersections_time.csv")
df$d_start <- as.Date(df$d_start, format = "%d/%m/%Y")

sunlight <- getSunlightTimes(date = df$d_start, lon = 25.45, lat = 59.05, 
                             keep = c("sunriseEnd", "sunset", "night", "nightEnd"),
                             tz = "Europe/Tallinn" )

final <- left_join(df, sunlight, join_by("d_start" == "date") , relationship = "many-to-many", multiple = "first")


#------------------------------------------ interpolation of crossing time---------------------------------

time <- read.csv("D:\\Users\\amand\\Documents\\qgis\\masters_qgis\\vectors\\crossing_time\\intersections_distance.csv")

# convert to time that is easy to do math operations
time$start_h <- chron(times = time$start_h)
time$end_h <- chron(times = time$end_h)

# find the time of crossing through linear interpolation 
time$interpolated_time <- time$start_h + 
  (time$distance_to_start / time$distance_of_track) * (time$end_h - time$start_h)

time$d_start <- as.Date(time$d_start, format = "%Y/%m/%d")



sunlight <- getSunlightTimes(
  date = steps2$date,
  lon = 25.45, 
  lat = 59.05, 
  keep = c("sunrise", "sunset", "dawn", "dusk"),
  tz = "Europe/Tallinn"
)


final <- left_join(time, sunlight, join_by("d_start" == "date") , relationship = "many-to-many", multiple = "first")


final <- final %>%
  mutate(
    sunrise = chron(times = format(sunrise, format = "%H:%M:%S")),
    sunset = chron(times = format(sunset, format = "%H:%M:%S")),
    dawn = chron(times = format(dawn, format = "%H:%M:%S")),
    dusk = chron(times = format(dusk, format = "%H:%M:%S")),

    time_category = case_when(
      interpolated_time >= sunrise & interpolated_time < sunset ~ "Day", # day
      (interpolated_time >= dawn & interpolated_time < sunrise) |
        (interpolated_time >= sunset & interpolated_time < dusk) ~ "Twilight", # twilight
      TRUE ~ "Night"),
    crossing_time = lubridate::hms(interpolated_time),
    month = lubridate::month(d_start, label = TRUE)
  )

# > crossing_counts_state
# Season
# Time       Spring Summer Winter
# Day           5      1      4
# Night        10      8     21
# Twilight      4      2      4
# > crossing_counts_local
# Season
# Time       Spring Summer Winter
# Day         117    215     59
# Night       143    133    341
# Twilight     44     77     46

# -------------------------------------------------------------------------------- make summary for thesis -------------------------------
summary_final <- final %>% 
  mutate(
    class_glmm = case_when(
      new_class >0 ~ "State",
      .default = "Local"
    )
  ) %>% 
  dplyr::select(fid, class_glmm, time_category, month, interpolated_time) %>% 
  filter(!(month == "sept"))


# Assign seasons (with November in Winter)
summary_final$season <- case_when(
  summary_final$month %in% c("Nov", "Dec", "Jan", "Feb", "Mar") ~ "Winter",
  summary_final$month %in% c("Apr", "May") ~ "Spring",
  summary_final$month %in% c("Jun", "Jul", "Aug") ~ "Summer"
)

# Create tables in one step
crossing_counts_state <- table(
  Time = summary_final$time_category[summary_final$class_glmm == "State"],
  Season = summary_final$season[summary_final$class_glmm == "State"]
)

crossing_counts_local <- table(
  Time = summary_final$time_category[summary_final$class_glmm == "Local"],
  Season = summary_final$season[summary_final$class_glmm == "Local"]
)

ggplot(summary_final, aes(x=month, y=as.POSIXct(as.character(interpolated_time), format = "%H:%M:%S"), color=time_category)) +
  geom_point(size=3, alpha=0.7) +
  scale_color_manual(values=c("Day"="yellow", "Night"="darkblue", "Twilight"="purple")) +
  labs(title="Crossing Observations by Time and Month",
       y="Time of Day", 
       x="Month",
       color="Time Category") +
  theme_minimal() +
  scale_y_datetime(date_labels="%H:%M", date_breaks="2 hour")

# ------------------------------------------------------------ visualise the crossings -----------------------------------


coul <- brewer.pal(6, "BuPu")
ggplot(final, aes(x = factor(new_class), fill = time_category)) +
  geom_bar(position = "fill") + 
  scale_fill_manual(
    values = c(coul[3], coul[4], coul[5]),
  ) + 
  labs(x = "Road class", y = "Proportion", fill = "Time category") +
  theme_minimal()

ggplot(final, aes(x = factor(new_class), y = crossing_time, color = time_category)) +
  geom_point(alpha = 0.6) + 
  scale_color_manual(values = c(coul[3], coul[4], coul[5])) +
  labs(x = "Road Class", y = "Hour of day", color = "Time category") +
  scale_y_time() +
  theme_minimal()


#chi-square ?
chi <- table(final$time_category, final$new_class)
chisq.test(chi)

# X-squared = 9.1278, df = 4, p-value = 0.05798





final$time_category <- ifelse(final$time_category == "Twilight", "Night", final$time_category)

chi_new <- table(final$time_category, final$new_class)
chi_test <- chisq.test(chi_new)

chi_test$residuals



# X-squared = 6.3294, df = 2, p-value = 0.04223

chi_table <- table( final$new_class, final$time_category)
chi_table
prop.table(chi_table, margin = 1)

chi_test2 <- chisq.test(chi_table)



#----------------------heatmaps ----------------------------------------------------------------------------------------------
tracks <- read.csv("D:\\Users\\amand\\Documents\\qgis\\masters_qgis\\vectors\\crossing_time\\moose_tracks.csv")
head(tracks)

# inspect the data completeness...
tracks %>%
  mutate(month = lubridate::month(start, label = TRUE)) %>%
  count(month) %>% 
  arrange(n)
  # filter(month == "Nov") %>%
  # summarise(num_days = n_distinct(as.Date(start)))

  

# -------------------------------------------------Filter for just August data at hour 20---------
august_hour20 <- tracks_filtered %>%
  filter(month == "Aug", hour == 20)

august_hour20_by_day <- august_hour20 %>%
  mutate(day_string = substr(start, 9, 10)) %>% # Extract just the day part manually
  group_by(day_string) %>%
  summarize(
    count = n(),
    avg_speed = mean(speed_kph),
    max_speed = max(speed_kph)
  ) %>%
  arrange(count)

august_hour20_by_day

# Identify the days with unusually high speeds
high_speed_threshold <- quantile(august_hour20$speed_kph, 0.9) # Top 10% speeds
print(paste0("High speed threshold: ", high_speed_threshold, " km/h"))

# Plot the daily maximum speeds in August at hour 20
ggplot(august_hour20_by_day, aes(x = day_string, y = avg_speed)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = high_speed_threshold, linetype = "dashed", color = "red") +
  labs(title = "Maximum Moose Speed by Day in August at 20:00",
       x = "Day of Month", 
       y = "Maximum Speed (km/h)") +
  theme_minimal()




# -------------------------------------------------Filter for just november data at hour 16---------
nov16<- tracks_filtered %>%
  filter(month == "Nov", hour == 16 | )

nov <- nov16 %>%
  mutate(day_string = substr(start, 9, 10)) %>% # Extract just the day part manually
  group_by(day_string) %>%
  summarize(
    count = n(),
    avg_speed = mean(speed_kph),
    max_speed = max(speed_kph)
  ) %>%
  arrange(desc(max_speed))

nov

# Identify the days with unusually high speeds
high_speed_threshold <- quantile(nov16$speed_kph, 0.9) # Top 10% speeds
print(paste0("High speed threshold: ", high_speed_threshold, " km/h"))

# Plot the daily maximum speeds in August at hour 20
ggplot(nov, aes(x = day_string, y = max_speed)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = high_speed_threshold, linetype = "dashed", color = "red") +
  labs(title = "Moose Speed by Day in Nov at 16:00",
       x = "Day of Month", 
       y = "Speed (km/h)") +
  theme_minimal()




# filter data based on the inspection...

tracks_filtered <- tracks %>%
  filter(dt_min <= 60) %>%
  mutate(
    month = factor(lubridate::month(start, label = TRUE)),
    hour = lubridate::hour(start)
  ) %>%
  filter(!(month == "Sep"))

heatmap_data <- tracks_filtered %>%
  group_by(month, hour) %>%
  summarise(
    total_time_min = sum(dt_min, na.rm = TRUE),
    avg_speed_kph = sum(speed_kph * dt_min, na.rm = TRUE) / total_time_min,
    .groups = "drop"
  ) 

heatmap_data %>%
  mutate(
    month = factor(
      month, 
      levels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>% 
ggplot(aes(x = month, y = hour, fill = avg_speed_kph)) +
  geom_tile() +
  coord_fixed(ratio = 0.4)+
  scale_fill_viridis_c() +
  labs(x = "Month", y = "Hour of day", fill = "Moose speed 
       (kmh)") +
  scale_x_discrete(labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  theme_minimal()

#------------------------------------------------- plotting for cutoff point -------------------------

# season classification
tracks_seasonal <- tracks_filtered %>%
  mutate(
    hour_half = hour + ifelse(minute(start) >= 30, 0.5, 0), 
    
    season = case_when(
      month %in% c("nov","dec", "janv", "febr", "marts") ~ "Winter",
      month %in% c("apr", "maijs") ~ "Spring",
      month %in% c("jūn", "jūl", "aug") ~ "Summer"
  ))

# hourly medians by season
seasonal_med <- tracks_seasonal %>%
  group_by(hour_half, season) %>%
  summarise(med_distance = median(distance_m), .groups = 'drop')

# plot with seasonal lines
ggplot(seasonal_med, aes(x = hour_half, y = med_distance)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 18, color = "red", linetype = "dashed") +
  facet_wrap(~season, ncol = 2) +
  labs(x = "Hour of day",
       y = "Median step length, m") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 23, by = 4))


# hourly means by season
seasonal_mean <- tracks_seasonal %>%
  group_by(hour_half, season) %>%
  summarise(mean_distance = mean(distance_m), .groups = 'drop')

# plot with seasonal lines
ggplot(seasonal_mean, aes(x = hour_half, y = mean_distance)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 18, color = "red", linetype = "dashed") +
  facet_wrap(~season, ncol = 2) +
  labs(x = "Hour of day",
       y = "Mean step length, m") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 23, by = 4))


# ---------------------------------------------------------- traffic heatmap -----------------------------


traffic <- read.csv("D:\\Users\\amand\\Documents\\qgis\\masters_qgis\\vectors\\crossing_time\\traffic_2069_2082.csv")

traffic <- traffic %>%
  mutate(
    ts = as.POSIXct(ts, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Tallinn"),
    month = factor(month(ts, label = TRUE, abbr = TRUE)),
    hour = hour(ts)
  ) %>%
  filter(!(month %in% c("Sep", "Oct"))) %>%                # Exclude September and October
  group_by(month, hour, siteno) %>%
  summarise(
    avg_total = mean(total, na.rm = TRUE),                # Average traffic per station
    .groups = "drop"
  )

traffic_aadt <- traffic %>%
  group_by(month, hour) %>%
  summarise(
    aadt = mean(avg_total, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    month = factor(
      month, 
      levels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")))

coul <- brewer.pal(9, "BuPu")

ggplot(traffic_aadt, aes(x = month, y = hour, fill = aadt)) +
  geom_tile() +
  coord_fixed(ratio = 0.4)+
  scale_fill_gradientn(colors = coul) +
  labs(
    x = "Month",
    y = "Hour of day",
    fill = "AADT on road 2
    2018-19 mean"
  ) +
  scale_x_discrete(labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  theme_minimal()


traffic2 <- read.csv("D:\\Users\\amand\\Documents\\qgis\\masters_qgis\\vectors\\crossing_time\\traffic_5_14_others.csv")



# ------------------------------------------------------------------------ FINDING WHERE TO SPLIT THE SEASONS -------------------


# Add a date column
tracks_filtered$date <- as.Date(tracks_filtered$start)

# Calculate daily average speeds
daily_speeds <- aggregate(speed_kph ~ date, data=tracks_filtered, FUN=mean)

# Add a day of year column for easier analysis
daily_speeds$doy <- as.numeric(format(daily_speeds$date, "%j"))

# Order by date
daily_speeds <- daily_speeds[order(daily_speeds$date),]

# Calculate 7-day moving average
library(zoo)
daily_speeds$ma7 <- rollmean(daily_speeds$speed_kph, 7, fill=NA)

# Plot for visualization
plot(daily_speeds$date, daily_speeds$speed_kph, type="l", 
     xlab="Date", ylab="Speed (kph)")
lines(daily_speeds$date, daily_speeds$ma7, col="blue", lwd=2)


winter_to_spring <- as.Date("2019-04-01")  # Winter to Spring
spring_to_summer <- as.Date("2019-06-01") 



# Create the plot
ggplot(daily_speeds, aes(x=date)) +
  theme_minimal() +
  geom_line(aes(y=speed_kph, color="Daily Speed"), alpha=0.6) +
  geom_line(aes(y=ma7, color="7-day Moving Average"), size=1.2) +
  geom_vline(xintercept = as.numeric(as.Date("2019-04-01")), 
             linetype = "dashed", color = "darkgreen", alpha = 0.7) +
  geom_vline(xintercept = as.numeric(as.Date("2019-07-01")), 
             linetype = "dashed", color = "orange", alpha = 0.7) +
  scale_color_manual(name="", 
                     values=c("Daily Speed"="gray40", 
                              "7-day Moving Average"="blue")) +
  
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  labs(title="Daily Movement Speed with 7-day Moving Average",
       x="Month",
       y="Speed (kph)") +
  theme(
    axis.text.x = element_text(angle=0, hjust=0.5, size=10),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust=0.5, size=14)
  )


# ---------------------------------------------- moose movement speed individual ---------------

tracks_filtered$month <- month(tracks_filtered$date, label = TRUE, abbr = TRUE)

# Order months correctly from November to August
month_order <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
tracks_filtered$month <- factor(tracks_filtered$month, levels = month_order)

# Calculate daily average speeds for each moose
daily_speeds_by_moose <- tracks_filtered %>%
  group_by(moose_id, date) %>%
  summarize(
    speed_kph = mean(speed_kph, na.rm = TRUE),
    month = first(month),
    .groups = 'drop'
  ) %>%
  arrange(moose_id, date)

# Calculate 7-day moving average for each moose
daily_speeds_by_moose <- daily_speeds_by_moose %>%
  group_by(moose_id) %>%
  mutate(ma7 = rollmean(speed_kph, 7, fill = NA, align = "center")) %>%
  ungroup()

# Visualization with individual panels for each moose
ggplot(daily_speeds_by_moose, aes(x = date, y = speed_kph, group = moose_id)) +
  geom_line(alpha = 0.6, color = "gray40") +
  geom_line(aes(y = ma7), color = "blue", size = 1.2) +
  facet_wrap(~ moose_id, scales = "free_y") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    expand = c(0.02, 0.02)
  ) +
  labs(
    title = "Movement speed for each moose",
    x = "Month",
    y = "Speed, km/h"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )