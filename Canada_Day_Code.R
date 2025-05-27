# To install packages:
install.packages("remotes")
remotes::install_github("ropensci/weathercan")
install.packages(c("lutz", "sf"))
install.packages("tidyverse")

# To import libraries:
library(weathercan)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyverse)
library(tibble)

# To verify the latest updated station data:
stations_dl()

# To make a list of weather stations from each capital city:
stations_list <- list(
  "Ottawa" = c(4333),
  "Toronto" = c(26953, 5148),
  "Victoria" = c(113, 114),
  "Edmonton" = c(27214, 1867),
  "Halifax" = c(6456, 6435),
  "Winnipeg" = c(28051, 3698),
  "Yellowknife" = c(51058, 1706),
  "Whitehorse" = c(50842, 1617),
  "Regina" = c(28011, 3002),
  "St. John's" = c(48871, 6720),
  "Quebec City" = c(26892, 5251),
  "Charlottetown" = c(50621, 6526),
  "Fredericton" = c(30309, 6157),
  "Iqaluit" = c(42503, 1758)
)

# To download the average temperature data from weather stations of each capital city of Canada:
get_july1_data <- function(city_name, station_ids) {
  city_data <- lapply(station_ids, function(id) {
    weather_dl(station_ids = id, interval = "day", start = "1949-07-01", end = "2024-07-01")
  }) |>
    bind_rows() |>
    filter(month(date) == 7, day(date) == 1) |>
    mutate(
      city = city_name,
      year = year(date),
      month = month(date),
      day = day(date)
    ) |>
    group_by(year) |>      
    slice(1) |>            # If two weather stations of the same city have data for the same year it will take the first weather stations data.
    ungroup() |>
    select(city, station_name,station_id, year, month, day, mean_temp, lat, lon)
  return(city_data)
}

# To call the function and to combine the result into a single data frame:
all_cities_july1 <- bind_rows(
  lapply(names(stations_list), function(city) {
    get_july1_data(city, stations_list[[city]])
  })
)

# To get the total number of NAs (Missing Data) in the dataset:
sum(is.na(all_cities_july1))

# To remove the rows with NAs:
cleaned_data <- na.omit(all_cities_july1)

# To ensure the removal of NAs:
sum(is.na(cleaned_data))

# To save the dataset in .csv format:
write.csv(
  cleaned_data,
  "D:/GG502 InternshipRA/july01_temperatures_1949_2024.csv", 
  row.names = FALSE    # To ensure that dataset doesn't contain any row numbers.
)

# Further modifications of the datafile were done in excel, in order to have similar latitude and longitude attributes for each city.

#######################################################################################
####### Data Visualizations #######

# To import libraries:
install.packages("patchwork")
library(patchwork)
library(readxl)
library(tidyverse)
library(plotly)

# To read excel file
temperature_data <- read_excel("D:/GG502 InternshipRA/july01_temperatures_1949_2024.csv.xlsx")

### Atlantic Capital Cities ###

# To filter for St.John's:
filtered_df_c <- temperature_data %>%
  filter(city %in% c("St. John's"))

# To plot a line graph in red color with a black trendline:
p1 <- ggplot(filtered_df_c, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("St. John's" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, by = 5),
    expand = c(0, 0)
  ) +
  labs(
    title = "Canada Day Average Temperatures in Atlantic Canadian Capital Cities, 1949-2024",
    caption = "St. John's",
    x = "Year",
    y = "Mean Temperature (°C)",
    color = NULL   
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 30),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )

# To filter for Charlottetown:
filtered_df <- temperature_data %>%
  filter(city %in% c("Charlottetown"))

# To plot a line graph in red color with a black trendline:
p2 <- ggplot(filtered_df, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Charlottetown" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, by = 5),
    expand = c(0, 0)
  ) +
  labs(
    caption = "Charlottetown",
    x = "Year",
    y = "Mean Temperature (°C)",
    color = NULL   
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 30),
    legend.position = "none",
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )

# To filter for Halifax:
filtered_df_b <- temperature_data %>%
  filter(city %in% c("Halifax"))

# To plot a line graph in red color with a black trendline:
p3 <- ggplot(filtered_df_b, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Halifax" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, by = 5),
    expand = c(0, 0)
  ) +
  labs(
    caption = "Halifax",
    x = "Year",
    y = "Mean Temperature (°C)",
    color = NULL   
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 30),
    legend.position = "none",
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )

# To filter for Fredericton:
filtered_df_a <- temperature_data %>%
  filter(city %in% c("Fredericton"))

# To plot a line graph in red color with a black trendline:
p4 <- ggplot(filtered_df_a, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Fredericton" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 30),
    breaks = seq(0, 30, by = 5)
  ) +
  labs(
    caption = "Fredericton",
    x = "Year",
    y = "Mean Temperature (°C)",
    color = NULL   
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 30),
    legend.position = "none",
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )

# To stack the graphs on top of each other:
(p1 / plot_spacer() / p2 / plot_spacer() / p3 / plot_spacer() / p4) +
  plot_layout(heights = c(1, 0.15, 1, 0.15, 1, 0.15, 1)) & 
  theme(
    plot.background = element_rect(fill = "#FAF2E6", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

### Western Capital Cities ###

# To filter for Edmonton:
filtered_df_c <- temperature_data %>%
  filter(city %in% c("Edmonton"))

# To plot a line graph in red color with a black trendline:
p1 <- ggplot(filtered_df_c, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Edmonton" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0,30),
    breaks = seq(0, 30, by = 5)
  ) +
  labs(
    caption = "Edmonton",
    x = "Year",
    y = "Mean Temperature (°C)",
    title = "Canada Day Average Temperatures in Western Canadian Capital Cities, 1949-2024",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 35),
    legend.position = "none",
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5)
  )

# To filter for Regina:
filtered_df_d <- temperature_data %>%
  filter(city %in% c("Regina"))

# To plot a line graph in red color with a black trendline:
p2 <- ggplot(filtered_df_d, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Regina" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0,30),
    breaks = seq(0, 30, by = 5)
  ) +
  labs(
    caption = "Regina",
    x = "Year",
    y = "Mean Temperature (°C)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 35),
    legend.position = "none",
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28)
  )

# To filter for Victoria:
filtered_df_e <- temperature_data %>%
  filter(city %in% c("Victoria"))

# To plot a line graph in red color with a black trendline:
p3 <- ggplot(filtered_df_e, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Victoria" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0,30),
    breaks = seq(0, 30, by = 5)
  ) +
  labs(
    caption = "Victoria",
    x = "Year",
    y = "Mean Temperature (°C)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 35),
    legend.position = "none",
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28)
  )

# To filter for Winnipeg:
filtered_df_f <- temperature_data %>%
  filter(city %in% c("Winnipeg"))

# To plot a line graph in red color with a black trendline:
p4 <- ggplot(filtered_df_f, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Winnipeg" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0,30),
    breaks = seq(0, 30, by = 5)
  ) +
  labs(
    caption = "Winnipeg",
    x = "Year",
    y = "Mean Temperature (°C)",
    color = NULL
  ) +
  theme_minimal()+
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 35),
    legend.position = "none",
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28)
)

# To stack the graphs on top of each other:
(p1 / plot_spacer() / p2 / plot_spacer() / p3 / plot_spacer() / p4) +
  plot_layout(heights = c(1, 0.05, 1, 0.05, 1, 0.05, 1)) &
  theme(
    plot.background = element_rect(fill = "#FAF2E6", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

### Central Capital Cities ###

# To filter for Quebec City:
filtered_df_b <- temperature_data %>%
  filter(city %in% c("Quebec City"))

# To plot a line graph in red color with a black trendline:
p1<- ggplot(filtered_df_b, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Quebec City" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0,30),
    breaks = seq(0,30, by = 5),
    expand = c(0, 0)
  ) +
  labs(
    caption = "Quebec City",
    x = "Year",
    y = "Mean Temperature (°C)",
    title = "Canada Day Average Temperatures in Central Canadian Capital Cities, 1949-2024",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 35),
    legend.position = "none",
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5)
  )

# To filter for Ottawa:
filtered_df_a <- temperature_data %>%
  filter(city %in% c("Ottawa"))

# To plot a line graph in red color with a black trendline:
p2 <- ggplot(filtered_df_a, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Ottawa" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0,30),
    breaks = seq(0,30, by = 5),
    expand = c(0, 0)
  ) +
  labs(
    caption = "Ottawa",
    x = "Year",
    y = "Mean Temperature (°C)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 35),
    legend.position = "none",
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
  )

# To filter for Toronto:
filtered_df <- temperature_data %>%
  filter(city %in% c("Toronto"))

# To plot a line graph in red color with a black trendline:
p3 <-ggplot(filtered_df, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Toronto" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0,30),
    breaks = seq(0,30, by = 5),
    expand = c(0, 0)
  ) +
  labs(
    caption = "Toronto",
    x = "Year",
    y = "Mean Temperature (°C)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 35),
    legend.position = "none",
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
  )

# To stack the graphs on top of each other:
(p1 / plot_spacer() / p2 / plot_spacer() / p3) +
  plot_layout(heights = c(1, 0.15, 1, 0.15, 1)) &
  theme(
    plot.background = element_rect(fill = "#FAF2E6", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

### Northern Capital Cities ###

# To filter for Iqaluit:
filtered_df_a <- temperature_data %>%
  filter(city %in% c("Iqaluit"))

# To plot a line graph in red color with a black trendline:
p1 <- ggplot(filtered_df_a, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Iqaluit" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, by = 5),
    expand = c(0, 0)
  ) +
  labs(
    title = "Canada Day Average Temperatures in Northern Canadian Capital Cities, 1949-2024",
    caption = "Iqaluit",
    x = "Year",
    y = "Mean Temperature (°C)",
    color = NULL   
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 35),
    plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
    legend.position = "none",
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )

# To filter for Yellowknife:
filtered_df <- temperature_data %>%
  filter(city %in% c("Yellowknife"))

# To plot a line graph in red color with a black trendline:
p2 <- ggplot(filtered_df, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Yellowknife" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, by = 5),
    expand = c(0, 0)
  ) +
  labs(
    caption = "Yellowknife",
    x = "Year",
    y = "Mean Temperature(°C)",
    color = NULL   
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 30),
    legend.position = "none",
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
    
  )

# To filter for Whitehorse:
filtered_df_b <- temperature_data %>%
  filter(city %in% c("Whitehorse"))

# To plot a line graph in red color with a black trendline:
p3 <- ggplot(filtered_df_b, aes(x = year, y = mean_temp, color = city)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Whitehorse" = "#FF0000")
  ) +
  scale_x_continuous(
    limits = c(1949, 2024),
    expand = c(0, 0),
    breaks = seq(1950, 2025, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, by = 5),
    expand = c(0, 0)
  ) +
  labs(
    caption = "Whitehorse",
    x = "Year",
    y = "Mean Temperature (°C)",
    color = NULL   
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "darkgrey", size = 1),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 30),
    legend.position = "none",
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )

# To stack the graphs on top of each other:
(p1 / plot_spacer() / p2 / plot_spacer() / p3) +
  plot_layout(heights = c(1, 0.15, 1, 0.15, 1)) & 
  theme(
    plot.background = element_rect(fill = "#FAF2E6", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
