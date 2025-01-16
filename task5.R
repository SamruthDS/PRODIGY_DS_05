# Install required libraries (run only once)
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("leaflet")) install.packages("leaflet")
if (!require("leaflet.extras")) install.packages("leaflet.extras")
if (!require("ggplot2")) install.packages("ggplot2")

# Load libraries
library(tidyverse)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(ggplot2)

# Load the dataset
file_path <- "C:/Users/ASUS/Downloads/Traffic_Crashes_-_Crashes.csv"  # Update with your file path
crashes <- read.csv(file_path)

# Preview the dataset
head(crashes)
str(crashes)

# 1. Data Cleaning
# Check for relevant columns in the dataset
colnames(crashes)

# Select relevant columns for analysis
crashes_clean <- crashes %>%
  select(CRASH_DATE, WEATHER_CONDITION, ROADWAY_SURFACE_COND, LIGHTING_CONDITION, LATITUDE, LONGITUDE) %>%
  filter(!is.na(CRASH_DATE), !is.na(WEATHER_CONDITION), !is.na(ROADWAY_SURFACE_COND),
         !is.na(LIGHTING_CONDITION), !is.na(LATITUDE), !is.na(LONGITUDE))  # Remove rows with missing data

# Convert CRASH_DATE to a datetime format
crashes_clean$CRASH_DATE <- ymd_hms(crashes_clean$CRASH_DATE)

# Extract the hour of the crash
crashes_clean$Hour <- hour(crashes_clean$CRASH_DATE)

# 2. Visualizing Accident Hotspots (Geographical Heatmap)
leaflet(crashes_clean) %>%
  addTiles() %>%  # Add the base map tiles
  addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, radius = 1, color = "red", opacity = 0.5) %>%
  addHeatmap(lng = ~LONGITUDE, lat = ~LATITUDE, radius = 10, blur = 20)

# 3. Time of Day Analysis
# Plot accidents by hour of the day
ggplot(crashes_clean, aes(x = Hour)) +
  geom_bar(fill = "blue", alpha = 0.7) +
  labs(title = "Accidents by Time of Day", x = "Hour of Day", y = "Accident Count") +
  theme_minimal()

# 4. Weather Conditions Analysis
# Plot accidents based on weather conditions
ggplot(crashes_clean, aes(x = WEATHER_CONDITION)) +
  geom_bar(fill = "green", alpha = 0.7) +
  labs(title = "Accidents by Weather Conditions", x = "Weather Conditions", y = "Accident Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Road Surface Conditions Analysis
# Plot accidents based on roadway surface conditions
ggplot(crashes_clean, aes(x = ROADWAY_SURFACE_COND)) +
  geom_bar(fill = "purple", alpha = 0.7) +
  labs(title = "Accidents by Roadway Surface Conditions", x = "Roadway Surface Conditions", y = "Accident Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. Lighting Conditions Analysis
# Plot accidents based on lighting conditions
ggplot(crashes_clean, aes(x = LIGHTING_CONDITION)) +
  geom_bar(fill = "orange", alpha = 0.7) +
  labs(title = "Accidents by Lighting Conditions", x = "Lighting Conditions", y = "Accident Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
