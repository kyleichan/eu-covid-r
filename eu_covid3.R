library(stats)
library(sf) # Map data
library(tidyverse)
library(giscoR) # GISCO is the Eurostat agency that deals with geographical data
library(readxl) # Importing Excel files
library(gganimate) # Animate ggplot
library(transformr) # For maps in R
library(av) # Allow you to create video file rather than GIF

setwd("C:/Users/kylei/Dropbox/Covid-19/EU Covid")

#------------------------------Get EU maps of NUTS regions from EU GISCO---------------------------------------------
# https://gisco-services.ec.europa.eu/distribution/v2/nuts/topojson/NUTS_RG_01M_2021_4326_LEVL_3.json

# Read in EU map data as an sf data object
map <- st_read("eu_map.json",
               stringsAsFactors = F) %>% 
  st_set_crs(4326)  # the data is in WGS84

# Drop all columns except, NUTS_ID (and geometry remains by default)
map <- map %>%
  select(NUTS_ID)

# Create new columns for each of the NUTS level in the map data frame
map <- map %>%
  mutate(NUTS1 = substring(NUTS_ID, 0, 3)) %>%
  mutate(NUTS2 = substring(NUTS_ID, 0, 4)) %>%
  mutate(NUTS3 = NUTS_ID)

# Remove outlier regions like Canary Islands
# map <- map %>%
#  filter(NUTS_ID != "ES70") %>%
#  filter(NUTS_ID != "PT30") %>%
#  filter(NUTS_ID != "PT20") %>%
#  filter(NUTS_ID != "FRY1") %>%
#  filter(NUTS_ID != "FRY2") %>%
#  filter(NUTS_ID != "FRY3") %>%
#  filter(NUTS_ID != "FRY4") %>%
#  filter(NUTS_ID != "FRY5")

#------------------------------Extract Data---------------------------------------------
# Source: European Centre for Disease Prevention and Control
# Data on the daily subnational 14-day notification rate of new COVID-19 cases
# https://www.ecdc.europa.eu/en/publications-data/subnational-14-day-notification-rate-covid-19

eu_weekly <- read.csv("./eu_covid_weekly_subnational.csv", stringsAsFactors = F)

# Rename columns. Be sure to include NUTS_ID
names(eu_weekly) <- c("country", "region", "NUTS_ID", "year_week", "rate", "source")
eu_weekly <- select(eu_weekly, c("NUTS_ID", "year_week", "rate"))

eu_weekly <- spread(eu_weekly, year_week, rate)

# Clean up some of the NUTS codes
eu_weekly[eu_weekly == "PTG301"] <- "PT18"
eu_weekly[eu_weekly == "PTG302"] <- "PT15"
eu_weekly[eu_weekly == "PTG305"] <- "PT17"
eu_weekly[eu_weekly == "PTG304"] <- "PT16"
eu_weekly[eu_weekly == "PTG307"] <- "PT11"
eu_weekly[eu_weekly == "PTG306"] <- "PT30"
eu_weekly[eu_weekly == "PTG303"] <- "PT200"
eu_weekly[eu_weekly == "NOG315"] <- "NO053"
eu_weekly[eu_weekly == "NOG318"] <- "NO071"
eu_weekly[eu_weekly == "NOG303"] <- "NO011"
eu_weekly[eu_weekly == "NOG311"] <- "NO043"
eu_weekly[eu_weekly == "NOG350"] <- "NO06"
eu_weekly[eu_weekly == "NOG354"] <- "NO07"
eu_weekly[eu_weekly == "BG412X"] <- "BG412"

#------------------------------EU NUTS mapping---------------------------------------------
# This allows us to determine which NUTS type each NUTS_ID is
# https://ec.europa.eu/eurostat/web/nuts/background

nuts_mapping <- read_excel("NUTS2021.xlsx", sheet = 2)
nuts_mapping <- nuts_mapping[,c(1,6)]
names(nuts_mapping) <- c("NUTS_ID", "NUTS_level")

# Use nuts_mapping to indicate which NUTS level each row's NUTS_ID is (e.g. NUTS1, NUTS2, or NUTS3)
eu_weekly <- left_join(eu_weekly, nuts_mapping)

# Create new columns for each of the NUTS levels
eu_weekly <- eu_weekly %>%
  mutate(NUTS1 = ifelse(NUTS_level == 1, NUTS_ID, NA)) %>%
  mutate(NUTS2 = ifelse(NUTS_level == 2, NUTS_ID, NA)) %>%
  mutate(NUTS3 = ifelse(NUTS_level == 3, NUTS_ID, NA)) %>%
  select(-NUTS_ID) # Drop NUTS_ID itself so it doesn't interfere with NUTS_ID in the maps data frame

#------------------------------------------------------------------------------------------

# Get latest week of COVID rates
last_date <- "2021-W16"

# Select the latest week of COVID data
covid_latest <- eu_weekly %>%
  filter(year_week == last_date)

# Add rates to map, matching on NUTS3
covid_map3 <- map %>%
  left_join(
    select(eu_weekly, -c(NUTS1, NUTS2)),
    by="NUTS3"
  )

covid_map2 <- na.omit(covid_map3$`2021-W16`)

# Add rates to map, matching on NUTS2
covid_map2 <- covid_map3 %>%
  filter(is.na(`2021-W16`)) %>%
  select(NUTS_ID, NUTS1, NUTS2, NUTS3) %>%
  left_join(
    select(eu_weekly, -c(NUTS1, NUTS3)),
    by="NUTS2"
  )

# Add rates to map, matching on NUTS1
covid_map1 <- covid_map2 %>%
  filter(is.na(`2021-W16`)) %>%
  select(NUTS_ID, NUTS1, NUTS2, NUTS3) %>%
  left_join(
    select(eu_weekly, -c(NUTS2, NUTS3)),
    by="NUTS1"
  )

# Combine maps matched on NUTS1, NUTS2, and NUTS3
covid_map <- rbind(
  filter(covid_map3, !is.na(`2021-W16`)),
  filter(covid_map2, !is.na(`2021-W16`)),
  filter(covid_map1, !is.na(`2021-W16`))
)

# Remove all variables except for actual weekly rates
covid_map <- covid_map %>%
  select(-c(NUTS1, NUTS2, NUTS3, NUTS_level))

covid_map <- covid_map %>%
# select(NUTS_ID, `2021-W10`:`2021-W16`)
  select(NUTS_ID, `2020-W20`:`2021-W16`)


covid_map_long <- covid_map %>%
  gather("year_week", "rate", -geometry, -NUTS_ID)


gg <- ggplot() +
  geom_sf(data = covid_map_long, aes(fill = rate)) +
  coord_sf(xlim = c(-11, 30), ylim = c(34, 70), expand = FALSE) +
  scale_fill_gradientn(colours = heat.colors(12, rev=TRUE), breaks = c(0, 1000)) +
  transition_manual(frames = year_week) +
  labs(title = "{current_frame}")

# animate(gg, fps = 5, end_pause = 20, renderer = av_renderer())
animate(gg, fps = 5, end_pause = 10, renderer = av_renderer())

#------------

# Plot map of COVID rates by NUTS, use color gradient
windows()
ggplot() +
  geom_sf(data = covid_map, aes(fill = rate)) +
  coord_sf(xlim = c(-20, 40), ylim = c(30, 70), expand = FALSE) +
  scale_fill_gradientn(colours = heat.colors(12, rev=TRUE)) +



windows()
ggplot() +
  geom_sf(data = map)

#---------------------------Debugging------------------------------------

test <- st_drop_geometry(covid_map) %>%
  group_by(country) %>%
  tally()

test <- st_drop_geometry(map) %>%
  group_by(NUTS_ID) %>%
  tally()

test <- covid_map %>%
  filter(NUTS2 == "AT11")

