library(tidyverse)
library(janitor)
library(sf)
library(hrbrthemes)
library(tigris)
library(gganimate)
library(ggrepel)

options(scipen = 999)

theme_set(theme_ipsum())

tuesdata <- tidytuesdayR::tt_load('2021-06-08')

fishing <- tuesdata$fishing %>% 
  clean_names() %>% 
  mutate(lake = str_c("Lake", lake, sep = " "))

stocked <- tuesdata$stocked %>% 
  clean_names()

fishing %>% 
  ggplot(aes(values)) +
  geom_histogram()

fishing %>% 
  ggplot(aes(grand_total)) +
  geom_histogram()

fishing <- 
  fishing %>% 
  mutate(species = case_when(species == "Amercian Eel" ~ "American Eel",
                             species == "Bullhead" ~ "Bullheads",
                             species == "Channel catfish" ~ "Channel Catfish",
                             species %in% c("Cisco and Chub", "Cisco and chubs") ~ "Cisco and Chubs",
                             species == "Crappie" ~ "Crappies",
                             species == "Pacific salmon" ~ "Pacific Salmon",
                             species == "White bass" ~ "White Bass",
                             TRUE ~ species))

fishing %>% 
  count(species) %>% 
  arrange(species) %>% 
  View()

fishing <-
  fishing %>% 
  group_by(lake, year, species) %>% 
  summarize(values = sum(values, na.rm = T)) %>% 
  ungroup()

fishing %>% 
  ggplot(aes(year, values, group = species)) +
  geom_point()

fishing %>% 
  group_by(year) %>% 
  summarize(fished = sum(values, na.rm = T)) %>% 
  ggplot(aes(year, fished)) +
  geom_line()

fishing %>% 
  distinct(lake)

fishing %>% 
  distinct(species) %>% 
  arrange(species)

fishing %>% 
  count(lake, year)

top_species_year_lake <- fishing %>% 
  select(year, lake, species, values) %>% 
  replace_na(list(values = 0)) %>% 
  group_by(year, lake) %>% 
  mutate(pct_of_catch = values / sum(values, na.rm = F)) %>% 
  ungroup() %>% 
  #filter(year == 2015) %>% 
  arrange(year, lake, desc(values)) %>% 
  group_by(year, lake) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  arrange(lake, year)


#https://www.sciencebase.gov/catalog/item/530f8a0ee4b0e7e46bd300dd

lake_map <- list.files("fishing/lake_shapefiles", 
                       pattern = "*.shp",
                       recursive = T,
                       full.names = T) %>% 
  map_dfr(read_sf) %>% 
  mutate(NAMEEN = case_when(str_detect(NAMEEN, "Clair") ~ "Lake Saint Clair",
                            TRUE ~ NAMEEN)) %>% 
  group_by(NAMEEN) %>%
  summarize() %>% 
  mutate(centroid = map(geometry, st_point_on_surface)) %>% 
  mutate(lon = map_dbl(centroid, 1),
         lat = map_dbl(centroid, 2)) %>% 
  mutate(lon_label = case_when(NAMEEN == "Lake Superior" ~ lon - 3,
                               NAMEEN == "Lake Michigan" ~ lon - 2,
                               NAMEEN == "Lake Huron" ~ lon,
                               NAMEEN == "Lake Saint Clair" ~ lon - 2.5,
                               NAMEEN == "Lake Erie" ~ lon + 3,
                               NAMEEN == "Lake Ontario" ~ lon + 1,
                               TRUE ~ lon),
         lat_label = case_when(NAMEEN == "Lake Superior" ~ lat + 1,
                               NAMEEN == "Lake Michigan" ~ lat,
                               NAMEEN == "Lake Huron" ~ lat + 2,
                               NAMEEN == "Lake Saint Clair" ~ lat - 1,
                               NAMEEN == "Lake Erie" ~ lat - .25,
                               NAMEEN == "Lake Ontario" ~ lat + 1,
                               TRUE ~ lat))

lake_map %>% 
  st_drop_geometry() %>% 
  distinct(NAMEEN) %>% 
  semi_join(top_species_year_lake, by = c("NAMEEN" = "lake"))

lake_map %>% 
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = NAMEEN))

lake_map %>% 
  ggplot() +
  geom_sf() +
  geom_point(aes(lon, lat)) +
  geom_segment(aes(x = lon, xend = lon_label,
                   y = lat, yend = lat_label)) +
  geom_label(aes(lon_label, lat_label, label = NAMEEN))

top_species_year_lake %>% 
  complete(lake, year) %>% 
  left_join(lake_map, by = c("lake" = "NAMEEN")) %>% 
  st_as_sf() %>% 
  filter(year == 2000) %>% 
  ggplot(aes(fill = species)) +
  geom_sf(aes(alpha = pct_of_catch)) +
  geom_segment(aes(x = lon, xend = lon_label,
                   y = lat, yend = lat_label)) +
  geom_label(aes(x = lon_label, y = lat_label, label = species)) +
  scale_alpha_continuous(range = c(.5, 1)) +
  facet_wrap(~year) +
  theme_void()

ts_data <- top_species_year_lake %>% 
  complete(lake, year) %>% 
  left_join(lake_map, by = c("lake" = "NAMEEN")) %>% 
  st_as_sf() %>% 
  filter(year >= 1950) %>% 
  mutate(group_id = str_c(lake, year, sep = "_"),
         year = as.integer(year))

ts_data %>% 
  st_drop_geometry() %>% 
  glimpse()

ts_data %>% 
  st_drop_geometry() %>% 
  View()

ts_data %>% 
  complete(lake, year) %>% 
  left_join(lake_map, by = c("lake" = "NAMEEN")) %>% 
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  count(lake, year) %>% 
  filter(n > 1)

top_species_year_lake %>% 
  complete(lake, year) %>% 
  left_join(lake_map, by = c("lake" = "NAMEEN")) %>% 
  st_as_sf() %>% 
  #filter(year >= 1980) %>% 
  mutate(group_id = str_c(lake, year, sep = "_"),
         year = as.integer(year)) %>% 
  st_drop_geometry() %>% 
  ggplot(aes(lake, year, fill = species, alpha = pct_of_catch)) +
  geom_tile() +
  scale_y_reverse() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

top_species_year_lake %>% 
  distinct(lake, year) %>% 
  group_by(lake) %>% 
  mutate(lag_diff = year - lag(year)) %>% 
  ungroup() %>% 
  count(lag_diff)

top_species_year_lake %>% 
  complete(lake, year) %>% 
  count(lake)

ts_data %>% 
  st_drop_geometry() %>% 
  distinct(year) %>% 
  View()

ts_data %>% 
  st_drop_geometry() %>% 
  group_by(lake) %>% 
  filter(year == max(year)) %>% 
  distinct(year)

anim_map <- ts_data %>% 
  mutate(group_id = str_c(lake, year, sep = "_"),
         year = as.integer(year)) %>% 
  #filter(year >= 1980) %>% 
  ggplot(aes(group = year)) +
  geom_sf(aes(fill = species)) +
  geom_segment(aes(x = lon, xend = lon_label,
                   y = lat, yend = lat_label)) +
  geom_label(aes(x = lon_label, y = lat_label, label = species),
             size = 8) +
  scale_alpha_continuous(range = c(.5, 1)) +
  transition_time(year) +
  guides(fill = FALSE) +
  labs(title = "Top species fished in the Great Lakes",
       subtitle = "{frame_time}",
       fill = "Top species fished",
       caption = "@conor_tompkins\nData from Great Lakes Fishery Commission") +
  theme_void(base_size = 15)

complete_animation <- animate(anim_map)
complete_animation

anim_save("fishing/output/great_lakes_anim.gif", anim_map,
          width = 800, height = 800,
          duration = 15)

