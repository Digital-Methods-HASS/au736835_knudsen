library(leaflet)
library(sf)
library(tidyverse)
library(dplyr)

#loading data
war_data <- read.csv2("data/peloponnesian_war_dam_spreadsheet.csv")
#creating a baseline map
peloponnesian_map <- leaflet() |> addTiles(urlTemplate="https://tiles.stadiamaps.com/tiles/stamen_terrain_background/{z}/{x}/{y}{r}.png", attribution='&copy; <a href="https://stadiamaps.com/" target="_blank">Stadia Maps</a> &copy; <a href="https://stamen.com/" target="_blank">Stamen Design</a> &copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> &copy; <a href="https://www.openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>') |> setView(lng = 24, lat = 38, zoom = 7)
peloponnesian_map
#dividing dataset into smaller pieces to make it easier to work with
war_data_first_war <- war_data |> filter(war_phase == "first_peloponnesian_war")
war_data_archidamean_war <- war_data |> filter(subphase == "archidamean_war")
war_data_nicias_peace <- war_data |> filter(subphase == "nicias_peace")
war_data_decelean_war <- war_data |> filter(subphase == "decelean_war")
#creating coordinate points in sf
#first peloponnesian war


start_location_first_war <- war_data_first_war |> st_as_sf(coords = c("home_lng", "home_lat"), crs = 4326)
battle_location_first_war <- war_data_first_war |> st_as_sf(coords = c("battle_lng", "battle_lat"), crs = 4326)
war_data_first_war_coordinates <- mutate(war_data_first_war, start_location = start_location_first_war$geometry, battle_location_coordinates = battle_location_first_war$geometry)
#second peloponnesian war - archidamean war
start_location_archidamean_war <- war_data_archidamean_war |> st_as_sf(coords = c("home_lng", "home_lat"), crs = 4326)
battle_location_archidamean_war <- war_data_archidamean_war |> st_as_sf(coords = c("battle_lng", "battle_lat"), crs = 4326)
war_data_archidamean_war_coordinates <- mutate(war_data_archidamean_war, start_location = start_location_archidamean_war$geometry, battle_location_coordinates = battle_location_archidamean_war$geometry)
#second peloponnesian war - Nicias peace
start_location_nicias_peace <- war_data_nicias_peace |> st_as_sf(coords = c("home_lng", "home_lat"), crs = 4326)
battle_location_nicias_peace <- war_data_nicias_peace |> st_as_sf(coords = c("battle_lng", "battle_lat"), crs = 4326)
war_data_nicias_peace_coordinates <- mutate(war_data_nicias_peace, start_location = start_location_nicias_peace$geometry, battle_location_coordinates = battle_location_nicias_peace$geometry)
#second peloponnesian war - decelean war
start_location_decelean_war <- war_data_decelean_war |> st_as_sf(coords = c("home_lng", "home_lat"), crs = 4326)
battle_location_decelean_war <- war_data_decelean_war |> st_as_sf(coords = c("battle_lng", "battle_lat"), crs = 4326)
war_data_decelean_war_coordinates <- mutate(war_data_decelean_war, start_location = start_location_decelean_war$geometry, battle_location_coordinates = battle_location_decelean_war$geometry)
#creating lines between coordinates
#first peloponnesian war
troop_movement_first_war <- map2(start_location_first_war$geometry, battle_location_first_war$geometry, ~ st_linestring(rbind(st_coordinates(.x), st_coordinates(.y))))|> st_sfc(crs = 4326) |> st_sf(war_data_first_war_coordinates)
#second peloponnesian war - archidamean war
troop_movement_archidamean_war <- map2(start_location_archidamean_war$geometry, battle_location_archidamean_war$geometry, ~ st_linestring(rbind(st_coordinates(.x), st_coordinates(.y)))) |> st_sfc(crs = 4326) |> st_sf(war_data_archidamean_war_coordinates)
#second peloponnesian war - Nicias peace
troop_movement_nicias_peace <- map2(start_location_nicias_peace$geometry, battle_location_nicias_peace$geometry, ~ st_linestring(rbind(st_coordinates(.x), st_coordinates(.y)))) |> st_sfc(crs = 4326) |> st_sf(war_data_nicias_peace_coordinates)
#second peloponnesian war- decelean war
troop_movement_decelean_war <- map2(start_location_decelean_war$geometry, battle_location_decelean_war$geometry, ~ st_linestring(rbind(st_coordinates(.x), st_coordinates(.y)))) |> st_sfc(crs = 4326) |> st_sf(war_data_decelean_war_coordinates)

#making a new tibble with one row per battle_id
#first peloponnesian war
locations_of_battles_first <- battle_location_first_war |> select(battle_id, year_start, year_end, battle_location, geometry) |> distinct()
#archidamean war
location_of_battles_archidamean <- battle_location_archidamean_war |> select(battle_id, year_start, year_end, battle_location, geometry) |> distinct()
#Nicias peace
location_of_battles_nicias <- battle_location_nicias_peace |> select(battle_id, year_start, year_end, battle_location, geometry) |> distinct()
#decelean war
location_of_battles_decelean <- battle_location_decelean_war |> select(battle_id, year_start, year_end, battle_location, geometry) |> distinct()



#creating city points 
cities <- tibble(city = c("Athens", "Sparta", "Corinth", "Megara", "Argos", "Thebes", "sicyon"), lat = c(37.972, 37.082, 37.933, 37.995, 37.633, 38.317, 37.984), lon = c(23.726, 22.424, 22.933, 23.344, 22.729, 23.317, 22.711))
cities_sf <- cities |> st_as_sf(coords = c("lon", "lat"), crs = 4326)
#creating colour palette for maps based on allegiance 
pal <- colorFactor(palette = c("yellow", "navy", "forestgreen", "red"), domain = war_data$allignment)


#map with coloured markers and layers of the four phases of the war
battle_location_first_war <- battle_location_first_war |> mutate(marker_color = pal(allignment))
battle_location_archidamean_war <- battle_location_archidamean_war |> mutate(marker_color = pal(allignment))
battle_location_nicias_peace <- battle_location_nicias_peace |> mutate(marker_color = pal(allignment))
battle_location_decelean_war <- battle_location_decelean_war |> mutate(marker_color = pal(allignment))

map_of_peloponnesian_war <- peloponnesian_map |> addPolylines(data = troop_movement_first_war, color = "white", weight = 1, opacity = 1, popup = ~paste0("From ", state, "<br>To ", battle_location), group = "First Peloponnesian War") |> addAwesomeMarkers(data = battle_location_first_war, icon = awesomeIcons(icon = "shield", library = "fa", markerColor = "white", iconColor = ~marker_color), popup = ~paste0("<b>army:</b> ", state, "<br><b>battle:</b> ", battle_id, "<br><b>year:</b> ", year_start, "<br><b>location:</b>", battle_location), clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE), group = "First Peloponnesian War") |> addPolylines(data = troop_movement_archidamean_war, color = "white", weight = 1, opacity = 1, popup = ~paste0("From ", state, "<br>To ", battle_location), group = "Archidamean War") |> addAwesomeMarkers(data = battle_location_archidamean_war, icon = awesomeIcons(icon = "shield", library = "fa", markerColor = "white", iconColor = ~marker_color), popup = ~paste0("<b>army:</b> ", state, "<br><b>battle:</b> ", battle_id, "<br><b>year:</b> ", year_start, "<br><b>location:</b>", battle_location), clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE), group = "Archidamean War") |> addPolylines(data = troop_movement_nicias_peace, color = "white", weight = 1, opacity = 1, popup = ~paste0("From ", state, "<br>To ", battle_location), group = "Nicias' Peace") |> addAwesomeMarkers(data = battle_location_nicias_peace, icon = awesomeIcons(icon = "shield", library = "fa", markerColor = "white", iconColor = ~marker_color), popup = ~paste0("<b>army:</b> ", state, "<br><b>battle:</b> ", battle_id, "<br><b>year:</b> ", year_start, "<br><b>location:</b>", battle_location), clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE), group = "Nicias' Peace") |> addPolylines(data = troop_movement_decelean_war, color = "white", weight = 1, opacity = 1, popup = ~paste0("From ", state, "<br>To ", battle_location), group = "Decelean War") |> addAwesomeMarkers(data = battle_location_decelean_war, icon = awesomeIcons(icon = "shield", library = "fa", markerColor = "white", iconColor = ~marker_color), popup = ~paste0("<b>army:</b> ", state, "<br><b>battle:</b> ", battle_id, "<br><b>year:</b> ", year_start, "<br><b>location:</b>", battle_location), clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE), group = "Decelean War") |> addAwesomeMarkers(data = cities_sf, icon = awesomeIcons(icon = "university", library = "fa", markerColor = "white", iconColor = "grey"), popup = ~city) |> addLegend(position = "topright", pal = pal, values = war_data$allignment, title = "allignment") |> addLayersControl(overlayGroups = c("First Peloponnesian War", "Archidamean War", "Nicias' Peace", "Decelean War"), options = layersControlOptions(collapsed = TRUE))
map_of_peloponnesian_war
