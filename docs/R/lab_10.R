# USA map + CA map
# Public supply colored polygons + population as dots ordered by size
# Irrigation colored by polygons + crop (something -- acres?) as dots ordered by size

# Data prep & cleaning
library(janitor)
library(here)
library(readxl)
# Data manipulation
library(tidyverse)


# Spatial packages
library(USAboundaries)
library(sf)
library(leaflet)

# Color palletes
library(RColorBrewer)

wu_all = readRDS("docs/data/data/wu_all.rds")

# wu_all RDS file 1960 - 2015
wu_all_pop <- readRDS("docs/data/data/wu_all_pop2.rds")

# Read data for 2015
d_wu_2015 <- read_excel(here("docs/data/data/us2015.xlsx"), sheet = 1, skip = 1) %>%
  clean_names() %>%
  mutate(across(c(2, 4:141), as.numeric)) %>%
  replace(is.na(.), 0)

# Get the state abbr and associated fips
fips <- d_wu_2015 %>%
  select(state, State = statefips) %>%
  unique() %>%
  filter(!State %in% c(0, 11,72, 78))

# add state names to data frame by inner joining
wu_all_pop <- inner_join(wu_all_pop, fips, by= "State")

# change column name "State" to fips
wu_all_pop <- rename(wu_all_pop, fips = State)

# Replace NA with 0
wu_all_pop <- wu_all_pop %>%
  replace(is.na(.), 0)



# --- LEAFLET 1 ---
# Public supply colored polygons + population as dots ordered by size

conus <- us_states() %>%
  select(state = state_abbr, name) # select state abbr to join on, state name in case we want to use it later

# 2015 public supply data
public_supply <- wu_all_pop %>%
  filter(Year == 2015, Sectors == "Public Supply") %>%
  mutate(scaled_pop = abs(Population - mean(Population)) / sd(Population)) # scale the population

# Join Public supply & conus, giving the Public supply data geometries
public_sf <- inner_join(public_supply, conus, by = "state") %>%
  st_as_sf() # make data frame an SF object, no other arguements needed as it works off the geometry column

# get centroid points of each state
centroids <- public_sf %>%
  st_centroid()

class(public_sf)
# hist(public_supply$scaled_pop, breaks = 300)
# ggpubr::ggdensity(public_supply$scaled_pop)
# rstatix::shapiro_test(public_supply$Population)

# ggplot(data = public_supply, aes(x = Population, y = Withdrawals)) +
#   geom_point(aes(color = state, size = scaled_pop))

RColorBrewer::display.brewer.all()

# Color Palletes
blues <- brewer.pal(9, "Blues")
yellows <- brewer.pal(9, "YlOrRd")
#
# plot(public_sf$geometry)

# public supply color scale
pal = colorNumeric(blues, reverse= TRUE, domain = public_sf$Withdrawals, n =9)

# Pop bubble colors
pal2 = colorNumeric(yellows, reverse= TRUE, domain = public_supply$scaled_pop, n = 9)


leaflet() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Base') %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>%
  setView(-95,40,4) %>%
  leafem::addMouseCoordinates() %>%
  addPolygons(data = public_sf,
              fillColor = ~pal(public_supply$Withdrawals),
              fillOpacity = .5,
              weight = 0.5,
              color = 'black',
              group = 'Public Supply') %>%
  addCircleMarkers(data = centroids,
                   fillColor = ~pal2(Population),
                   color = "black",
                   weight = .5,
                   radius = 5*(centroids$scaled_pop),
                   group = "Pop") %>%
  addLegend(pal = pal,
            values = public_sf$Withdrawals,
            opacity = .9,
            title = 'Withdrawals', # Title
            position = "bottomleft") %>%
    addLayersControl(overlayGroups = c('Public Supply', 'Pop'),
                     options = layersControlOptions(collapsed = FALSE),
                     baseGroups = c("Base", "Terrain"))
  addPolygons(data = sw_2015,
              fillColor = ~pal2(Withdrawals),
              fillOpacity = .5,
              color = 'black',
              group = 'SW') %>%
  addLayersControl(overlayGroups = c('GW', 'SW'),
                   options = layersControlOptions(collapsed = FALSE),
                   baseGroups = c("Base", "Terrain")) %>%
  addLegend(pal = pal,
            values = gw_2015$Withdrawals,
            opacity = .9,
            title = 'Withdrawals', # Title
            position = "bottomleft")
labFormat = function(type, cuts, p) {
  paste0(labels)})





