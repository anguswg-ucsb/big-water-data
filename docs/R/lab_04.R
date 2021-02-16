# Angus Watters
# ENVS 193
# Lab 04 code
# 01/21/2021

library(tidyverse)
library(janitor)
library(here)

water_use <-  readxl::read_xlsx(here('docs/data/usco2015v2.0.xlsx'), 1, skip = 1) %>%
  clean_names() %>%
  rename(population = tp_tot_pop)

data_dict <-  readxl::read_xlsx(here('docs/data/usco2015v2.0.xlsx'), 2)

data_dict <- data_dict %>%
  clean_names() %>%
  pivot_wider(names_from = "column_tag", values_from = "attribute") %>%
  clean_names()

data_dict <- data_dict %>%
  select(contains("w_fr_to")) %>%
  pivot_longer(1:13, names_to = "column_tag", values_to = "attribute")



water_use <- water_use %>% group_by(state)



water_use <- water_use %>% summarise(sumna = sum(is.na()))


wide_dict <- pivot_wider(data_dict, names_from = "Column Tag", values_from = "Attribute") %>%
  clean_names() %>%
  rename(population = tp_tot_pop)

xx = water_use %>%
  rowid_to_column() %>%
  filter(is.na())
matrix(A[!is.na(B)],ncol=ncol(A))
index <- is.na(water_use)
all_na <-water_use[index == TRUE]

water_use <- water_use %>%
  mutate(across(c(8:141), as.numeric))

these_are_na_rows = water_use[index[1:nrow,],]

index <- which(is.na(water_use), arr.ind = TRUE)
all_na <-water_use[index[,2]]
all_na <- water_use[index[2,], index[,1]]

all_na <-water_use[index[, 1], index[1, 2]]
x = is.na(water_use)



golf_irrigation = waater_use %>%
  select(water_use, state, county, population, contains('ig_')) %>%
  mutate(gw_percentage = ig_wgw_fr/ig_w_fr_to,
         sw_percentage = ig_wsw_fr/ig_w_fr_to)

# golf_irrigation$ig_wgw_fr = as.numeric(golf_irrigation$ig_wgw_fr)
# golf_irrigation$ig_wsw_fr = as.numeric(golf_irrigation$ig_wsw_fr)
# golf_irrigation$ig_w_fr_to = as.numeric(golf_irrigation$ig_w_fr_to)

# SELECT() --- text string using contains()
freshwater_total = select(water_use, state, contains('w_fr_to'))

sector_withdrawals <-  freshwater_total %>%
  mutate(Domestic = ps_w_fr_to + do_w_fr_to,
         Industrial = in_w_fr_to,
         Agriculture = ir_w_fr_to + li_w_fr_to + aq_w_fr_to,
         Mining = mi_w_fr_to,
         Thermoelectric = pt_w_fr_to) %>%
  select(1, 15:19)

sector_totals <- sector_withdrawals %>%
  # filter(state %in% c("CA", "NY")) %>%
  group_by(state) %>%
  summarise(across(Domestic:Thermoelectric, sum)) %>%
  pivot_longer(cols = c(2:6), names_to = "sector", values_to = "total_withdrawal") %>%
  group_by()

# ***********HOW I OBTAINED LAT/LNG FROM CENTROIDS************
# conus = USAboundaries::us_states() %>%
#   sf::st_as_sf(coords = c('lng', 'lat'), crs = 4326)
#   # filter(!name %in% c('Puerto Rico', 'Alaska', 'Hawaii'))
#==============
# conus = conus %>% rename(state = state_abbr)
#
# centroids = conus %>%
#   sf::st_centroid() %>%
#   mutate(lng = sf::st_coordinates(.)[,1],
#          lat = sf::st_coordinates(.)[,2]) %>%
#   select(state_abbr, lat, lng) %>%
#   sf::st_drop_geometry()
# centroids = centroids %>% rename(state = state_abbr)
# saveRDS(sector_totals, file = "data/sector_totals.rds")

centroids <-  readRDS("data/state_centroids.rds")


df1 <-  data.frame(name = c("Tom", "Jerry", "Micky"), hair = c("brown", "red", "blonde"))
df2 <-  data.frame(name = c("Tom", "Jerry", "Harold"), shoe_size = c("8", "11", "14"))

inner_join(df1, df2, by = "name")
left <- left_join(df1, df2, by = "name")
right <- right_join(df1, df2, by = "name")
full <- full_join(df1, df2, by = "name")


centroids <-  readRDS("data/state_centroids.rds")

sector_totals <-  inner_join(sector_totals, centroids, by = 'state')

sector_totals <- sector_withdrawals %>%
  # filter(state %in% c("CA", "NY")) %>%
  group_by(state) %>%
  summarise(across(Domestic:Thermoelectric, sum)) %>%
  pivot_longer(cols = c(2:6), names_to = "sector", values_to = "total_withdrawal") %>%
  group_by()

ggplot(sector_totals, aes(x = reorder(state, lng), y = total_withdrawal)) +
  geom_bar(aes(fill = sector), position = position_dodge(0.7), width = .8, stat="identity") +
  labs(title = "Total sector water withdrawals by state",
       subtitle = "Data source: USGS",
       x = "State",
       y = "Total withdrawals",
       fill = "Sector") +
  scale_fill_manual(values = c('green4', 'dodgerblue3', 'red2', 'gray56', 'darkorange')) +
  theme_bw() +
  theme(aspect.ratio = .5)














