
# Angus Watters
# ENVS 193
# LAB 1 --- Data wrangling + manipulation + organizing + cleaning
# 12/23/2020


# STEP 1 --- Load libraries
# Libraries

ca <- t2 %>%
  filter(state == "CA") %>%
  group_by(year)

ggplot(data = ca, aes(x = year, y = withdrawal)) +
  geom_col(aes(fill = sector), position = position_dodge(5))


# bind all the rows
ts3 <- bind_rows(ts3, df4)

# select desired cols
ts3 <- ts3 %>% select(1:12)
ts3 <- ts3 %>% rename(pop = tp_tot_pop)

# remove the string "County" from any of the county cols
ts3$county = gsub(" County", '', ts3$county)


west <- ts3 %>%
  filter(state %in% c("CA", "AZ", "WA", "OR")) %>%
  group_by(state, year) %>%
  summarise(across(Thermoelectric:Domestic, sum))


west2 <- west %>%
  pivot_longer(cols = c(3:7), names_to = "sector", values_to = "withdrawal")

ca <- ca %>% group_by(sector)

county <- ca %>%
  filter(fips == 6001)

ggplot(data = west2, aes(x=year, y = withdrawal)) +
  geom_point(aes(col = state, size = withdrawal)) +
  facet_wrap(~sector)




















############## STEP 2 ##############

# *** STEP 2.1 ***

water_use <-  readxl::read_xlsx(here('docs/data/usco2015v2.0.xlsx'), 1, skip = 1) %>%
  clean_names() %>%
  rename(population = tp_tot_pop)

# convert chr columns to numerics
water_use <- water_use %>%
  mutate(across(c(8:141), as.numeric)) %>%
  replace(water_use, is.na(water_use), 0)


index <- which(is.na(water_use), arr.ind=TRUE) %>%
  replace(is.na(water_use), 0)



# State geographic centroids (lat & lng)
centroids <- readRDS('docs/data/state_centroids.RDS')


# *** STEP 2.2 ***

# Data dict from second sheet of excel file. all entries, names in snake_case
data_dictionary <- readxl::read_xlsx(here('docs/data/usco2015v2.0.xlsx'), 2) %>%
  clean_names() %>%
  pivot_wider(id_cols = 1:2, names_from = "column_tag", values_from = "attribute") %>%
  clean_names() %>%
  pivot_longer(1:141, names_to = "column_tag",  values_to = "attribute")

############## STEP 3 ##############
# *** STEP 3.1 ***

# select columns with freshwater totals by chr string "w_fr_to"
freshwater_select = select(water_use, state, county, population, contains('w_fr_to'))

# aggregating columns into 5 sectors and total, then selecting only those cols & state
sectors <-  freshwater_select %>%
  mutate(Domestic = ps_w_fr_to + do_w_fr_to,
         Industrial = in_w_fr_to,
         Agriculture = ir_w_fr_to + li_w_fr_to + aq_w_fr_to,
         Mining = mi_w_fr_to,
         Thermoelectric = pt_w_fr_to,
         Total = Domestic + Industrial + Agriculture + Mining + Thermoelectric) %>%
  select(1:3, 17:22)

# *** STEP 3.2 ***
# summarize across columns then df pivot longer
sectors_sum <- sectors %>%
  # filter(state %in% c("CA", "NY")) %>%
  group_by(state) %>%
  summarise(across(population:Total, sum)) %>%
  pivot_longer(cols = c(3:8), names_to = "sector", values_to = "withdrawal")

############## STEP 4 ##############
# sectors <-  inner_join(sectors, centroids, by = 'state')
sectors_sum <-  inner_join(sectors_sum, centroids, by = 'state')

total_state <- sectors_sum %>%
  filter(sector ==  "Total", !state %in% c("DC", "AK", "PR", "HI")) %>%
  group_by(state) %>%
  mutate(withdrawal_per_cap = withdrawal/population)

ggplot(total_state, aes(x = -lng, y = withdrawal_per_cap)) +
  geom_point(aes(color = state, size = withdrawal_per_cap))
west <- sectors %>%
  filter(state %in% c("CA", "AZ", "OR", "WA", "NV"))
west <- west %>%
  pivot_longer(cols = c(4:9), names_to = "sector", values_to = "withdrawal")
west <- west %>%
  filter(sector != "Total")
ggplot(west, aes(x = population, y = withdrawal)) +
  geom_point(aes(color = sector, size = population))



library(tidyverse)
library(janitor)
library(here)

# read in data, use skip to ignore first line of data file, pipe to clean_names()
water_use = here('data/usco2015v2.0.csv') %>%
  read.csv(skip = 1) %>%
  clean_names() %>%
  rename(pop = tp_tot_pop)

water_use <-  read.csv(here('data/usco2015v2.0.csv'), skip = 1)
water_use <-  clean_names(water_use)
water_use <- rename(water_use, pop = tp_tot_pop)
data_dict <-  readxl::read_xlsx(here('data/usco2015v2.0.xlsx'), 2)

wide_dict <- pivot_wider(data_dict, names_from = "Column Tag", values_from = "Attribute")
# CLEANING
# rename a pop column
water_use <- rename(water_use, pop = tp_tot_pop)

# dealing with the "chr" data types ---- as.numeric()





x = as.numeric(x)
x
class(x)

water_use = mutate_at(water_use, c(7:141), as.numeric)
water_use <- water_use %>%
  mutate_at(c(7:141), as.numeric)

##
# SELECT()


# tmp1 <-  water_use %>% select(state, grep('w_fr_to', colnames(water_use)))
# totals = water_use[index]

select(water_use, !state:fips)
select(water_use, state, county, year, tp_tot_pop)

df
summarize()
# select() can also be used to rename columns (new name = old name)
select(water_use, state, county, date = year, tp_tot_pop)

# new df using select ---- population
pop_location <- water_use %>% select(state, county, fips, pop)

# new df using select ---- dom_water_use
dom_water_use <- water_use %>% select(state, county, fips, pop:do_w_delv)

# SELECT() --- text string using contains()
freshwater_total = select(water_use, state, contains('w_fr_to'))






water_use$new_column <- water_use$state



# GREP()
# use grep to find indices for freshwater total cols
# index = grep('w_fr_to', colnames(water_use))
#
# # subset using indices identified by grep() ----- freshwater_totals
# freshwater_total = select(water_use, state, index)


golfing_states <-  filter(water_use, state %in% c( "CA", "AZ", "OR", "WA", "NM", "TX" ))
golfing_states <-  arrange(golfing_states, -ig_w_fr_to)
golfing_states <-  select(golfing_states, state, county, pop, ig_w_fr_to)
golfing_states <-  slice(golfing_states, n = 1:20)


golfing_states = water_use %>%
  filter(state %in% c( "CA", "AZ", "OR", "WA", "NM", "TX" )) %>%
  arrange(-ig_w_fr_to) %>%
  select(state, county, pop, ig_w_fr_to) %>%
  slice(n = 1:20)


# filter examples:
ca =filter(water_use,  state == "CA")
ca = water_use %>% filter(state == "CA") # pipe

not_ca =filter(water_use,  state != "CA")
not_ca = water_use %>% filter(state != "CA") # pipe

ca_golf_water_use = water_use %>% filter(state == "CA", ig_w_fr_to >= 5)

ca_pop_greater_than_million = filter(water_use,  state == "CA", pop >= 1000)
ca_pop_greater_than_million = water_use %>% filter(state == "CA", pop >= 1000) # pipe

df6 <-  data.frame(water_use, pop2 = pop*2, pop3 = pop2*100)

mutated_cols = mutate(water_use, pop2 = pop*100, pop3 = pop2/1000)


df_not_grouped = water_use %>%
  ungroup() %>%
  select(state, county, pop, ig_w_fr_to) %>%
  mutate(total_pop = sum(pop), golf_per_cap = ig_w_fr_to/total_pop)

df_grouped = water_use %>%
  select(state, county, pop, ig_w_fr_to) %>%
  mutate(total_pop = sum(pop), golf_per_cap = ig_w_fr_to/total_pop)

states_total_pop <-  water_use %>%
  select(state, pop) %>%
  group_by(state) %>%
  summarise(total_pop = sum(pop))

# AGGREGATE FRESHWATER TOTALS INTO 5 CATEGORIES
sector_withdrawals <-  freshwater_total %>%
  mutate(Domestic = ps_w_fr_to + do_w_fr_to,
         Industrial = in_w_fr_to,
         Agriculture = ir_w_fr_to + li_w_fr_to + aq_w_fr_to,
         Mining = mi_w_fr_to,
         Thermoelectric = pt_w_fr_to) %>%
  select(1, 15:19)
head(sector_withdrawals)
which()
sector_totals <- sector_withdrawals %>%
  # filter(state %in% c("CA", "NY")) %>%
  group_by(state) %>%
  summarise(across(Domestic:Thermoelectric, sum)) %>%
  pivot_longer(cols = c(2:6), names_to = "sector", values_to = "total_withdrawal")

sector_ggplot <-  ggplot(sector_totals, aes(x = reorder(state, sector), y = total_withdrawal)) +
  geom_bar(aes(fill = sector), position = position_dodge(0.7), width = .5, stat="identity") +
  labs(title = "Total sector water withdrawals by state",
       subtitle = "Data source: USGS",
       x = "State",
       y = "Total withdrawals",
       fill = "Sector") +
  scale_fill_manual(values = c('green4', 'dodgerblue3', 'red2', 'gray56', 'darkorange')) +
  theme_bw()

library(sf)
eqdc = '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

conus = USAboundaries::us_states() %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>%
  filter(!name %in% c('Puerto Rico', 'Alaska', 'Hawaii'))

conus = st_transform(conus, eqdc)

conus = conus %>% rename(state = state_abbr)

centroids = conus %>%
  st_centroid() %>%
  mutate(lat = st_coordinates(.)[,1],
         lng = st_coordinates(.)[,2]) %>%
  select(state, lat, lng) %>%
  st_drop_geometry()



tmp1 = inner_join(sector_totals, centroids, by = 'state')

tmp2 = tmp1 %>%
  filter(sector == "Thermoelectric") %>%
  arrange(-lat)

conus_sectors_ggplot = ggplot(tmp1, aes(x = reorder(state, lat), y = total_withdrawal)) +
  geom_col(aes(fill = sector), position = position_dodge(0.7), width = .8, stat="identity") +
  labs(title = "Total sector water withdrawals by state",
       subtitle = "Data source: USGS",
       x = "State",
       y = "Total withdrawals",
       fill = "Sector") +
  scale_fill_manual(values = c('green4', 'dodgerblue3', 'red2', 'gray56', 'darkorange')) +
  theme_bw() +
  theme(aspect.ratio = .5)


ggsave(conus_sectors_ggplot, file = 'docs/img/sector_withdrawals_conus.png')

# ***** LAB 04 ********




























