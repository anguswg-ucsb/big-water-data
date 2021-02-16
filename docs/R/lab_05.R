# Angus Watters
# ENVS 193
# Lab 05 code
# 01/22/2021

# Data prep & cleaning
library(janitor)
library(here)

# Data manipulation
library(tidyverse)

# Spatial packages
library(USAboundaries)
library(sf)

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
freshwater_select = select(water_use, state, contains('w_fr_to'))

# aggregating columns into 5 sectors and total, then selecting only those cols & state
sectors <-  freshwater_select %>%
  mutate(Domestic = ps_w_fr_to + do_w_fr_to,
         Industrial = in_w_fr_to,
         Agriculture = ir_w_fr_to + li_w_fr_to + aq_w_fr_to,
         Mining = mi_w_fr_to,
         Thermoelectric = pt_w_fr_to,
         Total = Domestic + Industrial + Agriculture + Mining + Thermoelectric) %>%
  select(1, 15:20)

# *** STEP 3.2 ***
# summarize across columns then df pivot longer
sectors <- sectors %>%
  # filter(state %in% c("CA", "NY")) %>%
  group_by(state) %>%
  summarise(across(Domestic:Total, sum)) %>%
  pivot_longer(cols = c(2:7), names_to = "sector", values_to = "withdrawal")

############## STEP 4 ##############
# sectors <-  inner_join(sectors, centroids, by = 'state')
sectors <-  inner_join(sectors, centroids, by = 'state')


############## STEP 5 ##############

# *** STEP 5.1 ***

df1 = sectors %>% filter(sector != "Total")
# create a ggplot

# *** STEP 5.2 ***
ggplot(df1, aes(x = reorder(state, -withdrawal), y = withdrawal)) +
  geom_col(aes(fill = sector)) +
  # geom_bar(aes(fill = sector), stat="identity") +
  labs(title = "Total sector water withdrawals by state",
       caption = "Figure 1: Withdrawals by sector. Data from USGS (2015). Created by Angus Watters",
       x = "State",
       y = "Total withdrawals",
       fill = "") +
  scale_fill_manual(values = c('green4', 'dodgerblue3', 'red2', 'gray56', 'darkorange')) +
  theme_bw() +
  theme(plot.title =element_text(size = 16, vjust = 2),
        axis.text = element_text(face = "bold", size =10),
        axis.title = element_text(size = 12),
        legend.title =   element_text(size = 12),
        legend.position = "top",
        plot.caption = element_text(hjust = 0, face = "bold", size = 12))



# *** STEP 5.3 ***
aes(x = reorder(state, -withdrawal), y = withdrawal)

############## STEP 6 ##############

# *** STEP 6.1 ***
withdrawal_total = select(water_use, state, contains('to_')) %>%
  select(!ps_to_pop)

# *** STEP 6.2 ***
withdrawal_sw_gw <- withdrawal_total %>%
  select(state,to_wgw_to, to_wsw_to)

# *** STEP 6.3 ***
withdrawal_sw_gw <- withdrawal_sw_gw %>%
  group_by(state) %>%
  summarise(across(to_wgw_to:to_wsw_to, sum)) %>%
  rename(Groundwater = to_wgw_to, "Surface water" = to_wsw_to) %>%
  pivot_longer(cols = c(2:3), names_to = "source", values_to = "withdrawal")

# *** STEP 6.4 ***
ggplot(withdrawal_sw_gw, aes(x = reorder(state, -withdrawal), y = withdrawal)) +
  geom_col(aes(fill = source), alpha = 0.7)  +
  labs(title = "Groundwater and surface water withdrawals",
       caption = "Figure 1: Total groundwater and surface water withdrawals. Data from USGS (2015). Created by Angus Watters",
       x = "State",
       y = "Total withdrawals",
       fill = "") +
  scale_fill_manual(values = c('green4', 'dodgerblue3')) +
  theme_bw() +
  theme(plot.title =element_text(size = 16, vjust = 2),
        axis.text = element_text(face = "bold", size =10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0, face = "bold", size = 12))


############## STEP 7 ##############

# *** STEP 7.1 ***

############## STEP 8 ##############

centroids_spatial = centroids %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326)


# *** STEP 8.1 ***
conus = us_states() %>%
  st_transform(5070) %>%
  filter(!name %in% c('Puerto Rico', 'Alaska', 'Hawaii')) %>%
  select(state_name, state_abbr)


# *** STEP 8.2 ***
sector_spatial = sectors %>%
  filter(sector == 'Total', !state %in% c("PR", "VI", "DC", "HI", "AK")) %>%
  rename(state_abbr = state)

# *** STEP 8.3 ***
sector_spatial <- inner_join(sector_spatial, conus, by = "state_abbr") %>% st_as_sf()
# https://www.rapidtables.com/web/color/RGB_Color.html

# *** STEP 8.4 ***
ggplot() +
  geom_sf(data = sector_spatial)

# Map of total water withdrawals by state
ggplot() +
  geom_sf(data = sector_spatial, aes(fill = withdrawal), alpha = 0.9, col = "black", size = .1) +
  geom_sf_text(data = sector_spatial, aes(label = state_abbr), col = "black") +
  # ggrepel::geom_text_repel(data = sector_spatial, aes(geometry = geometry, label = state_abbr), stat = "sf_coordinates") +
  labs(title = "Estimated Water use in the United States",
       caption = "Figure 1: Total state water withdrawals. Data from USGS (2015). Created by Angus Watters",
       x = "",
       y = "",
       fill = "Water withdrawals (Mgal/day)") +
  scale_fill_gradient(low = '#CCE5FF', high = "#003366") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_classic() +
  theme(plot.title =element_text(size = 16, vjust = 2),
        axis.text = element_text(face = "bold", size =10),
        legend.title = element_text(size = 12),
        legend.position = "top",
        plot.caption = element_text(hjust = 0, face = "bold", size = 12))



############## STEP 9 ##############


# *** STEP 9.1 ***
sector_spatial2 = sectors %>%
  rename(state_abbr = state)

sector_spatial2 <- sector_spatial2 %>%
  group_by(state_abbr) %>%
  filter(sector != 'Total', !state_abbr %in% c("PR", "VI", "DC", "HI", "AK"))

# *** STEP 9.2 ***
sector_spatial2 <- sector_spatial2 %>%
  filter(withdrawal == max(withdrawal))

# *** STEP 9.3 ***
sector_spatial2 <- inner_join(conus, sector_spatial2, by = "state_abbr")

# *** STEP 9.4 ***

# Top sector withdrawals per state
ggplot() +
  geom_sf(data = sector_spatial2, aes(fill = sector), col = "black", size = .1) +
  geom_sf_text(data = sector_spatial2, aes(label = state_abbr), col = "black") +
  labs(title = "Top water users in each state",
       caption = "Figure 1: Top water withdrawaling sector in each state. Data from USGS (2015). Created by Angus Watters",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values = c('green4', 'dodgerblue3', 'red2', 'darkorange')) +
  ggthemes::theme_fivethirtyeight() +
  theme(plot.title =element_text(size = 16, vjust = 2),
        axis.text = element_text(face = "bold", size =10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "top",
        plot.caption = element_text(hjust = 0, face = "bold", size = 12))






for ( obj in ls() ) { print(get(obj)) }


mget(ls())
lsf.str()

for ( obj in ls() ) {
  cat('---',obj,'---\n');
  if ( class(get(obj)) == 'matrix'  ){
    print( get(obj)[1:min(ncol(get(obj)),10),1:min(ncol(get(obj)),10)] )
  }else if ( class(get(obj)) == 'numeric' |  class(get(obj)) == 'integer'  ){
    print( get(obj)[1:min(length(get(obj)),10)] )
  }else if( class(get(obj)) == 'list'){
    for (i in 1:length(get(obj))){
      if ( class(get(obj)) == 'matrix'  ){
        print( get(obj)[[i]][1:min(ncol(get(obj)[[i]]),10),1:min(ncol(get(obj)[[i]]),10)] )
      }else if ( class(get(obj)[[i]]) == 'numeric' |  class(get(obj)[[i]]) == 'integer'  ){
        print( get(obj)[[i]][1:min(length(get(obj)[[i]]),10)] )
      }else{
        print( get(obj)[[i]] )
      }
    }
  }else{
    print( get(obj) )
  }
}

ls(objects())
