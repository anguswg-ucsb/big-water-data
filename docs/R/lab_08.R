
# Angus Watters
# ENVS 193
# Lab 8

# Data prep & cleaning
library(janitor)
library(here)
library(readxl)
# Data manipulation
library(tidyverse)
library(cowplot)

# Spatial packages
library(USAboundaries)
library(sf)

# Step 1:
# Read in wu_all RDS
wu_all = readRDS("docs/data/data/wu_all.rds")

wu_all_pop = readRDS("docs/data/data/wu_all_pop.rds")

# Read data for 2015
d_wu_2015 <- read_excel(here("docs/data/data/us2015.xlsx"), sheet = 1, skip = 1) %>%
  clean_names() %>%
  mutate(across(c(2, 4:141), as.numeric)) %>%
  replace(is.na(.), 0)

fips <- d_wu_2015 %>%
  select(state, State = statefips) %>%
  unique() %>%
  filter(!State %in% c(0, 11,72, 78))

# add state names to data frame by inner joining
wu_all <- inner_join(wu_all, fips, by= "State")
wu_all <- rename(wu_all, fips = State)
wu_all <- wu_all %>%
  replace(is.na(.), 0)

#--------------------- STEP 2: ---------------------
#------------ YEARLY TOTALS BAR GRAPH --------------

  # group by sectors and year, and sum the withdrawals, this gives you the total US withdrawals by sector per year
conus_sum <- wu_all %>%
  group_by(Sectors, Year) %>%
  summarise(Withdrawals = sum(Withdrawals))

# Create another data frame to total withdrawals for all sectors in each year, this data frame will be used to add total withdrawals geom_line
conus_total <- conus_sum %>%
  group_by(Year) %>%
  replace(is.na(.), 0) %>%
  summarise(Total = sum(Withdrawals))

#--------------------- PLOT 1 ---------------------
ggplot() +
  geom_col(data = conus_sum, aes(x = Year, y = Withdrawals, fill = Sectors),
           col = "black",
           position = position_dodge(3.2),
           width = 3) +
  geom_line(data = conus_total, aes(x = Year, y = Total),
            size = 1,
            col = "deepskyblue") +
  geom_point(data = conus_total, aes(x = Year, y = Total),
             size = 4,
             col = "deepskyblue",
             shape = 1) +
  scale_fill_manual(values = c("darkgray", "green4", "dodgerblue", "tomato3", "darkgoldenrod2")) +
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n = 10),
                     expand = c(0, 0),
                     limits = c(0, NA),
                     sec.axis = sec_axis(trans = ~./2*(2.75),
                                         breaks = scales::pretty_breaks(n = 12),
                                         name= "Totalwithdrawals (Mgal/day)",
                                         labels = scales::comma)) +
  expand_limits(y = max(conus_total$Total) + 20000) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  labs(title = "Trends in total water withdrawals by water-use category (1950 - 2015)",
       x = "",
       y = "Withdrawals (Mgal/day)",
       fill = "") +
  ggthemes::theme_few() +
  theme(axis.text = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),# Changes grid style
        plot.caption = element_text(hjust = 0, color = "black", size = 9, face = "bold"),
        legend.position = "bottom")


#--------------------------- STEP 4: --------------------------
#-------------------- GW vs. SW BAR PLOT ----------------------

wu_all_source = readRDS("docs/data/data/wu_all_source.rds")

# yearly US totals for groundwater & surface water
sw_gw <- wu_all_source %>%
  group_by(Source, Year) %>%
  summarize(Withdrawals = sum(Withdrawals))

# Sum the final yearly total, pivot with the same column names in our other data so that we can bind the rows back onto sw_gw. This is so we can use one dataframe for the ggplot, so we can have the total value as another bar in our barchart
sw_gw_total <- sw_gw %>%
  group_by(Year) %>%
  summarize(Total = sum(Withdrawals)) %>%
  pivot_longer(2, values_to = "Withdrawals", names_to = "Source")

# bind rows back with sw_gw
sw_gw <- bind_rows(sw_gw, sw_gw_total)

# calculate each year total US pop
pop <- wu_all_source %>%
  filter(Source == "Groundwater") %>%
  group_by(Year) %>%
  summarize(total_pop = sum(Population)) %>%
  mutate(Population = "Population")

# join back to sw_gw dataframe
sw_gw <- inner_join(sw_gw, pop, by = "Year")



ggplot(data = sw_gw, aes(x = Year, y = Withdrawals)) +
  geom_col(aes(fill = Source),  col = "black",
           position = position_dodge(3.2),
           width = 3) +
  geom_line(aes(y = total_pop, col = Population), size = 2) +
  scale_color_manual(values = c("deeppink")) +
  scale_fill_manual(values = c("lightsteelblue1", "deepskyblue", "midnightblue")) +
  # scale_color_manual(values = c("deeppink")) +
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     expand = c(0, 1)) +
  labs(title = "Trends in population and freshwater withdrawals by source  (1960 - 2015)",
       fill = "EXPLANATION",
       col = " ",
       x = "Year",
       y = "Withdrawals (Mgal/day)") +
  ggthemes::theme_few() +
  theme(axis.text = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        plot.caption = element_text(hjust = 0, color = "black", size = 9, face = "bold"))








# pct <- pct %>%
#   filter(!Year %in% c(1950, 1955, 1960)) %>%
#   group_by(Year) %>%
#   arrange(Sectors)
#
# pct <- pct %>%
#   pivot_longer(5, values_to = "Total_withdrawals", names_to = "Totals")
#
# pct <- pct %>%
#   group_by(Totals) %>%
#   mutate(pct_chg_total = (Total_withdrawals/lag(Total_withdrawals) - 1)*100) %>%
#   replace(is.na(.), 0)

# pct2 <- conus_total %>%
#   mutate(pct_chg = (Total/lag(Total) - 1)*100) %>%
#   replace(is.na(.), 0) %>%
#   filter(!Year %in% c(1950, 1955, 1960))


#--------------------- STEP 2: ---------------------
#------------ YEARLY TOTALS BAR GRAPH --------------

# conus_sum2 <- conus_sum2 %>%
#   group_by(year) %>%
#   arrange(desc(sector)) %>%
#   mutate(sector2=factor(sector, levels= c("Domestic", "Agriculture", "Thermoelectric", "Industrial", "Mining")))
# conus_total <- conus_sum2 %>%
#   group_by(Year) %>%
#   replace(is.na(.), 0) %>%
#   summarise(Total = sum(Totals))

#--------------------- PLOT 1 ---------------------
 ggplot() +
  geom_col(data = conus_sum2, aes(x = Year, y = Withdrawals, fill = Sectors),
           col = "black",
           position = position_dodge(3.2),
           width = 3) +

  geom_point(data = conus_total, aes(x = Year, y = Total),
             size = 3.5,
             col = "cyan4") +
  geom_line(data = conus_total, aes(x = Year, y = Total),
            size = 1,
            col = "cyan4")  +
  scale_fill_manual(values = c("darkgray", "green4", "dodgerblue", "tomato3", "darkgoldenrod2")) +
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n = 10),
                     expand = c(0, 0),
                     limits = c(0, NA)) +
                     # sec.axis = sec_axis(trans = ~. + 100000, labels = scales::comma)) +
  expand_limits(y = max(conus_total$Total) + 20000) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  labs(caption = "Figure 1: ",
       x = "Year",
       y = "Withdrawals (Mgal/day)",
       fill = "") +
  theme_classic() +
  theme(axis.text = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "lightgrey"), # Changes grid style
        plot.caption = element_text(hjust = 0, color = "black", size = 9, face = "bold"))

p2 <- ggplot() +
  geom_col(data = conus_sum2, aes(x = Year, y = Withdrawals, fill = Sectors),
           col = "black",
           position = position_dodge(3.2),
           width = 3) +

  geom_point(data = conus_total, aes(x = Year, y = Total),
             size = 3.5,
             col = "cyan4") +
  geom_line(data = conus_total, aes(x = Year, y = Total),
            size = 1,
            col = "cyan4")  +
  scale_fill_manual(values = c("darkgray", "green4", "dodgerblue", "tomato3", "darkgoldenrod2")) +
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n = 10),
                     expand = c(0, 0),
                     limits = c(0, NA)) +
  # sec.axis = sec_axis(trans = ~. + 100000, labels = scales::comma)) +
  expand_limits(y = max(conus_total$Total) + 20000) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme_classic() +
  theme(axis.text = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "lightgrey"), # Changes grid style
        plot.caption = element_text(hjust = 0, color = "black", size = 9, face = "bold"))


wu_all_pop = readRDS("docs/data/data/wu_all_pop.rds")

# Change population units
wu_all_pop$Population <- (wu_all_pop$Population*1000)

wu_all_pop$Withdrawals <- (wu_all_pop$Withdrawals*10000)

wu_capita <- wu_all_pop %>%
  group_by(state, Year) %>%
  filter(Sectors == "Public Supply") %>%
  mutate(wupc = Withdrawals/Population)
hist(wu_capita$wupc, breaks = 20)

wu_capita <- wu_all_pop %>%
  group_by(state, Year) %>%
  summarize(state_total = sum(Withdrawals), wupc = state_total/Population)


df1 <- wu_capita %>%
  filter(state %in% c("CA", "TX", "NYC", "AZ"))
# wu_capita2 <- wu_all_pop %>%

wu_capita2 <- wu_all_pop %>%
  group_by(Year) %>%
  mutate(Population = sum(Population), usa_total = sum(Withdrawals)) %>%
  mutate(wupc = usa_total/Population)

tmp1 <- wu_all_pop %>%
  group_by(Year) %>%
  summarize(pop = sum(Population), usa_total = sum(Wit))



states_sectors <- wu_all %>%
  filter(state %in% c("NY", "AZ", "WA")) %>%
  group_by(state, Year) %>%
  mutate(total = sum(Withdrawals)) %>%
  group_by(state, Year) %>%
  mutate(percentage = (Withdrawals/total)*100, size = abs(Withdrawals-mean(Withdrawals)/sd(Withdrawals)))
ggplot(states_sectors, aes(x = Year, y = Withdrawals)) +
  geom_col(aes(fill = Sectors)) +
  facet_grid(state~Sectors)














d_wu_1950 <- lapply(excel_sheets(here("docs/data/data/us1950.xlsx")),
                    function(x) read_excel(here("docs/data/data/us1950.xlsx"),
                                           skip = 3,
                                           sheet = x)) %>%
  reduce(left_join, by = "Area") %>%
  clean_names() %>%
  mutate(across(c(1:9), as.numeric)) %>%
  replace(is.na(.), 0)

# Read data for 1955
d_wu_1955 <- lapply(excel_sheets(here("docs/data/data/us1955.xlsx")),
                    function(x) read_excel(here("docs/data/data/us1955.xlsx"),
                                           skip = 3,
                                           sheet = x)) %>%
  reduce(left_join, by = "Area") %>%
  clean_names() %>%
  mutate(across(c(1:11), as.numeric)) %>%
  replace(is.na(.), 0)

# Read data for 1960
d_wu_1960 <- lapply(excel_sheets(here("docs/data/data/us1960.xlsx")),
                    function(x) read_excel(here("docs/data/data/us1960.xlsx"),
                                           skip = 3,
                                           sheet = x)) %>%
  reduce(left_join, by = "Area") %>%
  clean_names() %>%
  mutate(across(c(1:35), as.numeric)) %>%
  replace(is.na(.), 0)

# Read data for 1965
d_wu_1965 <- lapply(excel_sheets(here("docs/data/data/us1965.xlsx")),
                    function(x) read_excel(here("docs/data/data/us1965.xlsx"),
                                           skip = 3,
                                           sheet = x)) %>%
  reduce(left_join, by = "Area") %>%
  clean_names() %>%
  mutate(across(c(1:33), as.numeric)) %>%
  replace(is.na(.), 0)

# Read data for 1970
d_wu_1970 <- lapply(excel_sheets(here("docs/data/data/us1970.xlsx")),
                    function(x) read_excel(here("docs/data/data/us1970.xlsx"),
                                           skip = 3,
                                           sheet = x)) %>%
  reduce(left_join, by = "Area") %>%
  clean_names() %>%
  mutate(across(c(1:34), as.numeric)) %>%
  replace(is.na(.), 0)


# Read data for 1975
d_wu_1975 <- lapply(excel_sheets(here("docs/data/data/us1975.xlsx")),
                    function(x) read_excel(here("docs/data/data/us1975.xlsx"),
                                           skip = 3,
                                           sheet = x)) %>%
  reduce(left_join, by = "Area") %>%
  clean_names() %>%
  mutate(across(c(1:34), as.numeric)) %>%
  replace(is.na(.), 0)

# Read data for 1980
d_wu_1980 <- lapply(excel_sheets(here("docs/data/data/us1980.xlsx")),
                    function(x) read_excel(here("docs/data/data/us1980.xlsx"),
                                           skip = 3,
                                           sheet = x)) %>%
  reduce(left_join, by = "Area") %>%
  clean_names() %>%
  mutate(across(c(1:34), as.numeric)) %>%
  replace(is.na(.), 0)

# Read data for 1985
d_wu_1985 <- read_delim(here("docs/data/data/us1985.txt"), delim = "\t") %>%
  clean_names() %>%
  mutate(across(c(2, 4:163), as.numeric)) %>%
  replace(is.na(.), 0)

# Read data for 1990
d_wu_1990 <- read_excel(here("docs/data/data/us1990.xls"), sheet = 1) %>%
  clean_names() %>%
  filter(state != 3226) %>%
  mutate(across(c(2, 4:163), as.numeric)) %>%
  replace(is.na(.), 0)

# Read data for 1995
d_wu_1995 <- read_excel(here("docs/data/data/us1995.xls"), sheet = 1) %>%
  clean_names() %>%
  mutate(across(c(1:4, 6:252), as.numeric)) %>%
  replace(is.na(.), 0)

# Read data for 2000
d_wu_2000 <- read_excel(here("docs/data/data/us2000.xls"), sheet = 1) %>%
  clean_names() %>%
  mutate(across(c(2:70), as.numeric)) %>%
  replace(is.na(.), 0)

# Read data for 2005
d_wu_2005 <- read_excel(here("docs/data/data/us2005.xls"), sheet = 1) %>%
  clean_names() %>%
  mutate(across(c(2:4, 6:108), as.numeric)) %>%
  replace(is.na(.), 0)

# Read data for 2010
d_wu_2010 <- read_excel(here("docs/data/data/us2010.xlsx"), sheet = 1) %>%
  clean_names() %>%
  mutate(across(c(2, 4:117), as.numeric)) %>%
  replace(is.na(.), 0)

# Read data for 2015
d_wu_2015 <- read_excel(here("docs/data/data/us2015.xlsx"), sheet = 1, skip = 1) %>%
  clean_names() %>%
  mutate(across(c(2, 4:141), as.numeric)) %>%
  replace(is.na(.), 0)

source_GW_2015 <- st_transform(source_GW_2015, 4326)

gw_2015 <- source_GW_2015 %>%
  filter(Source == "GW")
sw_2015 <- source_GW_2015 %>%
  filter(Source == "SW")

pal = colorNumeric("viridis", reverse= TRUE, domain = gw_2015$Withdrawals, n = 10)
pal2 = colorNumeric("viridis", reverse= TRUE, domain = sw_2015$Withdrawals, n = 10)

leaflet() %>%
    addProviderTiles(providers$OpenStreetMap, group = 'Base') %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>%
    addPolygons(data = gw_2015,
                fillColor = ~pal(Withdrawals),
                fillOpacity = .5,
                color = 'black',
                group = 'GW') %>%
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

### Step 3: Organize data by sector and FIPS (definition here)


wu_1950 <- d_wu_1950 %>%
  mutate(State = area,
         "Public Supply" = ps_wgw_fr + ps_wsw_fr,
         "Irrigation" = ir_wgw_fr + ir_wsw_fr,
         "Rural" = NA,
         "Industrial" = inpt_wgw_fr + inpt_wsw_fr,
         "Thermoelectric" = NA,
         "Year" = 1950) %>%
  select("State", "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric", "Year") %>%
  pivot_longer(2:6, values_to = "Withdrawals", names_to = "Sectors")

wu_1955 <- d_wu_1955 %>%
  mutate(State = area,
         "Public Supply" = ps_wgw_fr + ps_wsw_fr,
         "Irrigation" = ir_wgw_fr + ir_wsw_fr,
         "Rural" = NA,
         "Industrial" = inpt_wgw_fr + inpt_wsw_fr,
         "Thermoelectric" = NA,
         "Year" = 1955) %>%
  select("State", "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric", "Year") %>%
  pivot_longer(2:6, values_to = "Withdrawals", names_to = "Sectors")


wu_1960 <- d_wu_1960 %>%
  mutate(State = area,
         Population = tp_tot_pop,
         "Public Supply" = ps_wgw_fr + ps_wsw_fr,
         "Irrigation" = ir_w_fr_to,
         "Rural" = do_wgw_fr + do_wsw_fr + ls_wgw_fr + ls_wsw_fr,
         "Industrial" = oi_wsw_fr + oi_wgw_fr,
         "Thermoelectric" = pt_wgw_fr + pt_wsw_fr,
         "Year" = 1960) %>%
  select("State", Population, "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric", "Year") %>%
  pivot_longer(3:7, values_to = "Withdrawals", names_to = "Sectors")

wu_1965 <- d_wu_1965 %>%
  mutate(State = area,
         Population = tp_tot_pop,
         "Public Supply" = ps_wgw_fr + ps_wsw_fr,
         "Irrigation" = ir_wgw_fr + ir_wsw_fr,
         "Rural" = do_wgw_fr + do_wsw_fr + ls_wgw_fr + ls_wsw_fr,
         "Industrial" = oi_wsw_fr + oi_wgw_fr,
         "Thermoelectric" = pt_wgw_fr + pt_wsw_fr,
         "Year" = 1965) %>%
  select("State", Population, "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric", "Year") %>%
  pivot_longer(3:7, values_to = "Withdrawals", names_to = "Sectors")

wu_1970 <- d_wu_1970 %>%
  mutate(State = area,
         Population = tp_tot_pop,
         "Public Supply" = ps_wgw_fr + ps_wsw_fr,
         "Irrigation" = ir_wgw_fr + ir_wsw_fr,
         "Rural" = do_wgw_fr + do_wsw_fr + ls_wgw_fr + ls_wsw_fr,
         "Industrial" = oi_wsw_fr + oi_wgw_fr,
         "Thermoelectric" = pt_wgw_fr + pt_wsw_fr,
         "Year" = 1970) %>%
  select("State", Population, "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric", "Year") %>%
  pivot_longer(3:7, values_to = "Withdrawals", names_to = "Sectors")

wu_1975 <- d_wu_1975 %>%
  mutate(State = area,
         Population = tp_tot_pop,
         "Public Supply" = ps_wgw_fr + ps_wsw_fr,
         "Irrigation" = ir_wgw_fr + ir_wsw_fr,
         "Rural" = do_wgw_fr + do_wsw_fr + ls_wgw_fr + ls_wsw_fr,
         "Industrial" = oi_wsw_fr + oi_wgw_fr,
         "Thermoelectric" = pt_wgw_fr + pt_wsw_fr,
         "Year" = 1975) %>%
  select("State", Population, "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric", "Year") %>%
  pivot_longer(3:7, values_to = "Withdrawals", names_to = "Sectors")

wu_1980 <- d_wu_1980 %>%
  mutate(State = area,
         Population = tp_tot_pop,
         "Public Supply" = ps_wgw_fr + ps_wsw_fr,
         "Irrigation" = ir_wgw_fr + ir_wsw_fr,
         "Rural" = do_wgw_fr + do_wsw_fr + ls_wgw_fr + ls_wsw_fr,
         "Industrial" = oi_wsw_fr + oi_wgw_fr,
         "Thermoelectric" = pt_wgw_fr + pt_wsw_fr,
         "Year" = 1980) %>%
  select("State", Population, "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric", "Year")%>%
  pivot_longer(3:7, values_to = "Withdrawals", names_to = "Sectors")

# Starting with 1985, we cannot add year column until summarize function, or we will get a summed year for each state
wu_1985 <- d_wu_1985 %>%
  mutate(State = scode,
       Population = po_total,
       "Public Supply" = ps_wgwfr + ps_wswfr,
       "Irrigation" = ir_wgwfr + ir_wswfr,
       "Rural" = do_ssgwf + do_ssswf + ls_gwtot + ls_swtot,
       "Industrial" = in_wgwfr + in_wswfr +	mi_gwtot + mi_swtot,
       "Thermoelectric" = pt_wgwfr + pt_wswfr)%>%
  select("State", Population, "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric") %>%
  group_by(State) %>%
  summarize(across(1:6, sum), Year = 1985) %>%
  pivot_longer(3:7, values_to = "Withdrawals", names_to = "Sectors")

wu_1990 <- d_wu_1990 %>%
  mutate(State = scode,
         Population = po_total,
         "Public Supply" = ps_wgwfr + ps_wswfr,
         "Irrigation" = ir_wgwfr + ir_wswfr,
         "Rural" = do_ssgwf + do_ssswf + ls_gwtot + ls_swtot,
         "Industrial" = in_wgwfr + in_wswfr +	mi_gwtot + mi_swtot,
         "Thermoelectric" = pt_wgwfr + pt_wswfr)%>%
  select("State", Population, "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric") %>%
  group_by(State) %>%
  summarize(across(1:6, sum), Year = 1990) %>%
  pivot_longer(3:7, values_to = "Withdrawals", names_to = "Sectors")

wu_1995 <- d_wu_1995 %>%
  mutate(State = state_code,
         Population = total_pop,
         "Public Supply" = ps_wgw_fr + ps_wsw_fr,
         "Irrigation" = ir_wgw_fr + ir_wsw_fr,
         "Rural" = do_wgw_fr + do_wsw_fr + ls_wgw_fr + ls_wsw_fr,
         "Industrial" = in_wgw_fr + in_wsw_fr +	mi_wgw_fr + mi_wsw_fr,
         "Thermoelectric" = pt_wgw_fr + pt_wsw_fr) %>%
  select("State", Population, "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric") %>%
  group_by(State) %>%
  summarize(across(1:6, sum), Year = 1995) %>%
  pivot_longer(3:7, values_to = "Withdrawals", names_to = "Sectors")

wu_2000 <- d_wu_2000 %>%
  mutate(State = statefips,
         Population = tp_tot_pop,
         "Public Supply" = ps_wgw_fr + ps_wsw_fr,
         "Irrigation" = it_wgw_fr + it_wsw_fr,
         "Rural" = do_wgw_fr + do_wsw_fr + ls_wgw_fr + ls_wsw_fr,
         "Industrial" = in_wgw_fr + in_wsw_fr +	mi_wgw_fr + mi_wsw_fr,
         "Thermoelectric" = pt_wgw_fr + pt_wsw_fr)%>%
  select("State", Population, "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric") %>%
  group_by(State) %>%
  summarize(across(1:6, sum), Year = 2000) %>%
  pivot_longer(3:7, values_to = "Withdrawals", names_to = "Sectors")

wu_2005 <- d_wu_2005 %>%
  mutate(State = statefips,
         Population = tp_tot_pop,
         "Public Supply" = ps_wgw_fr + ps_wsw_fr,
         "Irrigation" = ir_wgw_fr + ir_wsw_fr,
         "Rural" = do_wgw_fr + do_wsw_fr + ls_wgw_fr + ls_wsw_fr,
         "Industrial" = in_wgw_fr + in_wsw_fr +	mi_wgw_fr + mi_wsw_fr,
         "Thermoelectric" = pt_wgw_fr + pt_wsw_fr)%>%
  select("State",Population, "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric") %>%
  group_by(State) %>%
  summarize(across(1:6, sum), Year = 2005) %>%
  pivot_longer(3:7, values_to = "Withdrawals", names_to = "Sectors")

wu_2010 <- d_wu_2010 %>%
  mutate(State = statefips,
         Population = tp_tot_pop,
         "Public Supply" = ps_wgw_fr + ps_wsw_fr,
         "Irrigation" = ir_wgw_fr + ir_wsw_fr,
         "Rural" = do_wgw_fr + do_wsw_fr + li_wgw_fr + li_wsw_fr,
         "Industrial" = in_wgw_fr + in_wsw_fr +	mi_wgw_fr + mi_wsw_fr,
         "Thermoelectric" = pt_wgw_fr + pt_wsw_fr)%>%
  select("State",Population, "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric") %>%
  group_by(State) %>%
  summarize(across(1:6, sum), Year = 2010) %>%
  pivot_longer(3:7, values_to = "Withdrawals", names_to = "Sectors")

wu_2015 <- d_wu_2015 %>%
  mutate(State = statefips,
         Population = tp_tot_pop,
         "Public Supply" = ps_wgw_fr + ps_wsw_fr,
         "Irrigation" = ir_wgw_fr + ir_wsw_fr,
         "Rural" = do_wgw_fr + do_wsw_fr + li_wgw_fr + li_wsw_fr,
         "Industrial" = in_wgw_fr + in_wsw_fr +	mi_wgw_fr + mi_wsw_fr,
         "Thermoelectric" = pt_wgw_fr + pt_wsw_fr)%>%
  select("State",Population, "Public Supply", "Irrigation", "Rural", "Industrial", "Thermoelectric") %>%
  group_by(State) %>%
  summarize(across(1:6, sum), Year = 2015) %>%
  pivot_longer(3:7, values_to = "Withdrawals", names_to = "Sectors")


# ---------------------------------------------------------------------------
# ---------- READ IN DATA W/ GROUNDWATER + SURFACE COLUMNS ------------------


wu_1960 <- d_wu_1960 %>%
  mutate(State = area,
         Population = tp_tot_pop,
         "Groundwater" = ps_wgw_fr +ir_wgw_fr +do_wgw_fr + ls_wgw_fr + oi_wgw_fr + pt_wgw_fr,
         "Surface water" = ps_wsw_fr + ir_wsw_fr + do_wsw_fr + ls_wsw_fr + oi_wsw_fr + pt_wsw_fr,
         "Year" = 1960) %>%
  select(State, Population, Groundwater, "Surface water", Year) %>%
  pivot_longer(3:4, values_to = "Withdrawals", names_to = "Source")

wu_1965 <- d_wu_1965 %>%
  mutate(State = area,
         Population = tp_tot_pop,
         "Groundwater" = ps_wgw_fr +ir_wgw_fr +do_wgw_fr + ls_wgw_fr + oi_wgw_fr + pt_wgw_fr,
         "Surface water" = ps_wsw_fr + ir_wsw_fr + do_wsw_fr + ls_wsw_fr + oi_wsw_fr + pt_wsw_fr,
         "Year" = 1965) %>%
  select(State, Population, Groundwater, "Surface water", Year) %>%
  pivot_longer(3:4, values_to = "Withdrawals", names_to = "Source")

wu_1970 <- d_wu_1970 %>%
  mutate(State = area,
         Population = tp_tot_pop,
         "Groundwater" = ps_wgw_fr +ir_wgw_fr +do_wgw_fr + ls_wgw_fr + oi_wgw_fr + pt_wgw_fr,
         "Surface water" = ps_wsw_fr + ir_wsw_fr + do_wsw_fr + ls_wsw_fr + oi_wsw_fr + pt_wsw_fr,
         "Year" = 1970) %>%
  select(State, Population, Groundwater, "Surface water", Year) %>%
  pivot_longer(3:4, values_to = "Withdrawals", names_to = "Source") %>%
  filter(State != 0)

wu_1975 <- d_wu_1975 %>%
  mutate(State = area,
         Population = tp_tot_pop,
         "Groundwater" = ps_wgw_fr +ir_wgw_fr +do_wgw_fr + ls_wgw_fr + oi_wgw_fr + pt_wgw_fr,
         "Surface water" = ps_wsw_fr + ir_wsw_fr + do_wsw_fr + ls_wsw_fr + oi_wsw_fr + pt_wsw_fr,
         "Year" = 1975) %>%
  select(State, Population, Groundwater, "Surface water", Year) %>%
  pivot_longer(3:4, values_to = "Withdrawals", names_to = "Source")

wu_1980 <- d_wu_1980 %>%
  mutate(State = area,
         Population = tp_tot_pop,
         "Groundwater" = ps_wgw_fr +ir_wgw_fr +do_wgw_fr + ls_wgw_fr + oi_wgw_fr + pt_wgw_fr,
         "Surface water" = ps_wsw_fr + ir_wsw_fr + do_wsw_fr + ls_wsw_fr + oi_wsw_fr + pt_wsw_fr,
         "Year" = 1980) %>%
  select(State, Population, Groundwater, "Surface water", Year) %>%
  pivot_longer(3:4, values_to = "Withdrawals", names_to = "Source")

# Starting with 1985, we cannot add year column until summarize function, or we will get a summed year for each state

wu_1985 <- d_wu_1985 %>%
  mutate(State = scode,
         Population = po_total,
         "Groundwater" = ps_wgwfr + ir_wgwfr + do_ssgwf + ls_gwtot + in_wgwfr + mi_wgwfr + pt_wgwfr,
         "Surface water" =  ps_wswfr + ir_wswfr +do_ssswf + ls_swtot + in_wswfr +mi_wswfr + pt_wswfr) %>%
  select("State",Population, "Groundwater", "Surface water") %>%
  group_by(State) %>%
  summarize(across(1:3, sum), Year = 1985) %>%
  pivot_longer(3:4, values_to = "Withdrawals", names_to = "Source")

wu_1990 <- d_wu_1990 %>%
  mutate(State = scode,
         Population = po_total,
         "Groundwater" = ps_wgwfr + ir_wgwfr + do_ssgwf + ls_gwtot + in_wgwfr + mi_wgwfr + pt_wgwfr,
         "Surface water" =  ps_wswfr + ir_wswfr +do_ssswf + ls_swtot + in_wswfr +mi_wswfr + pt_wswfr) %>%
  select("State",Population, "Groundwater", "Surface water") %>%
  group_by(State) %>%
  summarize(across(1:3, sum), Year = 1990) %>%
  pivot_longer(3:4, values_to = "Withdrawals", names_to = "Source")

wu_1995 <- d_wu_1995 %>%
  mutate(State = state_code,
         Population = total_pop,
         "Groundwater" = ps_wgw_fr + ir_wgw_fr + do_wgw_fr + ls_wgw_fr + in_wgw_fr + mi_wgw_fr + pt_wgw_fr,
         "Surface water" =  ps_wsw_fr + ir_wsw_fr +do_wsw_fr + ls_wsw_fr + in_wsw_fr +mi_wsw_fr + pt_wsw_fr) %>%
  select("State",Population, "Groundwater", "Surface water") %>%
  group_by(State) %>%
  summarize(across(1:3, sum), Year = 1995) %>%
  pivot_longer(3:4, values_to = "Withdrawals", names_to = "Source")

wu_2000 <- d_wu_2000 %>%
  mutate(State = statefips,
         Population = tp_tot_pop,
         "Groundwater" = ps_wgw_fr + it_wgw_fr + do_wgw_fr + ls_wgw_fr + in_wgw_fr + mi_wgw_fr + pt_wgw_fr,
         "Surface water" =  ps_wsw_fr + it_wsw_fr +do_wsw_fr + ls_wsw_fr + in_wsw_fr +mi_wsw_fr + pt_wsw_fr) %>%
  select("State",Population, "Groundwater", "Surface water") %>%
  group_by(State) %>%
  summarize(across(1:3, sum), Year = 2000) %>%
  pivot_longer(3:4, values_to = "Withdrawals", names_to = "Source")

wu_2005 <- d_wu_2005 %>%
  mutate(State = statefips,
         Population = tp_tot_pop,
         "Groundwater" = ps_wgw_fr + ir_wgw_fr + do_wgw_fr + ls_wgw_fr + in_wgw_fr + mi_wgw_fr + pt_wgw_fr,
         "Surface water" =  ps_wsw_fr + ir_wsw_fr +do_wsw_fr + ls_wsw_fr + in_wsw_fr +mi_wsw_fr + pt_wsw_fr) %>%
  select("State",Population, "Groundwater", "Surface water") %>%
  group_by(State) %>%
  summarize(across(1:3, sum), Year = 2005) %>%
  pivot_longer(3:4, values_to = "Withdrawals", names_to = "Source")

wu_2010 <- d_wu_2010 %>%
  mutate(State = statefips,
         Population = tp_tot_pop,
         "Groundwater" = ps_wgw_fr + ir_wgw_fr + do_wgw_fr + li_wgw_fr + in_wgw_fr + mi_wgw_fr + pt_wgw_fr,
         "Surface water" =  ps_wsw_fr + ir_wsw_fr +do_wsw_fr + li_wsw_fr + in_wsw_fr +mi_wsw_fr + pt_wsw_fr) %>%
  select("State",Population, "Groundwater", "Surface water") %>%
  group_by(State) %>%
  summarize(across(1:3, sum), Year = 2010) %>%
  pivot_longer(3:4, values_to = "Withdrawals", names_to = "Source")


wu_2015 <- d_wu_2015 %>%
  mutate(State = statefips,
         Population = tp_tot_pop,
         "Groundwater" = ps_wgw_fr + ir_wgw_fr + do_wgw_fr + li_wgw_fr + in_wgw_fr + mi_wgw_fr + pt_wgw_fr,
         "Surface water" =  ps_wsw_fr + ir_wsw_fr +do_wsw_fr + li_wsw_fr + in_wsw_fr +mi_wsw_fr + pt_wsw_fr) %>%
  select("State",Population, "Groundwater", "Surface water") %>%
  group_by(State) %>%
  summarize(across(1:3, sum), Year = 2015) %>%
  pivot_longer(3:4, values_to = "Withdrawals", names_to = "Source")




### Step 4: Combine data

# Use rbind() to combine all data, then filter out fips codes not linked to one of 50 states

### 1960 - 2015 population column
wu_all_pop <- rbind(wu_1960, wu_1965, wu_1970, wu_1975, wu_1980, wu_1985, wu_1990, wu_1995, wu_2000, wu_2005, wu_2010, wu_2015) %>%
  filter(!State %in% c(0, 11, 72, 78))

saveRDS(wu_all_source, file = "docs/data/data/wu_all_source.rds")

wu_all_source <- rbind(wu_1960, wu_1965, wu_1970, wu_1975, wu_1980, wu_1985, wu_1990, wu_1995, wu_2000, wu_2005, wu_2010, wu_2015) %>%
  filter(!State %in% c(0, 11, 72, 78))


fips <- d_wu_2015 %>%
  select(state, State = statefips) %>%
  unique() %>%
  filter(!State %in% c(0, 11,72, 78))

wu_all_source <- inner_join(wu_all_source, fips, by = "State")






























