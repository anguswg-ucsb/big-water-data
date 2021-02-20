
# Angus Watters
# ENVS 193
# Lab 8

# Data prep & cleaning
library(janitor)
library(here)
library(readxl)

# Data manipulation
library(tidyverse)
library(ggthemes)

# ggplot alterations
library(scales)
library(ggthemes)

# Step 1:
# Read in RDS
wu_all = readRDS("docs/data/data/wu_all.rds")
wu_all_source = readRDS("docs/data/data/wu_all_source.rds")

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

# change the "State" column to fips to make things less confusing with the state abbr.
wu_all <- rename(wu_all, fips = State)

# replace NA w/ 0
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
             shape = 1) + # make an open circle point
  scale_fill_manual(values = c("darkgray", "green4", "dodgerblue", "tomato3", "darkgoldenrod2")) +
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n = 10),
                     expand = c(0, 0),
                     limits = c(0, NA),
                     sec.axis = sec_axis(trans = ~.+150000, # transform the second axis by +150000 to give more scale to the total line
                                         breaks = scales::pretty_breaks(n = 12), # increase num. breaks on second scale
                                         name= "Total withdrawals (Mgal/day)",
                                         labels = scales::comma)) + # give commas to second scale labels
  expand_limits(y = max(conus_total$Total) + 20000) + # deals w/ line cut off by expanding the limits to 20000 greater than the max of the line
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  labs(title = "Trends in total water withdrawals by water-use category (1950 - 2015)",
       x = "",
       y = "Withdrawals (Mgal/day)",
       fill = "") +
  ggthemes::theme_few() + # matches USGS plot best
  theme(axis.text = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),# Changes grid style
        plot.caption = element_text(hjust = 0, color = "black", size = 9, face = "bold"),
        legend.position = "bottom")


#--------------------------- STEP 4: --------------------------
#-------------------- GW vs. SW BAR PLOT ----------------------

# Read in gw, sw, and pop water use data
wu_all_source = readRDS("docs/data/data/wu_all_source.rds")

# replace NA w/ 0
wu_all_source <- wu_all_source %>%
  replace(is.na(.), 0)

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

#--------------------- PLOT 2 ---------------------

ggplot(data = sw_gw, aes(x = Year, y = Withdrawals)) +
  geom_col(aes(fill = Source),  col = "black",
           position = position_dodge(3.2),
           width = 3) +
  geom_line(aes(y = total_pop, col = Population), size = 2) +
  scale_color_manual(values = c("deeppink")) +
  scale_fill_manual(values = c("lightsteelblue1", "deepskyblue", "midnightblue")) +
  # scale_color_manual(values = c("deeppink")) +
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n = 10),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
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




# #######################################################################
# --------------------------- # READ IN DATA ----------------------------


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






# ###########################################################################
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


# ##################################################################################
# ------------------------ BIND DATAFRAMES + SAVE RDS ------------------------------


### Step 4: Combine data

# Use rbind() to combine all data, then filter out fips codes not linked to one of 50 states




wu_all_source <- rbind(wu_1960, wu_1965, wu_1970, wu_1975, wu_1980, wu_1985, wu_1990, wu_1995, wu_2000, wu_2005, wu_2010, wu_2015) %>%
  filter(!State %in% c(0, 11, 72, 78))

fips <- d_wu_2015 %>%
  select(state, State = statefips) %>%
  unique() %>%
  filter(!State %in% c(0, 11,72, 78))

wu_all_source <- inner_join(wu_all_source, fips, by = "State")

saveRDS(wu_all_source, file = "docs/data/data/wu_all_source.rds")


# ###########################################################################
# ------------------------ READ IN DATA WU_ALL ------------------------------

###


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





























