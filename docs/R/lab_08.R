
# Angus Watters
# ENVS 193
# Lab 8

# Data prep & cleaning
library(janitor)
library(here)

# Data manipulation
library(tidyverse)

# Spatial packages
library(USAboundaries)
library(sf)



# Misc. cleaning steps


add_fips <- function(df) {
  df = df %>%
    mutate(statefips = state_code)
  df
}

wu_1995 <- add_fips(wu_1995)





# matching character type county column
df5$state <- as.character(df5$state)

# match county columns
wu_1995 <- rename(wu_1995, county = county_name)


# make all years have identical columns to bind together
df1 <- wu_2015 %>%
mutate(Domestic = ps_w_fr_to + do_w_fr_to,
       Industrial = in_w_fr_to,
       Agriculture = ir_w_fr_to + li_w_fr_to + aq_w_fr_to,
       Mining = mi_w_fr_to,
       Thermoelectric = pt_w_fr_to,
       Total = Domestic + Industrial + Agriculture + Mining + Thermoelectric) %>%
  select(1:7, last_col(1):last_col(5))

df2 <- wu_2010 %>%
  mutate(Domestic = ps_w_fr_to + do_w_fr_to,
         Industrial = in_w_fr_to,
         Agriculture = ir_w_fr_to + li_w_fr_to + aq_w_fr_to,
         Mining = mi_w_fr_to,
         Thermoelectric = pt_w_fr_to,
         Total = Domestic + Industrial + Agriculture + Mining + Thermoelectric) %>%
  select(1:7, last_col(1):last_col(5))

df3 <- wu_2005 %>%
  mutate(Domestic = ps_w_fr_to + do_w_fr_to,
         Industrial = in_w_fr_to,
         Agriculture = ir_w_fr_to,
         Mining = mi_w_fr_to,
         Thermoelectric = pt_w_fr_to,
         Total = Domestic + Industrial + Agriculture + Mining + Thermoelectric) %>%
  select(1:7, last_col(1):last_col(5))

df4 <- wu_2000 %>%
  mutate(Domestic = ps_w_fr_to + do_w_fr_to,
         Industrial = in_w_fr_to,
         Agriculture = it_w_fr_to,
         Mining = mi_w_fr_to,
         Thermoelectric = pt_w_fr_to,
         Total = Domestic + Industrial + Agriculture + Mining + Thermoelectric) %>%
  select(1:7, last_col(1):last_col(5))
df5 <- wu_1995 %>%
  mutate(Domestic = ps_w_fr_to + do_w_fr_to,
         Industrial = in_w_fr_to,
         Agriculture = ir_w_fr_to,
         Mining = mi_w_fr_to,
         Thermoelectric = pt_w_fr_to,
         Total = Domestic + Industrial + Agriculture + Mining + Thermoelectric) %>%
  select(1:7, last_col(1):last_col(5))

df6 <- wu_1990 %>%
  mutate(Domestic = ps_wtofr + do_sstot,
         Industrial = in_wtofr,
         Agriculture = ir_frtot,
         Mining = mi_frtot,
         Thermoelectric = pt_frtot,
         Total = Domestic + Industrial + Agriculture + Mining + Thermoelectric) %>%
  select(1:7, last_col(1):last_col(5))

df7 <- wu_1985 %>%
  mutate(Domestic = ps_wtofr + do_sstot,
         Industrial = in_wtofr,
         Agriculture = ir_frtot,
         Mining = mi_frtot,
         Thermoelectric = pt_frtot,
         Total = Domestic + Industrial + Agriculture + Mining + Thermoelectric) %>%
  select(1:7, last_col(1):last_col(5))

# 2005 and 2000 need year cols
df3 <- df3 %>% mutate(year = 2005)
df4 <- df4 %>% mutate(year = 2000)



state <- USAboundaries::us_states()

state$statefp = gsub("0", '', state$statefp)

state <- state %>%
  select(statefips = statefp, state = state_abbr) %>%
  st_drop_geometry()

state$statefips <- as.numeric(state$statefips)

df5 <- df5 %>%
  mutate(statefips = state_code)

df5 <- left_join(df5, state, by = "statefips")
rm(lst)
# make a list of the data frames
lst <- list(df1, df2, df3, df4, df6, df7)

# bind the rows of the 4 data frames in the list
timeseries <- bind_rows(lst)

# remove the string "County" from any of the county cols
timeseries$county = gsub(" County", '', timeseries$county)

# summing the sectors for each state and each year
timeseries_sums <- timeseries %>%
  group_by(state, year) %>%
  summarise(across(Thermoelectric:Domestic, sum))

# pivot longer for plotting
timeseries_sums <- timeseries_sums %>%
  pivot_longer(cols = c(3:7), names_to = "sector", values_to = "withdrawal")


conus_sum <- timeseries_sums %>%
  group_by(sector, year) %>%
  summarise(withdrawal = sum(withdrawal))

conus_sum$year = gsub("1986", "1985", conus_sum$year)

conus_sum2 <- conus_sum %>%
  group_by(sector, year) %>%
  summarise(Totals = sum(withdrawal))

conus_sum2 <- conus_sum2 %>%
  group_by(year) %>%
  arrange(desc(sector)) %>%
  mutate(sector2=factor(sector, levels= c("Domestic", "Agriculture", "Thermoelectric", "Industrial", "Mining")))

conus_total <- conus_sum2 %>%
  group_by(year) %>%
  summarise(Total = sum(Totals))

ggplot() +
  geom_point(data = conus_total, aes(x = year, y = Total)) +
  geom_line(data = conus_total, aes(x = year, y = Total)) +
  geom_col(data = conus_sum2, aes(x = year, y = Totals, fill = sector2), position = position_dodge()) +
  scale_fill_manual(values = c("dodgerblue", "green4", "darkgoldenrod2", "tomato3", "darkgray")) +
  scale_y_continuous(labels = scales::comma, sec.axis = ~ ./4)

# ------------------------------------------------------------------------------------------------
# ------------- Function makes sector columns and returns dataframe with 12 columns --------------




# creates sector cols and selects those and first 7 cols, doesn't work great as the sector columns vary in some years.
trim_cols <- function(df) {
  sectors <-  df %>%
    mutate(Domestic = ps_w_fr_to + do_w_fr_to,
           Industrial = in_w_fr_to,
           Agriculture = it_w_fr_to,
           Mining = mi_w_fr_to,
           Thermoelectric = pt_w_fr_to,
           Total = Domestic + Industrial + Agriculture + Mining + Thermoelectric) %>%
    select(1:7, last_col(1):last_col(5))
  sectors
}

# apply trim to df so we have the same cols in all dfs
df1 = trim_cols(wu_2015)
df2 = trim_cols(wu_2010)
df3 = trim_cols(wu_2005)
df4 = trim_cols(wu_2000)






