# Debra Perrone, lab_04, 1/21/2021

library(tidyverse)
library(here)
library(janitor)
library(readxl)

# Prelab_04
# Estimated Use of Water in the United States County-Level Data for 2015 (USGS)
water_use <- read_xlsx(here("data/usco2015v2.0.xlsx"), skip = 1) %>%
  clean_names()

# Import data dictionary, clean, pivot, clean, select, pivot
data_dictionary <- read_xlsx(here('data/usco2015v2.0.xlsx'), 2) %>%
  clean_names() %>%
  pivot_wider(1:2, names_from = "column_tag", values_from = "attribute") %>%
  clean_names() %>%
  select(contains("w_fr_to")) %>%
  pivot_longer(1:13, names_to = "column_tag",  values_to = "attribute")

# Import coordinates
centroids <-  readRDS("data/state_centroids.rds")


# Select only columns with w_fr_to
# freshwater_select <- select(water_use, state, contains("w_fr_to"))

# Note that summarize creates error if we do not mutate_at! as.numeric
# Mutate_at then creates NAs which will cauase issues!! 
freshwater_select <- water_use %>%
select(state, contains("w_fr_to")) %>%
  mutate_at(2:14, as.numeric) %>%
  group_by(state) %>%
  summarize(across(everything(), mean)) %>%
  # summarize(across(c(2:14), mean))
  filter(state != "DC", state != "PR", state != "VI") %>%
  inner_join(centroids, by = "state") %>%
  replace(is.na(.), 0) %>%
  mutate(Domestic = ps_w_fr_to + do_w_fr_to, 
         Industrial = po_w_fr_to + pc_w_fr_to + in_w_fr_to + mi_w_fr_to, 
         Agricultural = ir_w_fr_to + li_w_fr_to + aq_w_fr_to,
         Total = to_w_fr_to, 
         Total_check = Domestic + Industrial + Agricultural) %>%
  arrange(desc(Total)) %>%
  slice(1:20) %>%
  select(State = state, Domestic, Industrial, Agricultural, lng) %>%
  pivot_longer(2:4, names_to = "Sector",  values_to = "Withdrawal" )




ggplot(freshwater_select, aes(reorder(State, lng), Withdrawal)) +
geom_bar(aes(fill = Sector), position = position_dodge(0.5), width = .5, stat="identity")  +       
  labs(x = "State", 
      y = "Freshwater Withdrawals (Mgal/day)", 
      caption = "Figure 1: Total withdrawals by major sector in the USA. Data from USGS (2015). Created by D Perrone", 
      fill = "") +   
  theme(axis.text = element_text(size = 7), 
        text = element_text(size=9),
        legend.position="top", 
        plot.caption = element_text(hjust = 0, face = "bold", size = 10)) +
  scale_fill_manual(values = c("darkgreen","deepskyblue3", "darkorange"))












