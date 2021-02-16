
############## STEP 10 ##############
# Definition: Total annual amount of water withdrawn per capita.
# Calculation Criteria: [Total water withdrawal per capita] = [Total water withdrawal]*1000/[Total population]

# Unit: m3/year per inhabitant
# *** STEP 10.1 ***
# convert pop to actual number by multiplying by 1000
water_per_cap <- water_use %>% select(state, county, population, to_wtotl) %>%
  mutate(to_wtotl = to_wtotl*3875)

water_per_cap <- water_per_cap %>%
  group_by(state) %>%
  mutate(state_pop = sum(population)) %>%
  mutate(water_withdrawal_per_cap = to_wtotl/state_pop)

ggplot(water_per_cap, aes(x = reorder(state, -water_withdrawal_per_cap), y = water_withdrawal_per_cap)) +
  geom_col(aes(fill = state)) +
  labs(title = "Water withdrawal per capita")
# *** STEP 10.2 ***
# convert to_wtotl from Mgal to cubic meters (https://pubs.usgs.gov/wdr/wdr-id-03-3/ID03v3-InsideBackCover.pdf)
#3.785*1000

hist(water_per_cap$water_withdrawal_per_cap, breaks = 100, xlim = c(0, 300))


alloc <- sf::read_sf("docs/data/Network.dbf")
