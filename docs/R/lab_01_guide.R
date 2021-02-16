
library(tidyverse)
library(here)

x = readr::read_csv(here('data/aquastat-pop.csv'))


example = 10
example = example + 4
example

example = 10
example = example + 4 # example will equal 14 just like we found out above
example = 12 # but if we REASSIGN example to a new value of 12 we no longer have the value of 14 created above
example

# 6 ways to subset a vector

# 1. Nothing returns the original vector.
water_demands[] # []

# 2. Positive integers return elements at the specified positions:
water_demands = c("Public Supply", "Domestic", "Industrial")

water_demands[1]

water_use = c(49.94, 0.52, 6.36)
water_use[c(1, 2)]


# 3. Negative integers exclude elements at the specified positions:

water_demands[-1]

water_use[-c(1, 2)]

# 4. Logical vectors select elements where the corresponding logical value is TRUE

water_use[water_use > 1] # water use greater than 1

water_use[water_use >= 6.36] # water use greater than or equal to 6.36

water_use[water_use == 0.52] # water use equal to 0.52


# 5. Zero returns a zero-length vector. We won't use this very much but it can be used to make test data

water_demands[0]


# 6. Named vector. If your vector has names, you can use character vectors to return elements with the matching names.
water_demands <- setNames(water_demands, c("sector_1", "sector_2", "sector_3"))
water_demands

water_demands[c("sector_1", "sector_3")]


water_demands[c("sector_1", "sector_1", "sector_2")] # If you repeat name indices you will get a repeated return












