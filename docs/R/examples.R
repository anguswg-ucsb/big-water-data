
# Angus Watters
# Big Data in water
# Data wrangling + manipulation + organizing + cleaning
# 12/18/2020


# STEP 1 --- INSTALL PACKAGES & LOAD LIBRARIES
# install a package ---> install.packages("<package_name>")
# load library into workspace ---> library(package)

# Libraries
library(tidyverse)
library(readr)

# STEP 2 --- READ IN DATA FROM DATA FOLDER

data()
# aquastat = read_csv(file ='data/aquastat-data-02.csv')
#
# tmp1 = aquastat %>% mutate(row = 1:n()) %>%
#   pivot_wider(names_from = 'Variable Name', values_from = Value)
base::Sys.which("bash")
pop = read_csv(file ='data/aquastat-pop.csv')
pop = pop %>% pivot_wider(names_from = 'Variable Name', values_from = 'Values') %>%
  select(country = Area,
         year = Year,
         total_pop = `Total population`)
huron = LakeHuron
pop = pop %>% slice(n = 1:2220)

water = read_csv(file ='data/aquastat-water-resources.csv') %>%
  pivot_wider(names_from = 'Variable Name', values_from = Value) %>%
  select(country = Area,
         year = Year,
         total_water = `Total renewable water resources`)

water = water %>%
  group_by(country) %>%
  arrange(-year) %>%
  slice(n = 1) %>%
  na.omit()

join = left_join(pop, select(water, country, total_water), by = 'country')

class(join$total_water[1])

tmp1 = join %>% mutate(total_pop = unlist(total_pop))
class(tmp1$total_pop[1])
tmp1 = tmp1 %>% mutate(total_water = unlist(total_water))

class(join$total_pop)
# Like a Matrix!
aquastat[3,4]
aquastat[2,]

# Like a list!
aquastat[[3]]

# Like a vector
aquastat$age[2]

# 1. Grammar of Data Manipulation:
# - dplyr is a package for data manipulation
# - It is built to be fast, flexable and generic about how your data is stored.
# - It is installed as part of the tidyverse meta-package
# - Think of this as a consistent set of verbs that help you solve common data manipulation challenges

# 'PURE' verbs:
# select()
# - picks variables based on names
# filter()
# - picks cases based on their values.

# "MANIPULATION" verbs
# mutate()
# - adds new variables that are functions of existing variables
# summarise()
# - reduces multiple values down to a single summary.
# arrange()
# - changes the ordering of the rows.

head(aquastat)
class(aquastat)

# SELECT
# select() subsets variable or columns you want
# data.frame is ALWAYS first argument
select(aquastat, Area)
select(aquastat, Area, 'Variable Name')

# select can remove columns, ! negates selection
select(aquastat, !Area)

# rename existing columns by new_name = old_name
aquastat = select(aquastat, country = Area, area_id = 'Area Id', variable = 'Variable Name', year = Year,
            value = Value, symbol = Symbol, md = Md)

# FILTER
# takes logical (binary) expression and returns rows in which condition is TRUE
# does NOT impact columns
# data.frame is always first argument

# filter for all rows in which the country is Mexico
# NOTICE that we use the binary expression "==" instead of assignment expression "="
filter(aquastat, country == 'Mexico')

# Now we'll filter for all rows in which the country is NOT Mexico
# the binary expression "!=" means find all rows where the country column is NOT equal to the character string 'Mexico'
filter(aquastat, country == 'Mexico')

# Next, we'll filter for all rows in which the country is Mexico AND the year is 2007
# NOTICE 2007 is a numeric thus quotations are not needed
class(aquastat$year) # In the column year from the aquastat dataframe, the values are class numeric
class(aquastat$country) # In the column country from the aquastat dataframe, the values are class character

filter(aquastat, country == 'Mexico', year == 2007)

# filter for rows in which the year is greater than or equal to 1980
filter(aquastat, year >= 1980)

# filter for rows in which the year is between 1980 and 2010
filter(aquastat, year >= 1980, year <= 2010)

# filter all rows where the country column contains 'Mexico', 'Brazil', and 'Afghanistan' and the year is after 2000
# c() creates a list, c() is not necessary with %in% operator if filtering only one value --- 'Brazil'
filter(aquastat, country %in% c("Mexico", "Brazil", "Afghanistan"), year >= 2000)

# filter all rows where the year column contains 2002, 2007, 2017 and the country is Brazil
filter(aquastat, year %in% c(2002, 2007, 2017), country == 'Brazil')

# ALL OF THESE FILTERS CAN BE ACCOMPLISHED WITH BASE R CODE:
# FOLLOWING CODE READS AS:

# filter rows of aquastat dataframe (aquastat[row, col)
# in which the column named country in the aquastat equals Chile (aquastat$country == 'Chile')
# AND (&) the column named year equals 2007 (aquastat$year == 2007)
aquastat[aquastat$country == 'Chile' & aquastat$year == 2007, ]



######################################





































