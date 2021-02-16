
# Angus Watters
# Big Data in water
# Data wrangling + manipulation + organizing + cleaning
# 12/26/2020

# Libraries + how to install
install.packages("gapminder")

library(tidyverse)
library(gapminder)
x = getwd()
install.packages('here')
# Read in data from gapminder package
gapminder = gapminder

across(mutate(()

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

head(gapminder) # head default displays the first 6 rows of a dataframe
gapminder %>% head() # same return as above but using the pipe

head(gapminder, 10) # displays first 10 rows
gapminder %>% head(10) # same return as above but using the pipe

class(gapminder)


#######################################################################
######################### SELECT() & FILTER() #########################
#######################################################################
# SELECT
# select() subsets variable or columns you want
# data.frame is ALWAYS first argument
select(gapminder, country)
select(gapminder, country, year)

# select can remove columns, ! negates selection
select(gapminder, !country)

# rename existing columns by new_name = old_name
select(gapminder, Country = country, Year = year)
gapminder %>% select(Country = country, Year = year) # same as above but using the pipe

# FILTER
# takes logical (binary) expression and returns rows in which condition is TRUE
# does NOT impact columns
# data.frame is always first argument

# filter for all rows in which the country is Mexico
# NOTICE that we use the binary expression "==" instead of assignment expression "="
filter(gapminder, country == 'Mexico')
gapminder %>% filter(country == 'Mexico')

# Now we'll filter for all rows in which the country is NOT Mexico
# the binary expression "!=" means find all rows where the country column is NOT equal to the character string 'Mexico'
filter(gapminder, country != 'Mexico')
gapminder %>% filter(country != 'Mexico')

# Next, we'll filter for all rows in which the country is Mexico AND the year is 2007
# NOTICE 2007 is a numeric thus quotations are not needed
class(gapminder$year) # In the column year from the gapminder dataframe, the values are class integer
class(gapminder$country) # In the column country from the gapminder dataframe, the values are class factor

filter(gapminder, country == 'Mexico', year == 2007)
gapminder %>% filter(country == 'Mexico', year == 2007) # using pipe

# filter for rows in which the year is greater than or equal to 1980
filter(gapminder, year >= 1980)
gapminder %>% filter(year >= 1980) # using pipe

# filter for rows in which the year is between 1980 and 2010
filter(gapminder, year >= 1980, year <= 2010)
gapminder %>% filter(year >= 1980, year <= 2010) # using pipe

# filter all rows where the country column contains 'Mexico', 'Brazil', and 'Afghanistan' and the year is after 2000
# c() creates a list, c() is not necessary with %in% operator if filtering only one value --- 'Brazil'
filter(gapminder, country %in% c("Mexico", "Brazil", "Afghanistan"), year >= 2000)
gapminder %>% filter(country %in% c("Mexico", "Brazil", "Afghanistan"), year >= 2000) # using pipe

# filter all rows where the year column contains 2002, 2007, 2017 and the country is Brazil
filter(gapminder, year %in% c(2002, 2007, 2017), country == 'Brazil')
gapminder %>% filter(year %in% c(2002, 2007, 2017), country == 'Brazil') # using pipe


# ALL OF THESE FILTERS CAN BE ACCOMPLISHED WITH BASE R CODE:
# FOLLOWING CODE READS AS:
# filter rows of gapminder dataframe (gapminder[row, col)
# in which the column named country in the gapminder equals Chile (gapminder$country == 'Chile')
# AND (&) the column named year equals 2007 (gapminder$year == 2007)

gapminder[gapminder$country == 'Chile' & gapminder$year == 2007, ]
filter(gapminder, country == 'Chile', year == 2007) # same as above code
gapminder %>% filter(country == 'Chile', year == 2007) # using pipe


#######################################################################
####################### EXTENDING THE PIPE ############################
#######################################################################

gapminder %>% filter(year == 2007) %>% #filter for only the rows containing year of 2007
  select(country, lifeExp) # select only the columns country and lifeExp after filtering for year == 2007

gapminder %>% select(country, !lifeExp)  %>% # select all columns that are NOT lifeExp
  filter(country == 'Angola') # filter the remaining columns for rows in which the country equals Angola


#######################################################################
############### MUTATE() + GROUP_BY() + ARRANGE() #####################
#######################################################################

# MUTATE adds a new column to the dataframe
mutate(gapminder, gdp = pop*gdpPercap) # adds a gdp column by multiplying the pop and gdpPercap columns
gapminder %>% mutate(gdp = pop*gdpPercap) # using the pipe

# GROUP_BY() - takes a dataframe and creates a grouped dataframe defined by a variable, take note of the structure shown above the data after running
gapminder # before any grouping operation we have 1704 rows and 6 columns
group_by(gapminder, continent) # when we group by continent we know have 5 groups

# operations are now performed "by group"
no_groups = gapminder %>% mutate(total_pop = sum(pop)) # look what happens if we make a new column that sums up all the values in the pop column; the new column has added up every pop value

groups = gapminder %>% group_by(continent) %>% # now if we group by continent, we can perform the same operation but applying it to each of our 5 groups individually
  mutate(total_pop = sum(pop)) # the new column now will generate a sum of the pop columns by continent (continent is our grouping variable)


#######################################################################
####################### SUBSETTING A DATAFRAME ########################
#######################################################################

df = data.frame(state = c("California", "Arizona", "Texas"),
                 size_rank  = c(1,3,2),
                 pop = c(T,F,F))

# ASPECTS OF THE DATAFRAME
typeof(df)
attributes(df)
str(df)
dim(df) #3 objects by 3 variables

# DIFFERENT WAYS OF SUBSETTING
df[1,2]

df[2,]

df[[1]][1]

df$state[1] # first row in column state

df$pop[2] # second row in column pop

df[1:2] # columns 1, 2 of dataframe



constraints = matrix(c(x1, x2, x3, x4, x5, x6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), nrow=3)







mverkuilen@ucsb.edu











