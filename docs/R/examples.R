
# Angus Watters
# Big Data in water
# Data wrangling + manipulation + organizing + cleaning
# 12/18/2020

# Libraries + how to install
library(tidyverse)
library(readr)

# Read in data from data folder
aquastat = read_csv(file ='data/aquastat-data.csv')

# Subsetting a dataframe:

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

tmp1 = aquastat %>% pivot_wider(names_from = 'Variable Name', values_from = Value)

######################################


# RStudio terminal

The RStudio terminal provides access to the operating system (OS) shell from within the RStudio IDE. Beneath the surface of any OS is an entire world that can only be accessed only from the command line.

The terminal is a gateway to that command line.

With it, instead of pointing and clicking, you can type commands and have your computer respond.

# Locate the terminal

The terminal is a computer application. On a Mac, it is most frequently accessed via your Terminal application , and on Windows, through PowerShell. Fortunately, RStudio provides a built in terminal that imitates these applications within the RStudio IDE. The Terminal tab can be found next to the Console tab. If it is not visible, show it via Shift+Alt+T (or Tools > Terminal > New Terminal). Once visible, it should look something like this:


  # Terminal commands

  Using the terminal expedites basic tasks such as creating new files and folders, exploring folder structures, and seeing what it can tell us about our computer system. Here are 6 common commands that are useful for navigating a file system and creating files.

## pwd - print working directory
displays directory or folder you are currently in

```{r}
'pwd'
print('c/Users/angus/OneDrive/Desktop/github/big-water-data/docs/R')

```

## mkdir - make a directory
mkdir creates a new folder (directory) inside your current working directory

to make a new folder called 'docs'

```{r}
'mkdir docs'
print('c/Users/angus/OneDrive/Desktop/github/big-water-data/docs/R')

```




# Installing git

## Step 1: Check if git is already installed
In your terminal, type the following (note: PC USERS type where git) and hit Enter.

If git is installed, it will return a path like below.

```{r}
'which git'

```
If prompted to install git do it, if not process to Step 2 to manually install


## Step 2: Manually install git

**You should only have to do this if git was NOT found in Step 1 !!!**

  ### **MacOS**
  Install the Xcode command line tools (not all of Xcode), which includes Git, by typing the following in your terminal:

  ```{r}
'xcode-select --install'

```

### **Windows**
Install Git for Windows, also known as “Git Bash”, to get Git and some other useful tools, such as the Bash shell.

‘Git for Windows’ places the Git executable in the conventional location, which will help you and other programs (like RStudio) find and use it.

When asked about “Adjusting your PATH environment”, make sure to select “Git from the command line and also from 3rd-party software”. Otherwise, accept the defaults unless you have specific reasons not to.

Once git is installed - restart RStudio and make sure that you can see the git pathway (which/where git) in the terminal. Once installed, continue to Step 3.

## Step 3: Check the version
After git has installed, type ***git --version*** into your terminal and press Enter to display the version of git.


```{r}
'git --version'
'git version 2.28.0.windows.1'
```


## Github

### Create a GitHub account
- Go to Github and make an account
- Choose your email and user name wisely as it will appear on all websites and repos you

### Find and fork a repository
- Through GitHub you can find, share, and use open source code uploaded by other people.
- In the top right hand corner, enter ***anguswg-ucsb/big-water-data*** into the search bar to find your first repository

Once you find the repository, notice the 3 options in top right hand corner

- **watch**: will notify you via email when changes to the repo are made (notifications can be a lot)
- **star**: places the repo in “stared” repositories section making it convenient to find
- **fork**: this makes a copy of the current repo in your account that you can modify and build on.



### Tell Git who you are
You now have a local Git and a GitHub account, the next step is to connect them:

  The first thing we need to do is tell git who we are. Git comes with a tool called git config that lets us set the configuration variables that control how Git looks and operates.

In the RStudio Terminal, enter the following, where YOUR NAME is what you want git to know you by (this can be your user.name or real name)

```{r}
"git config --global user.name 'YOUR NAME'"
```

- what this did: We used the termial to the local Git program to set a global configuration variable, called ***user.name*** to the specified 'YOUR NAME'

We will then do the same thing, telling Git the email associated with your new GitHub account


```{r}
"git config --global user.email 'Your Email Here'"
```

After you have done both, enter the following into your terminal to list your global configuration variables:

  ```{r}
"git config --list --global"

"user.name=anguswg-ucsb"
'user.email=anguswatters@gmail.com'
'filter.lfs.clean=git-lfs clean -- %f'
'filter.lfs.smudge=git-lfs smudge -- %f'
'filter.lfs.process=git-lfs filter-process'
'filter.lfs.required=true'
```

Make sure to check that your **user.name** and **user.email** are the same as your input

### Create local directory for Git-enabled projects

The last thing we will do is make a folder in our **home** directory named 'Github'. This is not necissarily a necessary step but it helps alot with file organization!

  In 2 steps:

  #### 1. Change to home directory

  ```{r}

'cd~'

```

#### 1. Change to home directory

```{r}
"mkdir github"
```


