library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

state_names_and_codes <- read.csv("~/Documents/info201/assignments/a4-paigestanley1/source/state_names_and_codes.csv")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here 

# Write a paragraph of summary information, citing at least three values calculated 
# from the data. Your goal is to summarize some of the key variables that you are 
# interested in. Report the values and explain why they are important; that is, how 
# do the variables and value help you to understand patterns of inequality in the prison system.
# Examples: 
# What is the average value of my variable across all the counties (in a given year)? 
# Where is my variable the highest or lowest?  
# How much has my variable change over the last N years?

# 1. Total jail populations by race.

# a. Black jail population

  # create a vector of the total number of Black people in jail.

  total_jail_blackpop <- incarceration_trends %>%
    pull(black_jail_pop)

  # Find the sum of all the values in the vector 

total_jail_blackpop <- sum(total_jail_blackpop, na.rm=TRUE)

# b. Asian and Pacific Islander jail population
  
  # create a vector of the total number of Asian and Pacific Islander people in jail.
  
  total_jail_aapipop <- incarceration_trends %>%
    pull(aapi_jail_pop)
  
  # Find the sum of all the values in the vector 
  
  total_jail_aapipop <- sum(total_jail_aapipop, na.rm=TRUE)
  
# c. Latinx jail population
  
  # create a vector of the number of Latinx people in each county in 2018.
  
  total_jail_latinxpop <- incarceration_trends %>%
    pull(latinx_jail_pop)
  
  # Find the sum of all the values in the vector 
  
  total_jail_latinxpop <- sum(total_jail_latinxpop, na.rm=TRUE)
  
# d. Native jail population
  
  # create a vector of the number of Native people in jail in each county in 2018.
  
  total_jail_nativepop <- incarceration_trends %>%
    pull(native_jail_pop)
  
  # Find the sum of all the values in the vector 
  
  total_jail_nativepop <- sum(total_jail_nativepop, na.rm=TRUE)
  
# e. White jail population
  
  # create a vector of the number of white people in jail in each county in 2018.
  
  total_jail_whitepop <- incarceration_trends %>%
    pull(white_jail_pop)
  
  # Find the sum of all the values in the vector 
  
  total_jail_whitepop <- sum(total_jail_whitepop, na.rm=TRUE)


#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

# Use DPLYR and ggplot2 to replicate Figure 1 (above).  That is, produce a bar chart 
# that shows the growth of the U.S. prison population from 1970 to 2018.  To organize 
# your code, you should create two functions: 
# - get_year_jail_pop() : This data wrangling function should return a data frame that 
# is suitable for visualization. This function takes no parameters. 
# - plot_jail_pop_for_us() : This plotting function should return the chart. This 
# function: (1) Takes no parameters; and (2) Should call the data wrangling function.
# 1. Chart. Present the chart.
# 2. Chart caption. Include a caption that describes the chart.
# 3. Summary paragraph. A brief paragraph (50 words or more) that summarizes the key 
# patterns that appear to be revealed in the chart.
  
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#

get_year_jail_pop <- function() {
  total_jail_pop <- incarceration_trends %>%
    pull(total_jail_pop)
  
  year_jail_pop <- incarceration_trends %>%
    group_by(year) %>%
    summarize(total_jail_pop = sum(year))
  return()   
}

plot_jail_pop_for_us <- function() {
   us_growth_plot <- ggplot(data = incarceration_trends) +
    geom_col(
      mapping = aes(x = year, y = total_jail_pop)
    ) +
    labs(
      title = "Growth of the U.S. Prison Population",
      x = "Year",
      y = "Total U.S. Jail Population",
      caption = "This chart shows the total amount of people in U.S. prisons each year from 1970 to 2018")
  return()   
} 

## Section 4  ---- 
# Use DPLYR and ggplot2 to produce a line chart that shows the growth of the U.S. 
# prison population from 1970 to 2018 by one or more states. You should write two 
# functions: 
# - get_jail_pop_by_states(states): This data wrangling function should return a 
# data frame that is suitable for visualization. The parameter states should be a 
# vector of states.
# - plot_jail_pop_by_states(states): This plotting function should return the chart. 
# The parameter states should be a vector of states. This function should call the 
# data wrangling function.
# If plot_jail_pop_by_states(c("WA", "OR", "CA")) is called it will produce a line 
# chart with three lines, one for each of the states.  Show more than three states but fewer than 10 states.  

# For the report, include the following in this section: 
# 1. Chart. Present the chart.
# 2. Chart caption. Include a caption that names and briefly describes the chart.
# 3. Summary paragraph. Include a brief paragraph (50 words or more) that summarizes 
# the key patterns that appear to be revealed in the chart. Explain the reason for the states that you show.

#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

# Your functions might go here ... <todo:  update comment>
# See Canvas

get_jail_pop_by_states <- function(states) {
  GA <- incarceration_trends %>%
    group_by(state, year) %>%
    filter(state == "GA") %>%
    summarize(total_jail_pop = sum(year))
  
  TX <- incarceration_trends %>%
    group_by(state, year) %>%
    filter(state == "TX") %>%
    summarize(total_jail_pop = sum(year))

  FL <- incarceration_trends %>%
    group_by(state, year) %>%
    filter(state == "FL") %>%
    summarize(total_jail_pop = sum(year))
  
  OH <- incarceration_trends %>%
    group_by(state, year) %>%
    filter(state == "OH") %>%
    summarize(total_jail_pop = sum(year))
  return(states)
}

plot_jail_pop_by_states <- function(states) {
  state_growth_plot <-
    ggplot() +
    geom_line(data = GA, mapping = aes(x = year, y = total_jail_pop), color = "blue") +
    geom_line(data = TX, mapping = aes(x = year, y = total_jail_pop), color = "red") +
    geom_line(data = FL, mapping = aes(x = year, y = total_jail_pop), color = "green") +
    geom_line(data = OH, mapping = aes(x = year, y = total_jail_pop), color = "yellow") +
    labs(
      title = "Growth of the Prison Population by State",
      x = "Year",
      y = "Total State Jail Population",
      caption = "This chart shows the growth of prison populations in Texas (red), Georgia (blue), Florida (green), and Ohio (yellow)
      each year from 1970 to 2018")
    return(states)
}

#----------------------------------------------------------------------------#

## Section 5  ----
# In this section, your goal is to produce a chart that reveals a potential inequality. 
# This chart should show how two different continuous variables are related to one 
# another. Think carefully about what such a comparison means, and what you want to 
# communicate to your user. Your first step should be to find potential trends in the dataset.

# For the report, include the following in this section: 
# 1. Chart. Present the chart.
# 2. Chart caption. Include a caption that names and briefly describes the chart.
# 3. Summary paragraph. Include the following: 
# A specific question(s).
# A succinct answer to the question, referring to evidence in the chart.
# A brief paragraph (50 words or more) that summarizes the key patterns that appear 
# to be revealed in the chart.
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>

incarceration_trends <- incarceration_trends %>%
  rename(AAPI = aapi_jail_pop, Black = black_jail_pop, White = white_jail_pop, Latinx = latinx_jail_pop, Native = native_jail_pop)

get_ethnicity_jail_pop <- function(ethnicity) {
  ethnicity_jail_pop <- incarceration_trends %>%
    select(AAPI, Black, White, Latinx, Native) %>%
    filter(year == 2018) 
    gather(key = ethnicity, value = jail_population)
return(ethnicity)
}

plot_ethnicity_jail_plot <- function(ethnicity) {
    ethnicity_plot <-
      ggplot(data = ethnicity_jail_pop) +
      geom_col(
        mapping = aes(x = ethnicity, y = jail_population),
      ) +
      labs(
        title = "Prison Population by Ethnicity in U.S.",
        x = "Ethnicity",
        y = "Total U.S. Jail Population",
        caption = "This chart shows the total amount of people in U.S. prisons from 1970 to 2018 by ethnicity")
    return(ethnicity)   
} 

#----------------------------------------------------------------------------#

## Section 6  ---- 
#  Your first step should be to find potential trends in the dataset. 
# (1)  Use a map based coordinate system to set the aspect ratio of your map; and 
# (2) Use a minimalist theme for the map (see reading). 
#----------------------------------------------------------------------------#

get_map_jail_pop <- function(map) {
  map_jail_pop <- incarceration_trends %>%
    group_by(state) %>%
    filter(year == 2018) %>%
    select(state, total_jail_pop) %>%
    summarise(across(c(total_jail_pop), sum, na.rm = TRUE))
  return(map)
} 
  
blank_theme <- theme_bw() +    # for future use on map plot
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(), 
    axis.ticks = element_blank(),     
    axis.title = element_blank(),  
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()    
  )

plot_map_jail_pop <- function(map) {
  state_shape <- map_data("state") %>%
    rename(state = region)
  
  state_names_and_codes <- state_names_and_codes %>%
    rename(state = State)
  
  state_names_and_codes$state <- tolower(state_names_and_codes$state)
  
  map_jail_pop <- map_jail_pop %>%
    rename(Code = state)
  
  map_jail_pop <- left_join(map_jail_pop, state_names_and_codes, by = "Code")
  
  state_shape <- state_shape %>%
    left_join(map_jail_pop, by = "state")
  
  state_plot <-
    ggplot(state_shape) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop.y),
      color = "white",
      size = .1
    ) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Red") +
    labs(fill = "State Jail Population") +
    blank_theme
  return(map)
}
  
#----------------------------------------------------------------------------#
