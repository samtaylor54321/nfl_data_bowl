########################################### NFL Data Bowl Script #############################################

# Set Up  -------------------------------------------------------------

# Load packages
library(RPostgreSQL)
library(tidyverse)
library(getPass)
library(lubridate)
library(glue)
library(yaml)

# Load in config file
config <- read_yaml("src/config.yaml")

# Set up connection to postgres 
drv <- dbDriver('PostgreSQL')
conn <- dbConnect(drv, 
                  host='localhost',
                  user='postgres',
                  password=getPass(),
                  dbname='nfl_data_bowl')

# Load data into memory
nfl_play_raw <- dbGetQuery(conn, "select * from nfl_data")

# Build Feature Vector ----------------------------------------------------

# BTransform first features
nfl_data <- nfl_play_raw %>% 
  transmute(
    gameid, 
    playid,
    team,
    playerbirthdate = mdy(playerbirthdate),
    age_at_game = round(as.numeric(as.Date(glue('{season}{str_sub(playerbirthdate,5)}'))-playerbirthdate)/365),
    time_remaining = case_when(
      quarter == '1' ~ 2700 + as.numeric(str_sub(gameclock,1,2))*60 + as.numeric(str_sub(gameclock, 4,5)),
      quarter =='2' ~ 1800 + as.numeric(str_sub(gameclock,1,2))*60 + as.numeric(str_sub(gameclock, 4,5)),
      quarter =='3' ~ 900 + as.numeric(str_sub(gameclock,1,2))*60 + as.numeric(str_sub(gameclock, 4,5)),
      quarter =='4' ~ as.numeric(str_sub(gameclock,1,2))*60 + as.numeric(str_sub(gameclock, 4,5))),
    yards_per_play_for_first_down = case_when(
      down == '1' ~ as.numeric(distance) / 4,
      down =='2' ~ as.numeric(distance) / 3,
      down =='3' ~ as.numeric(distance) / 2,
      down =='4' ~ as.numeric(distance)/1),
    x_position = as.numeric(x),
    y_position = as.numeric(y),
    s_position = as.numeric(s),
    a_position = as.numeric(a),
    dis_position = as.numeric(dis),
    orientation_position = as.numeric(orientation),
    dir_position = as.numeric(dir),
    defendersinthebox = as.numeric(defendersinthebox),
    playerweight = as.numeric(playerweight),
    playerheight = (as.numeric(str_sub(playerheight,1,1))*30.5) + (as.numeric(substr(playerheight, 3, length(playerheight))) * 2.5),
    yards) %>% 
  group_by(gameid, playid) %>% 
  summarise(avg_age = mean(age_at_game),
    time_remaining = min(time_remaining),
    yards_per_play_for_first_down = min(yards_per_play_for_first_down),
    x_position = mean(x_position),
    y_position = mean(y_position),
    s_position = mean(s_position),
    a_position = mean(a_position),
    dis_position = mean(dis_position),
    orientation_position = mean(orientation_position),
    dir_position = mean(dir_position),
    defendersinthebox = min(defendersinthebox),
    playerweight = mean(playerweight),
    playerheight = mean(playerheight),
    yards = min(yards))
            
ind <- sample(c(1:dim(nfl_data)[1]), 5000)

test <- nfl_data[ind, ]
training <- nfl_data[-ind, ]











  
  









