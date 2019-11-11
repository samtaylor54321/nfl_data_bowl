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
    college_conference = case_when(
      playercollegename =='Air Force'  ~  'Mountain West Conference',
      playercollegename =='Akron'  ~  'Mid-American Conference',
      playercollegename =='Alabama'  ~  'Southeastern Conference',
      playercollegename =='Alabama-Birmingham'  ~  'Conference USA',
      playercollegename =='Appalachian State'  ~  'Sun Belt Conference',
      playercollegename =='Arizona'  ~  'Pacific 12 Conference',
      playercollegename =='Arizona State'  ~  'Pacific 12 Conference',
      playercollegename =='Arkansas'  ~  'Southeastern Conference',
      playercollegename =='Arkansas State'  ~  'Sun Belt Conference',
      playercollegename =='Army'  ~  'Independents',
      playercollegename =='Auburn'  ~  'Southeastern Conference',
      playercollegename =='Ball State'  ~  'Mid-American Conference',
      playercollegename =='Baylor'  ~  'Big 12 Conference',
      playercollegename =='Boise State'  ~  'Mountain West Conference',
      playercollegename =='Boston College'  ~  'Atlantic Coast Conference',
      playercollegename =='Bowling Green'  ~  'Mid-American Conference',
      playercollegename =='Brigham Young'  ~  'Independents',
      playercollegename =='Buffalo'  ~  'Mid-American Conference',
      playercollegename =='California'  ~  'Pacific 12 Conference',
      playercollegename =='Central Florida'  ~  'American Athletic Conference',
      playercollegename =='Central Michigan'  ~  'Mid-American Conference',
      playercollegename =='Cincinnati'  ~  'American Athletic Conference',
      playercollegename =='Clemson'  ~  'Atlantic Coast Conference',
      playercollegename =='Coastal Carolina'  ~  'Sun Belt Conference',
      playercollegename =='Colorado'  ~  'Pacific 12 Conference',
      playercollegename =='Colorado State'  ~  'Mountain West Conference',
      playercollegename =='Connecticut'  ~  'American Athletic Conference',
      playercollegename =='Duke'  ~  'Atlantic Coast Conference',
      playercollegename =='East Carolina'  ~  'American Athletic Conference',
      playercollegename =='Eastern Michigan'  ~  'Mid-American Conference',
      playercollegename =='Florida'  ~  'Southeastern Conference',
      playercollegename =='Florida Atlantic'  ~  'Conference USA',
      playercollegename =='Florida International'  ~  'Conference USA',
      playercollegename =='Florida State'  ~  'Atlantic Coast Conference',
      playercollegename =='Fresno State'  ~  'Mountain West Conference',
      playercollegename =='Georgia'  ~  'Southeastern Conference',
      playercollegename =='Georgia Southern'  ~  'Sun Belt Conference',
      playercollegename =='Georgia State'  ~  'Sun Belt Conference',
      playercollegename =='Georgia Tech'  ~  'Atlantic Coast Conference',
      playercollegename =='Hawai`i'  ~  'Mountain West Conference',
      playercollegename =='Houston'  ~  'American Athletic Conference',
      playercollegename =='Illinois'  ~  'Big 10 Conference',
      playercollegename =='Indiana'  ~  'Big 10 Conference',
      playercollegename =='Iowa'  ~  'Big 10 Conference',
      playercollegename =='Iowa State'  ~  'Big 12 Conference',
      playercollegename =='Kansas'  ~  'Big 12 Conference',
      playercollegename =='Kansas State'  ~  'Big 12 Conference',
      playercollegename =='Kent State'  ~  'Mid-American Conference',
      playercollegename =='Kentucky'  ~  'Southeastern Conference',
      playercollegename =='Liberty'  ~  'Independents',
      playercollegename =='Louisiana Tech'  ~  'Conference USA',
      playercollegename =='Louisiana-Lafayette'  ~  'Sun Belt Conference',
      playercollegename =='Louisiana-Monroe'  ~  'Sun Belt Conference',
      playercollegename =='Louisville'  ~  'Atlantic Coast Conference',
      playercollegename =='Louisiana State'  ~  'Southeastern Conference',
      playercollegename =='Marshall'  ~  'Conference USA',
      playercollegename =='Maryland'  ~  'Big 10 Conference',
      playercollegename =='Massachusetts'  ~  'Independents',
      playercollegename =='Memphis'  ~  'American Athletic Conference',
      playercollegename =='Miami'  ~  'Atlantic Coast Conference',
      playercollegename =='Miami OH'  ~  'Mid-American Conference',
      playercollegename =='Michigan'  ~  'Big 10 Conference',
      playercollegename =='Michigan State'  ~  'Big 10 Conference',
      playercollegename =='Middle Tennessee State'  ~  'Conference USA',
      playercollegename =='Minnesota'  ~  'Big 10 Conference',
      playercollegename =='Mississippi'  ~  'Southeastern Conference',
      playercollegename =='Mississippi State'  ~  'Southeastern Conference',
      playercollegename =='Missouri'  ~  'Southeastern Conference',
      playercollegename =='Navy'  ~  'American Athletic Conference',
      playercollegename =='Nebraska'  ~  'Big 10 Conference',
      playercollegename =='Nevada'  ~  'Mountain West Conference',
      playercollegename =='New Mexico'  ~  'Mountain West Conference',
      playercollegename =='New Mexico State'  ~  'Independents',
      playercollegename =='North Carolina'  ~  'Atlantic Coast Conference',
      playercollegename =='North Carolina State'  ~  'Atlantic Coast Conference',
      playercollegename =='North Texas'  ~  'Conference USA',
      playercollegename =='Northern Illinois'  ~  'Mid-American Conference',
      playercollegename =='Northwestern'  ~  'Big 10 Conference',
      playercollegename =='Notre Dame'  ~  'Independents',
      playercollegename =='Ohio State'  ~  'Big 10 Conference',
      playercollegename =='Ohio'  ~  'Mid-American Conference',
      playercollegename =='Oklahoma'  ~  'Big 12 Conference',
      playercollegename =='Oklahoma State'  ~  'Big 12 Conference',
      playercollegename =='Old Dominion'  ~  'Conference USA',
      playercollegename =='Oregon'  ~  'Pacific 12 Conference',
      playercollegename =='Oregon State'  ~  'Pacific 12 Conference',
      playercollegename =='Penn State'  ~  'Big 10 Conference',
      playercollegename =='Pittsburgh'  ~  'Atlantic Coast Conference',
      playercollegename =='Purdue'  ~  'Big 10 Conference',
      playercollegename =='Rice'  ~  'Conference USA',
      playercollegename =='Rutgers'  ~  'Big 10 Conference',
      playercollegename =='San Diego State'  ~  'Mountain West Conference',
      playercollegename =='San Jose State'  ~  'Mountain West Conference',
      playercollegename =='SMU'  ~  'American Athletic Conference',
      playercollegename =='South Alabama'  ~  'Sun Belt Conference',
      playercollegename =='South Carolina'  ~  'Southeastern Conference',
      playercollegename =='South Florida'  ~  'American Athletic Conference',
      playercollegename =='USC'  ~  'Pacific 12 Conference',
      playercollegename =='Southern Miss'  ~  'Conference USA',
      playercollegename =='Stanford'  ~  'Pacific 12 Conference',
      playercollegename =='Syracuse'  ~  'Atlantic Coast Conference',
      playercollegename =='Texas Christian'  ~  'Big 12 Conference',
      playercollegename =='Temple'  ~  'American Athletic Conference',
      playercollegename =='Tennessee'  ~  'Southeastern Conference',
      playercollegename =='Texas'  ~  'Big 12 Conference',
      playercollegename =='Texas A&M'  ~  'Southeastern Conference',
      playercollegename =='Texas State'  ~  'Sun Belt Conference',
      playercollegename =='Texas Tech'  ~  'Big 12 Conference',
      playercollegename =='Texas-San Antonio'  ~  'Conference USA',
      playercollegename =='Toledo'  ~  'Mid-American Conference',
      playercollegename =='Troy'  ~  'Sun Belt Conference',
      playercollegename =='Tulane'  ~  'American Athletic Conference',
      playercollegename =='Tulsa'  ~  'American Athletic Conference',
      playercollegename =='UCLA'  ~  'Pacific 12 Conference',
      playercollegename =='UNC-Charlotte'  ~  'Conference USA',
      playercollegename =='UNLV'  ~  'Mountain West Conference',
      playercollegename =='Utah'  ~  'Pacific 12 Conference',
      playercollegename =='Utah State'  ~  'Mountain West Conference',
      playercollegename =='UTEP'  ~  'Conference USA',
      playercollegename =='Vanderbilt'  ~  'Southeastern Conference',
      playercollegename =='Virginia'  ~  'Atlantic Coast Conference',
      playercollegename =='Virginia Tech'  ~  'Atlantic Coast Conference',
      playercollegename =='Wake Forest'  ~  'Atlantic Coast Conference',
      playercollegename =='Washington'  ~  'Pacific 12 Conference',
      playercollegename =='Washington State'  ~  'Pacific 12 Conference',
      playercollegename =='West Virginia'  ~  'Big 12 Conference',
      playercollegename =='Western Kentucky'  ~  'Conference USA',
      playercollegename =='Western Michigan'  ~  'Mid-American Conference',
      playercollegename =='Wisconsin'  ~  'Big 10 Conference',
      playercollegename =='Wyoming'  ~  'Mountain West Conference'),
    orientation_position = as.numeric(orientation),
    dir_position = as.numeric(dir),
    defendersinthebox = as.numeric(defendersinthebox),
    playerweight = as.numeric(playerweight),
    position = case_when(
      position ==    'CB'  ~  'DB',
      position ==    'FS'  ~  'DB',
      position ==    'SAF'  ~  'DB',
      position ==    'S'  ~  'DB',
      position ==    'SS'  ~  'DB',
      position ==    'DB'  ~  'DB',
      position ==    'OLB'  ~  'LB',
      position ==    'ILB'  ~  'LB',
      position ==    'MLB'  ~  'LB',
      position ==    'LB'  ~  'LB',
      position ==    'DE'  ~  'DL',
      position ==    'DT'  ~  'DL',
      position ==    'NT'  ~  'DL',
      position ==    'DL'  ~  'DL',
      position ==    'G'  ~  'OL',
      position ==    'OG'  ~  'OL',
      position ==    'T'  ~  'OL',
      position ==    'OT'  ~  'OL',
      position ==    'C'  ~  'OL',
      position ==    'RB'  ~  'RB',
      position ==    'FB'  ~  'RB',
      position ==    'HB'  ~  'RB',
      position ==    'WR'  ~  'SK',
      position ==    'QB'  ~  'SK',
      position ==    'TE'  ~  'SK'),
    possession_team = case_when(
      position %in% c('SK','RB','OL') ~ 'Offence',
      position %in% c('DB','LB','DL') ~ 'Defence'),
    home_field = case_when(
      possession_team == 'Offence' & team =='home' ~ 1),
    formation = case_when(
      offenseformation =='ACE' ~ 1,
      offenseformation =='EMPTY' ~ 23,
      offenseformation =='I_FORM' ~ 4821,
      offenseformation =='JUMBO' ~ 521,
      offenseformation =='PISTOL' ~ 610,
      offenseformation =='SHOTGUN' ~ 6862,
      offenseformation =='SINGLEBACK' ~ 10247,
      offenseformation =='WILDCAT' ~ 81,
      is.na(offenseformation) ~ 5),
    playerheight = (as.numeric(str_sub(playerheight,1,1))*30.5) + (as.numeric(substr(playerheight, 3, length(playerheight))) * 2.5),
    yards)

# %>% 
#   group_by(gameid, playid) %>% 
#   summarise(avg_age = mean(age_at_game),
#     time_remaining = min(time_remaining),
#     yards_per_play_for_first_down = min(yards_per_play_for_first_down),
#     x_position = mean(x_position),
#     y_position = mean(y_position),
#     s_position = mean(s_position),
#     a_position = mean(a_position),
#     dis_position = mean(dis_position),
#     orientation_position = mean(orientation_position),
#     dir_position = mean(dir_position),
#     defendersinthebox = min(defendersinthebox),
#     playerweight = mean(playerweight),
#     playerheight = mean(playerheight),
#     yards = min(yards))
# 









  
  









