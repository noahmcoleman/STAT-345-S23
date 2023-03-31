
#Load Libraries
library(tidyverse)
library(car)
library(ggplot2)
library(carData)
library(dplyr)
library(prismatic)
library(extrafont)
library(cowplot)
library(scales)
library(gganimate)
library(gifski)
library(png)

#Allow more data for shot charts
Sys.setenv(VROOM_CONNECTION_SIZE=365722389)



#packages used to get data
library("devtools")

library(nbastatR)




#Load data
bucks98 <- teams_shots(teams = "Milwaukee Bucks",seasons = 1998)
bucks99 <- teams_shots(teams = "Milwaukee Bucks",seasons = 1999)
bucks00 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2000)
bucks01 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2001)
bucks02 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2002)
bucks03 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2003)
bucks04 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2004)
bucks05 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2005)
bucks06 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2006)
bucks07 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2007)
bucks08 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2008)
bucks09 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2009)
bucks10 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2010)
bucks11 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2011)
bucks12 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2012)
bucks13 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2013)
bucks14 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2014)
bucks15 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2015)
bucks16 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2016)
bucks17 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2017)
bucks18 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2018)
bucks19 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2019)
bucks20 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2020)
bucks21 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2021)
bucks22 <- teams_shots(teams = "Milwaukee Bucks",seasons = 2022)


#rbind data in order to avoid multiple calls when cleaning data
bucks_df <- rbind(bucks98,bucks99,bucks00,bucks01,bucks02,bucks03,bucks04,bucks05,bucks06,bucks07,bucks08,bucks09,bucks10,bucks11,bucks12,bucks13,bucks14,bucks15,bucks16,bucks17,bucks18,bucks19,bucks20,bucks21,bucks22)
bucks_df <- bucks_df %>% group_by(yearSeason)

#Rescaling Location to fit court
bucks_df$locationX <- rescale(bucks_df$locationX, to=c(-25,25))

bucks_df$locationY <- rescale(bucks_df$locationY, to=c(0,94))



#Creating more data for analyzing
bucks_3pct <- bucks_df %>% summarise(rate= sum(typeShot=="3PT Field Goal" & isShotMade=="TRUE")/sum(typeShot=="3PT Field Goal")) %>% group_by(yearSeason)
bucks_3pct
#3pt attempts
bucks_3pa <- bucks_df %>% summarise(Attempts=sum(typeShot=="3PT Field Goal"))
#Creating graphs for 3-Attempts
bucks_3graph <- ggplot(bucks_3pa,aes(x=yearSeason,y=Attempts)) + geom_col(fill = "green4") + 
#Labels
  labs(title = "3-Pointers Attempted per Year", x = "Year", y="3-Point Attempts")
bucks_3graph

#Attempt to create Effective Field Goal Percentage

#Summing necessary stats: 3 Pointers Made, Field Goals Made, Field Goals Attempted
bucks_3pm <- bucks_df %>% summarise(made3= sum(typeShot=="3PT Field Goal" & isShotMade=="TRUE")) %>% group_by(yearSeason)
bucks_3pa <- bucks_df %>% summarise(Attempt3= sum(typeShot=="3PT Field Goal")) %>% group_by(yearSeason)
bucks_fgm <-bucks_df %>% summarise(made= sum(isShotMade=="TRUE")) %>% group_by(yearSeason)
bucks_fga <-bucks_df %>% summarise(attempt= sum(isShotAttempted=="TRUE")) %>% group_by(yearSeason)
#Joining stats into single data frame
bucksshots<- bucks_fga %>% left_join(bucks_fgm, by = "yearSeason") %>% left_join(bucks_3pm, by="yearSeason") %>% left_join(bucks_3pa, by = "yearSeason")
#Create EFG
bucksshots <- bucksshots %>% mutate(EFG=((made +(made3*.5))/attempt)*100)
round(bucksshots$EFG,digits=2)
bucks_efggraph <- ggplot(bucksshots,aes(x=yearSeason,y=EFG)) + geom_col(fill = "blue2") + 
#Labels
  labs(title = "Effective Field Goal Percentage(%) by Year", x = "Year", y="EFG(%)") + theme_minimal()
bucks_efggraph

#Creating 3 Point Percentage Chart
bucksshots <- bucksshots %>% mutate(pct3= made3/Attempt3)
#Creating barchart
bucks_pct3graph <- ggplot(bucksshots,aes(x=yearSeason,y=pct3)) + geom_col(fill = "green4") + 
  #Labels
  labs(title = "Three-Point Percentage(%) by Year", x = "Year", y="3-Point Percentage(%)") + theme_minimal()
bucks_pct3graph

#basketball court creation; code taken and modified fromhttps://www.owenlhjphillips.com/new-blog/2020/6/25/how-to-make-nba-shots-charts-in-r

circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data_frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

court_themes = list(
  light = list(
    court = 'floralwhite',
    lines = '#999999',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 1,
    hex_border_color = "#000000"
),
dark = list(
  court = '#000004',
  lines = '#999999',
  text = '#f0f0f0',
  made = '#00bfc4',
  missed = '#f8766d',
  hex_border_size = 0,
  hex_border_color = "#000000"
)
)


plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
  if (use_short_three) {
    three_point_radius = 22
    three_point_side_height = 0
  }
  
  court_points = data_frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points = bind_rows(court_points , data_frame(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = data_frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  
  court_points <- court_points
  
  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite'),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}
#Check to see if court plots correctly
plot_court(court_themes$light)



#Create object for court
bucks_court <- plot_court(court_themes$light)
#create shot char animation
bucks_plot <- bucks_court + 
  #all shots
  geom_point(data = bucks_df, aes(x=locationX,y=locationY,color= isShotMade),alpha = .25)  + 
  #Only show half of court
  ylim(0,70) + 
  #creates shift in animate based on year  
  transition_time(yearSeason) + 
  #wrappers to make shifts smooth  
  enter_fade() +exit_shrink() + labs(title = "Milwaukee Bucks Shot Chart: {as.integer(frame_time)}") + scale_color_discrete(labels= c("Miss","Made"), type= c("blue2","green4")) + labs(color="")

bucks_anim <- animate(bucks_plot,nframes=25,fps=.5)





  
  