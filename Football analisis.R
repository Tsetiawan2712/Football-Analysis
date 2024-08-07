install.packages('devtools')
devtools::install_github("statsbomb/SDMTools")
devtools::install_github("statsbomb/StatsBombR")
install.packages('tidyverse')
install.packages('ggsoccer')

# Load the libraries
library(tidyverse)
library(StatsBombR)
library(ggsoccer)

# Retrieve all available competitions
Comp <- FreeCompetitions()

# Filter kompetisi yang kita inginkan, Pilih Copa Amerika 2024
# Filter the competition
Copa_america <- Comp %>%
  filter(competition_id==223 & season_name=="2024")

# Retrieve all available matches
matches <- FreeMatches(Copa_america)
typeof(matches) #melihat tipe data

# Retrieve the event data
events_df <- get.matchFree(matches)
typeof(events_df)
## write_xlsx(events_df_as, "events_dfas")

# Preprocess the data
clean_df <- allclean(events_df)

# Passing Map
Messi_pass <- clean_df %>%
  filter(player.name == 'Lionel Andrés Messi Cuccittini') %>%
  filter(type.name == 'Pass')

ggplot(Messi_pass) +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_segment(aes(x=location.x, y=location.y, xend=pass.end_location.x, yend=pass.end_location.y),
               colour = "coral",
               arrow = arrow(length = unit(0.15, "cm"),
                             type = "closed")) +
  labs(title="Messi Passing Map",
       subtitle="Final Copa America 2024",
       caption="Data Source: StatsBomb")

ggplot(Messi_pass) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_segment(aes(x=location.x, y=location.y, xend=pass.end_location.x, yend=pass.end_location.y),
               colour = "coral",
               arrow = arrow(length = unit(0.15, "cm"),
                             type = "closed")) + 
  labs(title="Messi Passing Map",
       subtitle="Final Copa America 2024",
       caption="Data Source: StatsBomb") + 
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

Argentina_shot <- clean_df %>%
  filter(type.name == 'Shot') %>%
  filter(team.name == 'Argentina') %>%
  select(player.name, location.x, location.y, shot.end_location.x, shot.end_location.y, shot.statsbomb_xg)
Colombia_shot <- clean_df %>%
  filter(type.name == 'Shot') %>%
  filter(team.name == 'Colombia') %>%
  select(player.name, location.x, location.y, shot.end_location.x, shot.end_location.y, shot.statsbomb_xg)

ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour='white', fill='#021e3f') +
  geom_point(data=Argentina_shot, aes(x=location.x, y=location.y, size=shot.statsbomb_xg), color="red") +
  geom_point(data=Colombia_shot, aes(x=120-location.x, y=location.y, size=shot.statsbomb_xg), color="yellow") +
  labs(
    title="Argentina vs Colombia",
    subtitle = "Shots Map | Final Copa America 2024",
    caption="Data Source: StatsBomb"
  ) + 
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family="Geneva", color='white'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none"
  )

# Pressure Heat Map
Argentina_pressure <- clean_df %>%
  filter(team.name == 'Argentina') %>%
  filter(type.name == 'Pressure')

ggplot(Argentina_pressure) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="Argentina Pressure Heat Map",
       subtitle="Final Copa America 2024",
       caption="Data Source: StatsBomb") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )
