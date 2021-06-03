library(devtools)
devtools::install_github("statsbomb/StatsBombR")
library(tidyverse)
library(StatsBombR)
FreeCompetitions()
Comp <- FreeCompetitions() %>% 
  filter(competition_name =="Champions League")
print(comp)
Comp()
summary(Comp)
Comp
Matches <- FreeMatches(Comp)
StatsBombData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)
StatsBombData = allclean(StatsBombData)
summary(StatsBombData)
shots_goals = StatsBombData %>% 
  group_by(team.name) %>% 
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
goals = sum(shot.outcome.name=="Goal", na.rm = TRUE))
shots_goals

shots_goals= StatsBombData %>% 
  group_by(team.name) %>% 
  summarise(shots=sum(type.name=="Shot", na.rm = TRUE)/n_distinct(match_id),
goals = sum(shot.outcome.name=="Goal", na.rm=TRUE)/n_distinct(match_id))
shots_goals

library(ggplot2)
ggplot(data=shots_goals, aes(x=reorder(team.name,shots), y= shots))+
  geom_bar(stat="identity", width = 0.5)+
  labs(y="Shots")+
  theme(axis.title.y = element_blank())+
  scale_y_continuous(expand=c(0,0))+
  coord_flip()+
  theme_SB()

player_shots=StatsBombData %>% 
  group_by(player.name, player.id) %>% 
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE))
player_minutes=get.minutesplayed(StatsBombData)
player_minutes=player_minutes %>% 
  group_by(player.id) %>% 
  summarise(minutes=sum(MinutesPlayed))
player_shots=left_join(player_shots, player_minutes)
player_shots=player_shots %>% mutate(nineties = minutes/90)
player_shots=player_shots %>% mutate(shots_per90 = shots/nineties)
player_shots
filter(player_shots, minutes < 90, na.rm = TRUE)
arrange(player_shots, shots_per90)
?arrange
player_shots_filtered <- filter(player_shots, minutes > 90, shots_per90 > 1, na.rm = TRUE)
arrange(desc(player_shots_filtered)
player_shots_filtered %>% 
arrange(desc(shots_per90))
