library(StatsBombR)
library(tidyverse)
library(ggsoccer)

##initial blog: https://biscuitchaserfc.blogspot.com/2020/05/shot-maps-in-r-using-statsbomb-data.html

##load competitions
Comp<-FreeCompetitions() %>%
  filter(competition_id==37, season_name=="2019/2020")

##load matches for above competitions
Matches<-FreeMatches(Comp)

##load events 
StatsBombData<-StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)

#clean data
StatsBombData = allclean(StatsBombData)

##tally all player shots
player_shots<-StatsBombData%>%
  filter(type.name == "Shot") %>% 
  group_by(player.name) %>% 
  tally(name = "total_shots") 

##tally all player goals
player_goals<-StatsBombData %>%
  filter(shot.outcome.name == "Goal") %>% 
  group_by(player.name) %>% 
  tally(name = "Goals") 

##tally player total xG find top 9
player_xg<-StatsBombData%>%
  filter(type.name == "Shot")%>% 
  group_by(player.name)%>% 
  tally(shot.statsbomb_xg, name = "total_xg", sort = TRUE)%>% 
  top_n(9)

##puul top 9 players for future filter
top_9<-player_xg %>% 
  pull(player.name)

##join data
summary <- left_join(player_xg, player_shots, by = "player.name") %>% 
  mutate(xg_per_shot = sprintf("%0.2f",total_xg/total_shots)) 

summary<-left_join(summary, player_goals, by = "player.name")

##top 9 players via total xG and filter all shots they have taken
StatsBombData<-StatsBombData%>%
  filter(player.name%in%top_9,
         type.name == "Shot")

##set plot colours
c_1<-"grey20"
c_2<-"grey97"

##Plot!
ggplot()+
  annotate_pitch(dimensions = pitch_statsbomb, colour = c_2, fill = c_1)+
  theme_pitch()+
  coord_flip(xlim = c(55, 120),
             ylim = c(-12, 105))+
  geom_point(data = StatsBombData, aes(x = location.x, y = location.y, size = shot.statsbomb_xg, colour = shot.outcome.name == "Goal"))+
  scale_colour_manual(values = c("#FF8C94", "#A8E6CE"), labels = c("No-Goal", "Goal"), name = "Shot Outcome")+
  facet_wrap(~player.name)+
  
  geom_label(data=summary, size = 3, colour = c_1, aes(x = 70, y=17, label = paste0("xG Total: ", round(total_xg, digits = 2))))+
  geom_label(data=summary, size = 3, colour = c_1, aes(x = 63, y=17, label = paste0("xG Per Shot: ", xg_per_shot)))+
  geom_label(data=summary, size = 3, colour = c_1, aes(x = 70, y=63, label = paste0("Goals: ", Goals)))+
  geom_label(data=summary, size = 3, colour = c_1, aes(x = 63, y=63, label = paste0("Total Shots: ", total_shots)))+
  
  labs(title = "WSL Player Shot Map",
       subtitle = "Top 9 Total xG players 2019/2020",
       size = "xG",
       caption = ("Data: StatsBomb
                  By: @biscuitchaser"))+
  
  theme(plot.title = element_text(size = 20, colour = "white", face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, colour = "white", hjust = 0.5),
        legend.position = "none",
        plot.caption = element_text(size=10, colour = "white"),
        plot.background = element_rect(c_1),
        strip.background.x = element_rect(fill = "NA"),
        strip.text = element_text(colour = 'white', size = 11, hjust = 0.43))
