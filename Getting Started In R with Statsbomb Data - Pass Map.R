library(StatsBombR)
library(tidyverse)
library(SBpitch)

##load competitions and filter by WSL 2019/2020 - FreeCompetitions() will load availble competitions including WSL/Spain/Champions League
Comp<-FreeCompetitions()%>%
  filter(competition_id==37, season_name=="2019/2020")

##load available matches for WSL 2019/2020
Matches<-FreeMatches(Comp)

##load events for above matches
StatsBombData<-StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)

##clean data
StatsBombData = allclean(StatsBombData)

##filter events data to select match_id and all passes - create new "complete"/"incomplete" variable
data<-StatsBombData %>%
  filter(match_id==2275096, 
         type.name == "Pass" & is.na(pass.type.name), 
         team.name == "West Ham United LFC") %>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")))

##summary stats of complete/incomplete passes
passes<-data%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally()

##total passes
passes_1<-sum(passes$n)

##Plot!
create_Pitch()+
  geom_point(data = data, aes(x = location.x, y = location.y, colour = pass.outcome), alpha = 0.5)+
  geom_segment(data = data, aes(x = location.x, y = location.y, xend = pass.end_location.x, yend = pass.end_location.y, colour = pass.outcome), alpha = 0.5, arrow = arrow(length = unit(0.08,"inches"))) +
  scale_color_manual(values = c("#00b0f6", "#f8766d"), name = "Outcome")+
  scale_y_reverse()+
  labs(title = "Arsenal WFC",
       subtitle = "vs West Ham United LFC")+
  geom_text(aes(x = 5, y=82,label = paste0("Passes: ", passes_1)))+
  geom_text(aes(x = 5, y=84,label = paste0("Complete: ", passes$n[1]))) +
  
  ##use theme() to adjust aesthetics
  theme(plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 15, hjust = 0.5))




