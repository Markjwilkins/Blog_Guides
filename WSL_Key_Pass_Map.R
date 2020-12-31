library(StatsBombR)
library(tidyverse)
library(ggsoccer)
library(ggrepel)
library(janitor)

##initial blog here: https://biscuitchaserfc.blogspot.com/2020/04/using-statsbomb-data-part-3.html
##.csv files can be found https://github.com/Markjwilkins/blog_guides

##load key pass data from fbref
df <- read_csv("~/Downloads/key_pass.csv") 

##load shot data from fbref
df1<- read_csv("~/Downloads/shots.csv")

##select 'Player', 'X90s', and 'KP' from key passes data frame
df<-df%>%
  select("Player", "90s", "KP") %>% 
  rename(p90 = "90s")

##select 'Player' and 'Sh/90' from shot data frame
df1<-df1%>%
  select("Player", "Sh/90")

##combine key pass and shot data and filter players that have played >5 90s and clean column names
all_data<-left_join(df, df1)%>%
  filter(p90>=5) %>% 
  clean_names() 

##plot key passes and shots
ggplot(all_data, aes(x=kp, y=sh_90, label = player))+
  geom_point() +
  geom_text_repel(data = all_data %>% 
                    filter(kp>2 | sh_90>0.4),
                  aes(x=kp, y=sh_90)) +
  xlab("Key Passes P90")+
  ylab("Shots Per 90") +
  labs(title = "Key Passes and Shots P90",
       subtitle = "WSL 2019/2020")

##uniform names across fbref and StatsBomb - change "Beth Mead" to "Bethany Mead"
all_data$player<-gsub("Beth Mead", "Bethany Mead", all_data$player)

##find top 9 key passers
top_9=all_data%>%
  select(player, kp)%>%
  arrange(-kp)%>%
  top_n(9) %>% 
  pull(player)

##load competitions - this season WSL
Comp<-FreeCompetitions()%>%filter(competition_id==37, season_name=="2019/2020")

##load all matches
Matches<-FreeMatches(Comp)

##load all events
StatsBombData<-StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)

##clean data
StatsBombData = allclean(StatsBombData)

##filter data by top 9 players and pass.shot_assist
d1<-StatsBombData%>%
  filter(player.name %in% top_9,
         pass.shot_assist == TRUE | pass.goal_assist == TRUE)

##plot all shot assists from top 9 players
p<-ggplot()+
  annotate_pitch(dimensions = pitch_statsbomb)+
  geom_segment(data = d1, aes(x = location.x, y = 80-location.y, xend = pass.end_location.x, yend = 80-pass.end_location.y), alpha = 0.5, arrow = arrow(length = unit(0.08,"inches")))+
  theme_pitch()+
  facet_wrap(~player.name)
  
p+labs(title = "Shot Assists",
         subtitle = "WSL 2019/2020 - All Play",
         caption = "Data: StatsBomb / fbref.com
                  By: @biscuitchaser")+
  theme(text = element_text(size = 12),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 12, face = "bold"),
        plot.caption = element_text(size = 10),
        panel.background = element_blank(),
        strip.background.x = element_rect(fill = "NA"))

##remove set pieces - open play passes only
p<-ggplot()+
  annotate_pitch(dimensions = pitch_statsbomb)+
  geom_segment(data = d1 %>% 
                 filter(play_pattern.name=="Regular Play"), aes(x = location.x, y = 80-location.y, xend = pass.end_location.x, yend = 80-pass.end_location.y), alpha = 0.5, arrow = arrow(length = unit(0.08,"inches")))+
  theme_pitch()+
  facet_wrap(~player.name)

p+labs(title = "Shot Assists",
       subtitle = "WSL 2019/2020 - Open Play",
       caption = "Data: StatsBomb / fbref.com
                  By: @biscuitchaser")+
  theme(text = element_text(size = 12),
        title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10),
        panel.background = element_blank(),
        strip.background.x = element_rect(fill = "NA"))



