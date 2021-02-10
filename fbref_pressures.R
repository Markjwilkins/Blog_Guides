library(worldfootballR)
library(ggtext)
library(ggsoccer)
library(RColorBrewer)
library(glue)
library(cowplot)
library(janitor)

##install worldfootballR to pull fbref data
devtools::install_github("JaseZiv/worldfootballR")

##get defensive stats for big 5 European Leagues
df <- get_season_team_stats(country = c("ENG", "ESP", "ITA", "GER", "FRA"), 
                               gender = "M", season_end_year = 2021, stat_type = "defense") %>% 
  janitor::clean_names()

head(df)

##nest by league and run mean pressures in def/mid/att 3rds - select and filter by single league
data<-df %>% 
  group_by(country, team_or_opponent) %>% 
  nest() %>% 
  mutate(def_av = map(.x = data, ~mean(.x$def_3rd_pressures)),
         mid_av = map(.x = data, ~mean(.x$mid_3rd_pressures)),
         att_av = map(.x = data, ~mean(.x$att_3rd_pressures))) %>% 
  unnest() %>% 
  select(competition_name,country, team_or_opponent, squad, def_3rd_pressures:att_3rd_pressures, def_av:att_av) %>% 
  filter(country=="ENG",
         team_or_opponent=="team")

head(data)

##coords for 3rd divide
lines<-tibble(x = c(33.33, 66.66),
              y = 0,
              xend = c(33.33, 66.66),
              yend = 100)

##select colours to use in plot
colour_1<-"#4575B4"
colour_2<-"#D73027"
colour_3<-"grey97"

##league name
league<-unique(data$competition_name)

##season name
season_name<-"2020/2021"


##plot
p<-ggplot()+
  
  geom_rect(data = data, aes(xmin = 0, xmax = 33.33, ymin = 0, ymax =100), fill = ifelse(data$def_3rd_pressures>data$def_av, colour_1, colour_2))+

  geom_rect(data = data, aes(xmin = 33.33, xmax = 66.66, ymin = 0, ymax =100), fill = ifelse(data$mid_3rd_pressures>data$mid_av, colour_1, colour_2))+

  geom_rect(data = data, aes(xmin = 66.66, xmax = 100, ymin = 0, ymax =100), fill = ifelse(data$att_3rd_pressures>data$att_av, colour_1, colour_2))+
  
  geom_segment(data = lines, aes(x = x, y = y, xend = xend, yend = yend), colour = colour_3, alpha = 0.6)+
  
  annotate_pitch_one(colour = colour_3, fill = "NA")+

  theme_pitch() +

  facet_wrap(~squad)+

  labs(title = glue("{league} - Pressing Habits {season_name}"),
       subtitle = glue("<b style='color:grey97'>Do teams press </b><b style='color:#4575B4'>above </b><b style='color:grey97'>or </b><b span style='color:#D73027'>below </b><b style='color:grey97'>the {league} average in the defensive, middle and attacking third?</b>"),
       caption = ("Data: fbref.com
                  By: @biscuitchaser")) +

  theme(plot.title = element_textbox_simple(size = 30, colour = colour_3, face = "bold", halign = 0.5),

        plot.subtitle = element_textbox_simple(size = 14, halign = 0.5),

        plot.caption = element_text(size=10, colour = colour_3),

        legend.position = "none",

        plot.background = element_rect("grey20"),

        strip.background = element_blank(),

        strip.text = element_text(colour = colour_3, size = 12))

##create 0-1 coord with ggdraw - overlay plot...add attacking direction label
ggdraw(p, xlim = c(0,1), ylim = c(0,1))+

  draw_line(x = c(0.4, 0.6),
            y = c(0.03, 0.03),
            color = colour_3, size = 1.2,
            arrow = arrow(length = unit(0.12, "inches"), type = "closed"))+

  draw_text("Attacking Direction", x=0.5, y=0.015, colour = colour_3, size = 10)
