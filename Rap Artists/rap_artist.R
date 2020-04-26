
library(tidyverse)
library(plotly)
library(gghighlight)
library(glue)
library(ggthemes)
library(extrafont)

loadfonts()

polls <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
glimpse(polls)

rankings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')
glimpse(rankings)

rap_artist_rank <- inner_join(polls, rankings)

rar <- rap_artist_rank %>% count(artist, points, title, year) %>% arrange(points)
rar %>% view()

song_title2 <- rar %>% mutate(artist = as.character(artist),
                              title = as.character(title))



r <- rar %>% 
  group_by(year, artist, title) %>% 
  ggplot(aes(year, points)) + 
  geom_point(aes(colour=as.factor(n))) +
  gghighlight(max(n) > 8) +
  geom_text(aes(label=glue("{year}--{artist} :: {title}")), 
            hjust=0.2, vjust=-1, fontface="italic") +
  labs(colour="Ranked top most songs of the year") + 
  theme_wsj() + 
  theme(legend.position = "none") + 
  labs(title = "Best Rap Artist Year Wise", caption = "tidytuesday: Rap_Artist") + 
  theme(plot.title = element_text(family="Bradley Hand ITC", face = "italic", color = "#F2147D"),
        plot.caption = element_text(family="Bradley Hand ITC", face = "italic", color = "#F2147D"))

r
