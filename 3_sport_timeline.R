
require(tidyverse)
require(ggrepel)

dates <- read.csv("schedule.csv", header=T) %>% 
  mutate(Sport = fct_relevel(Sport, "MLB", "NBA", "NFL", "Draft"),
         Date = as.Date(Date, "%m/%d/%Y"))

Target_Draft_Start <- dates %>% 
  filter(Desc == "Target Draft Start")

schedule <- dates %>% 
  filter(Desc != "Target Draft Start")


theme_schedule <- function(){
  theme(text = element_text("sans-serif"),
        
        panel.grid.minor = element_blank(),
        panel.background =  element_blank(),
        panel.border = element_blank(), 
        
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        
        legend.position = "none"
  )
}

ggplot(data = schedule, aes(x=Date, y=Sport, color=Sport)) +
  geom_vline(xintercept = today(), linetype=4, colour="dark grey") +
  geom_vline(xintercept = as.Date("2019-07-01"), colour="black") +
  geom_point(size=2) +
  geom_point(size=4, alpha=.5) +
  geom_line(size=1) +
  geom_label_repel(aes(label=Desc, fill = Sport),
                   size = 4, 
                   force = 3,
                   nudge_y = .2,
                   fontface ="bold",
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.75, "lines"),
                   color= "white", 
                   segment.colour = "black") +
  geom_label_repel(aes(label=as.character(Date)),
                   size = 4,
                   force = 3,
                   nudge_y = -.2,
                   fontface ="bold",
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.75, "lines"),
                   #color= "white", 
                   segment.colour = "black") + 
  geom_point(size=6, alpha=.25) +
  ### 2nd set of points to not have the line continue to the last point in draft section
  geom_point(data= Target_Draft_Start,  aes(x=Date, y=Sport, color=Sport), size=2) +
  geom_point(data= Target_Draft_Start,  aes(x=Date, y=Sport, color=Sport), size=4, alpha=.5) +
  geom_point(data= Target_Draft_Start,  aes(x=Date, y=Sport, color=Sport), size=6, alpha=.25) +
  geom_label_repel(data =Target_Draft_Start, aes(label=Desc, fill = Sport),
                   size = 4, 
                   force = 3,
                   nudge_y = .2,
                   fontface ="bold",
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.75, "lines"),
                   color= "white", 
                   segment.colour = "black") +
  geom_label_repel(data =Target_Draft_Start, aes(label=as.character(Date)),
                   size = 4,
                   force = 3,
                   nudge_y = -.2,
                   fontface ="bold",
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.75, "lines"),
                   #color= "white", 
                   segment.colour = "black") + 
  
  annotate("text", x = today()+10, y = .5, label = "Today", color="dark grey", fontface="bold") +
  annotate("text", x = as.Date("2019-06-01"), y = 4, label = "Draft", color="#C77CFF",size = 8, fontface="bold") +
  annotate("text", x = as.Date("2019-06-01"), y = 3, label = "NFL", color="#00BFC4",size = 8, fontface="bold") +
  annotate("text", x = as.Date("2019-06-01"), y = 2, label = "NBA", color="#7CAE00",size = 8, fontface="bold") +
  annotate("text", x = as.Date("2019-06-01"), y = 1, label = "MLB", color="#F8766D",size = 8, fontface="bold") +
  scale_x_date(date_minor_breaks = "1 month") +
  theme_schedule()

ggsave("3_sport_timeline.png", width = 14.5, height = 7)
