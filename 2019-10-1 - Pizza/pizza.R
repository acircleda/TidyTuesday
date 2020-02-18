library(tidyverse)
library(zipcode)
library(stringr)
library(maps)
library(ggrepel)
library(gridExtra)
library(grid)

#load US map
US <- map_data("state")

#setwd
setwd("C:/Users/aschmidt/Dropbox (Personal)/Tidy Tuesday/Pizza")

#load main data
pizza_datafiniti <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_datafiniti.csv")

#get unique pizza locations based on address (not name)
data <- pizza_datafiniti %>% distinct(address, .keep_all = TRUE)

#Remove AL and HI
data <- subset(data, province != "AK")
data <- subset(data, province != "HI")

#main plot
main <- ggplot() +
  geom_polygon(data = US, aes(x=long, y=lat, group=group), color="black", fill="#383838") +
  geom_point(data=data, aes(x=longitude, y=latitude), color="#ca190e", size=4) +
  geom_point(data=data, aes(x=longitude, y=latitude), color="#ff8200", size=3) +
  geom_point(data=data, aes(x=longitude, y=latitude), color="#ffe495", size=1, alpha=.6)+
  coord_fixed(ratio = 1.5) +
  theme_void() +
  theme(
    plot.background = element_blank(),
    line = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.margin=unit(c(-1,0,0,0), "cm"))
  
#find top 5
top5 <- data %>% group_by(province) %>%
  summarize (n()) %>% top_n(5) %>% arrange(`n()`) %>% rename(sum = `n()`)


#create abbreviation column
map <- US %>% mutate(
  state_name = tools::toTitleCase(region))


map <- map %>% mutate(
    state = state.abb[match(state_name,state.name)]
  )


#select only top-10 states
top10 <- subset(data, province == "NY" |
                  province == "CA"|
                  province == "PA"|
                  province == "FL" |
                  province == "TX" |
                  province == "IL" |
                  province == "OH" |
                  province == "MI" |
                  province == "AZ" |
                  province == "NC")

top_states <- subset(map, state == "NY" |
                       state == "CA"|
                       state == "PA"|
                       state == "FL" |
                       state == "TX" |
                       state == "IL" |
                       state == "OH" |
                       state == "MI" |
                       state == "AZ" |
                       state == "NC")



map_plot <- function(st, map_title) {
  tmp <- subset(top10, province == st)
  ggplot() +
    geom_polygon(data = subset(top_states, state == st), aes(x=long, y=lat, group=group), color="black", fill="#383838") +
    geom_point(data=tmp, aes(x=longitude, y=latitude), color="#ca190e", size=4) +
    geom_point(data=tmp, aes(x=longitude, y=latitude), color="#ff8200", size=3) +
    geom_point(data=tmp, aes(x=longitude, y=latitude), color="#ffe495", size=1, alpha=.6)+
    coord_fixed(ratio = 1.5) +
    theme_void() +
    labs(
      title=map_title
    )+
    theme(
      plot.background = element_blank(),
      plot.title = element_text(color="white", size = 10, margin = margin(l = 10, unit = "pt")),
    )
}

NY <- map_plot("NY", "#1 - New York")
CA <- map_plot("CA", "#2 - California")
PA <- map_plot("PA", "#3 - Pennsylvania")
FL <- map_plot("FL", "#4 - Florida")
TX <- map_plot("TX", "#5 - Texas")

title <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       title = "Pizza Restaurant Density in the United States", 
       subtitle = "Based on a sample of 2,274 restaurants") +
  theme(line = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(color = "white", size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size=12, color = "grey80", hjust = 0.5))

midtitle <- ggplot(data.frame(x = 1:2, y = 1:2)) +
  labs(x = NULL, y = NULL,
       title = "Top Five Most Pizza-Dense States")+ 
  theme(line = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(color = "white", size = 14, face = "bold", hjust = 0.5),
        plot.margin=unit(c(0,0,0,0), "cm"))

caption <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       caption = "Graphic: @AnthonyTeacher | Source: Datafiniti")+
  theme(line = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(size=8, color = "#ff8200", hjust = 1))

toplayout <- rbind(c(1,2,3,4,5))

fivestates <- grid.arrange(NY, CA, PA, FL, TX, layout_matrix = toplayout)

fivestates2 <- cowplot::ggdraw(fivestates) + 
  theme(plot.background = element_rect(fill="black", color = NA),
        plot.margin=unit(c(-1,0,0,0), "cm"))


layout <- rbind(c(8,0,0,0,8),
                c(1,1,1,1,1),
                c(1,1,1,1,1),
                c(1,1,1,1,1),
                c(8,2,2,2,8),
                c(3,3,3,3,3),
                c(8,8,8,4,4))

g1 <- grid.arrange(title, main, midtitle, fivestates2, caption, layout_matrix = layout)

g2 <- cowplot::ggdraw(g1) + 
  theme(plot.background = element_rect(fill="black", color = NA))
g2
