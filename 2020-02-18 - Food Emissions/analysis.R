##########################
#TidyTuesday 2020 Week 8
library(tidyverse)
library(maps)
library(patchwork)

## load and prepare data ----
co2 <- read.csv("food emissions.csv") %>% 
  mutate(name = as.factor(tolower(country)), #make lowercase for left_join
         food_category = recode(food_category, #shorten names
                "Milk - inc. cheese" = "Dairy",
                "Wheat and Wheat Products" = "Wheat",
                "Nuts inc. Peanut Butter" = "Nuts/Peanuts")
         )

#### make barplot ----
co2 %>% group_by(food_category) %>% summarize(
  con = mean(consumption),
  emit = mean(co2_emmission)
) %>% arrange(desc(emit)) %>%
  ggplot()+
  geom_bar(aes(x=reorder(food_category, emit), y=emit, fill=emit), stat="identity")+
  coord_flip()+
  scale_fill_viridis_c(option = "viridis", direction = 1)+
  ylab("Mean kgCO2 per capita")+
  ggtitle(label = "Mean CO2 Emissions Per Food Category",
       subtitle = "Brighter colors indicate higher emissions")+
  theme(
    plot.background = element_rect(color="#39474D", fill="#39474D"),
    panel.background = element_rect(color="#39474D", fill="#39474D"),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(color="white"),
    axis.title.x = element_text(color="white"),
    legend.background = element_blank(),
    legend.position = "none",
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 1.2, color="#9CA3A6", face="bold"),
    plot.subtitle = element_text(hjust = .7, color="#9CA3A6", face="italic")
  ) -> barplot

#### tile map ----
#tutorial: https://www.maartenlambrechts.com/2017/10/22/tutorial-a-worldtilegrid-with-ggplot2.html

#download and save tile grid data for posterity
write.csv(read.csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv"), "worldtilegrid.csv")

#load and process tile grid data
worldtilegrid <- read.csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv") %>% mutate(
  name = tolower(name)
)

#### name checks ----
#compare names that don't match, first in the co2 dataset, and then rename them based on the names in the worldtilegrid dataset
co2_names <- anti_join(co2, worldtilegrid, by="name") %>% group_by(name) %>% count()

world_names <- data.frame(anti_join(worldtilegrid, co2, by="name")) %>% group_by(name) %>% count() %>% select(name)

#### recoding names ----                          
co2 <- co2 %>% mutate(name = 
                 recode(name,
  'bosnia and herz.' = 'bosnia & herzegovina',
  'czech rep.' = 'czech republic',
  'dem. rep. congo' = 'congo (democratic republic of the)',
  
  'hong kong' = 'china',
  'iran' = 'united states of america',
  'korea' = 'south korea',
  
  'russia' = 'russian federation',
  
  'trinidad and tobago' = 'trinidad & tobago',
  'united kingdom' = 'great britain and northern ireland',
  'united states' = 'united states of america',
  'vietnam' = 'viet nam'))

#### tile map data ----
#joining co2 to the worldtilegrid dataset
hexdata <- worldtilegrid %>% left_join(co2, by="name") %>%
  group_by(name) %>% 
  mutate(co2sum = sum(co2_emmission)) #sum per country


####tile map----
ggplot(hexdata, aes(xmin = x, ymin = y, xmax = x + 1, ymax = y + 1, fill=co2sum))+
  geom_rect(color = "#51636C")+
  geom_text(aes(x = x, y = y, label = alpha.2), color = "#ffffff", alpha = 0.5, nudge_x = 0.55, nudge_y = -.3, size = 3) +
  scale_fill_viridis_c(option = "viridis", na.value="#39474D", direction = 1)+
  annotate("text", x=2.9, y=23.7, label="Low", color="#9CA3A6")+
  annotate("text", x=28, y=23.7, label="High", color="#9CA3A6")+
  scale_y_reverse()+
  labs(title = "Map of Total Food-Based CO2 Emissions",
       subtitle = "Each country's emissions are based on combined per capita consumption of beef, dairy, lamb/goat, \npork, rice, fish, polutry, wheat, eggs, nuts, and soybeans. Brighter colors indicate higher emissions.",
       caption = "Visualization by @AnthonyTeacher | Data by nu3.de via @KKulma",
    fill="")+
  theme_minimal()+
  theme(plot.background = element_rect(fill="#39474D"),
        plot.title = element_text(color="#9CA3A6", face="bold", size=20),
        plot.subtitle = element_text(color="#9CA3A6", face="italic", size=10),
        plot.caption = element_text(color="#9CA3A6", face="italic", size=8),
        panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.position=c(.5,.05),
        legend.key.size = unit(.4, "cm"),
        legend.key.width = unit(3,"cm"),
        legend.direction = "horizontal")



###############chloropleth-----
data(country.map, package = "choroplethrMaps")
world <- ne_countries(scale = "medium", returnclass = "sf") %>% mutate(name = tolower(name))
#### name checks
co2_names <- anti_join(co2, world, by="name") %>% group_by(name) %>% count()
world_names <- data.frame(anti_join(world, co2, by="name") %>% group_by(name) %>% count() %>% select(name))

data <- world %>%
  left_join(co2, by="name") %>%
  group_by(name) %>% 
  mutate(co2sum = sum(co2_emmission))

data<-co2 %>% 
    group_by(name) %>% mutate(
    co2sum = sum(co2_emmission, na.rm = T)
  ) %>%
  left_join(world, by="name")

ggplot(data = data) +
  geom_sf(aes(geometry = geometry, fill=co2sum), color="black")+
  scale_fill_viridis_c(option = "magma", na.value="black", direction = -1)+
  theme_void()+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"))

tilemap

#### interactive -----
library(plotly)
ggplotly(map)
 