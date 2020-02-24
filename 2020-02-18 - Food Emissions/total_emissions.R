##########################
#TidyTuesday 2020 Week 8
library(tidyverse)
library(rnaturalearth)
library(sf)
library(patchwork)

## load and prepare data ----
co2 <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv') %>% 
  mutate(name = as.factor(tolower(country)), #make lowercase for left_join
         food_category = recode(food_category, #shorten names
                                "Milk - inc. cheese" = "Dairy",
                                "Wheat and Wheat Products" = "Wheat",
                                "Nuts inc. Peanut Butter" = "Nuts/Peanuts")
  )

###############chloropleth-----
world <- ne_countries(scale = "medium", returnclass = "sf") %>% mutate(name = tolower(name))

#### name checks
co2_names <- anti_join(co2, world, by="name") %>% group_by(name) %>% count()

world_names <- data.frame(anti_join(world, co2, by="name") %>% group_by(name) %>% count() %>% select(name))


food_consumption <- co2 %>% mutate(
  name = recode(name,
    "bosnia and herzegovina" = "bosnia and herz.",
    "czech republic" = "czech rep.",
    "french polynesia" = "fr. polynesia",
    "hong kong sar. china" = "hong kong",
    "south korea" = "korea",
    "tawiwan. roc" = "taiwan",
    "usa" = "united states"
                )
)


data <- world %>%
  left_join(food_consumption, by="name") %>%
  mutate(
    total_emissions = co2_emmission*pop_est
  )

data %>% filter(food_category == "Beef") %>%
  ggplot()+
  geom_sf(aes(geometry = geometry, fill=total_emissions), color="black")+
  scale_fill_viridis_c(option = "magma", na.value="black", direction = 1)+
  theme_void()+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        plot.title = element_text(color="white"),
        plot.subtitle = element_text(color="white")
        
        )+
  ggtitle("Total Emissions Per Country: Beef")+
  labs(subtitle="Per Capita Emissions x Population Estimate - Brighter colors represent higher emissions") ->beef

food_consumption_sum <- food_consumption %>% 
  filter(food_category != "Beef") %>%
  group_by(name) %>%
  summarize(sum = sum(co2_emmission))

data2 <- world %>%
  left_join(food_consumption_sum, by="name") %>%
  mutate(
    total_emissions = sum*pop_est
  )

data2 %>%
  ggplot()+
  geom_sf(aes(geometry = geometry, fill=total_emissions), color="black")+
  scale_fill_viridis_c(option = "magma", na.value="black", direction = 1)+
  theme_void()+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        plot.title = element_text(color="white"),
        plot.subtitle = element_text(color="white"),
        plot.caption = element_text(color="white")
        
  )+
  ggtitle("Total Emissions Per Country: All Other Foods")+
  labs(subtitle="Per Capita Emissions x Population Estimate - Brighter colors represent higher emissions",
       caption="Data: nu3.de | Visualization: @AnthonyTeacher") ->other

beef / other
                     