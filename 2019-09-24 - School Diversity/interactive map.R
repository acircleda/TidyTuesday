library(tidyverse)
library(highcharter)
library(gapminder)

##get school data
school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

schools <- school_diversity %>% 
  rename(state = ST) %>% 
  filter(SCHOOL_YEAR == "2016-2017") %>%
  select(state, White) %>%
  group_by(state) %>%
  summarize(average = mean(White)) %>%
  mutate(diverse = 100-average)

mapdata <- get_data_from_map(download_map_data("countries/us/us-all"))

mapdata <- mapdata %>% 
  rename(state = `postal-code`)

hcmap("countries/us/us-all", data = schools,
      name = "Average Diversity", value = "diverse", joinBy = c("postal-code", "state"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 100, by = 10)))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") %>%
  hc_tooltip(valueDecimals = 2,
             valueSuffix = "%") %>%
  hc_title(text = "Diversity in American School Districts (2017)",
           margin = -5,
           style = list(fontSize = "18pt", useHTML = TRUE)) %>%
  hc_subtitle(text = "The following chart represents the average school district diversity of each state.")
  
