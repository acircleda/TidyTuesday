library(ggplot2)
library(tidyverse)
library(sf)
library(janitor)

##set WD
setwd("../Tidy Tuesday/School Diversity")


##get school data
school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

##Read in shapefile downloaded from https://www.census.gov/cgi-bin/geo/shapefiles/index.php
tnsf <- read_sf("tl_2019_47_unsd.shp")

#subset for TN data ONLY
tn_schools <- subset(school_diversity, ST == "TN") %>% 
  rename(GEOID = LEAID) #rename to facilitate join
  
#create diverse into factor
tn_schools$diverse.factor <- factor(tn_schools$diverse)

tn_schools2 <- tn_schools %>%
  group_by(GEOID) %>%
  mutate(diverse_pct = 100-White) %>%
  mutate(org_diverse = lag(diverse.factor, default = first(diverse.factor))) %>%
  mutate(org_diverse_pct = 100 - lag(White, default = first(White))) %>%
  mutate(pct_change = 
           (diverse_pct - org_diverse_pct)/org_diverse_pct) %>%
  select(GEOID, LEA_NAME, ST, d_Locale_Txt, SCHOOL_YEAR, diverse.factor:pct_change) %>%
  filter(SCHOOL_YEAR == "2016-2017")

tn_schools2 <- tn_schools2 %>% mutate(change_type = case_when(
  org_diverse == "Diverse" & diverse.factor == "Diverse" ~ "Still Diverse",
  org_diverse == "Undiverse" & diverse.factor == "Undiverse" ~ "Still Undiverse",
  org_diverse == "Extremely undiverse" & diverse.factor == "Extremely undiverse" ~ "Still Undiverse",
  org_diverse == "Extremely undiverse" & diverse.factor == "Undiverse" ~ "Increased but Still Undiverse",
  org_diverse == "Extremely undiverse" & diverse.factor == "Diverse" ~ "Increased to Diverse",
  org_diverse == "Undiverse" & diverse.factor == "Diverse" ~ "Increased to Diverse",
  TRUE ~ "other"
)
)

#order factors
tn_schools2$change_type <- factor(tn_schools2$change_type, levels = c("Still Diverse", "Increased to Diverse", "Increased but Still Undiverse", "Still Undiverse"))

#get diversity percentage overall
div_pct <- tn_schools2 %>% mutate(diverse.factor = case_when(
  diverse.factor == "Diverse" ~ "Diverse",
  diverse.factor == "Undiverse" ~ "Undiverse",
  diverse.factor == "Extremely undiverse" ~ "Undiverse"
)) %>%
  group_by(diverse.factor) %>%
  summarize(sum = n())

#join with shapefile
data <- right_join(tnsf, tn_schools2, by = "GEOID")

#plot
ggplot() + 
  geom_sf(data = data, size = .2, color = "#616161", aes(fill=data$change_type)) + 
  coord_sf() +
  scale_fill_manual(values = c("#0570b0", "#74a9cf", "#bdc9e1", "#f1eef6")) +
  labs(
    fill="Change Categories",
    title = "Changes in Student Diversity from 1994/95 to 2016/17",
    subtitle = "For Tennessee School Districts",
    caption = "#TidyTuesday Visualization by @AnthonyTeacher | Data from NCES"
  ) +
  geom_curve(aes(y = 36.0153676, x = -88.2653012, yend = 37, xend = -88), curvature = .3, size=1)+
  geom_text(aes(x=-88, y=37, label="The Hollow Rock-Bruceton Special School District \nsaw the largest decrease in diversity (-11%)"), size = 3, nudge_y = .3)+
  geom_curve(aes(y = 35.3723358, x = -85.4100820, yend = 34.69, xend = -85), curvature = .3, size=1)+
  geom_text(aes(x=-85, y=34.69, label="Sequatchie County saw a 138-fold increase \nto 8% but still remains undiverse."), size = 3, nudge_y = -.3)+
  geom_curve(aes(y = 35.1639343, x = -89.9240519, yend = 34.69, xend = -91), curvature = .3, size=1)+
  geom_text(aes(x=-91, y=34.69, label="Shelby County is the second \nmost diverse district (92%) \nnext to the Achievement \nSchool District (not labeled)"), size = 3, nudge_y = -.5)+
  geom_curve(aes(y = 36.1101287, x = -82.2, yend = 35.5, xend = -82.3), curvature = .3, size=1)+
  geom_text(aes(x=-82.3, y=35.8, label="71% of Tennessee School \nDistricts are considered undiverse"), size = 3, nudge_y = -.5)+
  expand_limits(x=c(-92.5,-80), y=c(33.8, 37.5))+
  theme(
    panel.background = element_rect(fill="white"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(color = "#023858", size=16, face = "bold"),
    plot.subtitle = element_text(color = "#023858"),
    legend.title = element_text(face = "bold")
  )
