library(tidyverse)
library(ggplot2)
library(extrafont)
library(plotly)
setwd("C:/Users/aschmidt/Dropbox (Personal)/Tidy Tuesday/Weight")

ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

data2 <- ipf_lifts %>% na.omit() %>% subset(place == 1 | place == 2 | place == 3) %>% select(name, age, place, sex, bodyweight_kg:best3deadlift_kg) %>% mutate(
  squat_pct = best3squat_kg/bodyweight_kg,
  bench_pct = best3bench_kg/bodyweight_kg,
  lift_pct = best3deadlift_kg/bodyweight_kg,
  sex = factor(sex, levels = c("M", "F"), labels=c("Male", "Female"))
  )

data2 <- data2 %>% mutate(
    avg_pct = rowMeans(cbind(squat_pct,bench_pct,lift_pct)))

data3 <- data2 %>% gather(type, pct, squat_pct:lift_pct)

data3$type <- recode(data3$type, squat_pct = "Percentage of Bodyweight - Squats", bench_pct = "Percentage of Bodyweight - Benchpress", lift_pct = "Percentage of Bodyweight - Deadlifts")

ggplot(data3)+
  geom_point(aes(x=age, y=pct, size=pct, color=sex), alpha=.2)+
  facet_grid(~type)+
  scale_color_manual(values=c("#8d5c83", "#5c838d"))+
  labs(
    title="How much weight do champion lifters lift relative to their bodyweight and age?",
    subtitle = "Data includes only those who placed 1st, 2nd, or 3rd (1993-2019)",
    x = "Age",
    y = "Percent of\nBodyweight\nLifted",
    color = "Gender"
  )+
  scale_y_continuous(labels=scales::percent)+
  scale_size(guide = "none")+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=2)))+
  theme(
    panel.background = element_blank(),
    axis.title.y = element_text(angle = 0, vjust=.5, face="italic", color="grey50", family="Tw Cen MT"),
    axis.title.x = element_text(family="Tw Cen MT"),
    strip.text.x = element_text(face = "bold", vjust=-1, color="grey50", family="Tw Cen MT"),
    strip.background = element_blank(),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(size = 16, color="grey20", family="Tw Cen MT"),
    plot.subtitle = element_text(size = 12, color="grey50", family="Tw Cen MT"),
    axis.text = element_text(family="Tw Cen MT"),
    legend.text = element_text(family="Tw Cen MT"),
    legend.title = element_text(family="Tw Cen MT"),
    legend.position = c(.05, .95),
    legend.justification = c("left", "top"),
    legend.box.background = element_rect(color="grey80")
  )

ggsave("weightlifting.jpg", plot=last_plot(), dpi=300)
