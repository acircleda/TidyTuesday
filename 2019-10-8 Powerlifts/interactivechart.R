library(tidyverse)
library(ggplot2)
library(extrafont)
library(plotly) #note: the resulting plot exceeded maximum upload size for free accounts
Sys.setenv("plotly_username"="username")
Sys.setenv("plotly_api_key"="api")

ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

data2 <- ipf_lifts %>% na.omit() %>% subset(place == 1 | place == 2 | place == 3) %>% select(name, age, place, sex, bodyweight_kg:best3deadlift_kg) %>% mutate(
  squat_pct = best3squat_kg/bodyweight_kg,
  bench_pct = best3bench_kg/bodyweight_kg,
  lift_pct = best3deadlift_kg/bodyweight_kg,
  sex = factor(sex, levels = c("M", "F"), labels=c("Male", "Female"))
  )

data2 <- data2 %>% mutate(
    avg_pct = rowMeans(cbind(squat_pct,bench_pct,lift_pct)))

squat <- plot_ly(data2, x = ~age, 
              y = ~squat_pct*100, 
              color=~sex,
              legendgroup = ~sex,
              colors = c("#8d5c83", "#5c838d"),
              #marker = list(color = c("#8d5c83", "#5c838d"),
                            #opacity = .5),
              size=~squat_pct,
              hoverinfo = 'text',
              text = ~paste("Name: ", name,
                            '<br>Placed: ', place,
                            '<br>Sex: ', sex,
                            '<br>Bodyweight: ', bodyweight_kg,
                            "kg",
                            '<br>Squat Weight: ', best3squat_kg,
                            "kg",
                            '<br>Percent of Bodyweight: ', round(squat_pct*100, digits = 0),"%"))%>%
  layout(
    #title = 'Squat Lifts as a Percent of Bodyweight',
    xaxis = list(title='Age'),
    yaxis = list(title = 'Percent of Bodyweight Lifted')) %>%
  add_annotations(
    text = ~unique("Squats"),
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(size = 15)
  )


bench <- plot_ly(data2, x = ~age, 
        y = ~bench_pct*100, 
        color=~sex,
        legendgroup = ~sex,
        showlegend = F,
        colors = c("#8d5c83", "#5c838d"),
        #marker = list(color = c("#8d5c83", "#5c838d"),
        #opacity = .5),
        size=~bench_pct,
        hoverinfo = 'text',
        text = ~paste("Name: ", name,
                      '<br>Placed: ', place,
                      '<br>Sex: ', sex,
                      '<br>Bodyweight: ', bodyweight_kg,
                      "kg",
                      '<br>Bench Weight: ', best3squat_kg,
                      "kg",
                      '<br>Percent of Bodyweight: ', round(squat_pct*100, digits = 0),"%"))%>%
  layout(
    #title = 'Bench Lifts as a Percent of Bodyweight',
    xaxis = list(title='Age'),
    yaxis = list(title = 'Percent of Bodyweight Lifted')) %>%
  add_annotations(
    text = ~unique("Bench"),
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(size = 15)
  )

deadlift <- plot_ly(data2, x = ~age, 
                 y = ~lift_pct*100, 
                 color=~sex,
                 legendgroup = ~sex,
                 showlegend = F,
                 colors = c("#8d5c83", "#5c838d"),
                 #marker = list(color = c("#8d5c83", "#5c838d"),
                 #opacity = .5),
                 size=~lift_pct,
                 hoverinfo = 'text',
                 text = ~paste("Name: ", name,
                               '<br>Placed: ', place,
                               '<br>Sex: ', sex,
                               '<br>Bodyweight: ', bodyweight_kg,
                               "kg",
                               '<br>Deadlift Weight: ', best3squat_kg,
                               "kg",
                               '<br>Percent of Bodyweight: ', round(squat_pct*100, digits = 0),"%"))%>%
  layout(
    #title = 'Deadlifts as a Percent of Bodyweight',
    xaxis = list(title='Age'),
    yaxis = list(title = 'Percent of Bodyweight Lifted')) %>%
  add_annotations(
    text = ~unique("Deadlifts"),
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(size = 15)
  )


lifts_interactive <- subplot(squat, bench, deadlift, shareX = TRUE, shareY = TRUE)

api_create(lifts_interactive, filename = "lifts_interactive")
