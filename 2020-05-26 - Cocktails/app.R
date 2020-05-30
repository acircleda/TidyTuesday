library(tidyverse)
library(shiny)
library(DT)

isVowel <- function(char) char %in% c('a', 'e', 'i', 'o', 'u')

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv') %>%
  select(-date_modified, -id_drink, -iba, -video) %>%
  mutate(alcoholic = tolower(alcoholic),
         image = paste0('<img src="', drink_thumb,'" width=500px, align="center"></img>'),
         drink_article_out = tolower(substring(drink, 1,1)),
         drink_article = ifelse(isVowel(drink_article_out) == T, "an", "a"),
         category = case_when(
           str_detect(category, "Ordinary Drink") ~ "an ordinary drink",
           str_detect(category, "Other/Unknown") ~ "an ordinary drink",
           TRUE ~ paste0("a ", tolower(category))
         ),
         glass = case_when(
           str_detect(glass, "Old-fashioned glass") ~ "an old-fashioned glass",
           str_detect(glass, "Old-Fashioned glass") ~ "an old-fashioned glass",
           str_detect(glass, "Irish coffee cup ") ~ "an Irish coffee cup ",
           TRUE ~ paste0("a ", tolower(glass))),
         drink_first = word(drink, 1),
         drink = ifelse(nchar(drink_first) & drink_first == "A",
                       str_remove_all(drink, "^A"), drink))

# #get ingredients
# cocktails %>%
#   filter(drink == "747") %>%
#   select(ingredient, measure, image) %>%
#     datatable(escape = F)
# 
# #get glass type
# cocktails %>%
#   filter(drink == "747") %>%
#   distinct(glass)
#   pull(glass)
#   
#get alcoholic type
#   cocktails %>%
#     filter(drink == "747") %>%
# distinct(alcoholic) %>%
# pull(alcoholic) -> alco
#   
# #get category
# cocktails %>%
#     filter(drink == "747") %>%
#     distinct(category) %>%
#   pull(category)
# 
# #get image
# cocktails %>%
#   filter(drink == "747") %>%
#   distinct(drink_thumb) %>%
#   pull(drink_thumb)


## page ----

ui <- fluidPage(
  h1("The Shiny Bartender's Guide", align = "center"),
  h4("Become a Shiny Bartender and make delicious cocktails by using this guide written in R/Shiny! This app was inspired by the",
      a(href='https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-05-26/readme.md', "2020 Week 22 #TidyTuesday dataset.")),
  sidebarLayout(fluid = T,
                sidebarPanel(
                  selectInput("drink", label="Select a Drink",
                              cocktails$drink, 
                              selected = 1,
                              multiple= FALSE)
                ),
                mainPanel(
                  h3(textOutput("toptitle"), align = "center"),
                  p(textOutput("alcohol"), align = "left"),
                  tableOutput("recipe"),
                  htmlOutput("image")
                      )),
                div("App designed by",
                    a(href="http://www.anthonyschmidt.co", "Anthony Schmidt"), 
                    "| Code can be found ",
                    a(href="https://github.com/acircleda/TidyTuesday/tree/master/2020-05-26%20-%20Cocktails", "here"))
)

## server----
server <- function(input, output) {
  # Reactive elements
  display_dataset <- reactive({
    cocktails %>% filter(drink == input$drink)})
  
  output$toptitle <- renderText({
    paste("How to make ", display_dataset()$drink_article[1], " ", input$drink)
  })
  
  output$alcohol <- renderText({
    paste0("This beverage is ", display_dataset()$alcoholic[1], ". It should be served as ", display_dataset()$category[1], " in ", display_dataset()$glass[1],".")
  })
  
  output$recipe <- renderTable({
    display_dataset() %>% select(ingredient, measure)
  })
  
  output$image <- renderText ({
    display_dataset() %>%
      select(image) %>%
      distinct(image) %>%
      pull(image)
    
  })
}

## shiny ----

shinyApp(server = server, ui = ui)
