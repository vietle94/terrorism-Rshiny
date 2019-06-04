library(tidyverse)
library(leaflet)


data <- read_csv("data/data.csv", col_types = cols(
  ransomamtus = col_number(),
  ransomnote = col_character(),
  attacktype3 = col_integer(),
  attacktype3_txt = col_character(),
  claimmode2 = col_integer(),
  claimmode2_txt = col_character(),
  claimmode3 = col_integer(),
  claimmode3_txt = col_character(),
  weaptype4 = col_integer(),
  weaptype4_txt = col_character(),
  weapsubtype4 = col_integer(),
  weapsubtype4_txt = col_character(),
  gname3 = col_character(),
  gsubname3 = col_character(),
  gsubname2 = col_character(),
  ransompaidus = col_number(),
  compclaim = col_integer()
))

pop <- c("country_txt", "iyear", "imonth", "iday", "attacktype1_txt", "weapontype1_txt", "targtype1_txt",
         "gname", "nkill", "nwound", "propextent_txt")

data <- data %>% mutate(pop = str_c("Country:", " <br/> ", country_txt, " <br/> ", "iyear:", iyear))

data %>%
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(~longitude, ~latitude,
             label = ~data$pop %>% map(htmltools::HTML),
             clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = 3,
                                                   disableClusteringAtZoom = 3))

leaflet(options = leafletOptions(zoomControl = TRUE,
                                 minZoom = 1, maxZoom = 3)) %>%
  addTiles()

ui <- fluidPage(fluidRow(column(5,sliderInput(inputId = "num", label = "Choose a number",
                            value = 25, min = 1, max = 100)), 
                column(1, textInput(inputId = "title", 
                                    label = "Write a title", 
                                    value = "Histogram of Random Normal Values"))),
                fluidRow(column(3, actionButton(inputId = "go", label = "Update"), offset = 8)),
                plotOutput(outputId = "hist"),
                verbatimTextOutput("stats")
)

server <- function(input, output){
  data <-eventReactive(input$go, {
    rnorm(input$num)
  })
  output$hist <-  renderPlot({
    title <- str_c(input$num, " value histogram")
    hist(data(), main = isolate(input$title))
  })
  output$stats <- renderPrint(
    summary(data())
  )
  
}

shinyApp(ui = ui, server = server)


