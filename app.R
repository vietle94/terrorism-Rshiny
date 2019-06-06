library(tidyverse)
library(leaflet)
library(plotly)
library(streamgraph)
library(shinydashboard)
library(DT)

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

data$iday[data$iday == 0] <- 1
data$imonth[data$imonth == 0] <- 1

data$idate <- data %>% unite(date, iyear, imonth, iday, sep = "-") %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  pull(date)

data <- data %>% 
  mutate_at(vars(gname, gname2, gname3), function(x){gsub('[^ -~]', '', x)}) 

data$popmap <- data %>% 
  mutate(attacktype2_txt = ifelse(is.na(attacktype2_txt)," ", str_c(", ", attacktype2_txt)),
         attacktype3_txt = ifelse(is.na(attacktype3_txt)," ", str_c(", ", attacktype3_txt)),
         weaptype2_txt = ifelse(is.na(weaptype2_txt)," ", str_c(", ", weaptype2_txt)),
         weaptype3_txt = ifelse(is.na(weaptype3_txt)," ", str_c(", ", weaptype3_txt)),
         targtype2_txt = ifelse(is.na(targtype2_txt)," ", str_c(", ", targtype2_txt)),
         targtype3_txt = ifelse(is.na(targtype3_txt)," ", str_c(", ", targtype3_txt)),
         gname2 = ifelse(is.na(gname2)," ", str_c(", ", gname2)),
         gname3 = ifelse(is.na(gname3)," ", str_c(", ", gname3))) %>% 
  mutate(popmap = str_c("Country: ", country_txt, " <br/> ", 
                        "Date: ", idate, " <br/> ",
                        "Attack type: ", attacktype1_txt, attacktype2_txt, attacktype3_txt, " <br/> ",
                     "Weapon: ", weaptype1_txt, weaptype2_txt, weaptype3_txt, " <br/> ",
                     "Target: ", targtype1_txt, targtype2_txt, targtype3_txt, " <br/> ",
                     "Group responsible: ", gname, gname2, gname3, " <br/> ",
                     "Casualty: ", str_replace_na(nkill), " <br/> ",
                     "Injured: ", str_replace_na(nwound), " <br/> ",
                     "Property damage: ", str_replace_na(propextent_txt))) %>% pull(popmap)


sidebar <- dashboardSidebar(
  sidebarMenu(
    dateRangeInput("dateRange",
                   label = "Choose date range",
                   start = "2000-01-01", end = "2017-12-31",
                   min = "1970-01-01", max = "2017-12-31",
                   separator = " - ", format = "yyyy/mm/dd",
                   startview = 'year'
    ),
    menuItem("Map", tabName = "map", icon = icon("dashboard")),
    menuItem("More analysis", icon = icon("th"), tabName = "df",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("About", tabName = "about", icon = icon("address-book"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map",
            fluidRow(
              valueBoxOutput("nInc"),
              valueBoxOutput("nKill"),
              valueBoxOutput("nWound")
            ),
            fluidRow(
              column(
                width = 8,
                box(
                  width = NULL,
                  leafletOutput("map", height = 600),
                  status = "info"
                ),
                tabBox(
                  width = NULL,
                  title = "Streamgraph by country ",
                  selected = "Incidents",
                  tabPanel("Incidents",
                           streamgraphOutput("streamInc")
                           ),
                  tabPanel("Casualties",
                           streamgraphOutput("streamCas")
                           ),
                  tabPanel("Injuries",
                           streamgraphOutput("streamInj")
                           )
                  )
                ),
              column(
                width = 4,
                box(
                  width = NULL,
                  status = "info",
                  title =  "Number of incidents by region", solidHeader = T,
                  plotlyOutput("pieRegion")
                ),
                tabBox(
                  width = NULL,
                  title = "Worldwide ",
                  selected = "Incidents",
                  tabPanel("Incidents",
                           plotlyOutput("lineInc")
                           ),
                  tabPanel("Injuries",
                           plotlyOutput("lineInj")
                           ),
                  tabPanel("Casualties",
                           plotlyOutput("lineCas")
                           )
                  )
                )
              )
            ),
    tabItem(tabName = "df",
            fluidRow(
              column(
                width = 6,
                box(
                  width = NULL,
                  status = "info",
                  title = "Top country with highest number of incidents", solidHeader = T,
                  plotlyOutput("barCountry")
                ),
                tabBox(
                  width = NULL,
                  title = "Target type ",
                  tabPanel("Bar graph",
                           plotlyOutput("barTar")
                  ),
                  tabPanel("Stream graph",
                           streamgraphOutput("streamTar")
                  )
                )
              ),
              column(
                width = 6,
                tabBox(
                  width = NULL,
                  title = "Attack type ",
                  tabPanel("Bar graph",
                           plotlyOutput("barAtt")
                  ),
                  tabPanel("Stream graph",
                           streamgraphOutput("streamAtt")
                  )
                ),
                tabBox(
                  width = NULL,
                  title = "Group responsible ",
                  tabPanel("Bar graph",
                           plotlyOutput("barGroup")
                  ),
                  tabPanel("Stream graph",
                           streamgraphOutput("streamGroup")
                           )
                  )
                )
              ),
            fluidRow(
              box(
                width = NULL,
                status = "info",
                title = "Property damage", solidHeader = T,
                plotlyOutput("barPro")
                )
              )
            ),
    tabItem(tabName = "about",
            h1("About me"),
            p("My name is Viet Le and I do data analysis"),
            tags$a(href="https://vietle.netlify.com/", "Find out more about my projects"),
            br(),
            tags$a(href="https://github.com/vietle94", "Find me on Github"),
            br(),
            tags$a(href="https://www.linkedin.com/in/vietle94", "My Linkedin profile"),
            br(),
            tags$hr(),
            h1("About the dataset"),
            h2("Context"),
            p("Information on more than 180,000 Terrorist Attacks"),
            p("The Global Terrorism Database (GTD) is an open-source database including 
              information on terrorist attacks around the world from 1970 through 2017. 
              The GTD includes systematic data on domestic as well as international terrorist incidents 
              that have occurred during this time period and now includes more than 180,000 attacks. 
              The database is maintained by researchers at the National Consortium for the Study of 
              Terrorism and Responses to Terrorism (START), headquartered at the University of Maryland"),
            tags$a(href="http://start.umd.edu/gtd", "More information"),
            h3("Content"),
            p("Geography: Worldwide"),
            p("Time period: 1970-2017, except 1993"),
            p("Unit of analysis: Attack"),
            p("Variables: >100 variables on location, tactics, perpetrators, targets, and outcomes"),
            p("Sources: Unclassified media articles (Note: Please interpret changes over time with caution. Global patterns are driven by diverse trends in particular regions, and data collection is influenced by fluctuations in access to media coverage over both time and place.)"),
            p("Definition of terrorism:"),
            p('"The threatened or actual use of illegal force and violence by a non-state actor to attain a political, economic, religious, or social goal through fear, coercion, or intimidation."'),
            p("See the GTD Codebook for important details on data collection methodology, definitions, and coding schema."),
            tags$a(href="http://start.umd.edu/gtd/downloads/Codebook.pdf", "the GTD Codebook")
            )
  )
  )
    

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Global terrorism"),
  sidebar,
  body
)

server <- function(input, output){
  
  filteredData <- reactive({
    data[data$idate >= input$dateRange[1] & data$idate <= input$dateRange[2],]
  })

  output$map <- renderLeaflet(
    data %>% 
    leaflet() %>% 
      addTiles() %>%
      setMaxBounds(~min(data$longitude), ~min(data$latitude), ~max(data$longitude), ~max(data$latitude)) %>% 
      setView(0,0, 2))
  
  observe({
    leafletProxy("map", data = filteredData()) %>% 
      clearMarkers() %>%
      clearMarkerClusters() %>% 
      addMarkers(~longitude, ~latitude, label = ~filteredData()$popmap %>% map(htmltools::HTML),
                 clusterOptions = markerClusterOptions(disableClusteringAtZoom = 12))
      
  })
  
  output$barCountry <- renderPlotly({
    filteredData() %>% 
      mutate(country_txt = fct_lump(country_txt, 10)) %>%
      count(country_txt) %>% 
      plot_ly(y = ~ fct_reorder(country_txt, n), x = ~ n, type = "bar")
    })
  
  output$pieRegion <- renderPlotly({
    filteredData() %>% 
      count(region_txt) %>% 
      plot_ly(labels = ~ region_txt, values = ~ n, type = "pie") %>% 
      layout(showlegend = F)
  })
  
  output$streamInc <- renderStreamgraph({
    filteredData() %>% 
      mutate(country_txt = fct_lump(country_txt, 10)) %>% 
      count(country_txt, iyear) %>% 
      streamgraph(country_txt, n, iyear)
      
  })
  
  output$lineInc <- renderPlotly({
    filteredData() %>% 
      count(iyear) %>% 
      plot_ly(x = ~iyear, y = ~n, type = "scatter", mode = "lines")
  })
  
  output$barAtt <- renderPlotly({
    filteredData() %>% 
      select(attacktype1_txt, attacktype2_txt, attacktype3_txt) %>% 
      unlist() %>% 
      tibble(attacktype = .) %>% 
      count(attacktype) %>% 
      plot_ly(y = ~fct_reorder(attacktype, n), x = ~n, type = "bar") 
  })

  output$barTar <- renderPlotly({
    filteredData() %>% 
      select(targtype1_txt, targtype2_txt, targtype3_txt) %>% 
      unlist() %>% 
      tibble(targtype = .) %>% 
      count(targtype) %>% 
      plot_ly(y = ~fct_reorder(targtype, n), x = ~n, type = "bar")
  })
  
  output$barGroup <- renderPlotly({
    filteredData() %>% 
      select(gname, gname2, gname3) %>% 
      unlist() %>% 
      tibble(gname = .) %>% 
      count(gname) %>% 
      top_n(10,n) %>% 
      plot_ly(y = ~fct_reorder(gname, n), x = ~n, type = "bar")
  })
  
  output$barPro <- renderPlotly({
    filteredData() %>% 
      count(propextent_txt) %>% 
      plot_ly(y = ~fct_reorder(propextent_txt, n), x = ~n, type = "bar")
  })
  
  output$streamCas <- renderStreamgraph({
    filteredData() %>% 
      group_by(country_txt, iyear) %>% 
      summarise(kill = sum(nkill, na.rm = T)) %>% 
      streamgraph(country_txt, kill, iyear)
  })
  
  output$lineCas <- renderPlotly({
    filteredData() %>% 
      group_by(iyear) %>% 
      summarise(kill = sum(nkill, na.rm = T)) %>% 
      plot_ly(x = ~iyear, y = ~kill, type = "scatter", mode = "lines")
  })
  
  output$streamInj <- renderStreamgraph({
    filteredData() %>% 
      group_by(country_txt, iyear) %>% 
      summarise(wound = sum(nwound, na.rm = T)) %>% 
      streamgraph(country_txt, wound, iyear)
  })
  
  output$lineInj <- renderPlotly({
    filteredData() %>%
      group_by(iyear) %>% 
      summarise(wound = sum(nwound, na.rm = T)) %>% 
      plot_ly(x = ~iyear, y = ~wound, type = "scatter", mode = "lines")
  })
  
  output$nInc <- renderValueBox({
    valueBox(
      value = dim(filteredData())[1],
      subtitle = "Number of recorded Incidents",
      icon = icon("exclamation-triangle")
    )
  })
  
  output$nKill <- renderValueBox({
    valueBox(
      value = filteredData() %>% summarise(sum(nkill, na.rm = T)) %>% unlist(),
      subtitle = "Number of recorded Casualties",
      icon = icon("skull"),
      color = "red"
    )
  })
  
  output$nWound <- renderValueBox({
    valueBox(
      value = filteredData() %>% summarise(sum(nwound, na.rm = T)) %>% unlist(),
      subtitle = "Number of recorded Injuries",
      icon = icon("ambulance"),
      color = "yellow"
    )
  })
  
  output$streamTar <- renderStreamgraph({
    filteredData() %>% 
      count(iyear, targtype1_txt) %>% 
      left_join(
        filteredData() %>% 
          count(iyear, targtype2_txt), by = c("iyear", "targtype1_txt" = "targtype2_txt")) %>%
      left_join(
        filteredData() %>% 
          count(iyear, targtype3_txt), by = c("iyear", "targtype1_txt" = "targtype3_txt")) %>% 
      group_by(iyear, targtype1_txt) %>% 
      mutate(total = sum(n.x, n.y, n, na.rm = T)) %>% 
      streamgraph(targtype1_txt, total, iyear)
    
  })
  
  output$streamAtt <- renderStreamgraph({
    filteredData() %>% 
      count(iyear, attacktype1_txt) %>% 
      left_join(
        filteredData() %>% 
          count(iyear, attacktype2_txt), by = c("iyear", "attacktype1_txt" = "attacktype2_txt")) %>%
      left_join(
        filteredData() %>% 
          count(iyear, attacktype3_txt), by = c("iyear", "attacktype1_txt" = "attacktype3_txt")) %>% 
      group_by(iyear, attacktype1_txt) %>% 
      mutate(total = sum(n.x, n.y, n, na.rm = T)) %>% 
      streamgraph(attacktype1_txt, total, iyear)
    
  })
  
  output$streamGroup <- renderStreamgraph({
    filteredData() %>% 
      count(iyear, gname) %>% 
      left_join(
        filteredData() %>% 
          count(iyear, gname2), by = c("iyear", "gname" = "gname2")) %>%
      left_join(
        filteredData() %>% 
          count(iyear, gname3), by = c("iyear", "gname" = "gname3")) %>% 
      group_by(iyear, gname) %>% 
      mutate(total = sum(n.x, n.y, n, na.rm = T)) %>% 
      filter(gname != "Unknown") %>% 
      streamgraph(gname, total, iyear)
    
  })
  }


shinyApp(ui = ui, server = server)
