library(tidyverse)
library(leaflet)
library(plotly)
library(streamgraph)
library(shinydashboard)

data <- read_csv("data/newdat.csv", col_types = cols(
  iyear = col_integer(),
  latitude = col_double(),
  longitude = col_double(),
  attacktype3_txt = col_character(),
  gname3 = col_character(),
  nkill = col_integer(),
  nwound = col_integer()
))
sidebar <- dashboardSidebar(
  sidebarMenu(
    dateRangeInput("dateRange",
                   label = h4("Choose period for data"),
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
                  tags$style(
                    ".irs-bar {",
                    "  border-color: transparent;",
                    "  background-color: transparent;",
                    "}",
                    ".irs-bar-edge {",
                    "  border-color: transparent;",
                    "  background-color: transparent;",
                    "}"
                  ),
                  leafletOutput("map", height = 600),
                  sliderInput("Slider", label = NA, min = 1970, 
                              max = 2017, value = 2000, sep = ""),
                  title =  "Terrorist attack location by year", solidHeader = T,
                  status = "info"
                ),
                tabBox(
                  width = NULL,
                  title = "By country ",
                  p(em(icon("info-circle"), HTML("Streamgraph is a variation of stack area chart that
                                                 has values displaced around a varying central baseline. 
                                                 The result has organic shapes that somewhat resemble a 
                                                 river-like stream. <br/>"),
                       icon("info-circle"), "Hover to see the value along timeline.")),
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
                  selected = "Stream graph",
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
                  selected = "Stream graph",
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
  dashboardHeader(title = "Global terrorism", 
                  tags$li(class="dropdown",
                          tags$a(href="http://vietle.info", 
                                  "Home", target="_blank")),
                  tags$li(class="dropdown",
                          tags$a(href="https://github.com/vietle94", 
                                 icon("github"), "", target="_blank")),
                  tags$li(class="dropdown",
                          tags$a(href="https://www.linkedin.com/in/vietle94/", 
                                 icon("linkedin-in"), "", target="_blank"))
                  ),
  sidebar,
  body
)

server <- function(input, output){
  
  filteredData <- reactive({
    data[data$idate >= input$dateRange[1] & data$idate <= input$dateRange[2],]
  })
  
  yearData <- reactive({
    data %>% filter(iyear == input$Slider)
  })

  output$map <- renderLeaflet(
    data %>% 
      filter(iyear == 2000) %>% 
      leaflet() %>% 
      addTiles())
      
  
  observe(
    leafletProxy("map", data = yearData()) %>% 
      clearMarkers() %>% 
      clearMarkerClusters() %>% 
      addMarkers(~longitude, ~latitude, label = ~popmap %>% map(htmltools::HTML),
                 clusterOptions = markerClusterOptions(disableClusteringAtZoom = 12)) %>% 
      setMaxBounds(~longitude %>% min(), ~latitude %>% min(), ~longitude %>% max(), ~latitude %>% max()) %>% 
      setView(0,0, 2)
  )
    
  
  output$barCountry <- renderPlotly({
    filteredData() %>% 
      mutate(country_txt = fct_lump(country_txt, 10)) %>%
      count(country_txt) %>% 
      plot_ly(y = ~ fct_reorder(country_txt, n), x = ~ n, type = "bar") %>% 
      layout(
        xaxis = list(title = "Number of Incidents"),
        yaxis = list(title = "Country")
      )
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
      streamgraph(country_txt, n, iyear) %>% 
      sg_axis_x(5,"year") %>% 
      sg_legend(show = T, label = "Country: ")
      
  })
  
  output$lineInc <- renderPlotly({
    filteredData() %>% 
      count(iyear) %>% 
      plot_ly(x = ~iyear, y = ~n, type = "scatter", mode = "lines") %>% 
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Incidents")
      )
  })
  
  output$barAtt <- renderPlotly({
    filteredData() %>% 
      select(attacktype1_txt, attacktype2_txt, attacktype3_txt) %>% 
      unlist() %>% 
      tibble(attacktype = .) %>% 
      count(attacktype) %>% 
      plot_ly(y = ~fct_reorder(attacktype, n), x = ~n, type = "bar") %>% 
      layout(
        xaxis = list(title = "Number of incidents"),
        yaxis = list(title = "Attack type")
      ) 
  })

  output$barTar <- renderPlotly({
    filteredData() %>% 
      select(targtype1_txt, targtype2_txt, targtype3_txt) %>% 
      unlist() %>% 
      tibble(targtype = .) %>% 
      count(targtype) %>% 
      plot_ly(y = ~fct_reorder(targtype, n), x = ~n, type = "bar") %>% 
      layout(
        xaxis = list(title = "Number of incidents"),
        yaxis = list(title = "Target type")
      ) 
  })
  
  output$barGroup <- renderPlotly({
    filteredData() %>% 
      select(gname, gname2, gname3) %>% 
      unlist() %>% 
      tibble(gname = .) %>% 
      count(gname) %>% 
      top_n(10,n) %>% 
      plot_ly(y = ~fct_reorder(gname, n), x = ~n, type = "bar") %>% 
      layout(
        xaxis = list(title = "Number of incidents"),
        yaxis = list(title = "Group responsible")
      ) 
  })
  
  output$barPro <- renderPlotly({
    filteredData() %>% 
      count(propextent_txt) %>% 
      plot_ly(y = ~fct_reorder(propextent_txt, n), x = ~n, type = "bar") %>% 
      layout(
        xaxis = list(title = "Number of incidents"),
        yaxis = list(title = "Property damage")
      ) 
  })
  
  output$streamCas <- renderStreamgraph({
    filteredData() %>% 
      group_by(country_txt, iyear) %>% 
      summarise(kill = sum(nkill, na.rm = T)) %>% 
      streamgraph(country_txt, kill, iyear) %>% 
      sg_axis_x(5,"year") %>% 
      sg_legend(show = T, label = "Country: ")
  })
  
  output$lineCas <- renderPlotly({
    filteredData() %>% 
      group_by(iyear) %>% 
      summarise(kill = sum(nkill, na.rm = T)) %>% 
      plot_ly(x = ~iyear, y = ~kill, type = "scatter", mode = "lines") %>% 
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Casualties")
      )
  })
  
  output$streamInj <- renderStreamgraph({
    filteredData() %>% 
      group_by(country_txt, iyear) %>% 
      summarise(wound = sum(nwound, na.rm = T)) %>% 
      streamgraph(country_txt, wound, iyear) %>% 
      sg_axis_x(5,"year") %>% 
      sg_legend(show = T, label = "Country: ") 
      
  })
  
  output$lineInj <- renderPlotly({
    filteredData() %>%
      group_by(iyear) %>% 
      summarise(wound = sum(nwound, na.rm = T)) %>% 
      plot_ly(x = ~iyear, y = ~wound, type = "scatter", mode = "lines") %>% 
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Injuries"))
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
      streamgraph(targtype1_txt, total, iyear) %>% 
      sg_axis_x(5,"year") %>% 
      sg_legend(show = T, label = "Target type: ")
    
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
      streamgraph(attacktype1_txt, total, iyear) %>% 
      sg_axis_x(5,"year") %>% 
      sg_legend(show = T, label = "Attack type: ")
    
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
      streamgraph(gname, total, iyear) %>% 
      sg_axis_x(5,"year") %>% 
      sg_legend(show = T, label = "Responsible group: ")
    
  })
  }


shinyApp(ui = ui, server = server)
