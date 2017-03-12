#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
source("pa-vote-helper.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   h1("Pennsylvania Vulnerable State Reps", 
      style="color: rgba(255,255,255,1);
            background-color: #24292e;
    "),
   fluidRow(
     column(10,
            div("A project to measure the vulnerability of state elected reps in PA.  A rep is seen as vulnerable if a) a different party won the local election and the presdiential election in the district and b) the rep either won by a small margin or the opposite presidential candidate won by a large margin in district.")
     )
   ),
   fluidRow(
     column(4,
            radioButtons("vulnerable", label = h4("Measure of Vulnerability"),
                         choices = list("Low Margin of Victory, State" = "vulnerableLowStateMargin", "High Margin of Victory, Presidential" = "vulnerableHighPresMargin"), 
                         selected = "vulnerableLowStateMargin")
     )
   ),
   
   fluidRow(
     column(
       6,
       h3("House"), 
       br(),
       leafletOutput('houseMap')
     ),
     column(
       6,
       h3("Senate"), 
       br(),
       leafletOutput('senateMap')
     ) 
   
  ),
  
  fluidRow(
    column(
      6,
      dataTableOutput("houseTable")
    ),
    column (
      6,
      dataTableOutput("senateTable")
    )
  ),
  fluidRow(
    column(
      12,
      hr(),
      HTML("This project was created by and is maintained by <a href=mailto:hollander@gmail.com>Michael Hollander</a>.  You can find the code for this page on github here:<a href='https://github.com/mhollander/pa-vote-explorer' target=_blank>Github Link</a>.  All of the data for this website comes from <a href=https://ows.doleta.gov/unemploy/DataDownloads.asp target=_blank>the US Department of Labor</a>.")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$houseMap <- renderLeaflet({
     return(paHouseMap)
   })
   
   output$senateMap <- renderLeaflet({
     return(paSenateMap)
   })
   
   output$houseTable <- renderDataTable({
     houseTable <- DT::datatable(houseVotes[,c("StateHouseDistrict","DCandidate","HouseVoteDPercent","RCandidate","HouseVoteRPercent","PresVoteDPercent","PresVoteRPercent")],
                           options=list(
                             pageLength = 10,
                             lengthMenu = list(c(10, 20, 30, -1),c("10", "20", "30", 'All')),
                             order = list(0,'asc'),
                             searching=TRUE
                           ), 
                           colnames=c("St House District","Dem", "Dem %", "Rep", "Rep %", "Clinton %", "Trump %"),
                           class="stripe",
                           rownames=FALSE
      ) 
      return(houseTable)
   })

    output$senateTable <- renderDataTable({
     senateTable <- DT::datatable(senateVotes[,c("StateSenatorialDistrict","DCandidate","SenateVoteDPercent","RCandidate","SenateVoteRPercent","PresVoteDPercent","PresVoteRPercent")],
                                 options=list(
                                   pageLength = 10,
                                   lengthMenu = list(c(10, 20, 30, -1),c("10", "20", "30", 'All')),
                                   order = list(0,'asc'),
                                   searching=TRUE
                                 ), 
                                 colnames=c("St Senate District","Dem", "Dem %", "Rep", "Rep %", "Clinton %", "Trump %"),
                                 class="stripe",
                                 rownames=FALSE
     )
     return(senateTable)
   })
   
   observe({
     columnData = paHouse@data[[input$vulnerable]]
     # otherLayer = ifelse(input$vulnerable=="vulnerableLowStateMargin","vulnerableHighPresMargin","vulnerableLowStateMargin")
     leafletProxy("houseMap", data=paHouse) %>%
       # hideGroup(otherLayer) %>%
       # showGroup(input$vulnerable)
       clearShapes() %>%
       addPolygons(fillColor = ~pal(columnData),
                   fillOpacity = 0.8,
                   color = ifelse(paHouse$HouseVoteDlessR>0,"#0000A0","#8B0000"),
                   weight = 1,
                   popup = house_popup)

     
     columnData2 = paSenate@data[[input$vulnerable]]
     leafletProxy("senateMap", data=paSenate) %>%
                    # hideGroup(otherLayer) %>%
                    # showGroup(input$vulnerable)
       clearShapes() %>%
       addPolygons(fillColor = ~pal(columnData2),
                   fillOpacity = 0.8,
                   color = ifelse(paSenate$SenateVoteDlessR>0,"#0000A0","#8B0000"),
                   weight = 1,
                   popup = senate_popup)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

