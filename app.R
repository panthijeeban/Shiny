library(shiny)
library(readxl)
library(ggplot2)
library(tidyverse)
library(dbplyr)
library(DT)
library(plotly)

#load in data

data <- read_excel("composite.xlsx",col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
colnames(data)<-c("Date", "Ocean water level (m als)", "Groundwater level (m asl)","Precipitation (mm)","Maximum temperature (°C)","Minimum temperature (°C)")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(h4("Groundwater and related variables in Southern Rhode Island")),
  #img(src="logo.JPG", align="left", width=100, height=100, position='relative'),
  sidebarLayout(
    sidebarPanel(
      verticalLayout(
        radioButtons(inputId = "Variables",
                     label = "Select a variable:",
                     choices = as.list(colnames(data)[2:length(colnames(data))]),
                     selected = colnames(data)[2]
        ),
        #choiceValues = list("precipitation", "tmax", "tmin", "groundwater",
        #  "ocean")),
        dateRangeInput("daterange", "Select date range:",
                       start = "2009-01-01",
                       end   = "2018-12-31"),
        textOutput("startdate"), textOutput("enddate"),textOutput("range"),
        tags$b("Developer: Jeeban Panthi"),
        tags$img(src="logo.JPG", height=150, width=150)
      )
      
    ),
    mainPanel(
      tabsetPanel(type='tabs',
                  tabPanel("Graph view", plotlyOutput("plot")
                  ),
                  tabPanel("Data view", DTOutput("table")
                  ),
                  tabPanel("Data sources","Here are the links for the data that have been used in this application",br(),br(),
                           tags$a(href="https://waterdata.usgs.gov/ri/nwis/uv/?site_no=412154071462901&PARAmeter_cd=72019",target="_blank", "Groundwater level"),br(),
                           tags$a(href="https://tidesandcurrents.noaa.gov/stationhome.html?id=8452660",target="_blank","Tide data"),br(),
                           tags$a(href="https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00054796/detail",target="_blank", "Climate data")
                           
                  ),
                  tabPanel("About this app", HTML("This page shows the groundwater 
                           and related variables in the southern Rhode Island. 
                           All the data were collected from published sources, 
                           please check the Data Sources tab on the tab panel 
                           for the details of the source.<br/><br/>In the Graph View, 
                          the red line on the graph is linear trend of the data. It is suggested that you ignore 
                            the trend for short period of time.<br/><br/>Note: This application 
                           was developed as a part of URI course Bio594. 
                           Please write me at jeeban_panthi@uri.edu if you 
                           have any questions and comments about this application.
                        ")
      ),
                  tabPanel("Get code", "Please write me at jeeban_panthi@uri.edu to get the code")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$range<-renderText({
    #paste("Selected date range is ", input$daterange[1], "to", input$daterange[2])
  })
  output$subdata<-renderPlot({
    
    plot(s$status)
  })
  #to change the data from wide to long form
  data_reactive<-reactive({
    data_Variable <- data %>% gather(Variables, Value, 2:6, factor_key = TRUE)
    data_Variable$Date <- as.Date(data_Variable$Date) #try lubridate
    data_Variable <- data_Variable %>% filter(Variables == input$Variables)
    data_limited <- data_Variable %>%
      filter(Date>=input$daterange[1] & Date<=input$daterange[2])
    data_limited
  })
  output$table<-renderDT(data_reactive())
  output$plot <- renderPlotly({
    req(input$Variables)
    
    
    #subset by date
    
    ggplot(data_reactive(), aes(x = Date, y = Value)) + 
      geom_line(color='blue', size=.15)+geom_smooth(method='lm', SE=TRUE, color='red', size=0.5)+theme_bw()+
      xlab('Year')+ylab(input$Variables)+theme(axis.title.x =
                                                 element_text(size=10, face="bold"))+theme(axis.title.y =
                                                                                             element_text(margin = margin(-50,50,50,50),size=10, face="bold"))+theme(axis.text.x =
                                                                                                                                         element_text(size=12))+theme(axis.text.y = element_text(size=12))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



