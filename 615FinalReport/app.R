library(tm)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(benford.analysis)
library(leaflet)
library(wordcloud2)
library(DT)

MSD_S <- read.csv("Stanford_MSA_Database.csv")
Dictionary <- read.csv("Stanford_MSA_Data_Dictionary")
# Generate a new column
MSD_S$Total.Number.of.Injured <- MSD_S$Number.of.Civilian.Injured +
  MSD_S$Number.of.Enforcement.Injured

# Select interesting variables
MSD <- MSD_S %>%
  dplyr::select(Title, Location, City, State, Latitude, Longitude, 
                Total.Number.of.Injured, Total.Number.of.Fatalities,
                Total.Number.of.Victims, Date, Day.of.Week,
                Number.of.shooters, Average.Shooter.Age, Shooter.Sex, Shooter.Race, 
                Type.of.Gun...General, Total.Number.of.Guns, 
                Fate.of.Shooter.at.the.scene, Fate.of.Shooter, Shooter.s.Cause.of.Death,
                School.Related, Place.Type, Targeted.Victim.s...General, 
                History.of.Mental.Illness...General, Military.Experience)
# Date
MSD$Year <- format(as.Date(MSD$Date, format="%m/%d/%Y"),"%Y")
MSD$Month <- format(as.Date(MSD$Date, format="%m/%d/%Y"),"%m")



# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = h3("Mass Shooting in USA"),titleWidth = 250),
  dashboardSidebar(width = 250,
    sidebarMenu(
      id = "sidebarmenu",
      menuItem(h4("Introduction"), tabName = "intro", icon = icon("home",lib = "glyphicon")),
      menuItem(h4("Data"), tabName = "data", icon = icon("database")),
      menuItem(h4("Map"), tabName = "map", icon = icon("map-marker",lib = "glyphicon"),
               menuItem(h4("Total Victims"),
                        tabName = "tv"
               ),
               menuItem(h4("Location Distribution"),
                        tabName = "ld"
               )
               ),
      menuItem(h4("Exploration"),
               tabName = "exp", icon = icon("bomb"),
               
               menuItem(h4("Shooter Related"),
                        tabName = "sr",
                        icon = icon("user-friends")
               ),
               menuItem(h4("Location Related"),
                        tabName = "lr",
                        icon = icon("city")
               ),
               menuItem(h4("Time Line"),
                        tabName = "tl",
                        icon = icon("calendar-alt")
               )
      ),
      menuItem(h4("Benford Law Test"),
               tabName = "bf", icon = icon("stats", lib = "glyphicon")),
      menuItem(h4("Text Mining"), tabName = "tm", icon = icon("font")),
      menuItem(h4("About"), tabName = "about", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        fluidRow(
          box(solidHeader = TRUE, status = "danger",width = 11,
              h2("Mass Shootings in the United States of America (1966-2017)",style = "font-family: 'Arial'," ),
              h3("The US has witnessed 398 mass shootings in last 50 years that resulted in 1996 deaths and 2488 injured. The latest and the worst mass shooting of October 2, 2017 killed 58 and injured 515 so far. The number of people injured in this attack is more than the number of people injured in all mass shootings of 2015 and 2016 combined. The average number of mass shootings per year is 7 for the last 50 years that would claim 39 lives and 48 injured per year.")
          )
          ),
        fluidRow(
          column(12,
                 box(solidHeader = TRUE, status = "danger",
                     title=h3("Overview"),
                     h2("In this shiny app, users can be directed to:",style = "font-family: 'Arial'," ),
                     h3("  1) Data"),
                     h3("  2) Map"),
                     h3("  3) Exploration"),
                     h3("  4) Benford Law Test"),
                     h3("  5) Text Mining")),
          fluidRow(
            infoBoxOutput("name"),
            div(style="display: inline-block",
                img(src="data:image/png;base64,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",
                    height=300, width=600))
          ))
        )
      ),

      tabItem(
        tabName = "data",
        fluidRow(
          tabBox(width = 12,
            title = "Mass Shootings in America",id = "dataTabset",
            # The id lets us use input$tabset1 on the server to find the current tab
            tabPanel(h4("Data Discription"),
                     fluidRow(
                       box(solidHeader = TRUE, status = "danger",width = 11,
                           h2("Collection Background",style = "font-family: 'Arial'," ),
                           h3("The data used here is from Stanford Mass Shootings of America (MSA) data project, which began in 2012, in reaction to the mass shooting in Sandy Hook, CT. In their initial attempts to map this phenomena it was determined that no comprehensive collection of these incidents existed online. The Stanford Geospatial Center set out to create a single point repository for as many mass shooting events as could be collected via online media. The result was the Stanford MSA. The Stanford MSA is a data aggregation effort. It is a curated set of spatial and temporal data about mass shootings in America, taken from online media sources. It is an attempt to facilitate research on gun violence in the US by making raw data more accessible."),
                           h2("Definition of Mass Shooting",style = "font-family: 'Arial'," ),
                           h3("The definition of mass shooting used for the Stanford database is 3 or more shooting victims (not necessarily fatalities), not including the shooter. The shooting must not be identifiably gang, drug, or organized crime related."),
                           h2("Access",style = "font-family: 'Arial'," ),
                           h3(valueBoxOutput("userguide"))
                           )
                     )
            ),
            tabPanel(h4("Original Dataset"),
                     fluidRow(
                       box(solidHeader = TRUE, status = "danger"
                           , DT::dataTableOutput("table"), width = 12, height = 1000)
                     )),
            tabPanel(h4("Variable Description"),
                     fluidRow(
                       box(solidHeader = TRUE, status = "danger"
                           , DT::dataTableOutput("table2"), width = 12, height = 1000)
                     ))
            ))),
          
      tabItem(
        tabName = "tv",
        
        fluidRow(
          
          column(width = 12,
                 box(title = "Total Victims by States in USA",width = NULL,
                     solidHeader = TRUE, status = "danger",
                     leafletOutput("statemap", height = 600)
                 )
          )
        )
      ),
                 
      tabItem(
        tabName = "ld",
        
        fluidRow(
          
          column(width = 12,
                 box(title = "Location Distribution for Mass Shooting in USA",width = NULL,
                     solidHeader = TRUE, status = "danger",
                     leafletOutput("locationmap", height = 600)
                 ),
                 
                 column(width = 12,
                        box(title = "Controls", width = NULL, status = "warning",
                            "Select the time range",
                            dateRangeInput("dateRange2",
                                           label = "Select Date",
                                           start = "1966-08-01", end = "2016-06-31"),
                            "the number of Victims in mass shootings during the selected time range",
                            sliderInput("slider2", "Victims Range:", 3, 102, value = c(3, 5))
                            )
                 )
          )
        )
      ),
  
      tabItem(
        tabName = "sr",
        
        fluidRow(
          box(plotlyOutput("plot3"), solidHeader = TRUE, status = "danger"),
          box(title = "Controls for plot", solidHeader = TRUE, status = "warning",
            sliderInput("slider3", "Time Range (Year)", 1, 1966, 2016),
            checkboxGroupInput("check3","Shooter Race",
                               choices = unique(MSD$Shooter.Race), #REMEMBER 小写
                               selected = unique(MSD$Shooter.Race)),
            radioButtons("radio3", "Shooter Mental Status", 
                         c(unique(MSD$History.of.Mental.Illness...General)))
          )
        )
      ),

      tabItem(
        tabName = "lr",
        
        fluidRow(
          box(plotOutput("plot4"), solidHeader = TRUE, status = "danger"),
          box(title = "Controls for plot", solidHeader = TRUE, status = "warning",
              sliderInput("slider4", "Time Range", 1, 1966, 2016, value = c(1966, 2016)),
              radioButtons("radio4", "School Related or Not", 
                           c(unique(MSD$School.Related)))
          )
        )
      ), 
      
      tabItem(
        tabName = "tl",
        
        fluidRow(
          box(plotlyOutput("plot5"), solidHeader = TRUE, status = "danger")
        )
      ), 
         
      tabItem(
        tabName = "bf",
        
        fluidRow(
          column(width = 10,
          box(plotOutput("plot6"), solidHeader = TRUE, status = "danger"),
          box(solidHeader = TRUE, status = "danger", title="Benford Law Test",
              "The definition of mass shoot")
        )
      )), 
      
        tabItem(
          tabName = "tm",
          
          fluidRow(
            box(wordcloud2Output("plot8"), solidHeader = TRUE, status = "danger"),
            box(title = "Frequency Control", solidHeader = TRUE, status = "warning",
                sliderInput("slider8", "Frequency Range", 1, 1, 176, value = c(20, 176)) 
            )
          )
        ), 
        
      tabItem(
        tabName = "about",
        h3("This app includes data from NewYork Department of Finance.The dataset is Neighborhood Sales Data 
           of Manhattan through 2017 .The primary purpose of this app is to Visualize the dataset better and 
           present the result of benford analysis for this dataset.",size = 10,style = "font-family: 'Arial'," ),
        h3("This shiny app developed by Jinfei Xue."),
        br(),
        br()
        )
      )
  )
)

##############################################################################################

server <- function(input, output) {
  output$statemap <- renderLeaflet({
    MSD_state <- MSD %>%
      select(State, Total.Number.of.Injured, Total.Number.of.Victims) %>%
      group_by(State) %>%
      mutate(Sum.Injured=sum(Total.Number.of.Injured)) %>%
      mutate(Sum.Victims=sum(Total.Number.of.Victims)) %>%
      select(State, Sum.Injured, Sum.Victims) %>%
      unique()
    
    #draw map using leaflet
    # library(sp)
    # library(dplyr)
    # library(WDI)
    ## Download data from Natural Earth
    url <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_1_states_provinces.zip"
    tmp <- tempdir()
    file <- basename(url)
    download.file(url, file)
    unzip(file, exdir = tmp)
    ## Read the data into R
    state_spatial <- readOGR(dsn=tmp,
                             layer = "ne_50m_admin_1_states_provinces", 
                             encoding = "UTF-8")
    #get the states name in spatial data
    a<-state_spatial@data[["gn_name"]] #because the state name in "states" are in lower format
    # state_spatial@data[["gn_name"]]<-a
    data<-sp::merge(state_spatial,MSD_state,by.x="gn_name",by.y="State",sort=FALSE,duplicateGeoms = TRUE,all.x=FALSE)
    labels <- sprintf(
      "<strong>%s</strong><br/>%s Total number of injured<br/>%g Total number of victims",
      data$gn_name, data$Sum.Injured, data$Sum.Victims
    ) %>% lapply(htmltools::HTML)
    # leaflet
    m <- leaflet(data) %>%
      addTiles()%>%
      setView(-96, 37.8, 4)
    #bins
    pal <- colorQuantile("YlOrRd", domain = MSD_state$Sum.Victims)
    #pal(states$Donations)
    m_victims <- m %>% addPolygons(
      fillColor = ~pal(data$Sum.Victims),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) %>%
      addLegend(pal = pal, values = ~Sum.Victims, opacity = 0.7, title = "Quantile in Total Number of Victims",
                position = "bottomright")
    m_victims
  })
  
  output$plot3 <- renderPlotly({
    MSD_plot3 <- filter(MSD, MSD$Year== input$slider3,
                    MSD$Shooter.Race==input$check3,
                    MSD$History.of.Mental.Illness...General==input$radio3
                    )
    plot3 <- MSD_plot3 %>%
      select(Year, Shooter.Sex, Total.Number.of.Victims) %>%
      group_by(Year, Shooter.Sex) %>%
      mutate(Sum.Victims=sum(Total.Number.of.Victims)) %>%
      select(Year, Shooter.Sex, Sum.Victims) %>%
      unique()
    plot_ly(data = plot3
            ,type = 'bar'
            ,mode = 'markers'
            ,x = ~Year
            ,y = ~Sum.Victims
            ,color = ~Shooter.Sex
            ,alpha = 0.9) %>% 
      layout(title = "Total Victims by Gender"
             , xaxis = list(title = "Year")
             , yaxis = list(title = "Number of Total Victims")
             , showlegend = T
             , barmode = 'stack'
             , position = 1
             , xaxis = list(title = "")
             , yaxis = list(title = "")
             , legend = list(x = 0, y = 1)
             , hovermode = 'compare')
  })
  
  output$plot4 <- renderPlotly({
    MSD_plot4 <- filter(MSD, MSD$Year== input$slider4,
                    MSD$School.Related==input$radio4)
    plot4 <- MSD_plot4 %>%
      select(Year, Place.Type, Total.Number.of.Victims) %>%
      group_by(Year, Place.Type) %>%
      mutate(Sum.Victims=sum(Total.Number.of.Victims)) %>%
      select(Year, Place.Type, Sum.Victims) %>%
      unique()
    plot_ly(data = plot4
            ,type = 'bar'
            ,mode = 'markers'
            ,x = ~Year
            ,y = ~Sum.Victims
            ,color = ~Place.Type
            ,alpha = 0.9) %>% 
      layout(title = "Total Victims by Mass Shooting Location"
             , xaxis = list(title = "Year")
             , yaxis = list(title = "Number of Total Victims")
             , showlegend = T
             , barmode = 'stack'
             , position = 1
             , xaxis = list(title = "")
             , yaxis = list(title = "")
             , legend = list(x = 0, y = 1)
             , hovermode = 'compare')
  })
 
  output$plot5 <- renderPlotly({
    plot_ly(data = MSD
            ,type = 'scatter'
            ,mode = 'markers' 
            ,hoverinfo = 'text'
            ,x = ~Month
            ,y = ~Year
            ,size = ~Total.Number.of.Victims
            ,color = ~Shooter.Sex
            ,colors = c('Red', 'Blue', 'Green', 'Black')
            ,alpha = 0.6
            ,text = ~paste("Title: ", Title
                           ,"\nLocation: ", Location
                           ,'\n Date: ', Date 
                           ,'\n Total victims : ', Total.Number.of.Victims)) %>% 
      layout(title = "Mass Shootings in US by years and month"
             , xaxis = list(title = "Month")
             , yaxis = list(title = "Years"))
  })
  
  output$plot6 <- renderPlot({
    bfd <- benford(MSD$Total.Number.of.Victims)
    plot(bfd)
  })
  
  output$plot8 <- renderPlot({
    text <- data.frame(text=removeNumbers(as.character(MSD_S$Description)))
    text$text <- as.character(text$text)
    text_n <- text %>% 
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE)
    plot8<-filter(text_n, text_n$n==input$slider8)
    wordcloud2(plot8, color="random-light",rotateRatio = 0.3)
  })
  
  output$table<- DT::renderDataTable({
    DT::datatable(MSD, options = list(searching = TRUE,pageLength = 50,lengthMenu = c( 50, 100, 500), scrollX = T,scrollY = "300px"),rownames= FALSE
    )
  })
  
  output$table2<- DT::renderDataTable({
    DT::datatable(Dictionary, options = list(searching = TRUE,pageLength = 50,lengthMenu = c( 50, 100, 500), scrollX = T,scrollY = "300px"),rownames= FALSE
    )
  })
  
  output$userguide <- renderUI({
    url <- a("Webpage", href="https://github.com/StanfordGeospatialCenter/MSA")
    
    
    tagList("You can find the data from this", url)
  })
  
  # output$row1<- renderImage({
  #   Leg<-"bu.png"
  #   list(src=Leg)
  # },deleteFile = FALSE)  
  
  output$name <- renderInfoBox({
    infoBox(
      "Written by", "Jinfei Xue @ MSSP", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "red"
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

