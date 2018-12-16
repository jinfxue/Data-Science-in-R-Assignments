library(shinydashboard)
library(shiny)
library(benford.analysis)
library(leaflet)



# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Mass Shooting in USA"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Introduction", tabName = "intro", icon = icon("home",lib = "glyphicon")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Map", tabName = "map", icon = icon("map-marker",lib = "glyphicon"),
               menuItem("Total Victims",
                        tabName = "tv",
                        icon = icon("calculator")
               ),
               menuItem("Location Distribution",
                        tabName = "ld",
                        icon = icon("globe")
               )
               ),
      menuItem("Exploration",
               tabName = "exp", icon = icon("palette"),
               
               menuItem("Shooter Related",
                        tabName = "sr",
                        icon = icon("user-friends")
               ),
               menuItem("Location Related",
                        tabName = "lr",
                        icon = icon("city")
               ),
               menuItem("Time Line",
                        tabName = "tl",
                        icon = icon("calendar-alt")
               )
      ),
      menuItem("Distribution Test",
               tabName = "dt", icon = icon("stats", lib = "glyphicon"),
               
               menuItem("Benford Law Test",
                        tabName = "bf",
                        icon = icon("bitcoin", lib = "glyphicon")
               ),
               menuItem("Gamma Distribution Test",
                        tabName = "gm",
                        icon = icon("chart-area")
               )
               
      ),
      menuItem("Text Mining", tabName = "tm", icon = icon("file-alt")),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        fluidRow(imageOutput("row1"),
                 infoBox("Written by", "Jinfei XUE", icon = icon("grin-tongue-wink"))
                 ),
        fluidRow( 
          box(
            title = "Context", status = "danger", solidHeader = TRUE,
            "Mass Shootings in the United States of America (1966-2017) The US has witnessed 398 mass shootings in last 50 years that resulted in 1996 deaths and 2488 injured. The latest and the worst mass shooting of October 2, 2017 killed 58 and injured 515 so far. The number of people injured in this attack is more than the number of people injured in all mass shootings of 2015 and 2016 combined. The average number of mass shootings per year is 7 for the last 50 years that would claim 39 lives and 48 injured per year."
          ),
          column(6,
                         
                         box(width=11, solidHeader = TRUE, status = "danger",
                             title="Overview",
                             h4("In this shiny app, users can be directed to:",style = "font-family: 'Arial'," ),
                             h5("  1) Data"),
                             h5("  2) Map"),
                             h5("  3) Exploration"),
                             h5("  4) Distribution Test"),
                             h5("  5) Text Mining")
                         ))
        )
      ),
      
      tabItem(
        tabName = "data",
        fluidRow(
          tabBox(
            title = "Mass Shootings in America",id = "dataTabset",
            # The id lets us use input$tabset1 on the server to find the current tab
            tabPanel("Data Discription",
                     fluidRow(
                       box(solidHeader = TRUE, status = "danger", title="Collection Background",
                       "The data used here is from Stanford Mass Shootings of America (MSA) data project, which began in 2012, in reaction to the mass shooting in Sandy Hook, CT. In their initial attempts to map this phenomena it was determined that no comprehensive collection of these incidents existed online. The Stanford Geospatial Center set out to create a single point repository for as many mass shooting events as could be collected via online media. The result was the Stanford MSA. The Stanford MSA is a data aggregation effort. It is a curated set of spatial and temporal data about mass shootings in America, taken from online media sources. It is an attempt to facilitate research on gun violence in the US by making raw data more accessible."
                     )),
                     fluidRow(
                       box(solidHeader = TRUE, status = "danger", title="Definition of Mass Shooting",
                           "The definition of mass shooting used for the Stanford database is 3 or more shooting victims (not necessarily fatalities), not including the shooter. The shooting must not be identifiably gang, drug, or organized crime related."
                     )),
                     fluidRow(
                       box(solidHeader = TRUE, status = "danger", title="Access",
                           "All records, data dictionary, and methodology information are avaualble for public use at the https://github.com/StanfordGeospatialCenter/MSA"
                       ))
               ),
            tabPanel("dataTab2", "Original Dataset")
          ,
         box(title = "Original Dataset", status = "danger", collapsible = TRUE,
              solidHeader = TRUE, DT::dataTableOutput("table"), width = 12, height = 600)
        ),
        box(
          title = "Context", status = "danger", solidHeader = TRUE,
          "Mass Shootings in the United States of America (1966-2017) The US has witnessed 398 mass shootings in last 50 years that resulted in 1996 deaths and 2488 injured. The latest and the worst mass shooting of October 2, 2017 killed 58 and injured 515 so far. The number of people injured in this attack is more than the number of people injured in all mass shootings of 2015 and 2016 combined. The average number of mass shootings per year is 7 for the last 50 years that would claim 39 lives and 48 injured per year."
        )
      ),
      
      
      
      tabItem(
        tabName = "map",
        
        fluidRow(
          
          column(width = 12,
                 box(title = "Total sale price based on location",width = NULL, solidHeader = TRUE,
                     leafletOutput("busmap", height = 500)
                 ),
                 
                 column(width = 12,
                        box(title = "Controls", width = NULL, status = "warning",
                            "Select date only within year 2017",
                            dateRangeInput("dateRange1",
                                           label = "Select Date",
                                           start = "2017-01-01", end = "2017-12-31"
                            ),
                            "Total sale price during selected date range",
                            sliderInput("slider1", "Sales Price Range:", 1, 4000000000, value = c(600000000, 1500000000))
                            # selectInput("Month","Select Month", 
                            #             choices = 1:12),
                            
                        )
                        
                 )
          )
        )
      ),#First Tab
      tabItem(
        tabName = "vis",
        
        fluidRow(
          tabBox(
            height = 400 ,
            tabPanel("Total Sales for Each Neighborhood", plotlyOutput("plot1"), width = 200, height = 500),
            tabPanel("Sales Price Info", plotlyOutput("plot2",height = 800), width = 200, height =800),
            tabPanel("More Info", plotlyOutput("plot3",height = 500),plotlyOutput("plot4",height = 500),width = 200, height =1000)
          ),
          column(width = 6,
                 box(title = "Controls for Sales Info",width = NULL, solidHeader = TRUE,
                     selectInput("var", "Total sale price for:",
                                 c("Month" = "Month",
                                   "NEIGHBORHOOD." = "NEIGHBORHOOD.",
                                   "Building Class" = "BUILDING.CLASS.AT.TIME.OF.SALE.")
                     ),
                     radioButtons("fun","Interested in:",
                                  c("median","mean","sum"))
                 ),
                 box(title = "Controls for More Info",width = NULL, solidHeader = TRUE,
                     checkboxGroupInput("nei","Neighborhoods to shows:",
                                        choices = Ne_list, selected = c("MIDTOWN CBD","MIDTOWN EAST","MIDTOWN WEST","CHINATOWN")))
          )
        )
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          
          
          box(title = "Original Dataset", status = "success", collapsible = TRUE,
              solidHeader = TRUE, DT::dataTableOutput("table"), width = 12, height = 600)
          
          
          
        ),
        box(
          title = "Select Date", status = "success", solidHeader = TRUE,
          selectInput("selectmonth","Choose Month", choices = month_list )
        )
      ),
      tabItem(
        tabName = "bfd",
        plotOutput("plot5",height = 600)
      ),
      tabItem(
        tabName = "about",
        h3("This app includes data from NewYork Department of Finance.The dataset is Neighborhood Sales Data 
           of Manhattan through 2017 .The primary purpose of this app is to Visualize the dataset better and 
           present the result of benford analysis for this dataset.",size = 10,style = "font-family: 'Arial'," ),
        h3("This shiny app developed by Mark Yan."),
        br(),
        br(),
        valueBoxOutput("userguide")
        )
      )
  )
)
# Define server logic required to draw a histogram

server <- function(input, output) {
  output$busmap <- renderLeaflet({
    
    Data1  <- Manhattan_zip[Manhattan_zip$SALE.DATE.>=input$dateRange1[1] & Manhattan_zip$SALE.DATE. <= input$dateRange1[2],]
    
    map_sum <- aggregate(SALE.PRICE.~NEIGHBORHOOD.,data=Data1,sum)
    draw <- inner_join(map_sum,Manhattan_zip,by = "NEIGHBORHOOD.")
    draw <- distinct(draw,NEIGHBORHOOD.,.keep_all = TRUE)
    map_data <- draw[draw$SALE.PRICE..x>= input$slider1[1] & draw$SALE.PRICE..x <= input$slider1[2],]
    
    m <- leaflet(data =map_data ) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers( ~longitude,~latitude
                  , popup = ~as.character(SALE.PRICE..x), label = ~as.character(NEIGHBORHOOD.))
    m
  })
  
  output$plot1 <- renderPlotly({
    p <- ggplot(Manhattan)+
      geom_bar(mapping = aes(x = NEIGHBORHOOD.))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$plot2 <- renderPlotly({
    pp <- ggplot(data = Manhattan,
                 aes_string(x=input$var, y = "SALE.PRICE.")) +
      scale_y_continuous(labels = comma)+
      stat_summary(fun.y = input$fun, # adds up all observations for the month
                   geom = "bar") + # or "line"
      # custom x-axis labels
      theme(axis.text.x = element_text(angle =60, hjust = 1))
    ggplotly(pp)
  })
  
  output$plot3 <- renderPlotly({
    Data_2 <- filter(Manhattan, Manhattan$NEIGHBORHOOD.== input$nei)
    B <- ggplot(data = Data_2,aes(x=log(SALE.PRICE.),fill = NEIGHBORHOOD.))+
      geom_histogram()
    ggplotly(B)
  })
  
  output$plot4 <- renderPlotly({
    Data_2 <- filter(Manhattan, Manhattan$NEIGHBORHOOD.== input$nei)
    C <- ggplot(data = Data_2,
                aes(Month, fill = NEIGHBORHOOD.)) +
      theme(axis.text.x = element_text(angle =60, hjust = 1))+
      geom_bar(position = "dodge")
    ggplotly(C)
  })
  
  output$table<- DT::renderDataTable({
    tabledata <- Manhattan[Manhattan$Month == input$selectmonth,]
    DT::datatable(tabledata, options = list(searching = TRUE,pageLength = 50,lengthMenu = c( 50, 100, 500), scrollX = T,scrollY = "300px"),rownames= FALSE
    )
  })
  
  output$plot5 <- renderPlot({
    bfd <- benford(Manhattan$SALE.PRICE.)
    plot(bfd)
  })
  
  output$userguide <- renderUI({
    url <- a("Webpage", href="https://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page")
    
    
    tagList("You can find the data from this", url)
  })
  
  output$row1<- renderImage({
    Leg<-"www/picture1.png"
    list(src=Leg)
  },deleteFile = FALSE)  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
















############################################################################################
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

