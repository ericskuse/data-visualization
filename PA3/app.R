library(shiny)
library(shinydashboard) 
library(shinydashboardPlus) 
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(leaflet)
library(DT)
library(plotly)


ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(title = "BPD Arrests", 
                                 enable_rightsidebar = TRUE, rightSidebarIcon = "gears"),
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("Plotly", tabName = "page1", icon = icon("line-chart")),
            menuItem("Density", tabName = "page2", icon = icon("area-chart")),
            menuItem("Map", tabName = "page3", icon = icon("map-o")),
            menuItem("Data", tabName = "page4", icon = icon("table"))
        )
    ),
    body = dashboardBody(
        tabItems(
            tabItem(tabName = "page1",
                    checkboxInput("holiday", label = "Show holidays", value = FALSE),
                    plotlyOutput("plot2", height = 500)
                    ),
            tabItem(tabName = "page2",
                    sliderInput("year", "Year:", min = 2014, max = 2020, value = 1, 
                                step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                    plotOutput("plot1")
                    ),
            tabItem(tabName = "page3",
                    leafletOutput("myMap", width="100%")
                    ),
            tabItem(tabName = "page4",
                    dataTableOutput("myTable")
            )
        )
    ),
    rightsidebar = rightSidebar(tags$a(href="https://data.baltimorecity.gov/Public-Safety/BPD-Arrests/3i3v-ibrt", target="_blank")
        
    ),
    title = "DashboardPage"
)


server <- function(input, output, session) {

    data <- read_csv("Data/data.csv")
    data$ArrestDate <- as.Date(data$ArrestDate,format = "%m/%d/%Y")
    data$Longitude <- round(data$Longitude, digits = 3) #round the digits of the long/late data to be more readable (total of 5 digits)
    data$Latitude <- round(data$Latitude, digits = 3)
    
    d <- data %>% group_by(Date=ArrestDate) %>% summarise(N=n())
    
    data_hol <- d %>% #merge the arrest data from data frame d with the holidays
        merge(holidays, by = "Date", all.x = TRUE)
    
    output$plot1 = renderPlot({
        
        filteredData <- data%>%
            filter(as.numeric(format(ArrestDate,'%Y'))==input$year)
        
        c <- filteredData%>%  #part c density chart with # of observations as y axis and age as x axis, broken down by gender
            ggplot(aes(x=Age, color=Sex))+
            geom_density()+
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank())+
            annotate("text",label=input$year,color='grey80',size=20,x=60,y=.04)+
            scale_color_discrete(labels = c("Male","Female"))+
            labs(
                title = paste0("Age distribution of crimes reported within each gender"),
                color="Gender",
                y="Density"
            ) +
            xlim(0,80)+ ylim(0,.05)
        c
    })
    
    output$plot2 = renderPlotly({
        f <- d%>%  #part d line chart
            ggplot(aes(x=Date,y=N))+
            geom_line()+
            geom_smooth()
        
        if(input$holiday == TRUE) {
            f <- f+
                geom_point(data=subset(data_hol,!is.na(Holiday)), color = "purple")+
                geom_text(data=subset(data_hol,!is.na(Holiday)), aes(x=Date, y=N, label=Abb))}
        
        
        
        ggplotly(f)
    })
    
    output$myMap = renderLeaflet({
        loc_data <- data%>%
            group_by(lng=round(Longitude,3),lat=round(Latitude,3))%>%
            summarise(N=n())
        
        loc_data$latL = loc_data$lat-0.0005
        loc_data$latH = loc_data$lat+0.0005
        loc_data$lngL = loc_data$lng-0.0005
        loc_data$lngH = loc_data$lng+0.0005
        
        m <- loc_data %>% leaflet() %>% addTiles %>%
            setView(-76.6,39.31,zoom=12)%>%
            addProviderTiles(providers$Stamen.Toner, group = "Toner")%>%
            addLayersControl(baseGroups = c("Toner","OSM"),
                             options = layersControlOptions(collapsed = FALSE))%>%
            addRectangles(
                lng1=~lngL, lat1=~latL,
                lng2=~lngH, lat2=~latH,
                fillOpacity = ~N/150, opacity = 0, fillColor = "red", label = ~N) # add the rectangles using the location bounds data and coloring the rectangles according to the number of arrests
        
    })
    
    output$myTable = renderDataTable({
        return(datatable(data,rownames = FALSE))
    })
}

shinyApp(ui = ui, server = server)
