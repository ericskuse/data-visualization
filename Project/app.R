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
library(robotstxt)
library(xml2)
library(rvest)
library(tm)
library(tidyverse)
library(maps)
library(shinythemes)
library(tidytext)
library(igraph)
library(ggraph)
library(lubridate)
library(gtrendsR)
library(mapproj)



ui <- dashboardPagePlus(
  
  dashboardHeader(
    title = titlePanel("COVID-19 Data"),
    titleWidth = 2000
  ),

 # header = dashboardHeaderPlus(enable_rightsidebar = TRUE, title = "COVID-19"),
  sidebar = dashboardSidebar(
    
    tags$style(".left-side, .main-sidebar {padding-top: 120px};"),
    sidebarMenu(
      menuItem("Introduction", tabName = "page5", icon = icon("info")),
      menuItem("COVID-19 Data", tabName = "page2", icon = icon("viruses")),
      menuItem("Online Learning Trends", tabName = "page1", icon = icon("chart-line")),
      
      menuItem("References", tabName = "page3", icon = icon("book-open")),
      menuItem("About Us", tabName = "page4", icon = icon("id-card"))
    )
  ),
  body = dashboardBody(
    
    
    tabItems(
      tabItem(tabName = "page5",
             
              fluidPage(tags$h4(box(width = 16, height = 900,
                                    strong("What is COVID-19?"),br(),br(),br(),
                                    "COVID-19 is a global pandemic that is infectious and is 
                                    caused due to severe acute respiratory syndrome coronavirus 2(SARS-CoV-2). 
                                    The pandemic has caused major social and economic disruption across the globe, impacting every aspect of our lives.
                                    COVID-19 is spread by close person to person contact through respiratory droplets from speaking, 
                                    coughing or sneezing. Symptoms may appear 2 to 14 days after exposure to the virus that causes COVID-19.
                                    A person does not need to have symptoms to spread the virus. COVID-19 can only be diagnosed with a laboratory test.
                                     An antibody test can show if you were previously infected and if your body has created antibodies in an attempt 
                                    to defend itself from COVID-19.",
                                    
                                  
                                    br(),br(),br(),
                                    strong("About the Project"),br(),br(),br(),
                                    "In the wake of the ongoing COVID-19 pandemic, educational institutions across the country have 
                                    been forced to shut down their campuses to prevent the spread of the virus within their communities. 
                                    As a result, virtual learning platforms have emerged as the principal mechanisms by which the nation's student population 
                                    engages with their coursework. This dramatic shift in the basic educational framework is evident 
                                    in publicly available browse and search interest data from Google. Data in the Online Learning Trends dashboard tab is delivered in real-time through the gtrends R package. The project is comprised of two sections:",
                                    br(),br(),br(),
                                    
                                    strong("1. COVID-19 Generic Overview"),br(),br(),br(),
                                    
                                    "   It provides statistical count of all the cases related to the pandemic at a global level and in the United States.
                                    The first tab displays a global map that shows all the cases across the globe and key indicators that show the metrics. 
                                    The second tab gives users an option to toggle between
                                    metrics to understand the relative statistics for the top 50 countries that have reported the highest cases. And the third tab
                                    consists of detailed data about the metrics reported by all the countries across the globe. The fourth tab allows users to compare reported metrics reported by states in the United States.",
                                    br(),br(),br(),
                                    
                                    strong("2. Online Learning Trends"),br(),br(),br(),
                                    
                                    "Learners of all ages have had to adjust to education during the pandemic. And looking at the volume of google search trends during this time 
                                    period with data from Google Trends provides some insight on the different ways we look at education in the post pandemic world.", br(),br(),
                                    
                                    "For example:",br(),br(),
                                    
                                    "-	More parents are thinking of new ways to educate their children at home (search: childrens books, educational videos, home science experiments)",br(),
                                    "-  More people are taking up learning a new language in their spare time (search: duolingo, rosetta stone, babbel)",br(),
                                    "-  More people are searching for companies that provide online credentials (search: coursera, edx, udacity)",br(),
                                    "-  More people are searching about Johns Hopkins University after COVID, too (search: johns hopkins, stanford, harvard)",br(),br(),
                                    
                                    "Depending on your interests, you may use the tool to get insights into any number of educational and employment trends relevant to you.", br(),br(),br(),br(),
))
                                    ))
              
              ,
      
          tabItem(tabName = "page1",
               fluidPage(
               
                column(width = 2,
                  selectInput(
                      inputId = "input2", #label = "Select Period",
                      choices = c("Past year",
                                  "Past 6 months",
                                  "Past month"),
                      shiny::HTML("<p> Select Period</span></p>"),
                      selected = "Past year"
                    ),
                    textInput(
                      inputId = "input3",# label = "Search Term 1",
                      width = "100%", 
                      shiny::HTML("<p> Search Term 1</span></p>"),
                      value = "edX"
                    ),
                    textInput(
                      inputId = "input4", #label = "Search Term 2",
                      width = "100%", 
                      shiny::HTML("<p> Search Term 2</span></p>"),
                      value = "coursera"
                    ),
                    textInput(
                      inputId = "input5", #label = "Search Term 3",
                      width = "100%", 
                      shiny::HTML("<p> Search Term 3</span></p>"),
                      value = "udacity"
                    ),
                    br(),
                    actionButton("refresh","Go")
                  ),
            #   mainPanel(
                column( width = 10,    tabsetPanel(type = "tabs",
                                tabPanel("Plot", icon = icon("line-chart"), fluidRow(br(), plotOutput("plot1", height = 700))),
                                tabPanel("Map", icon = icon("flag-usa"), fluidRow(br(), plotOutput("plot2", height = 700))),
                                tabPanel("Related Queries",icon = icon("spider"), fluidRow(br(), plotOutput("plot3", height = 700)))        
           #          )
                )
                ))
              )
              
      ,
      tabItem(tabName = "page2",
              tabsetPanel(
                tabPanel("Overview", fluid = TRUE, icon = icon("atom"),
                         column(width = 2,
                                br(),
                                fluidRow(tags$h4(box("Cases", p(),width = NULL, height = 67, solidHeader = TRUE, textOutput("WorldTotCases")))),
                                p(),
                                fluidRow(tags$h4(box("Active", p(),width = NULL, height = 67, solidHeader = TRUE, textOutput("WorldTotAct")))),
                                p(),
                                fluidRow(tags$h4(box("Recovered",p(), width = NULL, height = 67, solidHeader = TRUE, textOutput("WorldTotRec")))),
                                p(),
                                fluidRow(tags$h4(box("Deaths", p(),width = NULL, height = 67, solidHeader = TRUE, textOutput("WorldTotDeath")))),
                                p(),
                                fluidRow(tags$h4(box("Critical",p(), width = NULL, height = 67, solidHeader = TRUE, textOutput("WorldTotSer")))),
                                p(),
                                fluidRow(tags$h4(box("New Cases", p(),width = NULL, height = 67, solidHeader = TRUE, textOutput("WorldTotNew"))))
                               
                                # fluidRow(style = "border-radius: 20px; border-left: 10px solid #0000d8; ", 
                                #          tags$h2(box(title = "Total Cases/M Pop", width = NULL, height = 100, solidHeader = TRUE, textOutput("WorldTot1M")))
                                ),
                         br(),br(),p(),
                         
                         column(width = 10, fluidRow( plotOutput("plot5")))
                ),
                tabPanel("World Summary", fluid = TRUE, icon = icon("globe"),
                         br(),  p(),
                         
                         tags$h5(column(width = 2,
                                        
                                        selectInput('Rnk', choices = seq(1,50,1), 
                                                    shiny::HTML("<p> <span style='color: #d4d3d5'>Top 50 Countries</span></p>"),
                                                    selected = 10),
                                        
                                        selectInput('Met1', 
                                                    choices =  c("Total_Cases","New_Cases","Total_Deaths","New_Deaths","Total_Recovered","NewRecovered","Active_Cases","Serious_Critical",
                                                                 "Total_Cases_1M_Pop","Deaths_1M_Pop","Total_Tests","Tests_1M_Pop","Population") , 
                                                    
                                                    shiny::HTML("<p> <span style='color: #d4d3d5'>Metrics</span></p>"),
                                                    selected = 'Total_Cases'),
                                        
                                        selectInput('Met2', 
                                                    choices = c("Total_Cases","New_Cases","Total_Deaths","New_Deaths","Total_Recovered","NewRecovered","Active_Cases","Serious_Critical",
                                                                "Total_Cases_1M_Pop","Deaths_1M_Pop","Total_Tests","Tests_1M_Pop","Population"), 
                                                    shiny::HTML("<p> <span style='color: #d4d3d5'>Comparison Metrics</span></p>"),
                                                    selected = 'Population'))),
                         
                         br(), br(), p(),
                         
                         column(width = 5, plotlyOutput("plot4")),
                         p(),
                         column(width = 5, plotOutput("plot7"))
                ),
                tabPanel("World Detail", fluid = TRUE, icon=icon("database"), DT::dataTableOutput('plot6')
                ),
                tabPanel("US", fluid = TRUE, icon = icon("flag-usa"),
                         br(),
                         p(),
                         tags$h6(column(width = 2,
                                        selectInput('Rnk1', choices = seq(1,51,1), 
                                                    shiny::HTML("<p> <span style='color: #d4d3d5'>State Rank</span></p>"),
                                                    selected = 10),
                                        
                                        selectInput('Met3', 
                                                    choices =  c("Total_Cases","New_Cases","Total_Deaths","New_Deaths",
                                                                 "Active_Cases","Total_Cases_1M_Pop","Deaths_1M_Pop","Total_Tests","Tests_1M_Pop") , 
                                                    shiny::HTML("<p> <span style='color: #d4d3d5'>Metrics</span></p>"),
                                                    selected = 'Total_Cases'),
                                        
                                        selectInput('Met4',
                                                    choices = c("Total_Cases","New_Cases","Total_Deaths","New_Deaths",
                                                                "Active_Cases","Total_Cases_1M_Pop","Deaths_1M_Pop","Total_Tests","Tests_1M_Pop"), 
                                                    shiny::HTML("<p> <span style='color: #d4d3d5'>Comparison Metrics</span></p>"),
                                                    selected = 'New_Cases'))),
                         br(), br(),  p(),
                         column(width = 5, plotlyOutput("plot8")),
                         p(),
                         column(width = 5, plotOutput("plot9"))
                )
              )
      ),
      tabItem(tabName = "page3", fluid = TRUE, 
              fluidRow(
                tags$h5(box(width = 12, height = 125,
                                   title = "Coronavirus Information", tags$a(href="https://www.hopkinsmedicine.org/health/conditions-and-diseases/coronavirus", "- Johns Hopkins Medicine"),p(),
                            tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019", "- World Health Organization"))),
                       
                       tags$h5(box(width = 12,height = 125,
                                   title = "Statistics and Count", tags$a(href="https://www.worldometers.info/coronavirus/", "- Worldometer"),p(),
                                   tags$a(href="https://coronavirus.jhu.edu/data", "- Johns Hopkins COVID Tracker"))),
                       
                       tags$h5(box(width = 12,height = 125,
                                   title = "Online Training Data", tags$a(href="https://trends.google.com/trends/", "- Google Trends"))))
      ),
      tabItem(tabName = "page4", fluid = TRUE, 
              br(), 
              fluidRow(
                tags$h1(box(width = 12, 
                                   br(),"Johns Hopkins University", br(), br() ))),
              fluidRow(
                tags$h3(box(width = 12,
                                   br(),"Course: Data Visualization",br(),"Course Instructor: Mohammad Ali Alamdar Yazdi", br(), br() ))),
              fluidRow(
                tags$h4(box(width = 12,
                                    br(),"Project By", br(), br(),  br(),
                                   'Eric Skuse', tags$a(href = "mailto: eskuse1@jhu.edu", "- eskuse1@jhu.edu"),p(), "Master in  Business Administration", br(), br(), br(), 
                                   "Josh Howard",tags$a(href = "mailto: jhowar63@jhu.edu", "- jhowar63@jhu.edu"),p(), "Master in  Business Administration",br(),br(),br(), 
                                   "Bhargav Reddy",tags$a(href = "mailto: breddy3@jhu.edu", "- breddy3@jhu.edu"),p(),  "Master in  Business Analytics & Risk Management",br(),br(),br(), 
                                   'Ruchika Raikar', tags$a(href = "mailto: rraikar1@jh.edu", "- rraikar1@jh.edu"),p(),  "Master in  Business Analytics & Risk Management"
                                      )
                                    )))
      )
    )
)

server <- function(input, output, session) {
    
    gtrendsScraper <- function(time,searchterm1,searchterm2,searchterm3){
      
      time <- if(input$input2=="Past year"){paste(Sys.Date()-365,Sys.Date()-1)}
      else if(input$input2=="Past 6 months"){paste(Sys.Date()-183,Sys.Date()-1)}
      else if(input$input2=="Past month"){paste(Sys.Date()-31,Sys.Date()-1)}
      
      searchterm1 <- input$input3
      searchterm2 <- input$input4
      searchterm3 <- input$input5
      
      dataInput <- gtrends(
        keyword = c(searchterm1, searchterm2, searchterm3),
        geo = "US",
        time = time,
        category = 0,
        hl = "en-US",
        low_search_volume = FALSE,
        cookie_url = "http://trends.google.com/Cookies/NID",
        tz = 0,
        onlyInterest = FALSE)
      return(dataInput) 
      
    }
    
    gtrendsPlot <- function(dataInput){
      
      df <- data.frame(dataInput$interest_over_time)
      
      d <- data.frame(date=as.POSIXct(c("2019-11-17","2019-12-31","2020-01-21","2020-02-03","2020-03-13","2020-05-28")),
                      event=c("first confirmed case in China","CDC becomes aware of virus", "first US case in Washington","US declares public health emergency","US national emergency declared","US reaches 100k deaths"))
      
      e <- data.frame(date=as.POSIXct(c("2020-02-03","2020-03-13","2020-05-28")),
                      event=c("US declares public health emergency","US national emergency declared","US reaches 100k deaths"))
      
      
      df$legend <- paste(df$keyword, " (", df$geo, ")", sep = "")
      
      plot1 <- ggplot(df, aes_string(x = "date", y = "hits", color = "legend")) +
        geom_line(size=3) +
        xlab("Search Date") +
        ylab("Daily Google Search Hits") +
        ggtitle(paste0("Google Search Interest Over Time - (\"",input$input3,"\", ","\"",input$input4,"\", ","\"",input$input5,"\")")) + 
        theme_bw() +
        theme(legend.title = element_blank())  
      
      if(input$input2 == "Past year") {   
        plot1 <- plot1 + 
          geom_vline(data=d, mapping=aes(xintercept=date), colour="black",linetype = "dashed") +
          geom_text(data=d, aes(x=d$date, label=d$event,y=75),colour="black", angle=90, text=element_text(size=11),vjust=-0.4)
      }
      
      if(input$input2 == "Past 6 months") {   
        plot1 <- plot1 + 
          geom_vline(data=e, mapping=aes(xintercept=date), colour="black",linetype = "dashed") +
          geom_text(data=e, aes(x=e$date, label=e$event,y=75),colour="black", angle=90, text=element_text(size=11),vjust=-0.4)
      }    
      
      return(plot1)
  }
  
    gtrendsMap <- function(dataInput){
      state <- map_data("state")
      statedf <- data.frame(dataInput()$interest_by_region) %>%
        mutate(region = tolower(location)) %>%
        filter(region %in% state$region) %>%
        select(region, hits)
      
      map1 <- state %>% ggplot() +
        geom_map(map = state,
                 aes(x = long, y = lat, map_id = region),
                 fill="#ffffff", color="#ffffff", size=0.15) +
        geom_map(data = statedf,
                 map = state,
                 aes(fill = hits, map_id = region),
                 color="#ffffff", size=0.15) +
        scale_fill_continuous(low = 'grey', high = 'red') +
        ggtitle(paste0("Google Search Interest by State for \"",input$input3,"\""))+
        theme(panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank())
      return(map1)
  }
  
    gtrendsNode <- function(dataInput){
      relateddf <- data.frame(dataInput$related_queries)
      a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
      
      topqueries_bigram <- relateddf %>% 
        filter(related_queries == 'top') %>% 
        unnest_tokens(bigram, value, token = 'ngrams', n = 3) %>% 
        separate(bigram, c("word1", "word2"), sep = " ") %>% 
        filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>% 
        count(word1, word2, sort = TRUE) %>% 
        filter(!is.na(word1), !is.na(word2)) %>% 
        graph_from_data_frame() 
      
      set.seed(5)
      
      node1 <- ggraph(topqueries_bigram, layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                       arrow = a, end_cap = circle(.07, 'inches')) +
        geom_node_point(color = 'steelblue', size = 3) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void() +
        ggtitle(paste0("Top Related Google Queries - (\"",input$input3,"\", ","\"",input$input4,"\", ","\"",input$input5,"\")"))
      return(node1)
  }
  
  dataInput = eventReactive(input$refresh, {
      return(gtrendsScraper(input$input2,input$input3,input$input4,input$input5))
  })
  
  output$plot1 = renderPlot({
    return(gtrendsPlot(dataInput()))
  })
  
  output$plot2 = renderPlot({
    return(gtrendsMap(dataInput()))
  })
  
  output$plot3 = renderPlot({
    return(gtrendsNode(dataInput()))
  })
  
  jhuhealthurl <- read_html("https://www.hopkinsmedicine.org/health/conditions-and-diseases/coronavirus")
  jhuhealth <-  jhuhealthurl %>% html_nodes('.rtf') %>% html_text()
  detail <- str_replace_all( jhuhealth,"\r\n","")  
  
  covid19wom <- read_html("https://www.worldometers.info/coronavirus/")
  Basedata <-  covid19wom %>% html_nodes('tr') %>% html_text()
  
  covid19womUS <- read_html("https://www.worldometers.info/coronavirus/country/us/")
  BasedataUS <-  covid19womUS %>% html_nodes('tr') %>% html_text()
  
  
  #World Data
  
  BasedataCountry <- Basedata[9:224]
  
  dfBasedataCountry <- data.frame(BasedataCountry, stringsAsFactors = FALSE) 
  
  dfBasedataCountry  <- separate(dfBasedataCountry, col = BasedataCountry,c("No","Country","Total_Cases","New_Cases","Total_Deaths",
                                                                            "New_Deaths","Total_Recovered","NewRecovered",
                                                                            "Active_Cases","Serious_Critical","Total_Cases_1M_Pop","Deaths_1M_Pop",
                                                                            "Total_Tests","Tests_1M_Pop","Population"), sep="\n", remove = TRUE)
  
  df1 <- dfBasedataCountry[2:216,]
  
  df1$No <- as.integer(as.character(df1$No))
  df1$Total_Cases <- as.numeric(gsub(",", "", df1$Total_Cases))
  df1$New_Cases <- as.numeric(gsub(",", "", df1$New_Cases))
  df1$Total_Deaths <- as.numeric(gsub(",", "", df1$Total_Deaths))
  df1$New_Deaths <- as.numeric(gsub(",", "", df1$New_Deaths))
  df1$Total_Recovered <- as.numeric(gsub(",", "", df1$Total_Recovered))
  df1$NewRecovered <- as.numeric(gsub(",", "", df1$NewRecovered))
  df1$Active_Cases <- as.numeric(gsub(",", "", df1$Active_Cases))
  df1$Serious_Critical <- as.numeric(gsub(",", "", df1$Serious_Critical))
  df1$Total_Cases_1M_Pop <- as.numeric(gsub(",", "", df1$Total_Cases_1M_Pop))
  df1$Deaths_1M_Pop <- as.numeric(gsub(",", "", df1$Deaths_1M_Pop))
  df1$Total_Tests <- as.numeric(gsub(",", "", df1$Total_Tests))
  df1$Tests_1M_Pop <- as.numeric(gsub(",", "", df1$Tests_1M_Pop))
  df1$Population <- as.numeric(gsub(",", "", df1$Population))
  
  df1 <- df1 %>% arrange(desc(Total_Cases)) %>% mutate(No = row_number())
  
  
  df_world <- dfBasedataCountry[1:1,]
  
  world_map <- map_data("world")
  
  #US Data
  
  BasedataUS1 <- BasedataUS[68:129]
  
  BasedataUS1 <- str_replace_all(BasedataUS1,"\n\n","\n")
  
  dfBasedataUS2 <- data.frame(BasedataUS1, stringsAsFactors = FALSE) 
  
  dfBasedataUS3  <- separate(dfBasedataUS2, col = BasedataUS1,c("No","State","Total_Cases","New_Cases","Total_Deaths","New_Deaths",
                                                                "Active_Cases","Total_Cases_1M_Pop","Deaths_1M_Pop","Total_Tests","Tests_1M_Pop"), sep="\n", remove = TRUE)
  
  dfBasedataUS3$No <- as.integer(as.character(dfBasedataUS3$No))
  dfBasedataUS3$Total_Cases <- as.numeric(gsub(",", "", dfBasedataUS3$Total_Cases))
  dfBasedataUS3$New_Cases <- as.numeric(gsub(",", "", dfBasedataUS3$New_Cases))
  dfBasedataUS3$Total_Deaths <- as.numeric(gsub(",", "", dfBasedataUS3$Total_Deaths))
  dfBasedataUS3$New_Deaths <- as.numeric(gsub(",", "", dfBasedataUS3$New_Deaths))
  dfBasedataUS3$Active_Cases <- as.numeric(gsub(",", "", dfBasedataUS3$Active_Cases))
  dfBasedataUS3$Total_Cases_1M_Pop <- as.numeric(gsub(",", "", dfBasedataUS3$Total_Cases_1M_Pop))
  dfBasedataUS3$Deaths_1M_Pop <- as.numeric(gsub(",", "", dfBasedataUS3$Deaths_1M_Pop))
  dfBasedataUS3$Total_Tests <- as.numeric(gsub(",", "", dfBasedataUS3$Total_Tests))
  dfBasedataUS3$Tests_1M_Pop <- as.numeric(gsub(",", "", dfBasedataUS3$Tests_1M_Pop))
  
  dfBasedataUS3 <- dfBasedataUS3 %>% arrange(desc(Total_Cases)) %>% mutate(No = row_number())
  
  
  
  output$detail = renderText({
    return(detail)})
  
  output$WorldTotCases = renderText({
    return(df_world$Total_Cases)})
  
  output$WorldTotRec = renderText({
    return(df_world$Total_Recovered)})
  
  output$WorldTotDeath= renderText({
    return(df_world$Total_Deaths)})
  
  output$WorldTotAct= renderText({
    return(df_world$Active_Cases)})
  
  output$WorldTotSer= renderText({
    return(df_world$Serious_Critical)})
  
  output$WorldTotNew= renderText({
    return(df_world$New_Cases)})
  
  output$WorldTot1M= renderText({
    return(df_world$Total_Cases_1M_Pop)})
  
  
  #Scatter
  
    output$plot4 <- renderPlotly({
    df1_10 <- df1[1:input$Rnk,]
    ggplot(data=df1_10, aes(color = Country)) + 
     theme_grey() + 
      geom_point(aes_string( x=input$Met1, y =input$Met2),alpha = .7,size = 3) + 
      theme( plot.title = element_text(size = rel(1.5),hjust = 0.5, face = "bold"), axis.ticks = element_blank() ) +  
      labs(title = "Country Comparison") 
  })
  
  
  #Bar
  
  output$plot7 = renderPlot({
    df1_10 <- df1[1:input$Rnk,]
    
    ggplot(df1_10, aes(x = Total_Cases,y= reorder(Country, -No), fill = Total_Cases)) +
      geom_col(show.legend  = FALSE) +
      
      scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      
      labs(title = "COVID Cases by Country") + 
      geom_text(aes(label = Total_Cases),position = position_stack(vjust = 0.5), size = 3) + 
      scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      theme(plot.title = element_text(size = rel(1),hjust = 0.5, face = "bold"), axis.title = element_blank(), axis.text.x = element_blank())
    
  })
  
  
  #Table
  
  output$plot6 = renderDataTable({
    datatable(df1, options = list(lengthMenu = c(10, 25, 50), pageLength = 19))
  })
  
  
  #Map
  
  output$plot5 = renderPlot({

    
    df1_10 <- df1
    WorldData <- map_data('world') %>% fortify
    df <- data.frame(region=df1_10$Country, value=df1_10$Total_Cases, stringsAsFactors=FALSE) 
    
    ggplot() +
      geom_map(data = WorldData, map = WorldData, aes(x=long, y = lat,  map_id=region), stat = "identity", fill = "#DCDCDC", colour = "#44e52b", size=1) + 
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
            plot.title = element_text(size = rel(1.5),hjust = 0.5, face = "bold"),
            panel.background = element_rect(fill = '#92d0e8')) +
      coord_map("rectangular", lat0=0,  xlim=c(-190,190), ylim=c(-90, 80)) +
      scale_y_continuous(breaks=c()) +
      scale_x_continuous(breaks=c()) +
      labs(title="Global COVID-19 : Total Cases") +
      geom_map(data = df, map=WorldData, aes(fill=value,map_id=region),colour="#44e52b",show.legend  = FALSE) +
      scale_fill_continuous(low="#eff474", high="#e82910", guide="colorbar") 
    
  }, height = 650)
  
  
  #US

  output$plot8 <- renderPlotly({
    dfBasedataUS31 <- dfBasedataUS3[1:input$Rnk,]
    ggplot(data=dfBasedataUS31, aes(color = State)) + 
      theme_grey() + 
      geom_point(aes_string( x=input$Met3, y =input$Met4),alpha = .7,size = 3) + 
      theme( plot.title = element_text(size = rel(1.5),hjust = 0.5, face = "bold"), axis.ticks = element_blank() ) +  
      labs(title = "US State Comparison") 
  })
  
  
  #Bar
  
  output$plot9 = renderPlot({
    dfBasedataUS31 <- dfBasedataUS3[1:input$Rnk1,]
    
    ggplot(dfBasedataUS31, aes(x = Total_Cases,y= reorder(State, -No), fill = Total_Cases)) +
      geom_col(show.legend  = FALSE) +
      
      scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      # scale_fill_distiller(palette = "Spectral", direction = 1) +
      labs(title = "COVID Cases by US State") + 
      geom_text(aes(label = Total_Cases),position = position_stack(vjust = 0.8), size = 3) + 
      scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      theme(plot.title = element_text(size = rel(1.5),hjust = 0.5, face = "bold"), axis.title = element_blank(), axis.text.x = element_blank())
    
  })
  
  

  
  
}  

shinyApp(ui = ui, server = server)