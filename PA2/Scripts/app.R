library(shiny)
library(tm)
library(wordcloud)
library(tidyverse)
library(rvest)

ui <- fluidPage(
    titlePanel("Wikipedia Text Visualizer"),
    sidebarLayout(
        sidebarPanel(
            textInput(
                inputId = "input1",label = "Wikipedia key", value = "Data Visualization"
            ),
            sliderInput(
                inputId = "input2", min = 5, max = 50, label = "Number of words",value = 30
            ),
            sliderInput(
                inputId = "input3", min = 1, max = 10, label = "Minimum Frequency", value = 3
            ),
            radioButtons(
                inputId = "input5", label = "Scale method", 
                choices = c("none","sqrt","log2","log"), selected = "sqrt"
            ),
            selectInput(
                inputId = "input4", label = "Color pallet",
                choices = c("Set1","Set2","Set3","Dark2","Accent"), selected = "Set1"
            )
        ),
        mainPanel(
            plotOutput("plot1"),
            uiOutput("images")
        )
    )
)

server <- function(input, output) {
    
    #### Paste your functions below
    
    wikiWebScraper <- function(key, num, minFreq, pal, method) {
        key <- gsub(" ","_",key)
        num = num
        minFreq = minFreq
        pal = pal
        URL <- paste0("https://en.wikipedia.org/wiki/", key)   #set url to the wiki page we want to analyze
        
        texts <- URL %>% read_html %>%                         #collect the body text from the url and store on a variable texts
            html_nodes("[id=bodyContent]")%>%
            html_text()
        
        v <-texts %>% 
            VectorSource %>% 
            Corpus %>% 
            tm_map(content_transformer(tolower)) %>%
            tm_map(removeNumbers) %>%
            tm_map(removeWords, stopwords("english")) %>%
            tm_map(removePunctuation) %>% 
            tm_map(stripWhitespace) %>%
            TermDocumentMatrix %>%
            as.matrix %>%
            rowSums %>%
            sort(decreasing=TRUE) 
        
        if(method =="none") {d <- data.frame(word = names(v),freq=v)}
        else if(method == "sqrt") {d <- data.frame(word = names(v),freq=round(sqrt(v)))}
        else if(method == "log") {d <- data.frame(word = names(v),freq=round(log(v)))}
        else if(method == "log2") {d <- data.frame(word = names(v),freq=round(log2(v)))}
        
        set.seed(5)
        w <- wordcloud(words=d$word, freq = d$freq, max.words = num,
                       random.order = FALSE, min.freq = minFreq, colors = brewer.pal(8, pal))
    return(w)    
    }
    
    
    wikiImgScraper <- function(key){
        key <- gsub(" ","_",key)
        URL <- paste0("https://en.wikipedia.org/wiki/", key)
    
        imgList <- URL %>% read_html %>%                         #collect the images from the url and store on a variable imgList
            html_nodes("img") %>%
            html_attr("src")
        
    return(imgList)
    }
        
    ##### Paste your functions above
    output$plot1 <- renderPlot({
        wikiWebScraper(input$input1,input$input2,input$input3,input$input4,input$input5)
    })
    
    output$images <- renderUI({
        srcs=wikiImgScraper(input$input1)
        v <- list()
        v <- lapply(1:length(srcs), function(index){ 
            v[[index]] <- column(width = 3,
                                 img(src=srcs[index], width = 150, height = 150)
            )
        })
        return(v)
    })
}

shinyApp(ui = ui, server = server)