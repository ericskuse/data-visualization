library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(DT)
library(plotly)

# Data Preparation

data <- read_csv("Data/data.csv")
data$ArrestDate <- as.Date(data$ArrestDate,format = "%m/%d/%Y")
data$Longitude <- round(data$Longitude, digits = 3) #round the digits of the long/late data to be more readable (total of 5 digits)
data$Latitude <- round(data$Latitude, digits = 3)

holidays <- read_csv("Data/usholidays.csv")%>% select(-c(1))
holidays$Date <- as.Date(holidays$Date,format = "%m/%d/%Y") #convert data data from character to date

filteredData <- data%>%
  filter(as.numeric(format(ArrestDate,'%Y'))==2014)

words = unique(holidays$Holiday)
Abb = c("NYD","MLK","WaB","MeD","InD","LaD","CoD","VeD","ThD","ChD","NYD","MLK","WaB")
    holidays$Abb=holidays$Holiday
    for (i in 1:length(words)){
      holidays$Abb=str_replace(holidays$Abb,words[i],Abb[i])
    }

d <- data %>% group_by(Date=ArrestDate) %>% summarise(N=n())
       
data_hol <- d %>% #merge the arrest data from data frame d with the holidays
  merge(holidays, by = "Date", all.x = TRUE)

loc_data <- data%>%
  group_by(lng=round(Longitude,3),lat=round(Latitude,3))%>%
  summarise(N=n())

loc_data$latL = loc_data$lat-0.0005
loc_data$latH = loc_data$lat+0.0005
loc_data$lngL = loc_data$lng-0.0005
loc_data$lngH = loc_data$lng+0.0005


# Plots

c <- filteredData%>%  #part c density chart with # of observations as y axis and age as x axis, broken down by gender
  ggplot(aes(x=Age, color=Sex))+
  geom_density()+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  annotate("text",label=2014,color='grey80',size=20,x=60,y=.04)+
  scale_color_discrete(labels = c("Male","Female"))+
  labs(
    title = paste0("Age distribution of crimes reported within each gender"),
    color="Gender",
    y="Density"
    ) +
  xlim(0,80)+ ylim(0,.05)

f <- d%>%  #part d line chart
  ggplot(aes(x=Date,y=N))+
  geom_line()+
  geom_smooth()

f <- f+
  geom_point(data=subset(data_hol,!is.na(Holiday)), color = "purple")+
  geom_text(data=subset(data_hol,!is.na(Holiday)), aes(x=Date, y=N, label=Abb))
  
ggplotly(f)


m <- loc_data %>% leaflet() %>% addTiles %>%
  setView(-76.6,39.31,zoom=12)%>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner")%>%
  addLayersControl(baseGroups = c("Toner","OSM"),
                   options = layersControlOptions(collapsed = FALSE))%>%
  addRectangles(
    lng1=~lngL, lat1=~latL,
    lng2=~lngH, lat2=~latH,
    fillOpacity = ~N/150, opacity = 0, fillColor = "red", label = ~N) # add the rectangles using the location bounds data and coloring the rectangles according to the number of arrests


