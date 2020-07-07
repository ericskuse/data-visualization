library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)

# data reading and manipulation parts

education <- read_csv("Data/education.csv")%>%select(-c(2,4:7))
names(education) <- c("country","year","female","male")

avgYearEdu <- read_csv("Data/avgYearEdu.csv")%>%select(-c(2))
names(avgYearEdu) <- c("country","year","avgYearEdu")

# cut all the codes from beginning to the “removing the NAs” line and paste them below

GDPperCapita <- read_csv("Data/GDP(1).csv")%>%select(-c(2))
names(GDPperCapita) <- c("country","year","GDPperCapita","population")
data <- education%>%
  merge(avgYearEdu, by=c("country","year"))%>%
  merge(GDPperCapita, by=c("country","year"))
na.omit(data)

ourPlotMaker <- function(thisYear, cnum, textStatus){
  
  # cut all the codes after the “removing the NAs” line and paste them below
  thisYear=thisYear
  cnum=cnum
  countriesToShow= filteredData%>% arrange(-population) %>%
    pull(country) %>% head(cnum)
  
  filteredData <- data%>%
    filter(year==thisYear)
  
  theme_set(theme_wsj())
  
  p <- filteredData%>%
    ggplot(aes(x=avgYearEdu,y=GDPperCapita,color=ifelse(male>=female,'Male','Female'), size=population))+
    geom_point()+
    annotate("text",label=thisYear,color='grey80',size=20,x=7.5,y=35000)+
    scale_y_continuous(labels = dollar)+
    theme(plot.title = element_text(size = rel(0.6)),
          legend.title = element_text(size = rel(0.5)),
          axis.title=element_text(face = "bold", size = rel(0.6)))
  
  if(textStatus == "show"){
    
    # cut the geom_text line and paste it below
    p <- p + geom_text(data= filteredData %>% 
                         filter(country %in% countriesToShow),
                       mapping = aes(label = country))
  }
  return(p)
}
