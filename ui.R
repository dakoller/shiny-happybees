library(shiny)

#Day of year - of today
today <- Sys.Date()
doytoday <- as.numeric(strptime(today,format="%Y-%m-%d")$yday+1)

# Load file for checking standard values for data range selection
kb <- read.csv(url("http://dl.dropboxusercontent.com/u/40615190/kb/kb.csv"), sep=';',header=TRUE)
kb2013 <- subset(kb, year==2013) 

# Define UI for miles per gallon application
shinyUI(bootstrapPage(
    
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput(inputId = "year",
                label = "Jahr",
                choices = c(2013, 2012, 2011),
                selected = 2013),
    
    # Specification of range within an interval
    sliderInput("range", "Datenbereich (Tag des Jahres):",
                min = 1, 
                max = 365 , 
                value = c((min(kb2013$doy)-2),doytoday), #(max(kb2013$doy)+1)
                ),
    
    selectInput(inputId = "interval",
               label = "Darstellung erfolgt",
                choices = c('tagesweise', 'wochenweise (bald verfügbar)', 'monatsweise (bald verfügbar)'),
                selected = 'tagesweise'),
    
   # checkboxInput("compare", "Compare with previous years", FALSE),
    
    selectInput(inputId = "filter",
                label = "Art der Schwarmmeldung",
                choices = c('Alle','Schwarmvermittlung', 'Herrenloser Schwarm', 'Direktmeldung an Klimabiene' ),
                selected = 'Alle')
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(  
    
    #plotOutput('mpgPlot_overall'),
    
    #plotOutput('mpgPlot_map2011'),
    
    #plotOutput('mpgPlot_map2012'),
    
    #plotOutput('mpgPlot_map2013'),
    
    plotOutput('mpgPlot_histogram'),
    
    plotOutput('mpgPlot_map')
    
  )
))
