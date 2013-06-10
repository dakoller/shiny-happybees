library(shiny)
library(datasets)
library(tseries)
library(lubridate)
library(maps)

# read data files
#kb <- read.csv("~/ShinyApps/happyBees/kb.csv", sep=';',header=TRUE)
kb <- read.csv(url("http://dl.dropboxusercontent.com/u/40615190/kb/kb.csv"), sep=';',header=TRUE)
temps <- read.csv(url("http://dl.dropboxusercontent.com/u/40615190/kb/kb_temps.csv"), sep=';',header=TRUE, dec=",")
is.na(temps$temp) <- (temps$temp == NA)

# load modification time stamp from file - just working offline!
# fileinfo <- file.info(file="http://dl.dropboxusercontent.com/u/40615190/kb/kb.csv",full.names = TRUE)
# update <- as.POSIXct(fileinfo[1,4],tz="MEZ")

update <- format(Sys.time(), "Tag %j - %e.%m.%Y, %H:%M Uhr", tz="Europe/Berlin")

kb$datum2 <- as.Date(kb$date, "%Y-%m-%d")
kb$amt <- 1

factos <- unique(factor(kb$Art))

kb$lat2 <- as.numeric(gsub(",",".", kb$lat))
kb$lon2 <- as.numeric(gsub(",",".", kb$lon))

kb2013 <- subset(kb, year==2013 )
kb2012 <- subset(kb, year==2012 )
kb2011 <- subset(kb, year==2011 )

### 

weeks <- unique(week(kb$datum2))

weeks2013 <- week(kb2013$datum2)
sums2013 <- tapply(kb2013$amt, weeks2013, sum)
span <- min(weeks2013):max(weeks2013)
out <- array(0, dim = length(span), dimnames = list(span))
out[dimnames(sums2013)[[1]]] <- sums2013

sums2013 <- out[dimnames(sums2013)[[1]]]

weeks2012 <- week(kb2012$datum2)
sums2012 <- tapply(kb2012$amt, weeks2012, sum)

weeks2011 <- week(kb2011$datum2)
sums2011 <- tapply(kb2011$amt, weeks2011, sum)


bees.d <- data.matrix(frame= data.frame(year = kb$year, week = kb$week, amt =  kb$amt))


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    cat(input$range)
    s2 <- start_data + input$range[1]
    cat(s2)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({    
    format(input$s2 ,"%d.%m.%Y")
  })
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  #output$mpgPlot <- renderPlot({
  #  boxplot(as.formula(formulaText()), 
  #          data = mpgData,
  #          outline = input$outliers)
  #})
  
#   output$mpgPlot_map2011 <- renderPlot({
# 
#     kb2 <- kb
#     switch (input$filter,
#             'Alle' = {
#               kb2 <- kb
#             },
#             'Schwarmvermittlung' = {
#               kb2 <- subset(kb, kb$Art %in% c('Schwarmvermittlung','Schwarmvermittlung/SMS'))
#             },
#             'herrenloser Schwarm' = {
#               kb2 <- subset(kb, kb$Art == "herrenloser Schwarm")
#             },
#             'Eintrag ueber Klimabiene.de' = {
#               kb2 <- subset(kb, kb$Art %in% c('freie Eingabe','freie Eingabe/SMS'))              
#             }       
#     )
#     
#     doy_from = min(input$range)
#     doy_to = max(input$range)
#     kb2 <- subset(kb2, kb2$doy > doy_from)
#     kb2 <- subset(kb2, kb2$doy < doy_to)
# 
#     kb2011 <- subset(kb2, year==2011 )
#     
#     map("world", region="germany", fill=TRUE, col="grey")
#     
#     points(x= kb2011$lon2, y= kb2011$lat2, col=col2011, cex= .4)  
#     
#     title('Geographical distribution in 2011')
#     
#   })
#   
#   output$mpgPlot_map2012 <- renderPlot({
#     
#     kb2 <- kb
#     switch (input$filter,
#             'Alle' = {
#               kb2 <- kb
#             },
#             'Schwarmvermittlung' = {
#               kb2 <- subset(kb, kb$Art %in% c('Schwarmvermittlung','Schwarmvermittlung/SMS'))
#             },
#             'herrenloser Schwarm' = {
#               kb2 <- subset(kb, kb$Art == "herrenloser Schwarm")
#             },
#             'Eintrag ueber Klimabiene.de' = {
#               kb2 <- subset(kb, kb$Art %in% c('freie Eingabe','freie Eingabe/SMS'))              
#             }       
#     )
#     
#     doy_from = min(input$range)
#     doy_to = max(input$range)
#     kb2 <- subset(kb2, kb2$doy > doy_from)
#     kb2 <- subset(kb2, kb2$doy < doy_to)
#     
#     kb2012 <- subset(kb2, year==2012 )
#     
#     map("world", region="germany", fill=TRUE, col="grey")
#     
#     points(x= kb2012$lon2, y= kb2012$lat2, col=col2012, cex= .4)  
#     
#     title('Geographical distribution in 2012')
#     
#   })
#   
#   output$mpgPlot_map2013 <- renderPlot({
#     
#     kb2 <- kb
#     switch (input$filter,
#             'Alle' = {
#               kb2 <- kb
#             },
#             'Schwarmvermittlung' = {
#               kb2 <- subset(kb, kb$Art %in% c('Schwarmvermittlung','Schwarmvermittlung/SMS'))
#             },
#             'herrenloser Schwarm' = {
#               kb2 <- subset(kb, kb$Art == "herrenloser Schwarm")
#             },
#             'Eintrag ueber Klimabiene.de' = {
#               kb2 <- subset(kb, kb$Art %in% c('freie Eingabe','freie Eingabe/SMS'))              
#             }       
#     )
#     
#     doy_from = min(input$range)
#     doy_to = max(input$range)
#     kb2 <- subset(kb2, kb2$doy > doy_from)
#     kb2 <- subset(kb2, kb2$doy < doy_to)
#     
#     kb2013 <- subset(kb2, year==2013 )
#     
#     map("world", region="germany", fill=TRUE, col="grey")
#     
#     points(x= kb2013$lon2, y= kb2013$lat2, col=col2013, cex= .4)  
#     
#     title('Geographical distribution in 2013')
#     
#   })
  
  
  output$mpgPlot_map <- renderPlot({
    
    kb2 <- kb
    switch (input$filter,
            'Alle' = {
              kb2 <- kb
            },
            'Schwarmvermittlung' = {
              kb2 <- subset(kb, kb$Art %in% c('Schwarmvermittlung','Schwarmvermittlung/SMS'))
            },
            'Herrenloser Schwarm' = {
              kb2 <- subset(kb, kb$Art == "herrenloser Schwarm")
            },
            'Direktmeldung an Klimabiene' = {
              kb2 <- subset(kb, kb$Art %in% c('freie Eingabe','freie Eingabe/SMS'))              
            }       
    )
    switch (input$year,
            '2013' = {
              kb2 <- subset(kb2, year==2013)
              year <- 2013
            },
            '2012' = {
              kb2 <- subset(kb2, year==2012)
              year <- 2012
            },
            '2011' = {
              kb2 <- subset(kb2, year==2011)
              year <- 2011
            }      
    )
    
    doy_from = min(input$range)
    doy_to = max(input$range)
    kb2 <- subset(kb2, kb2$doy >= doy_from)
    kb2 <- subset(kb2, kb2$doy <= doy_to)
    nr_swarms <- nrow(kb2)
    par(mar=c(8.1,8.0,2.2,8))
    map("world", region="germany", fill=TRUE, col="lightgrey")
    
    points(x= kb2$lon2, y= kb2$lat2, col="black", cex= .4)  
    
   # title(paste('Geographische Verteilung der Schwärme in',year,"\nZeitraum: Tag",min(input$range),"bis",max(input$range),sep=' '))
  })
  
  ############################################## in Bearbeitung
  
  output$mpgPlot_histogram <- renderPlot({
    
    kb2 <- kb
    temps2 <- temps
    switch (input$filter,
            'Alle' = {
              kb2 <- kb
            },
            'Schwarmvermittlung' = {
              kb2 <- subset(kb, kb$Art %in% c('Schwarmvermittlung','Schwarmvermittlung/SMS'))
            },
            'Herrenloser Schwarm' = {
              kb2 <- subset(kb, kb$Art == "herrenloser Schwarm")
            },
            'Direktmeldung an Klimabiene' = {
              kb2 <- subset(kb, kb$Art %in% c('freie Eingabe','freie Eingabe/SMS'))              
            }       
    )
    switch (input$year,
            '2013' = {
              kb2 <- subset(kb2, year==2013)
              year <- 2013
              temps2 <- subset(temps2, year==2013)
            },
            '2012' = {
              kb2 <- subset(kb2, year==2012)
              year <- 2012
              temps2 <- subset(temps2, year==2012)
            },
            '2011' = {
              kb2 <- subset(kb2, year==2011)
              year <- 2011
              temps2 <- subset(temps2, year==2011)
            }      
    )
    
    doy_from = min(input$range)
    doy_to = max(input$range)
    kb2 <- subset(kb2, kb2$doy >= doy_from)
    kb2 <- subset(kb2, kb2$doy <= doy_to)
    nr_swarms <- nrow(kb2)
    temps2 <- subset(temps2, temps2$doy >= doy_from)
    temps2 <- subset(temps2, temps2$doy <= doy_to)
    
    switch(input$interval, 
           'tagesweise' = {
                bins <- seq( (min(input$range)-0.5),(max(input$range)+0.5), by=1)
           },
           'wochenweise' = {
                bins <- seq( (min(input$range)-0.5),(max(input$range)+0.5), by=1) ## noch überarbeiten
           },
           'monatsweise' = {
                bins <- seq( (min(input$range)-0.5),(max(input$range)+0.5), by=1) ## noch überarbeiten
           }
    )
    
    if (min(temps2$temp,na.rm = TRUE) < -3){lowtemp <- min(temps2$temp,na.rm = TRUE)} else {lowtemp <- 0}
    
    par(mar=c(4.1,4.0,2.2,4)) #right war 1
    #histo <- hist(kb2$doy,breaks=bins)
    hist(kb2$doy,
              breaks=bins,
              ylim=c(0,70),
              #plot=FALSE,
              xlab="Tag des Jahres",
              ylab="Anzahl der Schwärme",
              main=paste('Zeitliche und räumliche Verteilung von',nr_swarms,'Schwarmereignissen in',year,"\nZeitraum: Tag",min(input$range),"bis",max(input$range),"- Stand:",update,sep=' '),
              #sub=paste(,sep=' '),
              col="black",
              #space=0
              #xaxt="n",
         )
    par(new=TRUE)
    
    plot(temps2$temp,
         type="l",
         col="red",
         lwd=2,
         #xlim=c(108.5,137.5),
         ylim=c(lowtemp,25),
         ann=FALSE,
         axes=FALSE)
    axis(4)
    mtext(4, text = "Temperatur [°C]", line = 3)
         
  })
  
  
#   output$mpgPlot_overall <- renderPlot({    
#     
#     kb <- read.csv("~/ShinyApps/happyBees/kb.csv", sep=';',header=TRUE)
#     #kb <- read.csv(url("https://dl.dropboxusercontent.com/u/40615190/kb/kb.csv"), sep=';',header=TRUE)
#     
#     kb$datum2 <- as.Date(kb$date, "%Y-%m-%d")
#     kb$amt <- 1
#     
#     kb$lat2 <- as.numeric(gsub(",",".", kb$lat))
#     kb$lon2 <- as.numeric(gsub(",",".", kb$lon))
#     
#     kb2 <- kb
#     switch (input$filter,
#             'Alle' = {
#               kb2 <- kb
#             },
#             'Schwarmvermittlung' = {
#               kb2 <- subset(kb, kb$Art %in% c('Schwarmvermittlung','Schwarmvermittlung/SMS'))
#             },
#             'Herrenloser Schwarm' = {
#               kb2 <- subset(kb, kb$Art == "herrenloser Schwarm")
#             },
#             'Direktmeldung an Klimabiene' = {
#               kb2 <- subset(kb, kb$Art %in% c('freie Eingabe','freie Eingabe/SMS'))              
#             }       
#             )
#     
#     doy_from = min(input$range)
#     doy_to = max(input$range)
#     kb2 <- subset(kb2, kb2$doy > doy_from)
#     kb2 <- subset(kb2, kb2$doy < doy_to)
#     
#     kb2013 <- subset(kb2, year==2013 )
#     kb2012 <- subset(kb2, year==2012 )
#     kb2011 <- subset(kb2, year==2011 )
#     
#     weeks <- unique(week(kb2$datum2))
#     
#     switch(input$interval, 
#            'wochenweise' = {
#       
#       weeks2013 <- week(kb2013$datum2)
#       sums2013 <- tapply(kb2013$amt, weeks2013, sum)
#       span <- min(weeks2013):max(weeks2013)
#       out <- array(0, dim = length(span), dimnames = list(span))
#       out[dimnames(sums2013)[[1]]] <- sums2013
#       
#       sums2013 <- out[dimnames(sums2013)[[1]]]
#       
#       weeks2012 <- week(kb2012$datum2)
#       sums2012 <- tapply(kb2012$amt, weeks2012, sum)
#       
#       weeks2011 <- week(kb2011$datum2)
#       sums2011 <- tapply(kb2011$amt, weeks2011, sum)
#       
#      
#       if (input$compare) {
#         
#         plot( unique(weeks2011), sums2011, type="l", col=col2011, ylab="Anzahl der Schwärme",xlab= "Kalenderwoche")
# 
#         points( unique(weeks2013), sums2013, type="l",col=col2013)
#         points( unique(weeks2012), sums2012, type="l",col=col2012)
#         
# 
#       } else {
#         barplot( unique(weeks2013),sums2013, col=col2013, ylab="Anzahl der Schwärme",xlab= "Kalenderwoche") 
#       }
#     
#     },
#     'tagesweise' = {
#       
#       doys2013 <- yday(kb2013$datum2)
#       sums2013 <- tapply(kb2013$amt, doys2013, sum)
#       span <- min(doys2013):max(doys2013)
#       out <- array(0, dim = length(span), dimnames = list(span))
#       out[dimnames(sums2013)[[1]]] <- sums2013
#       
#       sums2013 <- out[dimnames(sums2013)[[1]]]
#       
#       doys2012 <- yday(kb2012$datum2)
#       sums2012 <- tapply(kb2012$amt, doys2012, sum)
#       
#       doys2011 <- yday(kb2011$datum2)
#       sums2011 <- tapply(kb2011$amt, doys2011, sum)
#       
#       if (input$compare) {
#       
#         plot( unique(doys2011),sums2011, col=col2011, type="l", ylab="Anzahl der Schwärme",xlab= "Tag des Jahres") 
#         
#         points( unique(doys2013), sums2013, type="l",col=col2013)
#         points( unique(doys2012), sums2012, type="l",col=col2012)
#       
#       } else {
#         barplot( unique(doys2013),sums2013, col=col2013, ylab="Anzahl der Schwärme",xlab= "Tag des Jahres") 
#       }
#       
#     },
#     'monatsweise' = {
#       
#       ms2013 <- month(kb2013$datum2)
#       sums2013 <- tapply(kb2013$amt, ms2013, sum)
#       span <- min(ms2013):max(ms2013)
#       out <- array(0, dim = length(span), dimnames = list(span))
#       out[dimnames(sums2013)[[1]]] <- sums2013
#       
#       sums2013 <- out[dimnames(sums2013)[[1]]]
#       
#       ms2012 <- month(kb2012$datum2)
#       sums2012 <- tapply(kb2012$amt, ms2012, sum)
#       
#       ms2011 <- month(kb2011$datum2)
#       sums2011 <- tapply(kb2011$amt, ms2011, sum)
#       
#       if (input$compare) {
#         
#         plot( unique(ms2011),sums2011, type="l", col=col2011, ylab="Anzahl der Schwärme",xlab= "Monat") 
#         
#         points( unique(ms2013), sums2013, type="l",col=col2013)
#         points( unique(ms2012), sums2012, type="l",col=col2012)
#         
#         
#         
#       } else {
#         barplot( unique(ms2013),sums2013, col=col2013, ylab="Anzahl der Schwärme",xlab= "Monat") 
#       }
#       
#       
#       
#     })
#     
#   })
   
 })
