#Load packages from Library
library(shiny)
library(devtools)
require(slidify)
require(plyr)
library(rCharts)
library(shinyapps)
library(reshape2)

# Import and clean data
metro <- read.csv("./metrodata.csv")


# subset metro to gather, red, yellow, green, etc. lines
# adding variables for station order (to reorder variable $station)
# Assign colors for each graph's line and shaded area in color1 and color2, respectively
# Tell graph where to start and finish the line with start and finish?
# Assign what color metroline the data contains with color
Red <- subset(metro, r == 1)
Red$stationorder <- c(7, 21, 11, 13, 14, 25, 22, 8, 16, 27, 5, 17, 6, 15, 
                      19, 20, 2, 1, 24, 23, 9, 3, 18, 10, 26, 4, 12)
Red <- Red[order(Red$stationorder), ]

Red$color1 <- "#BD173B"
Red$color2 <- "#f8e7eb"
Red$start <- 6.5
Red$finish <- 22.5
Red$color <- "Red"

Orange <- subset(metro, o == 1)
Orange$stationorder <- c(5, 18, 24, 7, 8, 23, 2, 4, 19, 11, 17, 14, 10, 
                         16, 25, 12, 13, 22, 26, 20, 9, 15, 21, 1, 6, 3)
Orange <- Orange[order(Orange$stationorder), ]

Orange$color1 <- "#DC8622"
Orange$color2 <- "#fbf2e8"
Orange$start <- 8.5
Orange$finish <- 22.5
Orange$color <- "Orange"

Blue <- subset(metro, b == 1)
Blue$stationorder <- c(25, 9, 23, 4, 24, 19, 6, 20, 12, 18, 15, 11, 1, 
                       3, 17, 27, 13, 14, 26, 8, 7, 21, 5, 10, 16, 22, 2)
Blue <- Blue[order(Blue$stationorder), ]

Blue$color1 <- "#1C95D3"
Blue$color2 <- "#e8f4fa"
Blue$start <- 9.5
Blue$finish <- 23.5
Blue$color <- "Blue"

Silver <- subset(metro, s == 1)
Silver$stationorder <- c(26, 7, 24, 25, 20, 9, 10, 6, 21, 13, 19, 16, 12, 
                         3, 18, 28, 5, 14, 15, 27, 22, 11, 17, 2, 23, 4, 8, 1)
Silver <- Silver[order(Silver$stationorder), ]

Silver$color1 <- "#A1A3A1"
Silver$color2 <- "#f5f5f5"
Silver$start <- 10.5
Silver$finish <- 24.5
Silver$color <- "Silver"

Green <- subset(metro, g == 1)
Green$stationorder <- c(16, 12, 21, 2, 7, 17, 5, 11, 6, 1, 13, 10, 15, 
                        19, 3, 9, 18, 20, 8, 14, 4)
Green <- Green[order(Green$stationorder), ]

Green$color1 <- "#19AF56"
Green$color2 <- "#e8f7ee"
Green$start <- 3.5
Green$finish <- 16.5
Green$color <- "Green"


Yellow <- subset(metro, y == 1)
Yellow$stationorder <- c(8, 14, 3, 12, 16, 1, 7, 2, 17, 15, 9, 6, 10, 11, 
                         13, 5, 4)
Yellow <- Yellow[order(Yellow$stationorder), ]

Yellow$color1 <- "#F6D336"
Yellow$color2 <- "#fefaea"
Yellow$start <- 0
Yellow$finish <- 8.5
Yellow$color <- "Yellow"

# Bind all of these metro lines together
all <- rbind(Yellow, Green, Silver, Blue, Orange, Red)

# Melt all of the metro lines together by the variables
# WalkShed size, Homes in WalkShed, Jobs in WalkShed, and WalkScore
all.m <- melt(all, measure.vars = c("WS", "HHinWS", "jobsinWS", "walkscore"))

# gatherMetro is a function that takes the melted metrolines,
# (Will I get arrested for saying that?)
# the inputs from the choices of metroline color and which indicator to graph
#####
# Returns 
# metro - A dataframe of consisting of the unique variables for metro line
# and and the specific indicator (which will be plotted as "value" from melt)
#
# color1 - A string of the color for the line (idk why we make it a vector at this point)
#
# color2 - A string of the color of the shaded area representing DC
#
# start, finish - Single numeric value representing the start and stop of the line?
#######
gatherMetro <- function(all.m, input, output) {
  metro <- all.m[which(all.m$color == input$metroline), ]
  metro <- metro[which(metro$variable == input$indicator), ]
  color1 <- metro$color1[1]
  color2 <- metro$color2[1]
  finish <- metro$finish[1]
  start <- metro$start[1]
  metro$station <- factor(metro$station, levels = metro$station, ordered = TRUE)
  done <- list(metro = metro, color1 = color1, color2 = color2, finish = finish, 
               start = start)
  
  return(done)
}
#SERVER
shinyServer( 

function(input, output) {
# start the output function
output$chart1 <- renderChart({

  # use the function gatherMetro in a reactive to pull out
  # necessary stuff when user changes selection
  metroStuff <- reactive({
    gatherMetro(all.m,input,output)
  })
  
  # begin highchart line graph
  # take each of the 'values', which come from the melt and selection of indicator
  # plot against the station name using a spline to smooth graph
  h1 <- hPlot(value ~ station, data = metroStuff()$metro, type = "spline")
  
  # Chart style is a helvetica font with font of size 15
  h1$chart(style = list(fontFamily = "Helvetica", fontSize = "15px"))
  
  # Create options to specify color and width of line to be graphed
  # idk what marker is?
  h1$plotOptions(spline = list(color = metroStuff()$color1, 
                                    lineWidth = 9,
                                    marker = list(lineColor = "black", 
                                                  fillColor = "white",
                                                  lineWidth = "2px")))
  
  # what is this.point.name? This is just a function create a tooltip
  h1$tooltip(formatter = "#! function() { return this.point.name + ': ' + this.y; } !#")
  
  #specify y axis tile as blank and have minimum be zero
  h1$yAxis(min = 0,
           title = list(text = ""))
  
  # Remove labels of x axis and set tick and line colors
  # grab color of lines, as well as where x starts and ends from the metroStuff() reactive
  # Slap a lable in the top left to let people know this is for washington D.C.
  h1$xAxis(labels = list(enabled = FALSE), 
                tickColor = "white",
                lineColor = "D8D8D8",
                plotBands = list(color = metroStuff()$color2[1], 
                                 from = metroStuff()$start[1],
                                 to = metroStuff()$finish[1],
                                 label = list(text = "Washington, DC",
                                              align = "left",
                                              x = 5)))
  # Set the title to Chart1
  h1$set(dom = "chart1")
  
  return(h1)
})
  
}
)