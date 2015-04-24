library(shiny)
library(devtools)
library(slidify)
library(plyr)
library(rCharts)
library(shinyapps)

shinyUI(fluidPage(
  
  titlePanel("Walk sheds by Metrorail line: A data visualization"),
  
  fluidRow(
    column(12,
           p("A walk shed is the area around a transit station that can be reached on foot, usually defined as being half a mile for rail service. 
             Walk sheds are a function of built environment -- a grid allows for shorter, more direct paths relative to curvilinear suburban-style street layout, ultimately generating more ridership.
             This interactive visualization charts walk shed data by WMATA Metrorail line, showing which stations contribute most to car-free lifestyles in the DC metro area."),
           p("This project owes greatly to the work done", 
             a("here", href = "http://projects.newyorker.com/story/subway/"),
             "and",
             a("here.", href = "http://youarehere.cc/j/subway/WashingtonDC.html"),
             "Learn more about the underlying data, and walk sheds in general, from",
             a("this report", href = "http://planitmetro.com/2015/03/30/walk-this-way-metrorails-walkshed-atlas-1-0/"),
             "by PlanItMetro. Walk Score® data provided by",
             a("Redfin Real Estate in Washington, DC.", href = "https://www.redfin.com/city/12839/DC/Washington-DC")))),
 
  fluidRow(
    column(3,
           selectInput("metroline", 
                       label = "Line", 
                       choices = c("Red", "Blue", "Orange", "Green", "Yellow", "Silver")))),
  fluidRow(
    column(4,
           selectInput("indicator",
                       label = "Indicator",
                       choices = list("Households in walk shed" = "HHinWS",
                                      "Jobs in walk shed" = "jobsinWS",
                                      "Size of walk shed (sq mi)" = "WS",
                                      "Walk Score of walk shed" = "walkscore")
                       )
           )
    ),
  showOutput("chart1", "highcharts")
    
    ))