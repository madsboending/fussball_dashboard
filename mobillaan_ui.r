
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(plotly)
library(ggplot2)
library(ggthemes)
library(scales)
library(tidyr)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(readxl)


ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Bordfodbold Ratings", titleWidth = 450),
                    
                    dashboardSidebar(sidebarMenu( menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")))),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML('      .main-header .logo {
                                                font-family: Garamond, Baskerville, "Baskerville Old Face", "Hoefler Text", "Times New Roman", serif;
                                                font-weight: bold;
                                                font-size: 24px;
                                                }
                                                '))),
                      useShinyjs(),
                      tabItems(
                        # First tab content
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  
                                  # Dynamic valueBoxes
                                  valueBoxOutput("udlaan", width = 4),
                                  
                                  # valueBoxOutput("udlaanpersonale", width = 3),
                                  
                                  valueBoxOutput("indtjening", width = 4), 
                                  
                                  valueBoxOutput("antal_laan", width = 4)
                                ), fluidRow(
                                  box(title = "Alder - YTD", status = "danger", solidHeader = TRUE,
                                      plotlyOutput("knd_plot", height = 350)
                                      , width = 4
                                  ),
                                  box(title = "Likviditet og Ansøgt beløb - YTD", status = "danger", solidHeader = TRUE,
                                      plotlyOutput("tid_plot", height = 350)
                                      , width = 4
                                  ),
                                  box(title = "Ansøgt beløb - YTD", status = "danger", solidHeader = TRUE,
                                      plotlyOutput("ansogt_plot", height = 350)
                                      , width = 4
                                  )
                                  
                                ), fluidRow(
                                  box(title = "Gebyrer pr. dag - YTD", status = "danger", solidHeader = TRUE,
                                      plotlyOutput("indtj_plot", height = 350)
                                      , width = 4
                                  ), 
                                    box(title = "Lånetype - YTD", status = "danger", solidHeader = TRUE,
                                        plotlyOutput("laantype_plot", height = 350)
                                        , width = 4
                                    )
                                ) 
                        )
                      )
                    )
)