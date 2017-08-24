## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(googlesheets)
library(gsheet)
library(DT)
library(dplyr)
library(tidyr)



# UI ----------------------------------------------------------------------

#e4003f

ui <- dashboardPage(
  dashboardHeader(title = "Bordfodbold Stats", titleWidth = 450),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$head(tags$style(HTML('.skin-blue .main-header .logo {background-color: #504e4d;
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 28px;
                              }
                              .skin-blue .main-header .navbar {
                              background-color: #d0c6be;
                              }
                              .skin-blue .main-header .logo:hover {
                              background-color: #504e4d;
                              }
                              
                              '))),
    
    tags$style(HTML('.box.box-solid.box-success>.box-header {
                    background:#e4003f
                    }
                    .box.box-solid.box-success{
                    border-bottom-color:#e4003f;
                    border-left-color:#e4003f;
                    border-right-color:#e4003f;
                    border-top-color:#e4003f;}'
    )),
    
    tags$head(tags$style(HTML('.info-box {min-height: 175px;} .info-box-icon {height: 175px; line-height: 175px; background: #d0c6be; } .info-box-content {padding-top: 50px; padding-left: 35px; padding-bottom:50px; background: #e4003f;}'))),
    
    fluidRow(
      box(title = "Ratings", width = 6, solidHeader = TRUE, status = "success", 
          dataTableOutput('downloadTable', height = 550)
      ),
      # A static infoBox
      infoBox("Største bønneæder", value = tags$p(style = "font-size: 32px;", "Jeppe"), icon = icon("cutlery"), width = 3 , fill = TRUE),
      # A static infoBox
      infoBox("Flest vundne BO3-kampe", value = tags$p(style = "font-size: 32px;", "Mads Bønding"), icon = icon("trophy"), width = 3, fill = TRUE),
      # A static infoBox
      infoBox("Flest 2-0 sejre", value = tags$p(style = "font-size: 32px;", "Mads Barington"), icon = icon("futbol-o"), width = 3, fill = TRUE),
      # A static infoBox
      infoBox("A winning combo", value = tags$p(style = "font-size: 32px;", "Martin og Morten"), icon = icon("users"), width = 3, fill = TRUE)
    )
    
    
    )
  )


# SERVER ------------------------------------------------------------------


server <- shinyServer(function(input, output, session) {
  
  ## create checkfunc
  check_rows <- function() {
    url <- "https://docs.google.com/spreadsheets/d/1bVJq5BwWHmdWvTPz6kuiZJcqNI4fXJnuT4_Kxu3a1PY/edit?usp=sharing"
    
    games <- gsheet2tbl(url) %>% 
      arrange(desc(Timestamp))
    
    nrow(games)
    
  }
  
  ## Reactively get the table data (to pass into the render function) every x seconds
  tableData <-reactivePoll(3*1000, session = NULL,
                           check_rows,
                           valueFunc = function() {
                             # Load the match data from google sheets
                             url <- "https://docs.google.com/spreadsheets/d/1bVJq5BwWHmdWvTPz6kuiZJcqNI4fXJnuT4_Kxu3a1PY/edit?usp=sharing"
                             
                             match <- gsheet2tbl(url) %>% 
                               arrange(desc(Timestamp)) %>% # Order descending with newest record first/top
                               slice(1) # Pick first row
                             
                             # Load the current ratings for each player
                             gsheets_rating <- gs_url("https://docs.google.com/spreadsheets/d/1bDYlXePc02SD6-wB5hpacXmeJHHy6V_Dj2lL5jH9Hbs/edit?usp=sharing")
                             
                             current_ratings <- gsheets_rating %>% 
                               gs_read() %>% 
                               arrange(desc(Date), desc(Time)) %>% # Order descending with newest record first/top
                               slice(1) # Pick first row
                             
                             team_a_player1 <- ifelse(match$`Team A - Spiller 1` == "Mads Bønding", current_ratings$`Mads Bønding`, 
                                                      ifelse(match$`Team A - Spiller 1` == "Martin", current_ratings$Martin, 
                                                             ifelse(match$`Team A - Spiller 1` == "Morten H.", current_ratings$`Morten H.`,
                                                                    ifelse(match$`Team A - Spiller 1` == "Anders", current_ratings$Anders, 
                                                                           ifelse(match$`Team A - Spiller 1` == "Jeppe", current_ratings$Jeppe,
                                                                                  ifelse(match$`Team A - Spiller 1` == "Kristian", current_ratings$Kristian,
                                                                                         ifelse(match$`Team A - Spiller 1` == "Jesper", current_ratings$Jesper,
                                                                                                ifelse(match$`Team A - Spiller 1` == "Jesper", current_ratings$Jesper,
                                                                                                       ifelse(match$`Team A - Spiller 1` == "Mads Barington", current_ratings$`Mads Barington`, NA)))))))))
                             
                             team_a_player2 <- ifelse(match$`Team A - Spiller 2` == "Mads Bønding", current_ratings$`Mads Bønding`, 
                                                      ifelse(match$`Team A - Spiller 2` == "Martin", current_ratings$Martin, 
                                                             ifelse(match$`Team A - Spiller 2` == "Morten H.", current_ratings$`Morten H.`,
                                                                    ifelse(match$`Team A - Spiller 2` == "Anders", current_ratings$Anders, 
                                                                           ifelse(match$`Team A - Spiller 2` == "Jeppe", current_ratings$Jeppe,
                                                                                  ifelse(match$`Team A - Spiller 2` == "Kristian", current_ratings$Kristian,
                                                                                         ifelse(match$`Team A - Spiller 2` == "Jesper", current_ratings$Jesper,
                                                                                                ifelse(match$`Team A - Spiller 2` == "Jesper", current_ratings$Jesper,
                                                                                                       ifelse(match$`Team A - Spiller 2` == "Mads Barington", current_ratings$`Mads Barington`, NA)))))))))
                             
                             team_b_player1 <- ifelse(match$`Team B - Spiller 1` == "Mads Bønding", current_ratings$`Mads Bønding`, 
                                                      ifelse(match$`Team B - Spiller 1` == "Martin", current_ratings$Martin, 
                                                             ifelse(match$`Team B - Spiller 1` == "Morten H.", current_ratings$`Morten H.`,
                                                                    ifelse(match$`Team B - Spiller 1` == "Anders", current_ratings$Anders, 
                                                                           ifelse(match$`Team B - Spiller 1` == "Jeppe", current_ratings$Jeppe,
                                                                                  ifelse(match$`Team B - Spiller 1` == "Kristian", current_ratings$Kristian,
                                                                                         ifelse(match$`Team B - Spiller 1` == "Jesper", current_ratings$Jesper,
                                                                                                ifelse(match$`Team B - Spiller 1` == "Jesper", current_ratings$Jesper,
                                                                                                       ifelse(match$`Team B - Spiller 1` == "Mads Barington", current_ratings$`Mads Barington`, NA)))))))))
                             
                             team_b_player2 <- ifelse(match$`Team B - Spiller 2` == "Mads Bønding", current_ratings$`Mads Bønding`, 
                                                      ifelse(match$`Team B - Spiller 2` == "Martin", current_ratings$Martin, 
                                                             ifelse(match$`Team B - Spiller 2` == "Morten H.", current_ratings$`Morten H.`,
                                                                    ifelse(match$`Team B - Spiller 2` == "Anders", current_ratings$Anders, 
                                                                           ifelse(match$`Team B - Spiller 2` == "Jeppe", current_ratings$Jeppe,
                                                                                  ifelse(match$`Team B - Spiller 2` == "Kristian", current_ratings$Kristian,
                                                                                         ifelse(match$`Team B - Spiller 2` == "Jesper", current_ratings$Jesper,
                                                                                                ifelse(match$`Team B - Spiller 2` == "Jesper", current_ratings$Jesper,
                                                                                                       ifelse(match$`Team B - Spiller 2` == "Mads Barington", current_ratings$`Mads Barington`, NA)))))))))
                             
                             
                             # Team A's estimated chances of winning prior to the match
                             EA <- 1 / (1 + 10^(((team_b_player1 + team_b_player2) - (team_a_player1 + team_a_player2)) / 400))
                             
                             # Team B's estimated chances of winning prior to the match
                             EB <- 1 / (1 + 10^(((team_a_player1 + team_a_player2) - (team_b_player1 + team_b_player2)) / 400))
                             
                             
                             # A_player_1_rating - New rating
                             team_a_player1_new <- round(team_a_player1 + ifelse(match$`Resultat af BO3` == "2 - 0 til Team B" | match$`Resultat af BO3` == "2 - 0 til Team A", 1.2, 1) * 20 * (ifelse(match$`Resultat af BO3` == "2 - 0 til Team A" | match$`Resultat af BO3` == "2 - 1 til Team A", 1, 0) - EA), 0)
                             
                             # A_player_2_rating - New rating
                             team_a_player2_new <- round(team_a_player2 + ifelse(match$`Resultat af BO3` == "2 - 0 til Team B" | match$`Resultat af BO3` == "2 - 0 til Team A", 1.2, 1) * 20 * (ifelse(match$`Resultat af BO3` == "2 - 0 til Team A" | match$`Resultat af BO3` == "2 - 1 til Team A", 1, 0) - EA), 0)
                             
                             # B_player_1_rating - New rating
                             team_b_player1_new <- round(team_b_player1 + ifelse(match$`Resultat af BO3` == "2 - 0 til Team B" | match$`Resultat af BO3` == "2 - 0 til Team A", 1.2, 1) * 20 * (ifelse(match$`Resultat af BO3` == "2 - 0 til Team B" | match$`Resultat af BO3` == "2 - 1 til Team B", 1, 0) - EB), 0)
                             
                             # B_player_2_rating - New rating
                             team_b_player2_new <- round(team_b_player2 + ifelse(match$`Resultat af BO3` == "2 - 0 til Team B" | match$`Resultat af BO3` == "2 - 0 til Team A", 1.2, 1) * 20 * (ifelse(match$`Resultat af BO3` == "2 - 0 til Team B" | match$`Resultat af BO3` == "2 - 1 til Team B", 1, 0) - EB), 0)
                             
                             # Assign new rating to Mads Bønding
                             ifelse(match$`Team A - Spiller 1` == "Mads Bønding", current_ratings$`Mads Bønding` <- team_a_player1_new, 
                                    ifelse(match$`Team A - Spiller 2` == "Mads Bønding", current_ratings$`Mads Bønding` <- team_a_player2_new, 
                                           ifelse(match$`Team B - Spiller 1` == "Mads Bønding", current_ratings$`Mads Bønding` <- team_b_player1_new, 
                                                  ifelse(match$`Team B - Spiller 2` == "Mads Bønding", current_ratings$`Mads Bønding` <- team_b_player2_new, NA 
                                                  ))))
                             
                             # Assign new rating to Martin
                             ifelse(match$`Team A - Spiller 1` == "Martin", current_ratings$Martin <- team_a_player1_new, 
                                    ifelse(match$`Team A - Spiller 2` == "Martin", current_ratings$Martin <- team_a_player2_new, 
                                           ifelse(match$`Team B - Spiller 1` == "Martin", current_ratings$Martin <- team_b_player1_new, 
                                                  ifelse(match$`Team B - Spiller 2` == "Martin", current_ratings$Martin <- team_b_player2_new, NA 
                                                  ))))
                             
                             # Assign new rating to Morten H.
                             ifelse(match$`Team A - Spiller 1` == "Morten H.", current_ratings$`Morten H.` <- team_a_player1_new, 
                                    ifelse(match$`Team A - Spiller 2` == "Morten H.", current_ratings$`Morten H.` <- team_a_player2_new, 
                                           ifelse(match$`Team B - Spiller 1` == "Morten H.", current_ratings$`Morten H.` <- team_b_player1_new, 
                                                  ifelse(match$`Team B - Spiller 2` == "Morten H.", current_ratings$`Morten H.` <- team_b_player2_new, NA 
                                                  ))))
                             
                             # Assign new rating to Anders
                             ifelse(match$`Team A - Spiller 1` == "Anders", current_ratings$Anders <- team_a_player1_new, 
                                    ifelse(match$`Team A - Spiller 2` == "Anders", current_ratings$Anders <- team_a_player2_new, 
                                           ifelse(match$`Team B - Spiller 1` == "Anders", current_ratings$Anders <- team_b_player1_new, 
                                                  ifelse(match$`Team B - Spiller 2` == "Anders", current_ratings$Anders <- team_b_player2_new, NA 
                                                  ))))
                             
                             # Assign new rating to Jeppe
                             ifelse(match$`Team A - Spiller 1` == "Jeppe", current_ratings$Jeppe <- team_a_player1_new, 
                                    ifelse(match$`Team A - Spiller 2` == "Jeppe", current_ratings$Jeppe <- team_a_player2_new, 
                                           ifelse(match$`Team B - Spiller 1` == "Jeppe", current_ratings$Jeppe <- team_b_player1_new, 
                                                  ifelse(match$`Team B - Spiller 2` == "Jeppe", current_ratings$Jeppe <- team_b_player2_new, NA 
                                                  ))))
                             
                             # Assign new rating to Kristian
                             ifelse(match$`Team A - Spiller 1` == "Kristian", current_ratings$Kristian <- team_a_player1_new, 
                                    ifelse(match$`Team A - Spiller 2` == "Kristian", current_ratings$Kristian <- team_a_player2_new, 
                                           ifelse(match$`Team B - Spiller 1` == "Kristian", current_ratings$Kristian <- team_b_player1_new, 
                                                  ifelse(match$`Team B - Spiller 2` == "Kristian", current_ratings$Kristian <- team_b_player2_new, NA 
                                                  ))))
                             
                             # Assign new rating to Jesper
                             ifelse(match$`Team A - Spiller 1` == "Jesper", current_ratings$Jesper <- team_a_player1_new, 
                                    ifelse(match$`Team A - Spiller 2` == "Jesper", current_ratings$Jesper <- team_a_player2_new, 
                                           ifelse(match$`Team B - Spiller 1` == "Jesper", current_ratings$Jesper <- team_b_player1_new, 
                                                  ifelse(match$`Team B - Spiller 2` == "Jesper", current_ratings$Jesper <- team_b_player2_new, NA 
                                                  ))))
                             
                             # Assign new rating to Mads Barington
                             ifelse(match$`Team A - Spiller 1` == "Mads Barington", current_ratings$`Mads Barington` <- team_a_player1_new, 
                                    ifelse(match$`Team A - Spiller 2` == "Mads Barington", current_ratings$`Mads Barington` <- team_a_player2_new, 
                                           ifelse(match$`Team B - Spiller 1` == "Mads Barington", current_ratings$`Mads Barington` <- team_b_player1_new, 
                                                  ifelse(match$`Team B - Spiller 2` == "Mads Barington", current_ratings$`Mads Barington` <- team_b_player2_new, NA 
                                                  ))))
                             
                             # Append todays date to the current ratings
                             current_ratings <- current_ratings %>% 
                               mutate(Date = Sys.Date(), Time = format(Sys.time() + 7200, '%H:%M:%S')) # Be aware that when publishing to e.g. shinyapps.io the sys.time() will of the shinyapps server, hence the + 7200 secs to get shiny server to match my local time
                             
                             gsheets_rating %>% 
                               gs_add_row(input = current_ratings)
                             
                             # Transform from wide to long
                             current_ratings <- current_ratings %>% 
                               select(-Date, -Time) %>% 
                               gather(Navn, Rating)
                             
                             current_ratings
                           })
  
  
  output$downloadTable <- renderDataTable(datatable({
    tableData()[order(-tableData()$Rating),]
  }), width = '100%')
  
})

shinyApp(ui, server)
