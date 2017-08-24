## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(googlesheets)
library(gsheet)
library(DT)

output$tableData <- renderDataTable(datatable({
  newdata <- data()[order(-data()$Rating),]
  newdata
}))

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
      dataTableOutput('table', height = 550)
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


A_player_1_rating <- 1510

A_player_2_rating <- 1490

B_player_1_rating <- 1500
  
B_player_2_rating <- 1490
  
margin_of_victory <-  1.2    # 1.2 (if 2 - 0) or 1(if 2 - 1)
  
A_win <- 1     # 1 or 0
  
B_win <- 0     # 1 or 0 
  
EA <- 1 / (1 + 10^(((B_player_1_rating + B_player_2_rating) - (A_player_1_rating + A_player_2_rating)) / 400))

EB <- 1 / (1 + 10^(((A_player_1_rating + A_player_2_rating) - (B_player_1_rating + B_player_2_rating)) / 400))


# A_player_1_rating - New rating
`Mads Bønding` <- round(A_player_1_rating + margin_of_victory * 20 * (A_win - EA), 0)

# A_player_2_rating - New rating
`Mads Barington` <- round(A_player_2_rating + margin_of_victory * 20 * (A_win - EA), 0)

# B_player_1_rating - New rating
`Morten H.` <- round(B_player_1_rating + margin_of_victory * 20 * (B_win - EB), 0)

# B_player_2_rating - New rating
Martin <- round(B_player_2_rating + margin_of_victory * 20 * (B_win - EB), 0)


data_rating <- data.frame(Navn = c("Mads Bønding", "Martin", "Jeppe", "Morten H.", "Kristian","Mads Barington", "Anders", "Jesper" ), Rating = c(`Mads Bønding`, Martin, 1500, `Morten H.`, 1510, `Mads Barington`, 1500, 1500), `Antal kampe` = c(2, 2, 0, 1, 1, 2, 0, 0), `Antal Bønner` = c(0, 2, 0, 1, 0, 1, 0, 0))

server <- function(input, output) {
  
  data <- eventReactive(input$reload, {
    url <- "https://docs.google.com/spreadsheets/d/1bVJq5BwWHmdWvTPz6kuiZJcqNI4fXJnuT4_Kxu3a1PY/edit?usp=sharing"
    result <- gsheet2tbl(url)
    result
  }, ignoreNULL = FALSE)
  
  output$table <- renderDataTable(datatable({
    newdata <- data_rating[order(-data_rating$Rating),]
    newdata
  }))
  
}

shinyApp(ui, server)
