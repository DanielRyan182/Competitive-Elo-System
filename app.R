#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


# app.R

library(shiny)
library(googlesheets4)
library(readxl)
library(writexl)
library(openxlsx)
library(rsconnect)


gs4_auth(path = "ServiceAccount.json")

# ----------------------
# INITIAL DATA / RATINGS
# ----------------------
# 

StartingRankURL <- "GOOGLE SHEETS URL"

Rankings <- read_sheet("GOOGLE SHEETS URL", sheet = "Rankings")

Ratings <- setNames(as.numeric(Rankings[[2]]), Rankings[[1]])

# ----------------------
# ELO FUNCTIONS
# ----------------------
ExpectedWinPercentage <- function(PlayerA, PlayerB){
  AWins <- 1 / (1 + 10^((PlayerB - PlayerA)/400))
  BWins <- 1 / (1 + 10^((PlayerA - PlayerB)/400))
  return(list(AWins = AWins, BWins = BWins))
}

NewRating <- function(PlayerAPct, PlayerBPct, PlayerA, PlayerB){
  PlayerA <- PlayerA + 32*(1 - PlayerAPct)
  PlayerB <- PlayerB + 32*(0 - PlayerBPct)
  return(list(PlayerA = unname(PlayerA), PlayerB = unname(PlayerB)))
}

# ----------------------
# UI
# ----------------------
ui <- fluidPage(
  titlePanel("Darts Elo Leaderboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("winner", "Winner", choices = names(Ratings)),
      selectInput("loser", "Loser", choices = names(Ratings)),
      actionButton("submit", "Submit Match")
    ),
    mainPanel(
      tableOutput("leaderboard")
    )
  )
)

# ----------------------
# SERVER
# ----------------------
server <- function(input, output, session){
  
  # Reactive ratings vector (keeps Elo updates live)
  Ratings_Reactive <- reactiveVal(setNames(as.numeric(Rankings[[2]]), Rankings[[1]]))
  
  # When submit button is clicked
  observeEvent(input$submit, {
    
    if (input$winner == input$loser) {
      showNotification(
        "Winner and loser cannot be the same player.",
        type = "error"
      )
      return()
    }
    
    Current_Ratings <- Ratings_Reactive()
    
    
    
    # Read user input
    PlayerA_name <- input$winner
    PlayerB_name <- input$loser
    
    
    
    PlayerA <- Current_Ratings[PlayerA_name]
    PlayerB <- Current_Ratings[PlayerB_name]
    
    
    
    # Compute expected percentages
    Percentages <- ExpectedWinPercentage(PlayerA, PlayerB)
    
    
    # Compute new Elo
    RatingsUpdate <- NewRating(Percentages$AWins, Percentages$BWins, PlayerA, PlayerB)
    
    # Update reactive ratings vector
    Current_Ratings[PlayerA_name] <- RatingsUpdate$PlayerA
    Current_Ratings[PlayerB_name] <- RatingsUpdate$PlayerB
    
    Ratings_Reactive(Current_Ratings)
    
    Rankings <- data.frame(
      Player = names(Current_Ratings),        # use updated ratings
      Rating = as.numeric(Current_Ratings)
    )
    
    Rankings <- Rankings[order(-Rankings$Rating), ]   # sort by Elo descending
    
    match_record <- data.frame(
      winner = PlayerA_name,
      winner_Before = PlayerA,
      winner_Increase = RatingsUpdate$PlayerA - PlayerA,
      winner_After = RatingsUpdate$PlayerA,
      loser = PlayerB_name,
      loser_Before = PlayerB,
      loser_Decrease = RatingsUpdate$PlayerB - PlayerB,
      loser_After = RatingsUpdate$PlayerB
      
    )
    
    
    sheet_append(StartingRankURL, sheet = "Matches", data=match_record)
    sheet_write(StartingRankURL, sheet = "Rankings", data=Rankings)
    
  })
  
  # Render the leaderboard
  output$leaderboard <- renderTable({
    Current <- Ratings_Reactive()
    df <- data.frame(Player = names(Current), Rating = as.numeric(Current))
    df[order(-df$Rating), ]
  })
}


# ----------------------
# RUN APP
# ----------------------
shinyApp(ui, server)
