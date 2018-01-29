library(shiny)
library(shinydashboard)
library(shinyjs)

shinyUI(fluidPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
        conditionalPanel(condition = "output.setupComplete",
                         textInput("phrase", "Enter Phrase", ""),
                         actionButton("submit", "Enter"),
                         textOutput("nextWord")
        ),
        conditionalPanel(condition = "!output.setupComplete",
                         box( title = "Loading, Please Wait."))
    )))

