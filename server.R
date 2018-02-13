library(shiny)


# Set up data file


shinyServer(function(input, output) {
    rv <- reactiveValues()
    rv$setupComplete <- FALSE
    
    ## simulate data load
    observe({
        source('predictword.R')
        rv$setupComplete <- TRUE    

        ## the conditional panel reads this output
        output$setupComplete <- reactive({
            return(rv$setupComplete)
        })
        outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
        
    })
    
    observeEvent(input$submit, {
        answer <- process_gram(phrase)
        output$nextWord <- renderText({ answer })
    })
})