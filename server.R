library(shiny)


# Set up data file


shinyServer(function(input, output) {
    rv <- reactiveValues()
    rv$setupComplete <- FALSE
    
    ## simulate data load
    observe({
        source('prediction.R')
        rv$setupComplete <- TRUE    

        ## the conditional panel reads this output
        output$setupComplete <- reactive({
            return(rv$setupComplete)
        })
        outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
        
    })
    
    observeEvent(input$submit, {
        phrase <- preprocess_phrase(input$phrase)
        answer <- process_gram(phrase, sample)
        output$nextWord <- renderText({ answer })
    })
})