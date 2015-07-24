#Created by Andrew Dumit
#Located in the model builder tab
library(shiny)
library(shinydashboard)

##########
#Linear Regression Tab UI
##########
linearRegressionTabUI = '
output$linRegTabUI <- renderUI ({
  fluidRow(
    tabBox(width = 12,
           tabPanel("Build Your Model",
                    fluidRow(
                      column(width = 4,
                             uiOutput("linReg"),
                             actionButton("buildModelLinReg", "Build Model")),
                      column(width = 8,
                             verbatimTextOutput("linRegModelOutput")
                      )
                    )
           ),
           tabPanel("Auto-Select Variables",
                    fluidRow(
                      column(width = 4,
                             uiOutput("linReg2"),
                             actionButton("buildModelLinReg2", "Select Variable Subset")),
                      column(width = 8,
                             plotOutput("varImporatance1"),
                             uiOutput("linRegVarSelection1")
                      )
                    )
           ),
           tabPanel("Test for Variable Interaction",
                    fluidRow(
                      column(width = 4,
                             uiOutput("linReg3"),
                             actionButton("buildModelLinReg3", "Test Variable Interaction")),
                      column(width = 8,
                             plotOutput("varImporatance2"),
                             uiOutput("linRegVarInteraction")
                      )
                    )
           )
    )
  )
})
'
##########
#Linear Regression Tab Server Logic
##########
linearRegressionTabServer = '
  output$linReg <- renderUI ({
    box(width = 12,
        selectizeInput("linRegPredictedVar", "Variable to Predict:",
                       choices = names(getData())),
        selectizeInput("linRegPredictorVar", "Variables to Use in Model:",
                       choices = names(getData()), multiple = T))
  })
  
  createOutputLinReg <- reactive({
    if (input$buildModelLinReg == 0) {
      return(NULL)
    }
    isolate({
      linRegModel <- lm(as.formula(paste0(input$linRegPredictedVar,"~", paste0(input$linRegPredictorVar, collapse = "+"))), data = getData())
      return(summary(linRegModel))
    })
  })
  
  output$linRegModelOutput <- renderPrint ({
    print(createOutputLinReg())
  })
  
  output$linReg2 <- renderUI ({
    box(width = 12,
        selectizeInput("linRegPredictedVar2", "Variable to Predict:",
                       choices = names(getData())),
        selectizeInput("linRegPredictorVars2", "All Variables for Consideration:",
                       choices = names(getData()), multiple = T),
        textInput("numPredVars", "Maximum Number of Variables to Include in Model:"))
  })
  
  createOutputVarSelection <- reactive({
    if (input$buildModelLinReg2 == 0){
      return(NULL)
    }
    isolate({
      lm.model <- lm(as.formula(paste0(input$linRegPredictedVar2,"~", paste0(input$linRegPredictorVars2, collapse = "+"))), data = getData())
      graphicalShow <- regsubsets(as.formula(paste0(input$linRegPredictedVar2,"~", paste0(input$linRegPredictorVars2, collapse = "+"))), data = getData(), nvmax = as.numeric(input$numPredVars))
      bestModel <- step(lm.model, direction = "backward")
      return(list(graphicalShow, bestModel))
    })
  })
  
  output$varImporatance1  <- renderPlot ({
    graphical <- createOutputVarSelection()[[1]]
    print(plot(graphical, scale = "adjr2"))
  })
  
  output$linRegVarSelection1 <- renderUI ({
    bestModel <- createOutputVarSelection()[[2]]
    if (names(bestModel$coefficients)[1] == "(Intercept)") {
      int = bestModel$coefficients[1]
      coeffs <- bestModel$coefficients[-1]
    }
    else {
      coeffs <- bestModel$coefficients
      int <- ""
    }
    box(width = 12,
        h4("The best model to predict", input$linRegPredictedVar2 ,"from a subset of variables is:"),
        h5(paste(int, paste(names(coeffs), "*", coeffs, collapse = "+"), sep = "+"))
    )
  })
  
  output$linReg3 <- renderUI ({
    box(width = 12,
        selectizeInput("linRegPredictedVar3", "Variable to Predict:",
                       choices = names(getData())),
        selectizeInput("linRegPredictorVars3", "All Variables to Consider for Interaction:",
                       choices = names(getData()), multiple = T))
  })
  
  createOutputVarInteraction <- reactive({
    if (input$buildModelLinReg3 == 0) {
      return(NULL)
    }
    isolate ({
      lm.model <- lm(as.formula(paste0(input$linRegPredictedVar3,"~", paste0(input$linRegPredictorVars3, collapse = "+"))), data = getData())
      bestModel <- step(lm.model, scope =  (~.^2))
      graphicalShow <- regsubsets(bestModel$call$formula, data = getData())
      return(list(graphicalShow, bestModel))
    })
  })
  
  output$varImporatance2  <- renderPlot ({
    graphical <- createOutputVarInteraction()[[1]]
    print(plot(graphical, scale = "adjr2"))
  })
  
  output$linRegVarInteraction <- renderUI ({
    bestModel <- createOutputVarInteraction()[[2]]
    if (names(bestModel$coefficients)[1] == "(Intercept)") {
      int = bestModel$coefficients[1]
      coeffs <- bestModel$coefficients[-1]
    }
    else {
      coeffs <- bestModel$coefficients
      int <- ""
    }
    box(width = 12,
        h4("The best model to predict", input$linRegPredictedVar3 ,"with variable interactions included is:"),
        h5(paste(int, paste(names(coeffs), "*", coeffs, collapse = "+"), sep = "+"))
    )
  })
'


