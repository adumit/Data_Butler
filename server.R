#Data Butler by Andrew Dumit is licensed under a Creative Commons Attribution-ShareAlike 4.0 International License.
#Based on a work at https://github.com/adumit/Data_Butler.

library(shiny)
library(stringr)
library(ggplot2)
library(plyr)
library(fpc)
library(leaps)
options(shiny.maxRequestSize=300*1024^2)

shinyServer(function(input, output) {
  dataEdited <<- FALSE
  loadRData <- function(fileName){
    #loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }
  datList <<- list()
  ###########
  #Code for data uploader tab
  ###########
  inFile <- reactive({
    if (is.null(inFile)) {
      return(NULL)
    }
    else {
      input$file1
    }
  })
  
  assignNewDat <- reactive({
    if (input$finalizeUpload == 0) {
      return(NULL)
    }
    isolate({
      newList <- datList
      if (input$typeUpload == "CSV") {
        newList[[as.character(input$dataName)]] <- read.csv(inFile()$datapath, header=input$header, sep=input$sep,
                                                            quote = input$quote)
      }
      else if (input$typeUpload == "RData File") {
        newList[[as.character(input$dataName)]] <- loadRData(inFile()$datapath)
      }
      datList <<- newList
      return(datList)
    })
  })
  
  assignNewDatOldFile <- reactive({
    if (input$chooseFile == 0){
      return(NULL)
    }
    isolate ({
      newList <- datList
      if (!is.null(input$dataset) & as.character(input$dataset) != "") {
        newList[[as.character(input$dataName2)]] <- loadRData(paste0("data/", input$dataset))
      }
      datList <<- newList
      return(datList)
    })
  })
    
  output$csvType <- renderUI ({
    if (input$typeUpload == "CSV"){
      div(class = "span12",
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator',
                       c(Comma=',',
                         Semicolon=';',
                         Tab='\t'),
                       ','),
          radioButtons('quote', 'Quote',
                       c(None='',
                         'Double Quote'='"',
                         'Single Quote'="'"),
                       '"')
      )
    }
  })
  
  output$dataList <- renderUI ({
    if (length(names(assignNewDat())) == 0 & length(names(assignNewDatOldFile())) == 0){
      selectInput("currentDat", "Current Dataset:",
                  choices = c("None"))
    }
    else {
      selectInput("currentDat", "Current Dataset:",
                  choices = c(names(assignNewDat()), names(assignNewDatOldFile()), names(mergeDats())))
    }
  })
  
  initialGetData <- reactive({
    if (is.null(c(names(assignNewDat()), names(assignNewDatOldFile()), names(mergeDats())))) {
      return(NULL)
    }
    else {
      return(datList[[as.character(input$currentDat)]])
    }
  })
  
  ###########
  #Code for data merger tab
  ###########
  getData1 <- reactive({
    dat <- datList[[as.character(input$df1)]]
    return(dat)
  })
  getData2 <- reactive({
    dat <- datList[[as.character(input$df2)]]
    return(dat)
  })
  output$dataset1 <- renderUI ({
    div(class="span12",
        selectInput("df1", "Choose a dataset to merge:",
                    choices = c(names(assignNewDat()), names(assignNewDatOldFile()))),
        uiOutput("col1"))
  })
  output$col1 <- renderUI ({
    if (!is.null(input$df1)) {
      div(class="span12",
          selectizeInput("cols1", "Columns to keep from first dataset:",
                         choices = c("All", names(getData1())), multiple = T),
          selectInput("by1", "Choose the variable to merge this dataset by:",
                      choices = names(getData1()), multiple = T)
      )
    }
  })
  output$dataset2 <- renderUI ({
    div(class="span12",
        selectInput("df2", "Choose second dataset to merge:",
                    choices = c(names(assignNewDat()), names(assignNewDatOldFile()))),
        uiOutput("col2")
    )
  })
  output$col2 <- renderUI ({
    if (!is.null(input$df2)) {
      div(class="span12",
          selectizeInput("cols2", "Columns to keep from second dataset:",
                         choices = c("All", names(getData2())), multiple = T),
          selectInput("by2", "Choose the variable to merge this dataset by:",
                      choices = names(getData2()), multiple = T)
      )
    }
  })
  output$numObs1 <- renderText({
    if (!is.null(input$by1)) {
      paste("There are",length(unique(getData1()[[as.character(input$by1)]])), "unique values and",
            dim(getData1())[1], "total observations of the merge variable for this dataset")
    }
  })
  output$numObs2 <- renderText({
    if (!is.null(input$by2)) {
      paste("There are",length(unique(getData2()[[as.character(input$by2)]])), "unique values and",
            dim(getData2())[1], "total observations of the merge variable for this dataset")
    }
  })
  
  mergeDats <- reactive({
    if (input$mergeEm == 0) {
      return(NULL)
    }
    isolate({
      newList <- datList
      dat1 <- getData1()
      dat2 <- getData2()
      if ("All" %in% input$cols1) {
        dat1Sub <- dat1 #Don't change dat1
      } else {
        dat1Sub <- subset(dat1, select = c(input$cols1, input$by1)) #Select requested columns
      }
      if ("All" %in% input$cols2) {
        dat2Sub <- dat2 #Leave dat2 alone
      } else {
        dat2Sub <- subset(dat2, select = c(input$cols2, input$by2)) #Select requested columns
      }
      dat1Sub[[as.character(input$by1)]] <- as.character(dat1Sub[[as.character(input$by1)]])
      dat2Sub[[as.character(input$by2)]] <- as.character(dat2Sub[[as.character(input$by2)]])
      datFinal <- join(dat1Sub, dat2Sub, by = c(as.character(input$by1), as.character(input$by2)),
                         type = as.character(input$typeJoin), match = as.character(input$match))
      newList[[as.character(input$newDataName)]] <- datFinal
      datList <<- newList
      return(datList)
    })
  })
  
  ###########
  #Code for data examiner tab - raw Data tab
  ###########
  examinerDataRawData <- reactive({
    if (length(input$show_vars) == 0) {
      return(NULL)
    }
    tempData <- getData()[, input$show_vars, drop=FALSE]
    if (input$rmNA == "noNA") {
      #Do nothing
    }
    else if (input$rmNA == "allNA") {
      tempData <- na.omit(tempData)
    }
    else if (input$rmNA == "someNA") {
      tempData <- removeSomeNA()
    }
    return(tempData)
  })
  
  removeSomeNA <- reactive({
    tempData <- getData()[, input$show_vars, drop=FALSE]
    for (i in input$whichNAs) {
      tempData <- tempData[!is.na(tempData[toString(i)]),]
    }
    return(tempData)
  })
  
  output$particularCheckBoxes <- renderUI({
    if (input$rmNA == "someNA") {
      checkboxGroupInput("whichNAs", "Which variables would you like to remove NAs from?",
                        choices = input$show_vars)
    }
  })
  
  output$mytable <- renderDataTable({
    examinerDataRawData()
  })
  
  output$varCheckBoxes <- renderUI({
    selectizeInput('show_vars', 'Columns in data to show:',
                       choices = names(getData()), multiple=T)
  })
  
  ###########
  #Code for data examiner tab - summary statistics tab
  ###########
  output$selectizeSumStats <- renderUI({
    selectizeInput('sum_stats_vars', 'Which variables do you want stats about?',
                   choices = names(getData()), multiple=T)
  })
  
  getAllSummaryStats <- reactive({
    tempData <- getData()
    fullList <- list()
    for (i in input$sum_stats_vars) {
      singleSummary <- summary(tempData[i])
      if (length(singleSummary) == 7 | length(singleSummary) == 6) {
        . = paste0("Std.Dev:", round(sd(tempData[[i]], na.rm = T), digits = 3))
        singleSummary <- rbind(singleSummary, .)
      }
      fullList[[i]] <- singleSummary
    }
    return(fullList)
  })
  
  createSummaryStats1 <- reactive({
    if(length(input$sum_stats_vars) > 0) {
      fullList <- getAllSummaryStats()
      firstCol <- list()
      for (i in 1:length(fullList)) {
        if ((i %% 3) == 1) {
          firstCol[[names(fullList)[i]]] <- fullList[[i]]
        }
      }
      return(firstCol)
    }
    else {
      return(invisible())
    }
  })
  
  createSummaryStats2 <- reactive({
    if(length(input$sum_stats_vars) > 1) {
      fullList <- getAllSummaryStats()
      col <- list()
      for (i in 1:length(fullList)) {
        if ((i %% 3) == 2) {
          col[[names(fullList)[i]]] <- fullList[[i]]
        }
      }
      return(col)
    }
    else {
      return(invisible())
    }
  })
  
  createSummaryStats3 <- reactive({
    if(length(input$sum_stats_vars) > 2) {
      fullList <- getAllSummaryStats()
      col <- list()
      for (i in 1:length(fullList)) {
        if ((i %% 3) == 0) {
          col[[names(fullList)[i]]] <- fullList[[i]]
        }
      }
      return(col)
    }
    else {
      return(invisible())
    }
  })
  
  output$summaryStats1 <- renderPrint({
    createSummaryStats1()
  })
  output$summaryStats2 <- renderPrint({
    createSummaryStats2()
  })
  output$summaryStats3 <- renderPrint({
    createSummaryStats3()
  })
  
  ###########
  #Code for data editor
  ###########
  output$selectizeVariablesForEditing <- renderUI({
    if (input$editOptions != "Create a Variable") {
      selectizeInput('variableForEditing', 'Which variables would you like to edit?',
                     choices = names(getData()), multiple=F)
    }
  })
    
  output$imputation1 <- renderUI ({
    if (input$editOptions == "Make an Imputation") {
      box(width=12,
          selectizeInput('whatToChange', "What value in the variable would you like to change?",
                         choices = uniqueVals(), multiple=T),
          div(class = "advImputationDiv",
            textInput("changeTo", "What value would you like to impute"),
            actionButton("advImputation", "Complex Imputation", class="advImputation")
          ),
          uiOutput("advancedImputation"),
          helpText("If your variable is a factor variable, please convert it to an integer or character variable before imputation."),
          radioButtons("fromType", "What is the type of value that will be changed",
                       choices = c("Character", "Numeric", "Integer"), inline = F, selected = "Characters")
      )
    }
  })
  
  reactToAdvancedImputation <- observeEvent(input$advImputation, {
    output$advancedImputation <- renderUI({
      selectizeInput("selectizeAdvancedImputation", "Choose a method of imputation",
                     choices = c("Impute the mean", "Impute the median"))
    })
  })
  
  uniqueVals <- reactive({
    if (length(unique(dat[[as.character(input$variableForEditing)]])) < 1000) {
      return(as.character(unique(dat[[as.character(input$variableForEditing)]])))
    }
    else {
      return(as.character(unique(dat[[as.character(input$variableForEditing)]]))[1:1000])
    }
  })
  
  output$conversion1 <- renderUI ({
    if (input$editOptions == "Convert Variable Type") {
      box(width = 12,
          radioButtons("convertVarTo", "Convert Variable to type:",
                       choices = c("Character", "Integer", "Numeric", "Factor"), inline = T)
          )
    }
  })
  
  output$subset <- renderUI ({
    if (input$editOptions == "Subset Data") {
      box(width = 12,
          selectizeInput("howToSubset", "Condition to subset on:",
                         choices = c("Equal to", "Greater than", "Less than", "Between", "Not equal to"), multiple=F),
          uiOutput("subsetOn"),
          radioButtons("varType", "Type of variable being subsetted on:",
                       choices = c("Character", "Numeric", "Integer"), inline = F, selected = "Characters")
          )
    }
  })
  
  output$createVar <- renderUI ({
    if (input$editOptions == "Create a Variable") {
      div(class="span12",
          selectizeInput("howToCreate", "Method to create variable:",
                         choices = c("Add two variables", "Create categories based on other variables"), multiple = F),
          uiOutput("addVars"),
          uiOutput("createCat")
          )
    }
  })
  
  output$addVars <- renderUI ({
    if (input$howToCreate == "Add two variables") {
      div(class="span12",
          selectizeInput("firstVar", "Choose first variable:",
                         choices = names(getData()), multiple= F),
          selectizeInput("secondVar", "Choose second variable:",
                         choices = names(getData()), multiple = F),
          textInput("newVarName", "Name of the new variable:"),
          radioButtons("typeOfNewVar", "Type of new variable:",
                       choices = c("Character", "Numeric", "Integer"))
          )
      
    }
  })
  
  output$createCat <- renderUI ({
    if (input$howToCreate == "Create categories based on other variables") {

    }
  })
  
  output$changeVarName <- renderUI ({
    if (input$editOptions == "Change a variable name") {
      textInput("newName", "New name for selected variable:")
    }
  })
  
  output$subsetOn <- renderUI ({
    if (input$howToSubset == "Between") {
      div(
          textInput("firstVal", "From:"),
          textInput("secondVal", "To:")
      )
    }
    else {
      textInput("valueToSubsetOn", "Value to subset on:")
    }
  })
  
  observeEvent(input$previewSub, {
    previewEdit()
  })
  
  previewEdit <- function() {
    output$dataBeforeEditing <- renderPrint({
      test <- getData()
      randomRows <- sample(dim(test)[1], 100)
      test[[input$variableForEditing]][randomRows]
    })
    output$dataAfterEditing <- renderPrint({
      j <- input$editOptions[1]
      i <- input$variableForEditing[1]
      dat <- datList[[as.character(input$currentDat)]]
      if (j == "Remove NAs") {
        dat <- datList[[as.character(input$currentDat)]][!is.na(datList[[as.character(input$currentDat)]][toString(i)]),]
      }
      else if (j == "Center Variable") {
        dat[,as.character(i)] <- datList[[as.character(input$currentDat)]][,as.character(i)] - mean(datList[[as.character(input$currentDat)]][,as.character(i)], na.rm = T)
      }
      else if (j == "Scale Variable") {
        dat[,as.character(i)] <- datList[[as.character(input$currentDat)]][,as.character(i)]/sd(datList[[as.character(input$currentDat)]][,as.character(i)], na.rm = T)
      }
      else if (j == "Convert Variable Type") {
        if (input$convertVarTo == "Character") {
          dat[,as.character(i)] <- as.character(datList[[as.character(input$currentDat)]][,as.character(i)])
        }
        else if (input$convertVarTo == "Integer") {
          dat[,as.character(i)] <- as.integer(datList[[as.character(input$currentDat)]][,as.character(i)])
        }
        else if (input$convertVarTo == "Numeric") {
          dat[,as.character(i)] <- as.numeric(datList[[as.character(input$currentDat)]][,as.character(i)])
        }
        else if (input$convertVarTo == "Factor") {
          dat[,as.character(i)] <- as.factor(datList[[as.character(input$currentDat)]][,as.character(i)])
        }
      }
      else if (j == "Change to Lowercase") {
        if (typeof(dat[,as.character(i)][1]) == "character") {
          dat[,as.character(i)] <- tolower(datList[[as.character(input$currentDat)]][,as.character(i)])
        }
        else {
          #Do nothing
        }
      }
      else if (j == "Change to Uppercase") {
        if (typeof(dat[,as.character(i)][1]) == "character") {
          dat[,as.character(i)] <- toupper(datList[[as.character(input$currentDat)]][,as.character(i)])
        }
        else {
          #Do nothing
        }
      }
      else if (j == "Make an Imputation") {
        if (input$whatToChange == "NA") {
          if (input$selectizeAdvancedImputation == "Impute the mean") {
            dat[is.na(datList[[as.character(input$currentDat)]][,as.character(i)])] <- mean(datList[[as.character(input$currentDat)]][,as.character(i)], na.rm = T)
          }
          else if (input$selectizeAdvancedImputation == "Impute the median") {
            dat[is.na(datList[[as.character(input$currentDat)]][,as.character(i)])] <- median(datList[[as.character(input$currentDat)]][,as.character(i)], na.rm = T)
          }
          else if (input$fromType == "Numeric") {
            dat[is.na(datList[[as.character(input$currentDat)]][,as.character(i)])] <- as.numeric(input$changeTo)
          }
          else if (input$fromType == "Character") {
            dat[is.na(datList[[as.character(input$currentDat)]][,as.character(i)])] <- as.character(input$changeTo)
          }
          else if (input$fromType == "Integer") {
            dat[is.na(datList[[as.character(input$currentDat)]][,as.character(i)])] <- as.integer(input$changeTo)
          }
        }
        else if (input$selectizeAdvancedImputation == "Impute the mean") {
          dat[datList[[as.character(input$currentDat)]][,as.character(i)] == as.integer(input$whatToChange)] <- mean(datList[[as.character(input$currentDat)]][,as.character(i)], na.rm = T)
        }
        else if (input$selectizeAdvancedImputation == "Impute the median") {
          dat[datList[[as.character(input$currentDat)]][,as.character(i)] == as.integer(input$whatToChange)] <- median(datList[[as.character(input$currentDat)]][,as.character(i)], na.rm = T)
        }
        else if (input$fromType == "Numeric") {
          dat[datList[[as.character(input$currentDat)]][,as.character(i)] == as.numeric(input$whatToChange)] <- as.numeric(input$changeTo)
        }
        else if (input$fromType == "Character") {
          dat[datList[[as.character(input$currentDat)]][,as.character(i)] == as.character(input$whatToChange)] <- as.character(input$changeTo)
        }
        else if (input$fromType == "Integer") {
          dat[datList[[as.character(input$currentDat)]][,as.character(i)] == as.integer(input$whatToChange)] <- as.integer(input$changeTo)
        }
      }
      else if (j == "Subset Data") {
        if (input$howToSubset == "Equal to") {
          if (input$varType == "Character") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] == as.character(input$valueToSubsetOn))
          }
          else if (input$varType == "Numeric") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] == as.numeric(input$valueToSubsetOn))
          }
          else if (input$varType == "Integer") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] == as.integer(input$valueToSubsetOn))
          }
        }
        else if (input$howToSubset == "Greater than") {
          if (input$varType == "Character") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] > as.character(input$valueToSubsetOn))
          }
          else if (input$varType == "Numeric") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] > as.numeric(input$valueToSubsetOn))
          }
          else if (input$varType == "Integer") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] > as.integer(input$valueToSubsetOn))
          }
        }
        else if (input$howToSubset == "Less than") {
          if (input$varType == "Character") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] < as.character(input$valueToSubsetOn))
          }
          else if (input$varType == "Numeric") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] < as.numeric(input$valueToSubsetOn))
          }
          else if (input$varType == "Integer") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] < as.integer(input$valueToSubsetOn))
          }
        }
        else if (input$howToSubset == "Between") {
          if (input$varType == "Character") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] < as.character(input$secondVal) & datList[[as.character(input$currentDat)]][,as.character(i)] > as.character(input$firstVal))
          }
          else if (input$varType == "Numeric") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] < as.numeric(input$secondVal) & datList[[as.character(input$currentDat)]][,as.character(i)] > as.numeric(input$firstVal))
          }
          else if (input$varType == "Integer") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] < as.integer(input$secondVal) & datList[[as.character(input$currentDat)]][,as.character(i)] > as.integer(input$firstVal))
          }
        }
        else if (input$howToSubset == "Not equal to") {
          if (input$varType == "Character") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] != as.character(input$valueToSubsetOn))
          }
          else if (input$varType == "Numeric") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] != as.numeric(input$valueToSubsetOn))
          }
          else if (input$varType == "Integer") {
            dat <- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] != as.integer(input$valueToSubsetOn))
          }
        }
      }
      else if (j == "Create a Variable") {
        if (input$howToCreate == "Add two variables") {
          if (input$typeOfNewVar == "Numeric") {
            dat[[input$newVarName]] <- as.numeric(datList[[as.character(input$currentDat)]][[input$firstVar]]) + as.numeric(datList[[as.character(input$currentDat)]][[input$secondVar]])
          }
          else if (input$typeOfNewVar == "Character") {
            dat[[input$newVarName]] <- paste0((datList[[as.character(input$currentDat)]][[input$firstVar]]), (datList[[as.character(input$currentDat)]][[input$secondVar]]))
          }
          else if (input$typeOfNewVar == "Integer") {
            dat[[input$newVarName]] <- as.integer(as.numeric(datList[[as.character(input$currentDat)]][[input$firstVar]]) + as.integer(datList[[as.character(input$currentDat)]][[input$secondVar]]))
          }
        }
      }
      if (j != "Create a Variable") {
        randomRows <- sample(dim(dat)[1], 100)
        dat[[input$variableForEditing]][randomRows]
      }
      else {
        randomRows <- sample(dim(dat)[1], 100)
        dat[[input$newVarName]][randomRows]
      }
    })
  }
  
  getData <- reactive({
    if (input$submitEdits == 0) {
      datList[[as.character(input$currentDat)]] <<- initialGetData()
      return(datList[[as.character(input$currentDat)]])
    }
    isolate({
      for (i in input$variableForEditing) {
        for (j in input$editOptions) {
          if (j == "Remove NAs") {
            datList[[as.character(input$currentDat)]] <<- datList[[as.character(input$currentDat)]][!is.na(datList[[as.character(input$currentDat)]][,as.character(i)]),]
          }
          else if (j == "Center Variable") {
            datList[[as.character(input$currentDat)]][,as.character(i)] <<- datList[[as.character(input$currentDat)]][,as.character(i)] - mean(datList[[as.character(input$currentDat)]][,as.character(i)], na.rm = T)
          }
          else if (j == "Scale Variable") {
            datList[[as.character(input$currentDat)]][,as.character(i)] <<- datList[[as.character(input$currentDat)]][,as.character(i)]/sd(datList[[as.character(input$currentDat)]][,as.character(i)], na.rm = T)
          }
          else if (j == "Convert Variable Type") {
            if (input$convertVarTo == "Character") {
              datList[[as.character(input$currentDat)]][,as.character(i)] <<- as.character(datList[[as.character(input$currentDat)]][,as.character(i)])
            }
            else if (input$convertVarTo == "Integer") {
              datList[[as.character(input$currentDat)]][,as.character(i)] <<- as.integer(datList[[as.character(input$currentDat)]])
            }
            else if (input$convertVarTo == "Numeric") {
              datList[[as.character(input$currentDat)]][,as.character(i)] <<- as.numeric(datList[[as.character(input$currentDat)]][,as.character(i)])
            }
            else if (input$convertVarTo == "Factor") {
              datList[[as.character(input$currentDat)]][,as.character(i)] <<- as.factor(datList[[as.character(input$currentDat)]][,as.character(i)])
            }
          }
          else if (j == "Change to Lowercase") {
            if (typeof(dat[,as.character(i)][1]) == "character") {
              datList[[as.character(input$currentDat)]][,as.character(i)] <<- tolower(datList[[as.character(input$currentDat)]][,as.character(i)])
            }
            else {
              #Do nothing
            }
          }
          else if (j == "Change to Uppercase") {
            if (typeof(dat[,as.character(i)][1]) == "character") {
              datList[[as.character(input$currentDat)]][,as.character(i)] <<- toupper(datList[[as.character(input$currentDat)]][,as.character(i)])
            }
            else {
              #Do nothing
            }
          }
          else if (j == "Make an Imputation") {
            if (input$whatToChange == "NA") {
              if (input$selectizeAdvancedImputation == "Impute the mean") {
                datList[[as.character(input$currentDat)]][,as.character(i)][is.na(datList[[as.character(input$currentDat)]][,as.character(i)])] <<- mean(datList[[as.character(input$currentDat)]][,as.character(i)], na.rm = T)
              }
              else if (input$selectizeAdvancedImputation == "Impute the median") {
                datList[[as.character(input$currentDat)]][,as.character(i)][is.na(datList[[as.character(input$currentDat)]][,as.character(i)])] <<- median(datList[[as.character(input$currentDat)]][,as.character(i)], na.rm = T)
              }
              else if (input$fromType == "Numeric") {
                datList[[as.character(input$currentDat)]][,as.character(i)][is.na(datList[[as.character(input$currentDat)]][,as.character(i)])] <<- as.numeric(input$changeTo)
              }
              else if (input$fromType == "Character") {
                datList[[as.character(input$currentDat)]][,as.character(i)][is.na(datList[[as.character(input$currentDat)]][,as.character(i)])] <<- as.character(input$changeTo)
              }
              else if (input$fromType == "Integer") {
                datList[[as.character(input$currentDat)]][,as.character(i)][is.na(datList[[as.character(input$currentDat)]][,as.character(i)])] <<- as.integer(input$changeTo)
              }
            }
            else if (input$selectizeAdvancedImputation == "Impute the mean") {
              datList[[as.character(input$currentDat)]][,as.character(i)][datList[[as.character(input$currentDat)]][,as.character(i)] == as.integer(input$whatToChange)] <<- mean(datList[[as.character(input$currentDat)]], na.rm = T)
            }
            else if (input$selectizeAdvancedImputation == "Impute the median") {
              datList[[as.character(input$currentDat)]][,as.character(i)][datList[[as.character(input$currentDat)]][,as.character(i)] == as.integer(input$whatToChange)] <<- median(datList[[as.character(input$currentDat)]], na.rm = T)
            }
            else if (input$fromType == "Numeric") {
              datList[[as.character(input$currentDat)]][,as.character(i)][datList[[as.character(input$currentDat)]][,as.character(i)] == as.numeric(input$whatToChange)] <<- as.numeric(input$changeTo)
            }
            else if (input$fromType == "Character") {
              datList[[as.character(input$currentDat)]][,as.character(i)][datList[[as.character(input$currentDat)]][,as.character(i)] == as.character(input$whatToChange)] <<- as.character(input$changeTo)
            }
            else if (input$fromType == "Integer") {
              datList[[as.character(input$currentDat)]][datList[[as.character(input$currentDat)]][,as.character(i)] == as.integer(input$whatToChange)] <<- as.integer(input$changeTo)
            }
          }
          else if (j == "Subset Data") {
            if (input$howToSubset == "Equal to") {
              if (input$varType == "Character") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] == as.character(input$valueToSubsetOn))
              }
              else if (input$varType == "Numeric") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] == as.numeric(input$valueToSubsetOn))
              }
              else if (input$varType == "Integer") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] == as.integer(input$valueToSubsetOn))
              }
            }
            else if (input$howToSubset == "Greater than") {
              if (input$varType == "Character") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] > as.character(input$valueToSubsetOn))
              }
              else if (input$varType == "Numeric") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] > as.numeric(input$valueToSubsetOn))
              }
              else if (input$varType == "Integer") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] > as.integer(input$valueToSubsetOn))
              }
            }
            else if (input$howToSubset == "Less than") {
              if (input$varType == "Character") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] < as.character(input$valueToSubsetOn))
              }
              else if (input$varType == "Numeric") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] < as.numeric(input$valueToSubsetOn))
              }
              else if (input$varType == "Integer") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] < as.integer(input$valueToSubsetOn))
              }
            }
            else if (input$howToSubset == "Between") {
              if (input$varType == "Character") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] < as.character(input$secondVal) & datList[[as.character(input$currentDat)]][,as.character(i)] > as.character(input$firstVal))
              }
              else if (input$varType == "Numeric") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] < as.numeric(input$secondVal) & datList[[as.character(input$currentDat)]][,as.character(i)] > as.numeric(input$firstVal))
              }
              else if (input$varType == "Integer") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] < as.integer(input$secondVal) & datList[[as.character(input$currentDat)]][,as.character(i)] > as.integer(input$firstVal))
              }
            }
            else if (input$howToSubset == "Not equal to") {
              if (input$varType == "Character") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] != as.character(input$valueToSubsetOn))
              }
              else if (input$varType == "Numeric") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] != as.numeric(input$valueToSubsetOn))
              }
              else if (input$varType == "Integer") {
                datList[[as.character(input$currentDat)]] <<- subset(dat, datList[[as.character(input$currentDat)]][,as.character(i)] != as.integer(input$valueToSubsetOn))
              }
            }
          }
          else if (j == "Create a Variable") {
            if (input$howToCreate == "Add two variables") {
              if (input$typeOfNewVar == "Numeric") {
                datList[[as.character(input$currentDat)]][[input$newVarName]] <<- as.numeric(datList[[as.character(input$currentDat)]][[input$firstVar]]) + as.numeric(datList[[as.character(input$currentDat)]][[input$secondVar]])
              }
              else if (input$typeOfNewVar == "Character") {
                datList[[as.character(input$currentDat)]][[input$newVarName]] <<- paste0((datList[[as.character(input$currentDat)]][[input$firstVar]]), (datList[[as.character(input$currentDat)]][[input$secondVar]]))
              }
              else if (input$typeOfNewVar == "Integer") {
                datList[[as.character(input$currentDat)]][[input$newVarName]] <<- as.integer(as.numeric(datList[[as.character(input$currentDat)]][[input$firstVar]]) + as.integer(datList[[as.character(input$currentDat)]][[input$secondVar]]))
              }
            }
          }
          else if (j == "Change a variable name") {
            colnames(datList[[as.character(input$currentDat)]])[names(datList[[as.character(input$currentDat)]]) == as.character(i)] <<- as.character(input$newName)
          }
        }
      }
      return(datList[[as.character(input$currentDat)]])
    })
  })
  
  ###########
  #Code for data visualizer
  ###########
  
  #Set reactive values for zoom:
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  #Add zooming capabilities:
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  #Plotting functions:
  output$yAxisSelect <- renderUI ({
    selectizeInput("yAxisVar", "Select the Y-Axis Variable",
                   choices = c("None",names(getData())), multiple = F)
  })
  output$xAxisSelect <- renderUI ({
    selectizeInput("xAxisVar", "Select the X-Axis Variable",
                   choices = names(getData()), multiple = F)
  })
  
#   "Add color to plot output", "Color by a Variable",
#   "Group by a Variable", "Split plot by a variable",
#   "Add Title", "Change X-axis label", "Change Y-axis label",
#   "Change legend title", "Change background color",
#   "Remove legend", "Move legend to bottom", "Change X-axis limits",
#   "Change Y-axis limits", "Fill by a Variable"
  ####
  #Overall options
  ####
  output$colorVar <- renderUI ({
    if ("Color by a Variable" %in% input$graphOptions) {
      selectizeInput("colorBy", "Variable to color by:",
                     choices = c("None",names(getData())), multiple = F)
    }
  })
  output$fillVar <- renderUI ({
    if ("Fill by a Variable" %in% input$graphOptions) {
      selectizeInput("fillBy", "Variable to fill by:",
                     choices = c("None",names(getData())), multiple = F)
    }
  })
  output$addColor <- renderUI ({
    if ("Add color to plot output" %in% input$graphOptions) {
      textInput("colorAdd", "Name of color to add:")
    }
  })
  output$facetPlot <- renderUI ({
    if ("Split plot by a variable" %in% input$graphOptions) {
      selectizeInput("facetBy", "Variable to split plots by:",
                     choices = c("None",names(getData())), multiple = F)
    }
  })
  output$titleInput <- renderUI ({
    if ("Add Title" %in% input$graphOptions) {
      textInput("titleToAdd", "Enter the title of your graph:")
    }
  })
  output$changeXAxisLabel <- renderUI ({
    if ("Change X-axis label" %in% input$graphOptions) {
      textInput("newXAxisLabel", "New X-Axis label:")
    }
  })
  output$changeYAxisLabel <- renderUI ({
    if ("Change Y-axis label" %in% input$graphOptions) {
      textInput("newYAxisLabel", "New Y-Axis label:")
    }
  })
  output$changeLegendTitle <- renderUI ({
    if ("Change legend title" %in% input$graphOptions) {
      textInput("newLegendTitle", "New legend title:")
    }
  })
  output$changeBackgroundColor <- renderUI ({
    if("Change background color" %in% input$graphOptions) {
      textInput("plotBackgroundColor", "New background color:")
    }
  })
  output$changeXAxisLimit <- renderUI ({
    dat <- datList[[as.character(input$currentDat)]]
    if ("Change X-axis limits" %in% input$graphOptions) {
      sliderInput("newXLimits", "New X-Axis Limits:", min = min(dat[[as.character(input$xAxisVar)]], na.rm = T),
                  max = max(dat[[as.character(input$xAxisVar)]], na.rm = T), value = c(min(dat[[as.character(input$xAxisVar)]], na.rm = T),
                                                                                       max(dat[[as.character(input$xAxisVar)]], na.rm = T)))
    }
  })
  output$changeYAxisLimit <- renderUI ({
    dat <- datList[[as.character(input$currentDat)]]
    if ("Change Y-axis limits" %in% input$graphOptions) {
      sliderInput("newYLimits", "New Y-Axis Limits:", min = min(dat[[as.character(input$yAxisVar)]], na.rm = T),
                  max = max(dat[[as.character(input$yAxisVar)]], na.rm = T), value = c(min(dat[[as.character(input$yAxisVar)]], na.rm = T),
                                                                                       max(dat[[as.character(input$yAxisVar)]], na.rm = T)))
    }
  })
  
  ####
  #Specific types of plot options
  ####
  output$barPlotOptions <- renderUI ({
    if (input$typeOfPlot == "Bar Plot" & input$factorVar == "No") {
      box(width=12,
          title="Bar Plot Options",
          selectizeInput("position", "Position Options:",
                         choices = c("Stacked", "Side by Side")),
          textInput("binWid", "Width of each bin:")
      )
    }
    else if (input$typeOfPlot == "Bar Plot" & input$factorVar == "Yes") {
      box(width=12,
          title="Bar Plot Options",
          selectizeInput("position", "Position Options:",
                         choices = c("Stacked", "Side by Side"))
      )
    }
  })
  output$scatterPlotOptions <- renderUI ({
    if (input$typeOfPlot == "Scatter Plot") {
      box(width=12,
          title="Scatter Plot Options",
          checkboxInput("addTrans", "Add Transparency"),
          uiOutput("transparencyScale")
      )
    }
  })
  output$transparencyScale <- renderUI ({
    if (input$addTrans == T) {
      sliderInput("amountTransparency", "What % transparency?",
                  min = 0, max = 1, value = 0, step = .01, ticks = F)
    }
  })
  
  #Plot generation function
  plotMaker <- reactive({
    if (input$createPlot == 0) {
      return(NULL)
    }
    isolate ({
      dat <- datList[[as.character(input$currentDat)]]
      if (input$yAxisVar == "None") {
        if (input$factorVar == "Yes") {
          dat[[as.character(input$xAxisVar)]] <- as.factor(dat[[as.character(input$xAxisVar)]])
          plot = ggplot(data=dat,
                        aes_string(x=input$xAxisVar))
        }
        else {
          plot = ggplot(data=dat,
                        aes_string(x=input$xAxisVar))
        }
      }
      else {
        if (input$factorVar == "Yes") {
          dat[[as.character(input$xAxisVar)]] <- as.factor(dat[[as.character(input$xAxisVar)]])
          plot = ggplot(data=dat,
                        aes_string(x=input$xAxisVar, y=input$yAxisVar))
        }
        else {
          plot = ggplot(data=dat,
                        aes_string(x=input$xAxisVar, y=input$yAxisVar))
        }
      }
      if (input$typeOfPlot == "Scatter Plot") {
        if (input$addTrans == T) {
          if ("Add color to plot output" %in% input$graphOptions) {
            if (input$colorAdd != "") {
              plot = plot + geom_point(color = as.character(input$colorAdd), alpha = 1 - input$amountTransparency)
            }
          }
          else {
            plot = plot + geom_point(alpha = 1 - input$amountTransparency)
          }
        }
        else {
          if ("Add color to plot output" %in% input$graphOptions) {
            if (input$colorAdd != "") {
              plot = plot + geom_point(color = as.character(input$colorAdd))
            }
          }
          else {
            plot = plot + geom_point()
          }
        }
      }
      else if (input$typeOfPlot == "Bar Plot") {
        if (!is.null(input$binWid) & input$binWid != "") {
          if (input$yAxisVar != "None") {
            if ("Add color to plot output" %in% input$graphOptions) {
              if (input$colorAdd != "") {
                if (input$position == "Stacked") {
                  plot = plot + geom_bar(color = as.character(input$colorAdd), position = "stack", stat="identity", binwidth = as.numeric(input$binWid))
                }
                else if (input$position == "Side by Side") {
                  plot = plot + geom_bar(color = as.character(input$colorAdd), position = "dodge", stat="identity", binwidth = as.numeric(input$binWid))
                }
                else {
                  plot = plot + geom_bar(color = as.character(input$colorAdd), stat="identity", binwidth = as.numeric(input$binWid))
                }
              }
            }
            else {
              if (input$position == "Stacked") {
                plot = plot + geom_bar(position = "stack", stat="identity", binwidth = as.numeric(input$binWid))
              }
              else if (input$position == "Side by Side") {
                plot = plot + geom_bar(position = "dodge", stat="identity", binwidth = as.numeric(input$binWid))
              }
              else {
                plot = plot + geom_bar(stat="identity", binwidth = as.numeric(input$binWid))
              }
            }
          }
          else {
            if ("Add color to plot output" %in% input$graphOptions) {
              if (input$colorAdd != "") {
                if (input$position == "Stacked") {
                  plot = plot + geom_bar(color = as.character(input$colorAdd), position = "stack", binwidth = as.numeric(input$binWid))
                }
                else if (input$position == "Side by Side") {
                  plot = plot + geom_bar(color = as.character(input$colorAdd), position = "dodge", binwidth = as.numeric(input$binWid))
                }
                else {
                  plot = plot + geom_bar(color = as.character(input$colorAdd), binwidth = as.numeric(input$binWid))
                }
              }
            }
            else {
              if (input$position == "Stacked") {
                plot = plot + geom_bar(position = "stack", binwidth = as.numeric(input$binWid))
              }
              else if (input$position == "Side by Side") {
                plot = plot + geom_bar(position = "dodge", binwidth = as.numeric(input$binWid))
              }
              else {
                plot = plot + geom_bar(binwidth = as.numeric(input$binWid))
              }
            }
          }
        }
        else {
          if (input$yAxisVar != "None") {
            if ("Add color to plot output" %in% input$graphOptions) {
              if (input$colorAdd != "") {
                if (input$position == "Stacked") {
                  plot = plot + geom_bar(color = as.character(input$colorAdd), position = "stack", stat="identity")
                }
                else if (input$position == "Side by Side") {
                  plot = plot + geom_bar(color = as.character(input$colorAdd), position = "dodge", stat="identity")
                }
                else {
                  plot = plot + geom_bar(color = as.character(input$colorAdd), stat="identity")
                }
              }
            }
            else {
              if (input$position == "Stacked") {
                plot = plot + geom_bar(position = "stack", stat="identity")
              }
              else if (input$position == "Side by Side") {
                plot = plot + geom_bar(position = "dodge", stat="identity")
              }
              else {
                plot = plot + geom_bar(stat="identity")
              }
            }
          }
          else {
            if ("Add color to plot output" %in% input$graphOptions) {
              if (input$colorAdd != "") {
                if (input$position == "Stacked") {
                  plot = plot + geom_bar(color = as.character(input$colorAdd), position = "stack")
                }
                else if (input$position == "Side by Side") {
                  plot = plot + geom_bar(color = as.character(input$colorAdd), position = "dodge")
                }
                else {
                  plot = plot + geom_bar(color = as.character(input$colorAdd))
                }
              }
            }
            else {
              if (input$position == "Stacked") {
                plot = plot + geom_bar(position = "stack")
              }
              else if (input$position == "Side by Side") {
                plot = plot + geom_bar(position = "dodge")
              }
              else {
                plot = plot + geom_bar()
              }
            }
          }
        }
      }
      else if (input$typeOfPlot == "Box Plot") {
        if ("Add color to plot output" %in% input$graphOptions) {
          if (input$colorAdd != "") {
            plot = plot + geom_boxplot(color = as.character(input$colorAdd))
          }
        }
        else {
          plot = plot + geom_boxplot()
        }
      }
      else if (input$typeOfPlot == "Violin Plot") {
        if ("Add color to plot output" %in% input$graphOptions) {
          if (input$colorAdd != "") {
            plot = plot + geom_violin(color = as.character(input$colorAdd))
          }
        }
        else {
          plot = plot + geom_violin()
        }
      }
      else if (input$typeOfPlot == "Density Plot") {
        if ("Add color to plot output" %in% input$graphOptions) {
          if (input$colorAdd != "") {
            plot = plot + geom_density(color = as.character(input$colorAdd))
          }
        }
        else {
          plot = plot + geom_density()
        }
      }
      else if (input$typeOfPlot == "Jitter Plot") {
        if ("Add color to plot output" %in% input$graphOptions) {
          if (input$colorAdd != "") {
            plot = plot + geom_jitter(color = as.character(input$colorAdd))
          }
        }
        else {
          plot = plot + geom_jitter()
        }
      }
      else if (input$typeOfPlot == "Line Plot") {
        if ("Add color to plot output" %in% input$graphOptions) {
          if (input$colorAdd != "") {
            plot = plot + geom_line(color = as.character(input$colorAdd))
          }
        }
        else {
          plot = plot + geom_line()
        }
      }
      else if (input$typeOfPlot == "Hex Plot") {
        if ("Add color to plot output" %in% input$graphOptions) {
          if (input$colorAdd != "") {
            plot = plot + geom_hex(color = as.character(input$colorAdd))
          }
        }
        else {
          plot = plot + geom_hex()
        }
      }
      if ("Color by a Variable" %in% input$graphOptions) {
        if (input$colorBy != "None") {
          plot = plot + aes_string(color = input$colorBy)
        }
      }
      if ("Fill by a Variable" %in% input$graphOptions) {
        if (input$fillBy != "None") {
          plot = plot + aes_string(fill = input$fillBy)
        }
      }
      if ("Split plot by a variable" %in% input$graphOptions) {
        if (input$facetBy != "None") {
          plot = plot + facet_wrap(as.formula(paste("~", as.character(input$facetBy))))
        }
      }
      if ("Add Title" %in% input$graphOptions) {
        if (input$titleToAdd != "") {
          plot = plot + ggtitle(as.character(input$titleToAdd))
        }
      }
      if ("Change X-axis label" %in% input$graphOptions) {
        if (input$newXAxisLabel != "") {
          plot = plot + xlab(as.character(input$newXAxisLabel))
        }
      }
      if ("Change Y-axis label" %in% input$graphOptions) {
        if (input$newYAxisLabel != "") {
          plot = plot + ylab(as.character(input$newYAxisLabel))
        }
      }
      if ("Change legend title" %in% input$graphOptions) {
        if (input$newLegendTitle != "") {
          if ("Color by a Variable" %in% input$graphOptions) {
            plot = plot + labs(color=as.character(input$newLegendTitle))
          }
          else if ("Fill by a Variable" %in% input$graphOptions) {
            plot = plot + labs(fill=as.character(input$newLegendTitle))
          }
        }
      }
      if ("Change background color" %in% input$graphOptions) {
        if (input$plotBackgroundColor != "") {
          plot = plot + theme(panel.background = element_rect(fill=as.character(input$plotBackgroundColor)))
        }
      } 
      if ("Remove legend" %in% input$graphOptions) {
        plot = plot + theme(legend.position = "none")
      }
      if ("Move legend to bottom" %in% input$graphOptions) {
        plot = plot + theme(legend.position = "bottom")
      }
      if ("Change Y-axis limits" %in% input$graphOptions) {
        plot = plot + scale_y_continuous(limits = c(input$newYLimits[1], input$newYLimits[2]))
      }
      if ("Change X-axis limits" %in% input$graphOptions) {
        plot = plot + scale_x_continuous(limits = c(input$newXLimits[1], input$newXLimits[2]))
      }
      return(plot)
    })
  })
  
  #Validate plot is valid
  testPlot <- reactive({
    validate(
      need(try(print(plotMaker())), "Sorry, this plot is either invalid or isn't currently supported.")
    )
  })
  
  #Plot printer:
  output$plotOut <- renderPlot ({
    print(plotMaker())
  })
  #End of create plot function
  
  #Code to save plot:
  output$downloadPlot <- downloadHandler(
    filename = function() {paste0(input$plotName, ".pdf")},
    content = function(con) {
      pdf(file = con)
      print(plotMaker())
      dev.off(which=dev.cur())
    }
  )
  
  ####
  #Live ggplot tab in data visualizer
  ####
  # Get plot code from the aceEditor input object, and remove line breaks from it.
  plotCode <- reactive({
    input$plotCode
  })        
  
  # Create a plot object from the code in plotCode()
  plotObject <- reactive({
    plotNo <- input$plotButton
    isolate(eval(parse(text = gsub("\\n", "", plotCode()))))
  })
  
  # Include the printed plot in the output list.
  output$plotOut2 <- renderPlot({
    print(plotObject())
  })
  
  ########
  #Model builder tab
  ########
  
  #Linear Regression Tab
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
  
  #Logistic Regression Tab
  output$logReg <- renderUI ({
    box(width = 12,
        selectizeInput("logRegPredictedVar", "Variable to Predict:",
                       choices = names(getData())),
        selectizeInput("logRegPredictorVar", "Variables to Use in Model:",
                       choices = names(getData()), multiple = T))
  })
  
  createOutputLogReg <- reactive({
    if (input$buildModelLogReg == 0) {
      return(NULL)
    }
    isolate({
      logRegModel <- glm(as.formula(paste0(input$logRegPredictedVar,"~", paste0(input$logRegPredictorVar, collapse = "+"))), data = getData(), family=binomial)
      return(summary(logRegModel))
    })
  })
  
  output$logRegModelOutput <- renderPrint ({
    print(createOutputLogReg())
  })
  
  #Kmeans tab
  output$kmeansChoice <- renderUI ({
    selectizeInput("kmeansChoiceMade", "Options for clustering:",
                   choices = c("Choose number of clusters",
                               "Find optimal number of clusters in a range"))
  })
  output$kmeans <- renderUI ({
    if (input$kmeansChoiceMade == "Choose number of clusters") {
      box(width = 12,
          selectizeInput("kmeansVars", "Variables to Use in Cluster Analysis:",
                         choices = names(getData()), multiple = T),
          textInput("numClust", "Number of clusters:"),
          helpText("Please be aware that this operation can take some time with high numbers of clusters"),
          actionButton("cluster", "Cluster!")
      )
    }
    else if (input$kmeansChoiceMade == "Find optimal number of clusters in a range"){
      box(width = 12,
          selectizeInput("kmeansVars", "Variables to Use in Cluster Analysis:",
                         choices = names(getData()), multiple = T),
          textInput("maxClust", "Maximum Number of Clusters:"),
          helpText("Please be aware that this operation can take some time with high numbers of clusters"),
          actionButton("cluster2", "Cluster!")
      )
    }
  })
  
  output$kmeansOutput <- renderUI ({
    ret <- performClustering()
    box(width = 12, 
        p(ret[[1]]),
        p(ret[[2]]),
        p(ret[[3]])
    )
  })
  
  performClustering <- reactive ({
    if (input$cluster == 0 | input$cluster2 == 0) {
      return(NULL)
    }
    isolate ({
      if (input$kmeansChoiceMade == "Choose number of clusters") {
        dat <- makeClusterData()
        sigDat <- invisible(testClustering(dat, as.integer(input$numClust)))
        if (sigDat[[1]] == "good") {
          datList[[as.character(input$currentDat)]] <<- sigDat[[2]]
          sent1 <- paste("Clustering was a success! The number of clusters used was", sigDat[[3]])
          sent2 <- paste("The Jaccard similarity values for your clusters were:", paste(sigDat[[4]], collapse = ", "))
          sent3 <- paste("These values indicate that these clusters appear to hold significance and can be used for analysis. The cluster values have been added to the dataset.")
        }
        else if (sigDat[[1]] == "ok") {
          datList[[as.character(input$currentDat)]] <<- sigDat[[2]]
          sent1 <- paste("Clustering went ok. The number of clusters used was", sigDat[[3]])
          sent2 <- paste("The Jaccard similarity values for your clusters were:", paste(sigDat[[4]], collapse = ", "))
          sent3 <- paste("These values indicate that these clusters show some information, but should be used with caution. The cluster values have been added to the dataset.")
        }
        else if (sigDat[[1]] == "bad") {
          sent1 <- paste("Clustering went poorly. The number of clusters used was", sigDat[[3]])
          sent2 <- paste("The Jaccard similarity values for your clusters were:", paste(sigDat[[4]], collapse = ", "))
          sent3 <- paste("These values indicate that these clusters do not hold any significant information and should not be used for analysis. The cluster values have not been added to the dataset.")
        }
        return(list(sent1, sent2, sent3))
      }
      else if (input$kmeansChoiceMade == "Find optimal number of clusters in a range") {
        dat <- makeClusterData()
        numClust <- optimalCluster(dat, as.integer(input$maxClust))
        sigDat <- invisible(testClustering(dat, as.integer(numClust)))
        if (sigDat[[1]] == "good") {
          datList[[as.character(input$currentDat)]] <<- sigDat[[2]]
          sent1 <- paste("Clustering was a success! The number of clusters used was", sigDat[[3]])
          sent2 <- paste("The Jaccard similarity values for your clusters were:", paste(sigDat[[4]], collapse = ", "))
          sent3 <- paste("These values indicate that these clusters appear to hold significance and can be used for analysis. The cluster values have been added to the dataset.")
        }
        else if (sigDat[[1]] == "ok") {
          datList[[as.character(input$currentDat)]] <<- sigDat[[2]]
          sent1 <- paste("Clustering went ok. The number of clusters used was", sigDat[[3]])
          sent2 <- paste("The Jaccard similarity values for your clusters were:", paste(sigDat[[4]], collapse = ", "))
          sent3 <- paste("These values indicate that these clusters show some information, but should be used with caution. The cluster values have been added to the dataset.")
        }
        else if (sigDat[[1]] == "bad") {
          sent1 <- paste("Clustering went poorly. The number of clusters used was", sigDat[[3]])
          sent2 <- paste("The Jaccard similarity values for your clusters were:", paste(sigDat[[4]], collapse = ", "))
          sent3 <- paste("These values indicate that these clusters do not hold any significant information and should not be used for analysis. The cluster values have not been added to the dataset.")
        }
        return(list(sent1, sent2, sent3))
      }
    })
  })
  
  makeClusterData <- reactive ({
    dat <- getData()
    retDat <- subset(dat, select = c(input$kmeansVars))
    retDat <- na.omit(retDat)
    return(retDat)
  })
  
  optimalCluster <- function(dataset, maxClust) {
    #Run kmeansruns with max cluster arg and find best cluster
    kMeansRunsOut = kmeansruns(dataset, krange = 2:as.integer(maxClust))
    bestClust = kMeansRunsOut$bestk
    return(bestClust)
  }
  
  testClustering <- function(dataset, numClust) {
    #With best cluster, run bootcluster()
    km.boot <- invisible(clusterboot(dataset, B=100, bootmethod = "boot", clustermethod = kmeansCBI,
                           krange = numClust, seed = 15555))
    #Check if every cluster has at least a .75 bootmean or at least .6
    if (all(km.boot$bootmean > .75)) {
      dataset$cluster <- km.boot$partition
      signal <- "good"
    } 
    else if (all(km.boot$bootmean > .6)) {
      dataset$cluster <- km.boot$partition
      signal <- "ok"
    } 
    else {
      signal <- "bad"
    }
    retList <- list(signal, dataset, numClust, km.boot$bootmean)
    return(retList)
  }
  
  ########
  #Download tab code
  ########
  output$downloadData <- downloadHandler(
    filename = function() { paste0(input$fileName, '.csv') },
    content = function(file) {
      write.csv(getData(), file)
    }
  )
  
})
