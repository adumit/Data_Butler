#Created by Andrew Dumit
#Located in Data Visualizer Tab
library(shiny)
library(shinydashboard)

#GUIGrapher UI code:
GUIGrapherUI <- '
output$GUIGrapherUI <- renderUI ({
  fluidRow(
    column(width=3,
           div(style="visibility:hidden",
               box(width=12,
                   height = 115)
           ),
           uiOutput("yAxisSelect"),
           actionButton("createPlot", "Create Plot"),
           div(style="visibility:hidden",
               box(width=12,
                   height = 30)
           ),
           div(class="span12",
               textInput("plotName", "Name of saved plot:"),
               downloadButton("downloadPlot", "Save Plot as PDF")),
           div(style="visibility:hidden",
               box(width=12,
                   height = 54)
           ),
           box(width = 12,
               selectizeInput("graphOptions", "Graphing Options to Change:",
                              choices = c("Add color to plot output", "Color by a Variable",
                                          "Group by a Variable", "Split plot by a variable",
                                          "Add Title", "Change X-axis label", "Change Y-axis label",
                                          "Change legend title", "Change background color",
                                          "Remove legend", "Move legend to bottom", "Change X-axis limits",
                                          "Change Y-axis limits", "Fill by a Variable", "Change a variable name"), 
                              multiple =T)
           )
    ),
    column(width=9,
           plotOutput("plotOut",
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      )),
           fluidRow(
             column(width=4),
             column(width=4,
                    uiOutput("xAxisSelect")),
             column(width=4,
                    radioButtons("factorVar", "Make x variable a factor:",
                                 choices = c("No", "Yes"), inline = T))
           ),
           fluidRow(
             box(width = 12,
                 column(width=5,
                        selectizeInput("typeOfPlot", "Plot Type:",
                                       choices = c("Scatter Plot", "Bar Plot",
                                                   "Box Plot", "Violin Plot",
                                                   "Density Plot", "Jitter Plot",
                                                   "Line Plot", "Hex Plot")),
                        div(class="span12",
                            uiOutput("colorVar"),
                            uiOutput("fillVar"),
                            uiOutput("addColor"),
                            uiOutput("facetPlot"),
                            uiOutput("titleInput"),
                            uiOutput("changeXAxisLabel"),
                            uiOutput("changeYAxisLabel"),
                            uiOutput("changeLegendTitle"),
                            uiOutput("changeBackgroundColor"),
                            uiOutput("changeXAxisLimit"),
                            uiOutput("changeYAxisLimit")
                        )
                 ),
                 column(width=2),
                 column(width=5,
                        uiOutput("scatterPlotOptions"),
                        uiOutput("barPlotOptions"))
             )
           ),
           div(style="visibility:hidden",
               box(width=12,
                   height = 200))
    )
  )
})
'

##########
#GUIGrapher Server
##########
GUIGrapherServer <- '
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
      need(try(print(plotMaker())), "Sorry, this plot is either invalid or is not currently supported.")
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
'

