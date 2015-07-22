library(shiny)
library(shinydashboard)
library(DT)
library(shinyAce)
library(shiny)
source('server.R', local=FALSE)

#Get files already saved in the data directory
filenames<-list.files(path = "data/", pattern="\\.rda$")

dashboardPage(
  dashboardHeader(title = "Data Butler"),
  dashboardSidebar(
    uiOutput("dataList"),
    sidebarMenu(
      menuItem("Data Upload", tabName = "dataUpload", icon = icon("upload")),
      menuItem("Data Merger", tabName = "dataMerger", icon = icon("refresh")),
      menuItem("Data Examiner", tabName = "dataExaminer", icon=icon("search")),
      menuItem("Data Editor", tabName = "dataEditor", icon=icon("edit")),
      menuItem("Data Visualizer", tabName = "dataVisualizer", icon=icon("bar-chart")),
      menuItem("Model Builder", tabName = "modelBuilder", icon=icon("line-chart")),
      menuItem("Data Download", tabName = "dataDownload", icon=icon("download")),
      id="sidebar"
    )
  ),
  dashboardBody(
    includeScript("www/javascript.js"),
    tabItems(
      tabItem(tabName = "dataUpload",
              fluidRow(
                column(width=4,
                       box(
                         width = 12,
                         title = "Upload Your Data",
                         radioButtons("typeUpload", "Type of Upload File:",
                                      choices = c("CSV", "RData File"), inline=T),
                         fileInput('file1', 'Choose File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv',
                                            '.rda')),
                         textInput("dataName", "Name of Dataset:"),
                         actionButton("finalizeUpload", "Upload File"),
                         tags$hr(),
                         uiOutput("csvType")
                       )),
                column(width=4,
                       box(
                         width = 12,
                         title = "Preloaded Data Files to Work With:",
                         selectInput(inputId = "dataset",
                                     label = "Choose Data File",
                                     filenames
                         ),
                         textInput("dataName2", "Name of Dataset:"),
                         actionButton("chooseFile", "Upload File")
                       )),
                column(width=4)
              )
      ),
      tabItem(tabName = "dataMerger",
              fluidRow(
                column(width=2),
                column(width=8,
                       box(width=12,
                           fluidRow(
                             column(width=6,
                                    uiOutput("dataset1")),
                             column(width=6,
                                    uiOutput("dataset2"))
                           ),
                           fluidRow(
                             column(width = 12,
                                    textInput("newDataName", "Name for New Dataset:"),
                                    actionButton("mergeEm", "Merge!")
                             )
                           )
                        )
                ),
                column(width=2)
              )),
      tabItem(tabName = "dataExaminer",
              fluidRow(
                tabBox(width=12,
                       tabPanel("Raw Data",
                                fluidRow(
                                  column(width=3,
                                         uiOutput("varCheckBoxes")
                                  ),
                                  column(width=9,
                                         box(width=12,
                                             dataTableOutput('mytable')
                                         ),
                                         box(width = 12,
                                             radioButtons("rmNA", "How do you want to deal with NAs?",
                                                          choices = c("Dont remove NAs" = "noNA",
                                                                      "Only show complete observations" = "allNA",
                                                                      "Only remove NAs from certain columns" = "someNA"),
                                                          inline = T),
                                             uiOutput("particularCheckBoxes")
                                         )
                                  )
                                )
                          ),
                       tabPanel("Summary Statistics",
                                fluidRow(
                                  column(width=3,
                                         box(width=12,
                                             uiOutput("selectizeSumStats")
                                         )),
                                  column(width=9,
                                         box(width=12,
                                             column(width=4,
                                                    verbatimTextOutput("summaryStats1")),
                                             column(width=4,
                                                    verbatimTextOutput("summaryStats2")),
                                             column(width=4,
                                                    verbatimTextOutput("summaryStats3"))
                                             )
                                         )
                                )
                         ),
                       tabPanel("Advanced Examination",
                                helpText("Currently under construction"))
                       )
              )        
      ),
      tabItem(tabName = "dataEditor",
              fluidRow(
                column(width=3,
                       selectizeInput("editOptions", "How would you like to edit your data (one edit at a time):",
                                      choices = c("Remove NAs", "Center Variable", "Scale Variable", "Convert Variable Type",
                                                  "Change to Lowercase", "Change to Uppercase", "Make an Imputation",
                                                  "Remove Outliers", "Create a Variable", "Change a variable name",
                                                  "Subset Data"), #Remove outliers should remove any observations that are more than 3(?) standard deviations away from the mean
                                      multiple = F),
                       uiOutput("selectizeVariablesForEditing"),
                       uiOutput("imputation1"),
                       uiOutput("conversion1"),
                       uiOutput("subset"),
                       uiOutput("createVar"),
                       uiOutput("changeVarName"),
                       div(
                         column(width=6,
                                actionButton("previewSub", "Preview Edits")),
                         column(width=6,
                                actionButton("submitEdits", "Edit Data"))
                       )),
                column(width=9,
                       fluidRow(
                         column(width=6,
                                h4("Random sample of data before editing"),
                                verbatimTextOutput("dataBeforeEditing")),
                         column(width=6,
                                h4("Random sample of data after editing"),
                                verbatimTextOutput("dataAfterEditing"))
                       )
                )
              )
      ),
      tabItem(tabName = "dataVisualizer",
              fluidRow(
                tabBox(
                  width = 12,
                  tabPanel("Plot Creator",
                           column(width=3,
                                  div(style="visibility:hidden",
                                      box(width=12,
                                          height = 115)
                                  ),
                                  uiOutput("yAxisSelect"),
                                  actionButton("createPlot", "Create Plot"),
                                  div(class="span12",
                                      textInput("plotName", "Name of saved plot:"),
                                      downloadButton('downloadPlot', 'Save Plot as PDF')),
                                  div(style="visibility:hidden",
                                      box(width=12,
                                          height = 212)
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
                                  )
                           )),
                  tabPanel("Live ggplot",
                           fluidRow(
                             column(width=4,
                                    aceEditor("plotCode", 
                                              mode = "r"),
                                    helpText("In your call to ggplot, please refer to the data as dat. For example, call ggplot(data=dat, aes(...)) to create a plot.")),
                             column(width=8,
                                    plotOutput(outputId = "plotOut2"),
                                    actionButton("plotButton", "Update Plot"))
                           )
                )
              )
        
      )),
      tabItem(tabName = "modelBuilder",
              fluidRow(
                tabBox(width = 12,
                       tabPanel("Linear Regression",
                                fluidRow(
                                  box(width=12,
                                      column(width = 4,
                                             uiOutput("linReg"),
                                             actionButton("buildModelLinReg", "Build Model")),
                                      column(width = 8,
                                             verbatimTextOutput("modelOutput")
                                      )
                                  )
                                )
                        ),
                       tabPanel("Logistic Regression",
                                helpText("Currently under construction")),
                       tabPanel("Random Forest",
                                helpText("Currently under construction")),
                       tabPanel("Support Vector Machine",
                                helpText("Currently under construction")),
                       tabPanel("Generalized Additive Models",
                                helpText("Currently under construction")),
                       tabPanel("K-Nearest Neighbor",
                                helpText("Currently under construction")),
                       tabPanel("K-Means Clustering",
                                helpText("Currently under construction"))
                )
              )),
      tabItem(tabName = "dataDownload",
              fluidRow(
                column(width=4,
                       box(width=12,
                           title="Download Your Data:",
                           textInput("fileName", "Enter the name for the downloaded file:"),
                           downloadButton('downloadData', 'Download')
                       )),
                column(width = 8)
              ))
    )
  )
)