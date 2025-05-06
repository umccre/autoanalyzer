


rm(list = ls())
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(rsconnect)

fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  titlePanel("Automated Survey Analyzer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("File", "Upload CSV File", multiple = FALSE, accept='.csv'),
      hr(),
      checkboxInput("paired","Pre/Post or 2+ time pts"),
      disabled(selectInput("timevar","Specify Pre/Post or Time Point Variable",choices='')),
      hr(),
      disabled(checkboxInput("curate","Keep only participants with surveys at all time points")),
      disabled(selectInput("curatevar","Specify Participant Variable",choices='')),
      disabled(checkboxInput("curate2","When analyzing variables, look only at participants who never have missing data")),
      hr(),
      pickerInput("vars","Variables to Analyze", choices=c(""), options = list(`actions-box` = TRUE,showTick=TRUE),multiple = T),
      hr(),
      checkboxInput("filter","Look only at a subset of data"),
      disabled(selectInput("filtervar","Filter variable",choices='')),
      disabled(pickerInput("filtervalue","Variable equals: (select all that apply)",choices='', options = list(`actions-box` = TRUE,showTick=TRUE),multiple = T)),
      hr(),
      checkboxInput("character","Treat numeric variables as categorical"),
      checkboxInput("raw","Report raw frequencies and summary statistics",value=TRUE),
      checkboxInput("percent","For categorical variables, report percentages"),
      disabled(checkboxInput("dropNA","Drop missing data from percentages",value=TRUE)),
      actionButton("go","Analyze"),
      downloadButton("download", "Download Current Analysis")
    ),
    mainPanel(verbatimTextOutput("out"))
  )
)