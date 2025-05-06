rm(list=ls())
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(rsconnect)
options(shiny.maxRequestSize = 30*1024^2)

function(input, output, session) {
  
  observeEvent(input$paired,updateCheckboxInput(inputId="curate",value=input$paired),ignoreInit=TRUE)
  observeEvent(input$curate,updateCheckboxInput(inputId="curate2",value=input$curate),ignoreInit=TRUE)
  observeEvent(input$paired,toggleState("curate"),ignoreInit=TRUE)
  observeEvent(input$paired,toggleState("timevar"),ignoreInit=TRUE)
  observeEvent(input$curate,toggleState("curatevar"),ignoreInit=TRUE)
  observeEvent(input$curate,toggleState("curate2"),ignoreInit=TRUE)
  observeEvent(input$percent,toggleState("dropNA"),ignoreInit=TRUE)
  observeEvent(input$filter,toggleState("filtervar"),ignoreInit=TRUE)
  observeEvent(input$filter,{
    toggleState("filtervalue")
    runjs("$('#filtervalue').selectpicker('refresh');")
  }, ignoreInit = TRUE)
  
  observeEvent(input$File, {
    Thedata <<- read.csv(input$File$datapath, fileEncoding = "latin1")
    for (i in 1:ncol(Thedata)) {
      Thedata[which(Thedata[,i]==''),i] <<- NA
    }
    Choices <- colnames(Thedata)
    names(Choices) <- substr(Choices,1,100)
    updateSelectInput(inputId="timevar", choices = Choices)
    updateSelectInput(inputId="curatevar", choices = Choices)
    updatePickerInput(session, inputId='vars', choices = Choices, selected = Choices)
    updateSelectInput(inputId='filtervar', choices = Choices)
  })
  
  observeEvent(input$filtervar, {
    FilterChoices <- unique(Thedata[,input$filtervar])
    names(FilterChoices) <- substr(FilterChoices,1,100)
    updatePickerInput(session, inputId='filtervalue', choices = FilterChoices, selected = FilterChoices)
  }, ignoreInit = TRUE)
  
  instructions <- reactive({
"Welcome to the Automated Survey Analyzer Program.
(v1.01, January 10, 2024)

This program can quickly analyze survey or other data to 
generate frequencies, percentages, and summary statistics.

The program can handle one-time surveys, but can also
handle pre-post surveys (or even >2 time points) if
each individual assessment is recorded on a separate 
line. For this functionality, you must also include a 
variable identifying the time point.

To begin, upload a CSV file. Variable names must be in 
the first row, and the data should begin in the second
row. To indicate missing data, leave the cell blank.
Max file size: 30 MB

Note: variables are assumed to be categorical unless 
all of the variable's data is perfectly numeric, with 
no text in any data cell. You can select an option in
this program to treat all data as categorical if you
want. 

This software does not do tests of statistical
significance. This software also does not do cross-tabs, 
although you could get close by plugging in the column 
variable as the pre/post variable.

Disclaimer: This software is not warranted to do anything.

For technical support, contact:
Jonathan Bennett
Center for Community Research and Evaluation
University of Memphis
(662) 686-3945 
jrbnnett@memphis.edu
    
Version history:
v0.1 - first 
v0.11 - fixed bug related to file encoding
v1.01 - added filter feature
    "})
  
  outputtext <- eventReactive(input$go, {
    
    out <- function(x, Append = TRUE, Cat = TRUE) {
      
      if (Cat) {capture.output(cat(x), append = Append, file = 'output.txt', type = 'output')} else {
                capture.output(x,      append = Append, file = 'output.txt', type = 'output')}
      
      #sink('output.txt', append = Append, type = 'output', split = TRUE)
      #cat(x)
      #sink()
    }
    
    error <<- ''
  
    if (!input$raw & !input$percent) {error <<- "Error: You must report raw frequencies and/or percentages."}
    if (is.null(input$vars) | length(input$vars)==0) {error<<-'Error: No variables selected.'} else {
       if (nchar(input$vars[1])==0 | input$vars[1]=='') {error<<-'Error: No variables selected.'}}
    if (is.null(input$File$datapath)) {error<<-'Error: No data file uploaded.'} else {
         if (is.element("jbjbjbjbjbjbjb",colnames(Thedata))) {error <<- 
        "Error: Congratulations on finding the Easter egg. You may not name a variable jbjbjbjbjbjbjb."}}     

       
    
    
   
    if (error != '') {
      
      out(error,Append=FALSE)
      
    } else {       
      
      thedata <- Thedata
      
      # Step 0 - Overview of the data
      
      n <- nrow(thedata)
      
      out("Automated Survey Analyzer v0.1, code last updated November 8, 2022\n")
      out("Center for Community Research and Evaluation, University of Memphis\n")
      out(paste("Date and time: ",as.character(.POSIXct(Sys.time(),"America/Chicago")),'\n',"Data file: ",input$File$name,'\n',"Your original data file had ",n," rows.\n"),Append=FALSE)
      
      # Step 1 - Apply filter
    
      if (input$filter) {
        out(paste("You requested that we filter data to:\n",
                  "Filter variable:",
                  input$filtervar,
                  '\n Value equals:',
                  paste(input$filtervalue, collapse = ', '),'\n '))
        thedata <- thedata[ is.element(thedata[,input$filtervar], input$filtervalue), ]
        if (n>nrow(thedata)) {out(paste(n - nrow(thedata)," rows were deleted during filtering.\n",sep=''))}
        n <- nrow(thedata)
      }

      # Step 2 - if Paired, report this and delete rows without valid pre/post data
      if (input$paired) {
        out(paste("You requested an analysis looking across time (such as a pre/post test).\n",
                  "Time variable:\n ",
                  input$timevar,
                  '\n '))
        thedata <- subset(thedata, !is.na(thedata[,input$timevar]))
        if (n>nrow(thedata)) {out(paste(n - nrow(thedata)," rows were deleted for missing time data.\n",sep=''))}
        n <- nrow(thedata)
      }
      
      # Step 3, if curated, delete rows without 1 obs. at each time point
      if (input$curate) {
        out(paste("You requested an analysis looking only at participants with complete data at all time points.\n",
                  "Participant variable:\n ",
                  input$curatevar,
                  '\n '))
        
        #Missing participant variable
        thedata <- subset(thedata, !is.na(thedata[,input$curatevar]))
        if (n>nrow(thedata)) {out(paste(n - nrow(thedata)," rows were deleted for missing participant data.\n",sep=''))}
        out(paste("Before validation, there are ",nrow(thedata)," rows (",length(unique(thedata[,input$curatevar]))," unique participants) in the data.\n",sep=''))
        n <- nrow(thedata)
        pp <- length(unique(thedata[,input$curatevar]))
        
        #More than 1 test at a time point
        tabb <- table(thedata[,input$curatevar],thedata[,input$timevar])
        thedata <- subset(thedata, !is.element(thedata[,input$curatevar], names(which(apply(tabb>1,1,sum)>0))))
        if (n>nrow(thedata)) {out(paste(n - nrow(thedata)," rows (",pp - length(unique(thedata[,input$curatevar]))," participants) were deleted due to participants having multiple tests at one time point.\n",sep=''))}
        n <- nrow(thedata)      
        pp <- length(unique(thedata[,input$curatevar]))
        
        #No test at a time point
        tabb <- table(thedata[,input$curatevar],thedata[,input$timevar])
        thedata <- subset(thedata, !is.element(thedata[,input$curatevar], names(which(apply(tabb<1,1,sum)>0))))
        if (n>nrow(thedata)) {out(paste(n - nrow(thedata)," rows (",pp - length(unique(thedata[,input$curatevar]))," participants) were deleted due to participants having no data for some time point.\n",sep=''))}
        n <- nrow(thedata)           
        pp <- length(unique(thedata[,input$curatevar]))      
        
        out(paste("After validation, there are ",n," rows (",length(unique(thedata[,input$curatevar]))," unique participants) in the data.\n",sep=''))
        
        
        
      }
      
      # Step 4 - If numeric, treat as character if so requested
      
      if (input$character) {
        categ <- which(sapply(1:ncol(thedata),function(z){is.element(class(thedata[,z]), c("numeric", "integer"))}))
        if (length(categ)>0) {out(paste("Note:",length(categ),"numeric variables treated as categorical.\n"))}
        for (j in categ) {thedata[,j] <- as.character(thedata[,j])}
      }
      
      # Step 5 - If not prepost, name a new variable and make that the "time" variable
      
      if (!input$paired) {thedata$jbjbjbjbjbjbjb <- "All"}
      timevar <- ifelse(input$paired,input$timevar,"jbjbjbjbjbjbjb")
      curatevar <- input$curatevar
      
      # Step 6 - Evaluate All Variables
      
      for (i in input$vars) {
        
        # If participant identified, keep only the participants that is complete for all time points
        # Otherwise, keep all data
        
        vardata <- thedata[,c(curatevar, timevar, i)]
        out(paste("\n*** Variable Name: ",i,"***\n"))
        
        if (input$curate2) {
          n<-nrow(vardata)
          p<-length(unique(vardata[,curatevar]))
          vardata <- subset(vardata, !is.element(
            vardata[,curatevar],
            vardata[is.na(vardata[,i]),curatevar]))
          if (nrow(vardata)<n) {out(paste(
            n-nrow(vardata),"rows (",p-length(unique(vardata[,curatevar])),"participants) deleted due to some NA data.\n"
          ))}
        }
        
        if (is.element(class(vardata[,i]),c("numeric","integer"))) {
 
          out("\nSummary Statistics:\n\n")
          
          df <- do.call(rbind,lapply(unique(vardata[,timevar]), function(j) {
            data.frame(
              min =            min(vardata[vardata[,timevar]==j,i],na.rm=T),
              first_quartile = quantile(vardata[vardata[,timevar]==j,i],.25,na.rm=T),
              median =         quantile(vardata[vardata[,timevar]==j,i],.50,na.rm=T),
              mean =           mean(vardata[vardata[,timevar]==j,i],na.rm=T),
              third_quartile = quantile(vardata[vardata[,timevar]==j,i],.75,na.rm=T),
              max =            max(vardata[vardata[,timevar]==j,i],na.rm=T)
            ) }))
            
          try({rownames(df) <- unique(vardata[,timevar])})
          out(df,Cat=FALSE)
          out("\n")
             
        } else {
          
          if (input$raw) {
            out("\nRaw Frequencies:\n")
            out(table(vardata[,i], vardata[,timevar], useNA = 'ifany'),Cat=FALSE)
            out("\n")
          }
          if (input$percent) {
            if (sum(is.na(vardata[,i]))>0 & input$dropNA) {
              out("\nPercentage Frequencies (NAs dropped):\n")
              out(round(100*prop.table(table(vardata[,i], vardata[,timevar]),2),2),Cat=FALSE)
              out("\n")
              
            } else {
              out("\nPercentage Frequencies: \n")
              out(round(100*prop.table(table(vardata[,i], vardata[,timevar], useNA='ifany'),2),2),Cat=FALSE)
              out("\n")
              
            }}
            
          
        }}}
      
      paste(readLines("output.txt"),collapse='\n')
      
      })
  
  output$out <- renderText({if (!input$go) {instructions()} else {outputtext()}})
  output$download <- downloadHandler(
    filename = function(){paste("AutomatedSurveyAnalyzer-", format(.POSIXct(Sys.time(),"America/Chicago"),'%Y_%m_%d-%H_%M_%S'), ".txt", sep="")},
    content = function(file){writeLines({if (!input$go) {instructions()} else {outputtext()}}, file)}
  )
  }
  
  