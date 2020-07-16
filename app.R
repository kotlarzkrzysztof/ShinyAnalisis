#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source('t_test.R')
source('corel.R')
source('reg.R')
source('anov.R')

library(shiny)

# Define UI
ui <- fluidPage(navbarPage(
  'Analisis',
  tabPanel(
    strong('Load data'),
    # App title ----
    titlePanel("Uploading Files"),
    tags$hr(),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        # Input: Select a file ----
        fileInput(
          "file1",
          "Choose CSV File",
          multiple = TRUE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons(
          "sep",
          "Separator",
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          selected = ","
        ),
        radioButtons(
          "dec",
          "Decimal",
          choices = c(Dot = '.',
                      Comma = ","
                      ),
          selected = '.'
        ),
        tags$hr(),
        checkboxInput('string', "Strings as factor", TRUE),
        # Horizontal line ----
        tags$hr(),
        
        # Input: Select number of rows to display ----
        radioButtons(
          "disp",
          "Display",
          choices = c(Head = "head",
                      All = "all"),
          selected = "head"
        ),
        submitButton("Update view", icon("refresh"))
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(# Output: Data file ----
                tableOutput("contents"))
      
    )
  ),
  navbarMenu(
    'T Test',
    tabPanel('One Sample',
             sidebarLayout(
               sidebarPanel(
                 # App title ----
                 titlePanel("One Sample\n T Test"),
                 tags$hr(),
                 uiOutput('varone'),
                 numericInput('mu', 'm0', 0),
                 selectInput(
                   'altone',
                   'Alternative',
                   choices = c(
                     Less = 0,
                     Greater = 1,
                     TwoSided = 2
                   )
                 ),
                 numericInput(
                   'alphaone',
                   'Significance level',
                   value = 0.05,
                   step = 0.01
                 ),
                 submitButton()
               ),
               mainPanel(verbatimTextOutput('output_onesampe_t'))
             )),
    tabPanel('Two Sample',
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Two Sample/Paired\n T Test"),
                 tags$hr(),
                 uiOutput('vartwoX'),
                 uiOutput('vartwoY'),
                 checkboxInput('par', 'Paired', FALSE),
                 selectInput(
                   'alttwo',
                   'Alternative',
                   choices = c(
                     Less = 0,
                     Greater = 1,
                     TwoSided = 2
                   )
                 ),
                 numericInput(
                   'alphatwo',
                   'Significance level',
                   value = 0.05,
                   step = 0.01
                 ),
                 submitButton()
                 
               ),
               mainPanel(verbatimTextOutput('outputtwo'))
             ))
    
  ),
  tabPanel('Corelation',
           sidebarLayout(
             sidebarPanel(
               titlePanel("Correlation"),
               tags$hr(),
               uiOutput('varcorX'),
               uiOutput('varcorY'),
               tags$hr(),
               radioButtons(
                 'cor_method',
                 'Method',
                 choices = c(
                   Pearson = 'pearson',
                   Spearman = 'spearman',
                   Kendall = 'kendall'
                 )
               ),
               submitButton()
             ),
             mainPanel(verbatimTextOutput('outputcor'))
           )),
  tabPanel('Linear regression',
           sidebarLayout(
             sidebarPanel(
               titlePanel("Linear regression"),
               tags$hr(),
               helpText('Choosse continuous Y variable'),
               uiOutput('varlinY'),
               helpText('Choosse continuous X variables'),
               uiOutput('varlinX'),
               tags$hr(),
               checkboxInput('rstu', 'Studentized residuals', FALSE),
               submitButton()
             ),
             mainPanel(verbatimTextOutput('outputlin'))
           )),
  navbarMenu(
    'ANOVA',
    tabPanel('One Way',
             sidebarLayout(
               sidebarPanel(
                 titlePanel("One Way ANOVA"),
                 tags$hr(),
                 uiOutput('varaovY'),
                 helpText('Choose categorical X1 variable'),
                 uiOutput('varaovX1'),
                 tags$hr(),
                 submitButton()
               ),
               mainPanel(verbatimTextOutput('outputaovone'))
             )),
    tabPanel('Two Way',
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Two Way ANOVA"),
                 tags$hr(),
                 uiOutput('varaovYtwo'),
                 helpText('Choose categorical X1 variable'),
                 uiOutput('varaovX1two'),
                 helpText('Choose categorical X2 variable'),
                 uiOutput('varaovX2two'),
                 tags$hr(),
                 submitButton()
               ),
               mainPanel(verbatimTextOutput('outputaovtwo'))
             ))
    
  )
  
))

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 60 * 1024 ^ 2)
  data <- reactive({
    file1 <- input$file1
    if (is.null(file1)) {
      return()
    }
    data = read.table(
      file = file1$datapath,
      header = input$header,
      sep = input$sep,
      dec = input$dec,
      stringsAsFactors = input$string
    )
  })
  
  output$varone <- renderUI({
    selectInput('varone', 'Variable Y', choices = names(data()))
  })
  
  output$vartwoX <- renderUI({
    selectInput('vartwoX', 'Variable X', choices = names(data()))
  })
  
  output$vartwoY <- renderUI({
    selectInput('vartwoY', 'Variable Y', choices = names(data()))
  })
  
  output$varcorX <- renderUI({
    selectInput('varcorX', 'Variable X', choices = names(data()))
  })
  
  output$varcorY <- renderUI({
    selectInput('varcorY', 'Variable Y', choices = names(data()))
  })
  
  output$varlinY <- renderUI({
    selectInput('varlinY', 'Variable Y', choices = names(data()))
  })
  
  output$varlinX <- renderUI({
    checkboxGroupInput('varlinX', 'Variables X', choices = names(data()))
  })
  
  output$varaovY <- renderUI({
    selectInput('varaovY', 'Variable Y', choices = names(data()))
  })
  
  output$varaovX1 <- renderUI({
    radioButtons(
      'varaovX1',
      'Variable X1',
      choiceNames  = '',
      choiceValues = '',
      choices = names(data())
    )
  })
  
  output$varaovYtwo <- renderUI({
    selectInput('varaovYtwo', 'Variable Y', choices = names(data()))
  })
  
  output$varaovX1two <- renderUI({
    radioButtons(
      'varaovX1two',
      'Variable X1',
      choiceNames  = ' ',
      choiceValues = ' ',
      choices = names(data())
    )
  })
  
  output$varaovX2two <- renderUI({
    radioButtons(
      'varaovX2two',
      'Variable X2',
      choiceNames  = ' ',
      choiceValues = ' ',
      choices = names(data())
    )
  })
  
  
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    if (input$disp == "head") {
      return(head(data()))
    }
    else {
      return(data())
    }
    
  })
  
  output$output_onesampe_t <- renderPrint({
    req(input$varone)
    ttest(
      X = data()[[input$varone]],
      mu = input$mu,
      alternative = input$altone,
      alpha = input$alphaone
    )
  })
  
  output$outputtwo <- renderPrint({
    req(input$vartwoX)
    req(input$vartwoY)
    ttest(
      X = data()[[input$vartwoX]],
      Y = data()[[input$vartwoY]],
      paired = input$par,
      alternative = input$alttwo,
      alpha = input$alphatwo
    )
  })
  
  output$outputcor <- renderPrint({
    req(input$varcorX)
    req(input$varcorY)
    korel(x = data()[[input$varcorX]],
          y = data()[[input$varcorY]],
          method = input$cor_method)
  })
  
  output$outputlin <- renderPrint({
    req(input$varlinX)
    req(input$varlinY)
    regLin(
      y = data()[[input$varlinY]],
      data()[input$varlinX],
      names = input$varlinX,
      rstu = input$rstu
    )
  })
  
  output$outputaovone <- renderPrint({
    req(input$varaovY)
    req(input$varaovX1)
    anovaHSD(Y = data()[[input$varaovY]],
             X1 = data()[[input$varaovX1]],
             names = input$varaovX1)
  })
  
  output$outputaovtwo <- renderPrint({
    req(input$varaovYtwo)
    req(input$varaovX1two)
    req(input$varaovX2two)
    anovaHSD(
      Y = data()[[input$varaovYtwo]],
      X1 = data()[[input$varaovX1two]],
      X2 = data()[[input$varaovX2two]],
      names = c(input$varaovX1two, input$varaovX2two)
    )
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
