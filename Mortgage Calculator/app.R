#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(LaCroixColoR)
#library(shiny.semantic)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Theme
  theme = shinytheme("flatly"),
   # Application title
   titlePanel("Mortgage Calculator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textInput("property", "Property Name", value = ""),
        numericInput("price",
                    "Home Price", 
                    500000), 
        sliderInput("payment", 
                    "Down Payment", 
                    min = 0, 
                    max = 100, 
                    value = 20,
                    post = "%"),
        numericInput("interest", 
                    "Interest Rate", 
                    4), 
        sliderInput("years",
                     "Loan Term (Years)",
                     min = 1,
                     max = 50,
                     value = 30),
         selectInput("freq",
                     "Payment Frequency",
                     choices = list("Monthly" = 12, 
                                    "Yearly" = 1)),
        numericInput("tax", "Annual Property Tax",1.15),
        #selectInput("tax_unit", 
         #           "Percent or Dollars",
          #          choices = list("Dollars" = "$", 
           #                        "Percent" = "%")),
        numericInput("insurance", 
                    "Annual Home Insurance",
                    800),
        sliderInput("dues",
                    "HOA Dues", 
                    min = 0,
                    max = 100,
                    value = 0,
                    step = 1,
                    pre = "$"),
        sliderInput("pmi",
                    "PMI (% of Loan Amount)",
                    min = 0,
                    max = 100,
                    value = 0,
                    step = 0.001,
                    post = "%"),
        actionButton("calc", "Calculate")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h1("Expenses"),
        hr(),
        tableOutput("values"),
        h2("Total Monthly Payment"),
        hr(),
        tableOutput("monthlypay.df"), 
        br(),
        downloadButton("downloadData", "Download")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  sliderValues <- reactive({
    
    
    # Compose data frame
    data.frame(
      Name = c("Property",
               "Home Price",
               "Down Payment",
               "Interest Rate",
               "Loan Term (Years)", 
               "Payment Frequency",
               "Annual Property Tax",
               "Annual Home Insurance",
               "HOA Dues",
               "PMI (% of Loan Amount)"
               ),
      Value = as.character(c(input$property,
                             paste("$", input$price),
                             paste(input$payment, "%"),
                             paste(input$interest, "%"),
                             input$years, 
                             input$freq,
                             paste(input$tax, "%", sep = " "),
                             paste("$", input$insurance),
                             paste("$", input$dues),
                             paste(input$pmi, "%")
                             )), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
  observeEvent(input$calc, {
  
  #clean this up with mutate
  # Calculate total monthly payment

    price <- as.numeric(input$price)
    down <- as.numeric(input$payment)/100
    intrate <- (as.numeric(input$interest)/100)/12
    y <- as.numeric(input$years)
    freq <- as.numeric(input$freq)
    monthtax <- ((as.numeric(input$tax)/100)*price)/12
    monthinsurance <- as.numeric(input$insurance)/12
    #hoa fees are assumed to be per month, can just /12 if per year
    hoa <- as.numeric(input$dues)
    pmi <- (as.numeric(input$pmi)/100)*(price -(price*down))
    i <- intrate/12
    loan <- (price - (price*down))
    n <- y*freq
    interest_payment <- ((intrate*((1+intrate)^n))/(((1+intrate)^n)-1))
    monthly_payment <- round((interest_payment*loan)+monthtax+monthinsurance+hoa+pmi, 2)
    principle <- round(monthly_payment - sum(monthtax+monthinsurance+hoa+pmi), 2)
    
  output$monthlypay.df <- renderTable({
    payment.df <- data.frame(
      Name = c("Property Name",
               "Princple",
               "Property Taxes", 
               "Homeowners Insurance",  
               "HOA Dues",  
               "PMI",
               "Total Monthly Payment"),
      Payment = c(input$property,
                  principle,
                  round(monthtax,2), 
                  round(monthinsurance, 2), 
                  hoa,
                  pmi, 
                  paste("$", monthly_payment, sep = " ")))
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$property, ".csv", sep = "")
    },
    content = function(file) {
      payment.df <- data.frame(
        Name = c("Property Name",
                 "Princple",
                 "Property Taxes", 
                 "Homeowners Insurance",  
                 "HOA Dues",  
                 "PMI",
                 "Total Monthly Payment"),
        Payment = c(input$property,
                    principle,
                    round(monthtax,2), 
                    round(monthinsurance, 2), 
                    hoa,
                    pmi, 
                    paste("$", monthly_payment, sep = " ")))
      write.csv(payment.df, file, row.names = F)
    }
  )
  outputOptions(output, 'downloadData', suspendWhenHidden=FALSE)

  })  
    
  }


# Run the application 
shinyApp(ui = ui, server = server)

