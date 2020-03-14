shinyUI(fluidPage(
  tags$style(type = 'text/css', 'html, body {width:100%;height:100%}'),
  tags$title("RTS Index CAPM"),
  fluidRow(
    column(4, 
        tags$h4("Parameters"),
        wellPanel(
          fluidRow(
            column(4,
                   textInput("indexTicker", "Index", value = "RTS", width = "100%")
            ),
            column(8,
                   selectInput("tickersList", "Tickers list", c("rtsi_tickers.csv"), width = "100%")
            )
          ),
           fluidRow(
              column(4,
                     dateInput(inputId = "dateFrom", label = "From", value = seq(Sys.Date(), length=2, by = "-364 days")[2],
                               format = "yyyy-mm-dd", width = "100%")
              ),
              column(4,
                     dateInput(inputId = "dateTo", label = "To", value = Sys.Date(),
                               format = "yyyy-mm-dd", width = "100%")
              ),
              column(4,
                     numericInput(inputId = "rfrInput", label = "Risk-free rate, %", value = 6.0, 
                                  min = 0.0, max = 100.0, step = 0.25, width = "100%")
              )
           ),
           tags$b("Pick a last period quickly"),
           fluidRow(
              column(3,
                    actionButton(inputId = "last1Button", label = "1 month", width = "100%")
              ),
              column(3,
                     actionButton(inputId = "last3Button", label = "3 months", width = "100%")
              ),
              column(3,
                     actionButton(inputId = "last6Button", label = "6 months", width = "100%")
              ),
              column(3,
                     actionButton(inputId = "lastYearButton", label = "Year", width = "100%")
              )
            )
        ),
        tags$h4("Advanced plots"),
        wellPanel(
          fluidRow(
            column(12,
                   plotly::plotlyOutput(outputId = "smlPlot", width = "100%")  
            )
          ),
          tags$br(),
          fluidRow(
            column(12,
                   plotly::plotlyOutput(outputId = "alphaHist", width = "100%")  
            )
          ),
          tags$br(),
          fluidRow(
            column(12,
                   plotly::plotlyOutput(outputId = "assetsPlot", width = "100%") 
            )
          ),
          tags$br(),
          fluidRow(
            column(12,
                   plotly::plotlyOutput(outputId = "scatterPlot", width = "100%") 
            )
          )
        )
    ),
    column(8,
           tags$h4("Analyzed stocks"),
           fluidRow(
             column(12,
                    wellPanel(
                      DT::dataTableOutput(outputId = "stocksTable", width = "100%")
                    )
             )
           )
    )
  )
))