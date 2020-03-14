library(shiny)
library(DT)
library(PerformanceAnalytics)
library(ggplot2)
library(plotly)
library(rusquant)
library(curl)

function(input, output, session) {
  generalData <- reactive({
    stocks <- read.csv2(paste0('./data/', input$tickersList), stringsAsFactors = F)
    days.ago <- difftime(input$dateTo, input$dateFrom, units = c("days"))
    rf.rate <- as.numeric(input$rfrInput / 365 * days.ago)
    
    options(download.file.method = "libcurl")
    market.prices <- as.numeric(getSymbols(input$indexTicker, from = input$dateFrom, to = input$dateTo, 
                                           src = "Finam", auto.assign = FALSE)[, 4])
    market.returns <- na.omit(diff(market.prices) / market.prices[1 : (length(market.prices) - 1)])
    m.return <- (market.prices[length(market.prices)] - market.prices[1]) / market.prices[1] * 100
    
    withProgress(message = "Stock Data Loading", value = 0, {
      asset.prices <- sapply(t(stocks), 
                           function(x) {
                             incProgress(1 / nrow(stocks), detail = x)
                             as.numeric(getSymbols(as.character(x), from = input$dateFrom, to = input$dateTo,
                                                   src = "Finam", auto.assign = F)[, 4])
                           }, 
                           simplify=FALSE, USE.NAMES=TRUE)
    
      stocks.df <- data.frame(ticker = names(asset.prices), beta = rep(NA, nrow(stocks)), expected.return = rep(NA, nrow(stocks)), 
                            return = rep(NA, nrow(stocks)), alpha = rep(NA, nrow(stocks)), r2 = rep(NA, nrow(stocks)),
                            sortino = rep(NA, nrow(stocks)))
    })
    
    withProgress(message = "Price Data Analyzing", value = 0, {
      stocks.df[, c("beta", "expected.return", "return", 
                    "alpha", "r2", "sortino")] <- t(as.data.frame(
        lapply(asset.prices, 
             function(x){
               incProgress(1 / nrow(stocks))
               asset.returns <- na.omit(diff(x) / x[1 : (length(x) - 1)])
               beta = cov(asset.returns, market.returns) / var(market.returns)
               
               lm.df = data.frame(market = market.prices, asset = x)
               lm.fit = lm(formula = market~asset, data = lm.df)
               r2 = summary(lm.fit)$adj.r.squared
               
               expected.return = rf.rate + beta * (m.return - rf.rate)
               stock.return = (x[length(x)] - x[1]) / x[1] * 100
               alpha = stock.return - expected.return
               sortino = SortinoRatio(R = asset.returns)[, 1]
               round(c(beta, expected.return, stock.return, alpha, r2, sortino), 4)
             }
        )
      ))
    })
    
    step = 0.1
    sml <- rf.rate + seq(0, 2.5, by = step) * (m.return - rf.rate)
    slope = (sml[2] - sml[1]) / step
    list(stocks.df, rf.rate, slope, market.prices, asset.prices, stocks)
  })

  output$stocksTable <- DT::renderDataTable({
    stocks.df <- generalData()[[1]]
    stocks.df#[, c("ticker", "beta", "alpha", "r2")]
  }, server = TRUE, selection = "single",
  options = list(rowCallback = JS(
    'function(row, data) {
        if (data[5] > 0)
          $("td", row).css("color", "green");
        else if (data[5] < 0)
          $("td", row).css("color", "red");
      }'
  ), paging = FALSE, searching = FALSE, processing = FALSE))
  
  output$smlPlot <- plotly::renderPlotly({
    stocks <- generalData()[[6]]
    stocks.df <- generalData()[[1]]
    rf.rate <- generalData()[[2]]
    slope <- generalData()[[3]]
    
    ggplotly(ggplot(stocks.df, show_guide = F, aes(x=stocks.df$beta, y=stocks.df$return, colour = factor(stocks.df$ticker))) + 
      geom_abline(intercept = rf.rate, slope = slope, size = 1.25, colour='black') +
      geom_point(size=stocks.df$r2 * 3) +
      geom_text(show.legend = FALSE, label = stocks$ticker, vjust = 0, nudge_y = (max(stocks.df$return) - min(stocks.df$return)) / 100) +
      xlab('Beta') + ylab('Return, %') + ggtitle('Stock Market Line') + 
      theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)))
  })
  
  output$alphaHist <- plotly::renderPlotly({
    stocks.df <- generalData()[[1]]
    ggplotly(ggplot(stocks.df, aes(x=alpha)) + 
      geom_histogram(aes(y=..density..),
                     binwidth=.5,
                     colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666") +
      xlab('Alpha') + ylab('Density') + ggtitle('Alpha distribution') + 
      theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)))
  })
  
  output$assetsPlot <- plotly::renderPlotly({
    stocks <- generalData()[[6]]
    row_ind <- input$stocksTable_rows_selected[1]
    
    if(!is.null(row_ind)){
      ticker <- stocks[row_ind, "ticker"]
      data <- generalData()
      stock.prices <- data[[5]][[ticker]]
      market.prices <- data[[4]]
      pair.df <- data.frame(Stock = stock.prices, Market =  market.prices)
      
      ay <- list(
        tickfont = list(color = "black"),
        overlaying = "y",
        side = "right"
      )

      plot_ly() %>%
        add_lines(x = ~1 : nrow(pair.df), y = pair.df$Stock, name = ticker) %>%
        add_lines(x = ~1 : nrow(pair.df), y = pair.df$Market, name = "Market", yaxis = "y2") %>%
        layout(
          font = list(color = "black"),
          title = "Stock & Market prices", yaxis2 = ay,
          xaxis = list(title="Days")
        )
    } else plotly_empty()
  })
  
  
  output$scatterPlot <- plotly::renderPlotly({
    stocks <- generalData()[[6]]
    stocks.df <- generalData()[[1]]
    row_ind <- input$stocksTable_rows_selected[1]
    
    if(!is.null(row_ind)){
      ticker <- stocks[row_ind, "ticker"]
      data <- generalData()
      stock.prices <- data[[5]][[ticker]]
      market.prices <- data[[4]]
      pair.df <- data.frame(Stock = stock.prices, Market =  market.prices)
      
      ggplotly(ggplot(pair.df, aes(x=Stock, y=Market)) +
        geom_point(shape=1) + geom_smooth(method = 'loess') +
        ylab(label="Market") + xlab(ticker) + ggtitle("Stock & Market scatter plot") +
        theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)))
    } else plotly_empty()
  })
  
  observeEvent(input$last1Button, {
    value.from <- seq(Sys.Date(), length=2, by = "-29 days")[2]
    value.to <- Sys.Date()
    updateDateInput(session, "dateFrom", value=value.from)
    updateDateInput(session, "dateTo", value=value.to)
  })
  
  observeEvent(input$last3Button, {
    value.from <- seq(Sys.Date(), length=2, by = "-89 days")[2]
    value.to <- Sys.Date()
    updateDateInput(session, "dateFrom", value=value.from)
    updateDateInput(session, "dateTo", value=value.to)
  })
  
  observeEvent(input$last6Button, {
    value.from <- seq(Sys.Date(), length=2, by = "-181 days")[2]
    value.to <- Sys.Date()
    updateDateInput(session, "dateFrom", value=value.from)
    updateDateInput(session, "dateTo", value=value.to)
  })
  
  observeEvent(input$lastYearButton, {
    value.from <- seq(Sys.Date(), length=2, by = "-364 days")[2]
    value.to <- Sys.Date()
    updateDateInput(session, "dateFrom", value=value.from)
    updateDateInput(session, "dateTo", value=value.to)
  })
}