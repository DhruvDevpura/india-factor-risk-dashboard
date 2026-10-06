library(shiny)
library(shinydashboard)
library(ggplot2)
library(patchwork)

source("charts.R") #loads the data and builds every chart object and function

all_sectors = sort(unique(stock_info$Industry))
all_stocks = sort(stock_info$name)

#Page layout ---------------------------------------------------------------

ui = dashboardPage(
  dashboardHeader(title = "Indian factor risk"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("Stocks and sectors", tabName = "stocks"),
      menuItem("Factor risk over time", tabName = "vol"),
      menuItem("Out-of-sample check", tabName = "backtest"),
      menuItem("Portfolio", tabName = "portfolio"),
      menuItem("Conclusion", tabName = "conclusion")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro", h2("Introduction"), p("Coming soon.")),
      
      tabItem(tabName = "stocks",
              fluidRow(
                box(width = 3, title = "Show", solidHeader = TRUE, status = "primary",
                    radioButtons("pick_mode", NULL,
                                 choices = c("All stocks", "By sector", "By stocks")),
                    conditionalPanel("input.pick_mode == 'By sector'",
                                     selectizeInput("pick_sectors", "Sectors", choices = all_sectors,
                                                    selected = "Information Technology", multiple = TRUE,
                                                    options = list(plugins = list("remove_button")))),
                    conditionalPanel("input.pick_mode == 'By stocks'",
                                     selectizeInput("pick_stocks", "Stocks", choices = all_stocks,
                                                    selected = c("TCS", "INFY", "SBIN", "TATASTEEL"),
                                                    multiple = TRUE,
                                                    options = list(plugins = list("remove_button"))))),
                box(width = 9,
                    plotOutput("stock_chart", height = "auto"))
              )),
      
      tabItem(tabName = "vol",
              box(width = 12, plotOutput("vol_chart", height = "450px"))),
      
      tabItem(tabName = "backtest",
              box(width = 12,
                  radioButtons("half_life", "Half life (trading days)",
                               choices = c("21", "42", "63", "126", "252", "504", "No decay" = "Inf"),
                               selected = "252", inline = TRUE),
                  radioButtons("model", "Model",
                               choices = c("Full covariance", "Factor + diagonal"), inline = TRUE),
                  plotOutput("backtest_chart", height = "500px")),
              box(width = 12, plotOutput("error_chart", height = "450px"))),
      
      tabItem(tabName = "portfolio", h2("Portfolio"), p("Coming soon.")),
      tabItem(tabName = "conclusion", h2("Conclusion"), p("Coming soon."))
    )
  )
)

#What the app computes -----------------------------------------------------

server = function(input, output, session)
{
  #Which stocks to show on the Stocks and sectors page
  chosen_tickers = reactive(
    {
      if(input$pick_mode == "All stocks")
      {
        stock_info$ticker
      }
      else if(input$pick_mode == "By sector")
      {
        stock_info$ticker[stock_info$Industry %in% input$pick_sectors]
      }
      else
      {
        paste0(input$pick_stocks, ".NS")
      }
    })
  
  #Chart height grows with the number of rows shown
  stock_chart_height = function()
  {
    n_stocks = length(chosen_tickers())
    n_sectors = length(unique(stock_info$Industry[stock_info$ticker %in% chosen_tickers()]))
    max(300, 130 + 28 * n_stocks + 14 * n_sectors)
  }
  
  output$stock_chart = renderPlot(
    {
      validate(need(length(chosen_tickers()) > 0, "Pick at least one sector or stock."))
      make_stock_plot(chosen_tickers())
    }, height = stock_chart_height, res = 96)
  
  output$vol_chart = renderPlot(
    {
      vol_plot
    }, res = 96)
  
  output$backtest_chart = renderPlot(
    {
      make_backtest_plot(as.numeric(input$half_life), input$model)
    }, res = 96)
  
  output$error_chart = renderPlot(
    {
      error_plot
    }, res = 96)
}

shinyApp(ui, server)