library(shiny)
library(shinydashboard)
library(ggplot2)
library(patchwork)

source("charts.R") #loads the data and every chart function

all_sectors = sort(unique(stock_info$Industry))
all_stocks = sort(stock_info$name)

#----------------------------------------------------------------
#what the page looks like

ui = dashboardPage(
  dashboardHeader(title = "Indian factor risk"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",tabName = "intro"),
      menuItem("Stocks and sectors",tabName = "stocks"),
      menuItem("Factor risk over time",tabName = "vol"),
      menuItem("Out-of-sample check",tabName = "backtest"),
      menuItem("Portfolio",tabName = "portfolio"),
      menuItem("Conclusion",tabName = "conclusion")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",h2("Introduction"),p("Coming soon.")),
      
      tabItem(tabName = "stocks",
              fluidRow(
                box(width = 3,title = "Show",solidHeader = TRUE,status = "primary",
                    radioButtons("pick_mode",NULL,choices = c("All stocks","By sector","By stocks")),
                    conditionalPanel("input.pick_mode == 'By sector'", #only shown in this mode
                                     selectizeInput("pick_sectors","Sectors",choices = all_sectors,
                                                    selected = "Information Technology",multiple = TRUE,
                                                    options = list(plugins = list("remove_button")))), #x on each chip
                    conditionalPanel("input.pick_mode == 'By stocks'",
                                     selectizeInput("pick_stocks","Stocks",choices = all_stocks,
                                                    selected = c("TCS","INFY","SBIN","TATASTEEL"),multiple = TRUE,
                                                    options = list(plugins = list("remove_button"))))),
                box(width = 9,plotOutput("stock_chart",height = "auto"))
              )),
      
      tabItem(tabName = "vol",
              fluidRow(box(width = 12,plotOutput("vol_panels",height = "320px"))),
              fluidRow(box(width = 12,plotOutput("vol_combined",height = "480px")))),
      
      tabItem(tabName = "backtest",
              fluidRow(
                box(width = 12,
                    radioButtons("half_life","Half life (trading days)",
                                 choices = c("21","42","63","126","252","504","No decay" = "Inf"),
                                 selected = "252",inline = TRUE))),
              fluidRow(box(width = 12,plotOutput("backtest_chart",height = "560px"))),
              fluidRow(box(width = 12,plotOutput("error_chart",height = "420px"))),
              fluidRow(box(width = 12,plotOutput("dumbbell_chart",height = "360px"))),
              fluidRow(
                box(width = 12,
                    p(strong("Chosen using 2024, the best half life was 252 days (6.7% error)."),
                      "Carried into 2025, the same choice gave 16.4%. A half life of 21 days would",
                      "have given 6.9% in 2025, but that was only knowable after 2025 had happened.")))),
      
      tabItem(tabName = "portfolio",h2("Portfolio"),p("Coming soon.")),
      tabItem(tabName = "conclusion",h2("Conclusion"),p("Coming soon."))
    )
  )
)

#----------------------------------------------------------------
#what the app computes

server = function(input,output,session)
{
  #reruns whenever the mode, sectors or stocks change
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
        paste0(input$pick_stocks,".NS")
      }
    })
  
  #chart height grows with the number of rows shown
  stock_chart_height = function()
  {
    n_stocks = length(chosen_tickers())
    n_sectors = length(unique(stock_info$Industry[stock_info$ticker %in% chosen_tickers()]))
    max(300,130 + 28*n_stocks + 14*n_sectors)
  }
  
  output$stock_chart = renderPlot(
    {
      validate(need(length(chosen_tickers()) > 0,"Pick at least one sector or stock.")) #message instead of an error
      make_stock_plot(chosen_tickers())
    },height = stock_chart_height,res = 96) #res 96 gives bigger text and smoother lines than the default 72
  
  output$vol_panels = renderPlot(
    {
      make_vol_panels()
    },res = 96)
  
  output$vol_combined = renderPlot(
    {
      make_vol_combined()
    },res = 96)
  
  output$backtest_chart = renderPlot(
    {
      make_backtest_plot(as.numeric(input$half_life)) #"Inf" becomes Inf
    },res = 96)
  
  output$error_chart = renderPlot(
    {
      make_error_plot(as.numeric(input$half_life))
    },res = 96)
  
  output$dumbbell_chart = renderPlot(
    {
      make_dumbbell_plot(as.numeric(input$half_life))
    },res = 96)
}

shinyApp(ui,server)