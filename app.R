library(shiny)
library(shinydashboard)
library(ggplot2)

source("charts.R") #loads the data and builds every chart object and function

#Page layout ---------------------------------------------------------------

ui = dashboardPage(
  dashboardHeader(title = "Indian factor risk"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("Factor exposures", tabName = "exposures"),
      menuItem("Risk split", tabName = "split"),
      menuItem("Factor risk over time", tabName = "vol"),
      menuItem("Out-of-sample check", tabName = "backtest"),
      menuItem("Portfolio", tabName = "portfolio"),
      menuItem("Conclusion", tabName = "conclusion")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro", h2("Introduction"), p("Coming soon.")),
      tabItem(tabName = "exposures", h2("Factor exposures"), p("Coming soon.")),
      tabItem(tabName = "split", h2("Risk split"), p("Coming soon.")),
      tabItem(tabName = "vol", h2("Factor risk over time"), p("Coming soon.")),
      tabItem(tabName = "backtest",
              h2("Out-of-sample check"),
              plotOutput("error_chart", height = "450px")),
      tabItem(tabName = "portfolio", h2("Portfolio"), p("Coming soon.")),
      tabItem(tabName = "conclusion", h2("Conclusion"), p("Coming soon."))
    )
  )
)

#What the app computes -----------------------------------------------------

server = function(input, output)
{
  output$error_chart = renderPlot(
    {
      error_plot
    })
}

shinyApp(ui, server)