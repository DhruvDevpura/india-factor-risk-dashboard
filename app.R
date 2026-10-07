library(shiny)
library(bslib) #the top navigation bar, cards and number tiles
library(ggplot2)
library(patchwork)

source("charts.R") #loads the data and every chart function

all_sectors = sort(unique(stock_info$Industry))
all_stocks = sort(stock_info$name)

#preset portfolios for the Portfolio page
it_names = c("TCS","INFY","WIPRO","HCLTECH","TECHM")
bank_names = c("HDFCBANK","ICICIBANK","SBIN","AXISBANK","KOTAKBANK")

#----------------------------------------------------------------
#the look: dark bar on top, light page, white cards

dash_theme = bs_theme(version = 5,
                      bg = "#F4F6F9",fg = "#1F2A36",primary = "#2F3E4E",
                      base_font = font_google("Inter",local = FALSE), #loaded by the browser, nothing to install
                      "card-bg" = "#FFFFFF")

dash_css = "
body .navbar.navbar-default { background-color: #1F2A36 !important; border-bottom: 3px solid #E07B39; }
body .navbar .navbar-brand { color: #FFFFFF !important; font-weight: 700; }
body .navbar .nav-link { color: #C9D1D9 !important; border-radius: 6px; margin: 0 2px; }
body .navbar .nav-link:hover { color: #FFFFFF !important; }
body .navbar .nav-link.active { color: #FFFFFF !important; background-color: #E07B39 !important; }
.page-head { background: #FFFFFF; border-radius: 10px; padding: 16px 20px; margin-bottom: 16px; }
.page-head h4 { font-weight: 700; margin-bottom: 4px; }
.page-head p { color: #5C6773; margin: 0; }
.bslib-value-box { border-left: 5px solid #2F3E4E !important; }
.vb-blue { border-left-color: #5B8DB8 !important; }
.vb-orange { border-left-color: #E07B39 !important; }
.vb-green { border-left-color: #2E8B57 !important; }
.weights-note { color: #5C6773; font-size: 0.85rem; }
.sidebar .weights-note { color: #AEB7C2; }
.sidebar .sidebar-title { color: #FFFFFF; font-weight: 700; }
.bslib-value-box .value-box-area { padding: 10px 14px !important; }
.bslib-value-box .value-box-title { font-size: 0.85rem; margin-bottom: 2px; }
.bslib-value-box .value-box-value { font-size: 1.6rem; margin-bottom: 2px; }
.bslib-value-box p { font-size: 0.78rem; color: #5C6773; margin: 0; }
.weight-row { display: flex; align-items: center; justify-content: space-between; margin-bottom: 4px; }
.weight-row .form-group { margin-bottom: 0; }
.weight-row input { height: 32px; padding: 2px 8px; }
"

#title and one line under it, at the top of every page
page_head = function(title,text)
{
  div(class = "page-head",h4(title),p(text))
}

#----------------------------------------------------------------
#what the page looks like

ui = page_navbar(
  title = "Indian factor risk",
  theme = dash_theme,
  fillable = FALSE, #pages scroll instead of squeezing charts into one screen
  header = tags$head(tags$style(HTML(dash_css))),
  
  nav_panel("Introduction",icon = icon("house"),
            page_head("Factor risk in Indian large caps","46 NIFTY 50 stocks, daily, January 2020 to December 2025."),
            layout_columns(fill = FALSE,
                           value_box("Stocks",46,p("NIFTY 50 constituents with full history"),showcase = icon("building-columns")),
                           value_box("Trading days","1,485",p("January 2020 to December 2025"),showcase = icon("calendar-days"),class = "vb-blue"),
                           value_box("Factors",4,p("Market, size, value, momentum (IIM-A)"),showcase = icon("layer-group"),class = "vb-orange"),
                           value_box("Portfolios tested","348",p("Forecasts checked on 2024 and 2025"),showcase = icon("flask"),class = "vb-green")),
            card(card_body(p("Introduction text goes here.")))),
  
  nav_panel("Stocks and sectors",icon = icon("table-cells"),
            page_head("How each stock moves with the factors",
                      "Left: loading on each factor. Right: how much of each stock's daily movement the factors explain."),
            layout_sidebar(fillable = FALSE,
                           sidebar = sidebar(title = "Show",width = 280,bg = "#1F2A36", #dark panel: this is where you choose
                                             radioButtons("pick_mode",NULL,choices = c("All stocks","By sector","By stocks")),
                                             conditionalPanel("input.pick_mode == 'By sector'", #only shown in this mode
                                                              selectizeInput("pick_sectors","Sectors",choices = all_sectors,
                                                                             selected = "Information Technology",multiple = TRUE,
                                                                             options = list(plugins = list("remove_button")))), #x on each chip
                                             conditionalPanel("input.pick_mode == 'By stocks'",
                                                              selectizeInput("pick_stocks","Stocks",choices = all_stocks,
                                                                             selected = c("TCS","INFY","SBIN","TATASTEEL"),multiple = TRUE,
                                                                             options = list(plugins = list("remove_button"))))),
                           plotOutput("stock_chart",height = "auto"))),
  
  nav_panel("Factor risk over time",icon = icon("chart-line"),
            page_head("How risky each factor was, year by year",
                      "Volatility over the past 252 trading days, recalculated every day."),
            card(plotOutput("vol_panels",height = "320px")),
            card(plotOutput("vol_combined",height = "480px"))),
  
  nav_panel("Out-of-sample check",icon = icon("bullseye"),
            page_head("Did the predicted risk match what happened?",
                      "The model is built on earlier years only, then compared with the risk each portfolio actually showed."),
            card(radioButtons("half_life","Half life (trading days)",
                              choices = c("21","42","63","126","252","504","No decay" = "Inf"),
                              selected = "252",inline = TRUE)),
            card(plotOutput("backtest_chart",height = "560px")),
            card(plotOutput("dumbbell_chart",height = "360px")),
            card(plotOutput("error_chart",height = "420px")),
            card(card_body(p(strong("Chosen using 2024, the best half life was 252 days (6.7% error)."),
                             "Carried into 2025, the same choice gave 16.4%. A half life of 21 days would",
                             "have given 6.9% in 2025, but that was only knowable after 2025 had happened.")))),
  
  nav_panel("Portfolio",icon = icon("briefcase"),
            page_head("Build a portfolio and see its risk",
                      "Pick stocks and weights. Every number uses January 2020 to December 2025."),
            layout_sidebar(fillable = FALSE,
                           sidebar = sidebar(title = "Build it",width = 300,bg = "#1F2A36",
                                             p("Presets"),
                                             div(actionButton("preset_it","IT basket",class = "btn-sm btn-outline-light"),
                                                 actionButton("preset_banks","Banks",class = "btn-sm btn-outline-light"),
                                                 actionButton("preset_all","All 46",class = "btn-sm btn-outline-light")),
                                             selectizeInput("port_stocks","Stocks",choices = all_stocks,
                                                            selected = it_names,multiple = TRUE,
                                                            options = list(plugins = list("remove_button"))),
                                             p(class = "weights-note","Weights: share of the money in each stock. Any numbers work, they are rescaled: 3, 1, 1 means 60%, 20%, 20%."),
                                             uiOutput("weight_inputs"),
                                             textOutput("weights_used",container = function(...) p(class = "weights-note",...))),
                           layout_columns(fill = FALSE,
                                          value_box("Total volatility",textOutput("vb_total"),p("per year")),
                                          value_box("Market beta",textOutput("vb_beta"),p("1 moves like the market"),class = "vb-blue"),
                                          value_box("Risk from factors",textOutput("vb_factor_share"),p("share of the variance"),class = "vb-green"),
                                          value_box("Hidden risk",textOutput("vb_hidden"),
                                                    p("missed if stocks' own news is assumed unrelated"),class = "vb-orange")),
                           navset_card_tab( #two tabs inside the page, risk first
                             nav_panel("Risk",
                                       plotOutput("range_chart",height = "380px"),
                                       plotOutput("risk_chart",height = "340px")),
                             nav_panel("Returns",
                                       tableOutput("returns_table"),
                                       p(class = "weights-note",
                                         "NIFTY 50 is the price index without dividends, so its return is understated by roughly its dividend yield each year.",
                                         "All 46 stocks are today's NIFTY 50 members, so every one of them survived; that flatters the portfolio."),
                                       plotOutput("growth_chart",height = "340px"),
                                       plotOutput("drawdown_chart",height = "300px"),
                                       plotOutput("monthly_chart",height = "320px"))))),
  
  nav_panel("Conclusion",icon = icon("flag-checkered"),
            card(card_body(p("Conclusion text goes here."))))
)

#----------------------------------------------------------------
#what the app computes

server = function(input,output,session)
{
  #Stocks and sectors page: reruns whenever the mode, sectors or stocks change
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
  
  output$dumbbell_chart = renderPlot(
    {
      make_dumbbell_plot(as.numeric(input$half_life))
    },res = 96)
  
  output$error_chart = renderPlot(
    {
      make_error_plot(as.numeric(input$half_life))
    },res = 96)
  
  #Portfolio page: presets fill the stock picker, the weights reset to equal
  observeEvent(input$preset_it,updateSelectizeInput(session,"port_stocks",selected = it_names))
  observeEvent(input$preset_banks,updateSelectizeInput(session,"port_stocks",selected = bank_names))
  observeEvent(input$preset_all,updateSelectizeInput(session,"port_stocks",selected = all_stocks))
  
  #one weight box per chosen stock, starting at 1 each (equal weights)
  output$weight_inputs = renderUI(
    {
      stocks = input$port_stocks
      if(length(stocks) == 0)
      {
        return(NULL)
      }
      boxes = list()
      for(i in seq_along(stocks))
      {
        id = paste0("w_",stocks[i])
        old = isolate(input[[id]]) #keep a weight already typed when another stock is added
        if(is.null(old))
        {
          old = 1
        }
        boxes[[i]] = div(class = "weight-row",span(stocks[i]),
                         numericInput(id,NULL,value = old,min = 0,step = 1,width = "90px"))
      }
      boxes
    })
  
  #weights as typed, rescaled so they add up to 1
  port_weights = reactive(
    {
      stocks = input$port_stocks
      w = rep(NA,length(stocks))
      for(i in seq_along(stocks))
      {
        typed = input[[paste0("w_",stocks[i])]]
        if(is.null(typed)) #box not drawn yet, use an equal share
        {
          w[i] = 1
        }
        else if(is.na(typed)) #box left empty
        {
          w[i] = 0
        }
        else
        {
          w[i] = max(typed,0)
        }
      }
      if(sum(w) == 0)
      {
        w = rep(1,length(stocks))
      }
      w/sum(w)
    })
  
  port_tickers = reactive(
    {
      paste0(input$port_stocks,".NS")
    })
  
  port_result = reactive(
    {
      validate(need(length(input$port_stocks) > 0,"Pick at least one stock."))
      portfolio_risk(port_tickers(),port_weights())
    })
  
  output$weights_used = renderText(
    {
      req(length(input$port_stocks) > 0)
      paste0("Weights used: ",
             paste0(input$port_stocks," ",round(100*port_weights()),"%",collapse = ", "))
    })
  
  output$vb_total = renderText(scales::percent(port_result()$total_vol,accuracy = 0.1))
  output$vb_beta = renderText(sprintf("%.2f",port_result()$exposure[1]))
  output$vb_factor_share = renderText(scales::percent(port_result()$factor_vol^2/port_result()$total_vol^2,accuracy = 1))
  output$vb_hidden = renderText(sprintf("%+.1f pts",100*(port_result()$total_vol - port_result()$total_vol_indep))) #%+ keeps the sign
  
  output$returns_table = renderTable(
    {
      validate(need(length(input$port_stocks) > 0,"Pick at least one stock."))
      returns_table(port_tickers(),port_weights())
    },striped = TRUE,hover = TRUE,width = "100%",align = "lrrrrrr")
  
  output$growth_chart = renderPlot(
    {
      validate(need(length(input$port_stocks) > 0,"Pick at least one stock."))
      make_growth_plot(port_tickers(),port_weights())
    },res = 96)
  
  output$drawdown_chart = renderPlot(
    {
      validate(need(length(input$port_stocks) > 0,"Pick at least one stock."))
      make_drawdown_plot(port_tickers(),port_weights())
    },res = 96)
  
  output$monthly_chart = renderPlot(
    {
      validate(need(length(input$port_stocks) > 0,"Pick at least one stock."))
      make_monthly_plot(port_tickers(),port_weights())
    },res = 96)
  
  output$range_chart = renderPlot(
    {
      validate(need(length(input$port_stocks) > 0,"Pick at least one stock."))
      make_exposure_range_plot(port_tickers(),port_weights())
    },res = 96)
  
  output$risk_chart = renderPlot(
    {
      validate(need(length(input$port_stocks) > 0,"Pick at least one stock."))
      make_risk_plot(port_tickers(),port_weights())
    },res = 96)
}

shinyApp(ui,server)