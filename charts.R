library(ggplot2)
library(patchwork) #puts two charts side by side with rows lined up

estimates = readRDS("data/estimates.rds")
nifty_list = read.csv("data_raw/ind_nifty50list.csv")
panel = readRDS("data/panel.rds")
backtest = readRDS("data/backtest.rds")
summary_multi = readRDS("data/summary_multi.rds")
nifty_list$ticker = paste0(nifty_list$Symbol,".NS")

#same look for every chart
theme_dash = function(base_size = 13)
{
  theme_minimal(base_size = base_size) +
    theme(plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          plot.subtitle = element_text(colour = "grey35"),
          panel.grid.minor = element_blank())
}

#----------------------------------------------------------------
#Stocks and sectors page

stock_info = merge(estimates$loadings,nifty_list[,c("ticker","Industry")],by = "ticker")
stock_info$name = sub(".NS","",stock_info$ticker,fixed = TRUE)
stock_info$market_share = stock_info$r2_mkt
stock_info$other_share = stock_info$r2_4f - stock_info$r2_mkt
stock_info$own_share = 1 - stock_info$r2_4f

make_stock_plot = function(chosen_tickers)
{
  info = stock_info[stock_info$ticker %in% chosen_tickers,]
  info = info[order(info$Industry,info$b_mkt),] #grouped by sector
  info$name = factor(info$name,levels = unique(info$name))
  n = nrow(info)
  
  heat = data.frame(
    name = rep(info$name,4),
    Industry = rep(info$Industry,4),
    factor = factor(rep(c("Market","Size","Value","Momentum"),each = n),
                    levels = c("Market","Size","Value","Momentum")),
    beta = c(info$b_mkt,info$b_smb,info$b_hml,info$b_wml)
  )
  heat$shade = ifelse(heat$factor == "Market",NA,heat$beta) #no shade for the market column
  heat$text_col = ifelse(!is.na(heat$shade) & abs(heat$beta) > 0.45,"white","grey15") #white text on dark cells
  
  heat_plot = ggplot(heat,aes(x = factor,y = name)) +
    geom_tile(aes(fill = shade),colour = "white",linewidth = 1) +
    geom_text(aes(label = sprintf("%.2f",round(beta,2) + 0),colour = text_col),size = 3.6) + #+ 0 turns -0.00 into 0.00
    scale_colour_identity() +
    scale_fill_gradientn(colours = c("#8E1B1B","#D9534F","#F4C7C3","#F7F7F7","#C5E5CD","#4CA36A","#1D6B37"),
                         limits = c(-0.9,0.9),oob = scales::squish, #beyond 0.9 keeps the darkest shade
                         na.value = "#E4E7EB",guide = "none") +
    scale_x_discrete(position = "top") +
    facet_grid(Industry ~ .,scales = "free_y",space = "free_y",switch = "y",
               labeller = label_wrap_gen(width = 18)) +
    labs(title = "Factor loadings",
         subtitle = "Green: moves with the factor. Red: moves against it.",x = NULL,y = NULL) +
    theme_dash() +
    theme(panel.grid = element_blank(),
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0,hjust = 1,face = "bold",colour = "grey30"),
          axis.text.x = element_text(face = "bold"),
          panel.spacing = unit(0.4,"lines"))
  
  split = data.frame(
    name = rep(info$name,3),
    Industry = rep(info$Industry,3),
    part = factor(rep(c("Market","Other three factors","Stock-specific"),each = n),
                  levels = c("Stock-specific","Other three factors","Market")),
    share = c(info$market_share,info$other_share,info$own_share)
  )
  
  split_plot = ggplot(split,aes(x = share,y = name,fill = part)) +
    geom_col(width = 0.75) +
    geom_text(data = info,aes(x = r2_4f,y = name,label = scales::percent(r2_4f,accuracy = 1)),
              inherit.aes = FALSE,hjust = -0.15,size = 3.4,colour = "grey15") +
    scale_fill_manual(values = c("Market" = "#2F3E4E","Other three factors" = "#5B8DB8",
                                 "Stock-specific" = "#E4E7EB")) +
    scale_x_continuous(labels = scales::percent,position = "top",expand = c(0,0)) +
    facet_grid(Industry ~ .,scales = "free_y",space = "free_y") +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(title = "Where daily movement comes from",
         subtitle = "Number: share explained by the four factors",x = NULL,y = NULL,fill = NULL) +
    theme_dash() +
    theme(panel.grid.major.y = element_blank(),
          strip.text = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "bottom",
          panel.spacing = unit(0.4,"lines"))
  
  heat_plot + split_plot + plot_layout(widths = c(1.1,1)) #same rows, so TCS on the left lines up with TCS on the right
}

#----------------------------------------------------------------
#Factor risk over time page

factor_cols = c("Market" = "#2F3E4E","Size" = "#D08C2B","Value" = "#2A9D8F","Momentum" = "#7B5EA7")

vol_long = estimates$factor_vol
vol_long$factor = factor(vol_long$factor,levels = c("mf","smb","hml","wml"),
                         labels = c("Market","Size","Value","Momentum"))

#days where one big day entered or left the 252 day window
vol_events = data.frame(
  date = as.Date(c("2021-03-26","2024-06-04","2025-06-10")),
  label = c("March 2020 crash\nleaves the window",
            "4 June 2024, election\nresult day, enters",
            "...and leaves\none year later")
)

make_vol_panels = function()
{
  ghost = vol_long[,c("date","vol","factor")]
  names(ghost)[3] = "other"
  ghost = merge(ghost,data.frame(factor = levels(vol_long$factor)),by = NULL) #every line copied into every panel
  ghost = ghost[ghost$other != ghost$factor,] #a panel's own factor is drawn in colour, not grey
  ghost$factor = factor(ghost$factor,levels = levels(vol_long$factor))
  
  ggplot(vol_long,aes(x = date,y = vol)) +
    geom_line(data = ghost,aes(group = other),colour = "grey85",linewidth = 0.4) +
    geom_line(aes(colour = factor),linewidth = 0.8) +
    scale_colour_manual(values = factor_cols,guide = "none") +
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(~ factor,nrow = 1) +
    labs(title = "Each factor on its own",
         subtitle = "Coloured line: the factor. Grey lines: the other three, for comparison.",
         x = NULL,y = NULL) +
    theme_dash() +
    theme(strip.text = element_text(face = "bold",size = 12,hjust = 0),
          panel.spacing = unit(1.2,"lines"))
}

make_vol_combined = function()
{
  last_points = vol_long[vol_long$date == max(vol_long$date),]
  last_points = last_points[order(last_points$vol),]
  for(i in 2:nrow(last_points)) #push the end labels apart so they do not overlap
  {
    last_points$vol[i] = max(last_points$vol[i],last_points$vol[i-1] + 0.009)
  }
  
  ggplot(vol_long,aes(x = date,y = vol,colour = factor)) +
    geom_vline(data = vol_events,aes(xintercept = date),colour = "grey60",linetype = "dashed") +
    geom_text(data = vol_events,aes(x = date,y = 0.275,label = label),inherit.aes = FALSE,
              hjust = 0,nudge_x = 15,size = 3.3,colour = "grey30",lineheight = 0.9,vjust = 1) +
    geom_line(linewidth = 0.8) +
    geom_text(data = last_points,aes(label = factor),hjust = -0.15,size = 4,fontface = "bold") +
    scale_colour_manual(values = factor_cols,guide = "none") +
    scale_y_continuous(labels = scales::percent,limits = c(0.07,0.28)) +
    scale_x_date(expand = expansion(mult = c(0.01,0.12))) + #room on the right for the labels
    labs(title = "All four together",
         subtitle = "Volatility over the past year (252 trading days), annualised. Dashed lines: one day entering or leaving the window.",
         x = NULL,y = NULL) +
    theme_dash()
}

#----------------------------------------------------------------
#Out-of-sample check page

type_cols = c("Single stock" = "#C9CED4","Random" = "#8FA9C2","Equal weight" = "#6A4C93","IT basket" = "#E07B39")
model_cols = c("Full covariance" = "#2F3E4E","Factor + diagonal" = "#5B8DB8")

year_names = c("2024" = "Predicting 2024 (built on 2020 to 2023)",
               "2025" = "Predicting 2025 (built on 2020 to 2024)")

summary_multi$hl_label = factor(ifelse(is.infinite(summary_multi$half_life),"No decay",
                                       as.character(summary_multi$half_life)),
                                levels = c("21","42","63","126","252","504","No decay"))
summary_multi$panel = year_names[as.character(summary_multi$test_year)]

make_backtest_plot = function(chosen_hl)
{
  one = backtest[backtest$half_life == chosen_hl & backtest$model == "Full covariance",] #the model comparison is in the dumbbell chart
  one$type = factor(one$type,levels = names(type_cols))
  one$panel = year_names[as.character(one$test_year)]
  big = one[one$type %in% c("Equal weight","IT basket"),] #drawn larger and labelled
  
  ggplot(one,aes(x = predicted,y = actual,colour = type)) +
    geom_abline(slope = 1,intercept = 0,colour = "grey50",linetype = "dashed") +
    geom_point(data = one[!(one$type %in% c("Equal weight","IT basket")),],alpha = 0.8,size = 1.8) +
    geom_point(data = big,size = 4.5) +
    geom_text(data = big,aes(label = type),hjust = -0.2,size = 3.6,fontface = "bold",show.legend = FALSE) +
    scale_colour_manual(values = type_cols) +
    scale_x_continuous(labels = scales::percent,limits = c(0.08,0.60)) +
    scale_y_continuous(labels = scales::percent,limits = c(0.08,0.60)) +
    facet_wrap(~ panel) +
    coord_equal() +
    labs(title = "Predicted against actual risk",
         subtitle = "One dot per portfolio. Dashed line: perfect prediction. Above it: risk was under-predicted.",
         x = "Predicted volatility",y = "Actual volatility",colour = NULL) +
    theme_dash() +
    theme(legend.position = "top",
          strip.text = element_text(face = "bold",size = 12),
          panel.border = element_rect(colour = "grey80",fill = NA),
          panel.spacing = unit(1.5,"lines"))
}

make_error_plot = function(chosen_hl)
{
  chosen_label = ifelse(is.infinite(chosen_hl),"No decay",as.character(chosen_hl))
  chosen_pos = which(levels(summary_multi$hl_label) == chosen_label)
  picked = summary_multi[summary_multi$hl_label == chosen_label,]
  
  ggplot(summary_multi,aes(x = hl_label,y = ape,colour = model,group = model)) +
    geom_blank() + #sets up the half life axis before the shading, otherwise R errors
    annotate("rect",xmin = chosen_pos - 0.4,xmax = chosen_pos + 0.4,ymin = -Inf,ymax = Inf,fill = "grey92") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.8) +
    geom_label(data = picked,aes(label = scales::percent(ape,accuracy = 0.1),
                                 vjust = ifelse(model == "Full covariance",-0.6,1.6)),
               hjust = -0.2,size = 3.6,fontface = "bold",show.legend = FALSE,
               fill = "white",label.size = 0) + #white box so the line does not run through the number
    scale_colour_manual(values = model_cols) +
    scale_y_continuous(labels = scales::percent,limits = c(0,0.5)) +
    facet_wrap(~ panel) +
    labs(title = "Average error by half life",
         subtitle = "302 portfolios of two or more stocks. Shaded: the half life chosen above.",
         x = "Half life (trading days)",y = "Mean absolute percentage error",colour = NULL) +
    theme_dash() +
    theme(legend.position = "top",
          strip.text = element_text(face = "bold",size = 12),
          panel.border = element_rect(colour = "grey80",fill = NA),
          panel.spacing = unit(1.5,"lines"))
}

#the two models side by side for the portfolios where it matters, against what actually happened
make_dumbbell_plot = function(chosen_hl)
{
  one = backtest[backtest$half_life == chosen_hl & backtest$portfolio %in% c("IT basket","Equal weight"),]
  one$row = paste(one$portfolio,one$test_year)
  
  actual = unique(one[,c("row","portfolio","test_year","actual")]) #actual is the same under both models
  marks = rbind(
    data.frame(row = one$row,portfolio = one$portfolio,vol = one$predicted,what = one$model),
    data.frame(row = actual$row,portfolio = actual$portfolio,vol = actual$actual,what = "Actual")
  )
  marks$what = factor(marks$what,levels = c("Actual","Full covariance","Factor + diagonal"))
  marks$row = factor(marks$row,levels = rev(c("IT basket 2024","IT basket 2025","Equal weight 2024","Equal weight 2025")))
  
  ends = aggregate(vol ~ row,data = marks,FUN = range) #the line runs from the lowest mark to the highest
  ends = data.frame(row = ends$row,low = ends$vol[,1],high = ends$vol[,2])
  
  ggplot(marks,aes(x = vol,y = row)) +
    geom_segment(data = ends,aes(x = low,xend = high,y = row,yend = row),colour = "grey80",linewidth = 2) +
    geom_point(aes(colour = what,shape = what),size = 5) +
    geom_text(data = marks[marks$what != "Actual",],
              aes(label = scales::percent(vol,accuracy = 0.1),colour = what,
                  vjust = ifelse(what == "Full covariance",-1.3,2.3)),size = 3.4,show.legend = FALSE) + #full above, factor below so close values do not overlap
    geom_text(data = actual,aes(x = Inf,y = paste(portfolio,test_year),
                                label = paste("actual",scales::percent(actual,accuracy = 0.1))),
              hjust = 1,size = 3.6,fontface = "bold") +
    scale_colour_manual(values = c("Actual" = "black","Full covariance" = "#2F3E4E","Factor + diagonal" = "#5B8DB8")) +
    scale_shape_manual(values = c("Actual" = 18,"Full covariance" = 16,"Factor + diagonal" = 16)) +
    scale_x_continuous(labels = scales::percent,expand = expansion(mult = c(0.08,0.25))) +
    labs(title = "Where the two models differ",
         subtitle = "Diamond: what actually happened. IT basket: one sector. Equal weight: all 46 stocks.",
         x = "Volatility",y = NULL,colour = NULL,shape = NULL) +
    theme_dash() +
    theme(legend.position = "top",
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold",size = 12))
}

#----------------------------------------------------------------
#Portfolio page

#a portfolio's factor exposures and its risk split,
#using the full residual covariance and, for comparison, independent residuals.
portfolio_risk = function(chosen_stocks,chosen_weights)
{
  w_full = rep(0,46)
  w_full[match(chosen_stocks,estimates$loadings$ticker)] = chosen_weights
  
  B = as.matrix(estimates$loadings[,c("b_mkt","b_smb","b_hml","b_wml")])
  exposure = t(B) %*% w_full
  
  fac_rows = panel[panel$ticker == estimates$loadings$ticker[1],]
  fac_rows = fac_rows[order(fac_rows$date),]
  factor_cov = cov(fac_rows[,c("mf","smb","hml","wml")])
  
  resid_cov = cov(estimates$resid)
  
  factor_var = as.numeric(t(w_full) %*% B %*% factor_cov %*% t(B) %*% w_full) #risk from factors
  
  specific_var = as.numeric(t(w_full) %*% resid_cov %*% w_full) #stock-specific risk, full residual covariance
  
  specific_var_indep = as.numeric(t(w_full) %*% diag(diag(resid_cov)) %*% w_full) #same, if residuals were independent
  
  list(exposure = exposure,
       factor_vol = sqrt(factor_var * 252),
       specific_vol = sqrt(specific_var * 252),
       total_vol = sqrt((factor_var + specific_var) * 252),
       total_vol_indep = sqrt((factor_var + specific_var_indep) * 252))
}

#the portfolio's loading on each factor, year by year (weighted sum of the stocks' yearly loadings)
portfolio_yearly = function(chosen_stocks,chosen_weights)
{
  yearly = estimates$yearly
  years = sort(unique(yearly$year))
  factors = c("mf","smb","hml","wml")
  
  out = data.frame(year = rep(years,each = 4),
                   factor = rep(factors,length(years)),
                   loading = NA)
  
  for(k in 1:nrow(out))
  {
    rows = yearly[yearly$year == out$year[k] & yearly$factor == out$factor[k],]
    est = rows$est[match(chosen_stocks,rows$ticker)]
    out$loading[k] = sum(chosen_weights*est)
  }
  out
}


factor_names = c("mf" = "Market","smb" = "Size","hml" = "Value","wml" = "Momentum")
#each factor: one dot per year, a bar from the lowest to the highest year, a big dot for 2020 to 2025
make_exposure_range_plot = function(chosen_stocks,chosen_weights)
{
  full = portfolio_risk(chosen_stocks,chosen_weights)$exposure
  full_data = data.frame(factor = factor(factor_names,levels = rev(factor_names)),loading = as.numeric(full))
  full_data$colour = ifelse(full_data$loading > 0,"with","against")
  
  yearly = portfolio_yearly(chosen_stocks,chosen_weights)
  yearly$factor = factor(factor_names[as.character(yearly$factor)],levels = rev(factor_names))
  
  low = aggregate(loading ~ factor,data = yearly,FUN = min)
  high = aggregate(loading ~ factor,data = yearly,FUN = max)
  ends = merge(low,high,by = "factor",suffixes = c("_low","_high"))
  extremes = yearly[yearly$loading %in% c(low$loading,high$loading),] #the years at each end get a label
  
  ggplot(yearly,aes(x = loading,y = factor)) +
    geom_vline(xintercept = 0,colour = "grey60") +
    geom_segment(data = ends,aes(x = loading_low,xend = loading_high,y = factor,yend = factor),
                 colour = "#E4E7EB",linewidth = 7,lineend = "round") +
    geom_point(colour = "grey45",size = 2.2) +
    geom_text(data = extremes,aes(label = year),vjust = -1.3,size = 3.2,colour = "grey40") +
    geom_point(data = full_data,aes(colour = colour),size = 5.5) +
    geom_text(data = full_data,aes(label = sprintf("%.2f",round(loading,2) + 0),colour = colour),
              vjust = 2.3,size = 3.8,fontface = "bold") +
    scale_colour_manual(values = c("with" = "#2E8B57","against" = "#C0392B"),guide = "none") +
    scale_x_continuous(expand = expansion(mult = 0.08)) +
    labs(title = "Portfolio loading on each factor",
         subtitle = "Big dot: 2020 to 2025 (green with the factor, red against). Small dots: single years.",
         x = "Loading",y = NULL) +
    theme_dash() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold",size = 12))
}

#the portfolio's yearly volatility, and where it comes from
make_risk_plot = function(chosen_stocks,chosen_weights)
{
  result = portfolio_risk(chosen_stocks,chosen_weights)
  risk_data = data.frame(
    part = c("From the four factors","Stock-specific","Total","Total if stocks' own news were unrelated"),
    vol = c(result$factor_vol,result$specific_vol,result$total_vol,result$total_vol_indep),
    look = c("factor","specific","total","unrelated")
  )
  risk_data$part = factor(risk_data$part,levels = rev(risk_data$part)) #keep this order, top to bottom
  
  #the parts combine as squares, not as a plain sum (variances add, volatilities do not)
  note = paste0("Parts combine as squares: ",
                round(100*result$factor_vol,1),"\u00b2 + ",
                round(100*result$specific_vol,1),"\u00b2 = ",
                round(100*result$total_vol,1),"\u00b2. Dashed bar: what the textbook assumption would say.")
  
  ggplot(risk_data,aes(x = vol,y = part)) +
    geom_col(aes(fill = look),width = 0.6) +
    geom_col(data = risk_data[risk_data$look == "unrelated",],fill = NA,colour = "#2F3E4E",
             linetype = "dashed",width = 0.6) + #a hypothetical, so drawn hollow with a dashed outline
    geom_text(aes(label = scales::percent(vol,accuracy = 0.1)),hjust = -0.2,size = 3.8,colour = "grey15") +
    scale_fill_manual(values = c("factor" = "#5B8DB8","specific" = "#C9CED4","total" = "#2F3E4E","unrelated" = "white"),guide = "none") +
    scale_x_continuous(labels = scales::percent,expand = expansion(mult = c(0,0.2))) +
    labs(title = "Portfolio volatility per year, 2020 to 2025",subtitle = note,x = NULL,y = NULL) +
    theme_dash() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 11))
}
#----------------------------------------------------------------
#Portfolio page, returns

month_names = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#each day's return of the portfolio and of the NIFTY 50, weights kept fixed every day
portfolio_daily = function(chosen_stocks,chosen_weights)
{
  dates = sort(unique(panel$date))
  rets = matrix(NA,nrow = length(dates),ncol = length(chosen_stocks))
  
  for(i in 1:length(chosen_stocks))
  {
    one = panel[panel$ticker == chosen_stocks[i],]
    one = one[order(one$date),]
    rets[,i] = one$ret #full return with dividends, not the excess return
  }
  
  nifty = panel[panel$ticker == chosen_stocks[1],] #every stock's rows carry the same NIFTY return and risk-free rate
  nifty = nifty[order(nifty$date),]
  
  data.frame(date = dates,
             port_ret = as.numeric(rets %*% chosen_weights),
             nifty_ret = nifty$nifty_ret,
             rf = nifty$rf)
}

#growth of 100 rupees and the fall from the previous high, every day
portfolio_paths = function(chosen_stocks,chosen_weights)
{
  daily = portfolio_daily(chosen_stocks,chosen_weights)
  daily$port_value = 100*cumprod(1 + daily$port_ret)
  daily$nifty_value = 100*cumprod(1 + daily$nifty_ret)
  daily$port_fall = daily$port_value/pmax(cummax(daily$port_value),100) - 1 #highest value so far, starting from the 100 invested
  daily$nifty_fall = daily$nifty_value/pmax(cummax(daily$nifty_value),100) - 1
  daily
}

#the comparison table: one row for the portfolio, one for the NIFTY 50
returns_table = function(chosen_stocks,chosen_weights)
{
  daily = portfolio_paths(chosen_stocks,chosen_weights)
  n = nrow(daily)
  years = as.numeric(max(daily$date) - min(daily$date))/365.25
  
  out = data.frame(Portfolio = c("Portfolio","NIFTY 50"),
                   Growth = NA,Return = NA,Volatility = NA,Sharpe = NA,Fall = NA,Worst = NA)
  rets = list(daily$port_ret,daily$nifty_ret)
  values = list(daily$port_value,daily$nifty_value)
  falls = list(daily$port_fall,daily$nifty_fall)
  
  for(j in 1:2)
  {
    r = rets[[j]]
    worst = which.min(r)
    out$Growth[j] = sprintf("%.2fx",values[[j]][n]/100)
    out$Return[j] = scales::percent((values[[j]][n]/100)^(1/years) - 1,accuracy = 0.1) #compounded return per year
    out$Volatility[j] = scales::percent(sd(r)*sqrt(252),accuracy = 0.1)
    out$Sharpe[j] = sprintf("%.2f",mean(r - daily$rf)*252/(sd(r)*sqrt(252))) #return above the risk-free rate per unit of risk
    out$Fall[j] = scales::percent(min(falls[[j]]),accuracy = 0.1)
    out$Worst[j] = paste0(scales::percent(r[worst],accuracy = 0.1)," (",format(daily$date[worst],"%d %b %Y"),")")
  }
  names(out) = c("","Growth","Return per year","Volatility per year","Sharpe ratio","Biggest fall","Worst day")
  out
}

make_growth_plot = function(chosen_stocks,chosen_weights)
{
  daily = portfolio_paths(chosen_stocks,chosen_weights)
  lines = data.frame(
    date = rep(daily$date,2),
    value = c(daily$port_value,daily$nifty_value),
    who = factor(rep(c("Portfolio","NIFTY 50"),each = nrow(daily)),levels = c("Portfolio","NIFTY 50"))
  )
  ends = lines[lines$date == max(lines$date),]
  ends$label = paste0(ends$who," Rs ",round(ends$value))
  
  ggplot(lines,aes(x = date,y = value,colour = who)) +
    geom_hline(yintercept = 100,colour = "grey70",linetype = "dashed") +
    geom_line(linewidth = 0.8) +
    geom_text(data = ends,aes(label = label),hjust = -0.08,size = 3.8,fontface = "bold") +
    scale_colour_manual(values = c("Portfolio" = "#2F3E4E","NIFTY 50" = "#E07B39"),guide = "none") +
    scale_y_continuous(labels = function(x) paste0("Rs ",x)) +
    scale_x_date(expand = expansion(mult = c(0.01,0.18))) + #room on the right for the labels
    labs(title = "Growth of Rs 100 invested in January 2020",
         subtitle = "Weights kept fixed every day. NIFTY 50 is the price index, without dividends.",
         x = NULL,y = NULL) +
    theme_dash()
}

make_drawdown_plot = function(chosen_stocks,chosen_weights)
{
  daily = portfolio_paths(chosen_stocks,chosen_weights)
  lines = data.frame(
    date = rep(daily$date,2),
    fall = c(daily$port_fall,daily$nifty_fall),
    who = factor(rep(c("Portfolio","NIFTY 50"),each = nrow(daily)),levels = c("Portfolio","NIFTY 50"))
  )
  
  ggplot(lines,aes(x = date,y = fall,colour = who)) +
    geom_area(data = lines[lines$who == "Portfolio",],fill = "#2F3E4E",alpha = 0.12,colour = NA) +
    geom_line(linewidth = 0.6) +
    geom_hline(yintercept = 0,colour = "grey60") +
    scale_colour_manual(values = c("Portfolio" = "#2F3E4E","NIFTY 50" = "#E07B39")) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Fall from the previous high",
         subtitle = "0% means at a new high. The shaded area is the portfolio.",
         x = NULL,y = NULL,colour = NULL) +
    theme_dash() +
    theme(legend.position = "top")
}

#the portfolio's return in every month, like a calendar
make_monthly_plot = function(chosen_stocks,chosen_weights)
{
  daily = portfolio_daily(chosen_stocks,chosen_weights)
  daily$year = as.numeric(format(daily$date,"%Y"))
  daily$month = as.numeric(format(daily$date,"%m"))
  
  monthly = aggregate(port_ret ~ year + month,data = daily,FUN = function(r) prod(1 + r) - 1) #compound the days of each month
  monthly$month = factor(month_names[monthly$month],levels = month_names)
  monthly$year = factor(monthly$year,levels = rev(sort(unique(monthly$year)))) #2020 at the top, latest year at the bottom
  monthly$label = sprintf("%+.1f",round(100*monthly$port_ret,1) + 0) #+ 0 turns -0.0 into +0.0
  monthly$text_col = ifelse(abs(monthly$port_ret) > 0.06,"white","grey15")
  
  ggplot(monthly,aes(x = month,y = year)) +
    geom_tile(aes(fill = port_ret),colour = "white",linewidth = 1) +
    geom_text(aes(label = label,colour = text_col),size = 3.5) +
    scale_colour_identity() +
    scale_fill_gradientn(colours = c("#8E1B1B","#D9534F","#F4C7C3","#F7F7F7","#C5E5CD","#4CA36A","#1D6B37"),
                         limits = c(-0.12,0.12),oob = scales::squish,guide = "none") + #same colours as the loadings heat map
    scale_x_discrete(position = "top") +
    labs(title = "Return in every month (%)",subtitle = "Green: the portfolio gained that month. Red: it lost.",x = NULL,y = NULL) +
    theme_dash() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(face = "bold"))
}