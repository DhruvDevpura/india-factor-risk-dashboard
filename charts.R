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
    labs(title ="Predicted against actual risk",
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

#the portfolio's loading on each factor
make_exposure_plot = function(chosen_stocks,chosen_weights)
{
  result = portfolio_risk(chosen_stocks,chosen_weights)
  exposure_data = data.frame(
    factor = factor(c("Market","Size","Value","Momentum"),levels = c("Market","Size","Value","Momentum")),
    exposure = as.numeric(result$exposure)
  )
  exposure_data$colour = ifelse(exposure_data$exposure > 0,"with","against") #green if with the factor, red if against
  
  ggplot(exposure_data,aes(x = factor,y = exposure,fill = colour)) +
    geom_col(width = 0.6) +
    geom_hline(yintercept = 0,colour = "grey40") +
    geom_text(aes(label = round(exposure,2),vjust = ifelse(exposure > 0,-0.5,1.5))) +
    scale_fill_manual(values = c("with" = "seagreen","against" = "firebrick"),guide = "none") +
    scale_y_continuous(expand = expansion(mult = 0.15)) +
    labs(title = "Portfolio loading on each factor",
         subtitle = "Green: moves with the factor. Red: moves against it.",
         x = NULL,y = "Loading") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank())
}

#the portfolio's yearly volatility, and where it comes from
make_risk_plot = function(chosen_stocks,chosen_weights)
{
  result = portfolio_risk(chosen_stocks,chosen_weights)
  risk_data = data.frame(
    part = c("From the four factors","Stock-specific","Total","Total if stocks' own news were unrelated"),
    vol = c(result$factor_vol,result$specific_vol,result$total_vol,result$total_vol_indep)
  )
  risk_data$part = factor(risk_data$part,levels = rev(risk_data$part)) #keep this order, top to bottom
  
  #the parts combine as squares, not as a plain sum (variances add, volatilities do not)
  note = paste0("Parts combine as squares: ",
                round(100 * result$factor_vol,1),"\u00b2 + ",
                round(100 * result$specific_vol,1),"\u00b2 = ",
                round(100 * result$total_vol,1),"\u00b2")
  
  ggplot(risk_data,aes(x = vol,y = part,fill = part == "Total")) +
    geom_col(width = 0.6) +
    geom_text(aes(label = scales::percent(vol,accuracy = 0.1)),hjust = -0.2) +
    scale_fill_manual(values = c("TRUE" = "steelblue4","FALSE" = "skyblue"),guide = "none") +
    scale_x_continuous(labels = scales::percent,expand = expansion(mult = c(0,0.2))) +
    labs(title = "Portfolio volatility per year, 2020 to 2025",subtitle = note,x = NULL,y = NULL) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank())
}