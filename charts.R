library(ggplot2)
library(patchwork)

estimates = readRDS("data/estimates.rds")
nifty_list = read.csv("data_raw/ind_nifty50list.csv")
panel = readRDS("data/panel.rds")
nifty_list$ticker = paste0(nifty_list$Symbol,".NS")
stocks = merge(estimates$loadings,nifty_list[,c("ticker","Industry")],by = "ticker")

#Shared look for every dashboard chart
theme_dash = function(base_size = 13)
{
  theme_minimal(base_size = base_size) +
    theme(plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          plot.subtitle = element_text(colour = "grey35"),
          panel.grid.minor = element_blank())
}

#Stocks and sectors page: heat map of loadings and risk split, same rows ----------
#Table of everything needed per stock, one row per stock
stock_info = estimates$loadings
stock_info = merge(stock_info, nifty_list[,c("ticker","Industry")], by = "ticker")
stock_info$name = sub(".NS", "", stock_info$ticker, fixed = TRUE)
stock_info$market_share = stock_info$r2_mkt
stock_info$other_share = stock_info$r2_4f - stock_info$r2_mkt
stock_info$own_share = 1 - stock_info$r2_4f

make_stock_plot = function(chosen_tickers)
{
  #keep the chosen stocks, ordered by sector then by market loading
  info = stock_info[stock_info$ticker %in% chosen_tickers,]
  info = info[order(info$Industry, info$b_mkt),]
  info$name = factor(info$name, levels = unique(info$name))
  
  # heat map data, one row per stock and factor
  heat = data.frame(
    name = rep(info$name, 4),
    Industry = rep(info$Industry, 4),
    factor = factor(rep(c("Market","Size","Value","Momentum"), each = nrow(info)),
                    levels = c("Market","Size","Value","Momentum")),
    beta = c(info$b_mkt, info$b_smb, info$b_hml, info$b_wml)
  )
  heat$shade = ifelse(heat$factor == "Market", NA, heat$beta)   #market column is not shaded
  heat$text_col = ifelse(!is.na(heat$shade) & abs(heat$beta) > 0.45, "white", "grey15")
  
  heat_plot = ggplot(heat, aes(x = factor, y = name)) +
    geom_tile(aes(fill = shade), colour = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.2f", round(beta, 2) + 0), colour = text_col), size = 3.6) +   #+ 0 turns -0.00 into 0.00
    scale_colour_identity() +
    scale_fill_gradientn(colours = c("#8E1B1B", "#D9534F", "#F4C7C3", "#F7F7F7",
                                     "#C5E5CD", "#4CA36A", "#1D6B37"),
                         limits = c(-0.9, 0.9), oob = scales::squish,
                         na.value = "#E4E7EB", guide = "none") +
    scale_x_discrete(position = "top") +
    facet_grid(Industry ~ ., scales = "free_y", space = "free_y", switch = "y",
               labeller = label_wrap_gen(width = 18)) +
    labs(title = "Factor loadings",
         subtitle = "Green: moves with the factor. Red: moves against it.", x = NULL, y = NULL) +
    theme_dash() +
    theme(panel.grid = element_blank(),
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold", colour = "grey30"),
          axis.text.x = element_text(face = "bold"),
          panel.spacing = unit(0.4, "lines"))
  
  #Step 3: risk split data, same stocks in the same order
  split = data.frame(
    name = rep(info$name, 3),
    Industry = rep(info$Industry, 3),
    part = factor(rep(c("Market","Other three factors","Stock-specific"), each = nrow(info)),
                  levels = c("Stock-specific","Other three factors","Market")),
    share = c(info$market_share, info$other_share, info$own_share)
  )
  
  split_plot = ggplot(split, aes(x = share, y = name, fill = part)) +
    geom_col(width = 0.75) +
    geom_text(data = info, aes(x = r2_4f, y = name, label = scales::percent(r2_4f, accuracy = 1)),
              inherit.aes = FALSE, hjust = -0.15, size = 3.4, colour = "grey15") +
    scale_fill_manual(values = c("Market" = "#2F3E4E", "Other three factors" = "#5B8DB8",
                                 "Stock-specific" = "#E4E7EB")) +
    scale_x_continuous(labels = scales::percent, position = "top", expand = c(0, 0)) +
    facet_grid(Industry ~ ., scales = "free_y", space = "free_y") +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(title = "Where daily movement comes from",
         subtitle = "Number: share explained by the four factors", x = NULL, y = NULL, fill = NULL) +
    theme_dash() +
    theme(panel.grid.major.y = element_blank(),
          strip.text = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "bottom",
          panel.spacing = unit(0.4, "lines"))
  
  #Step 4: side by side, rows aligned
  heat_plot + split_plot + plot_layout(widths = c(1.1, 1))
}

#-------------------------------------------------------------
vol_data = estimates$factor_vol

vol_data$factor = factor(vol_data$factor,levels = c("mf","smb","hml","wml"),
                         labels = c("Market","Size","Value","Momentum"))
vol_plot = ggplot(vol_data, aes(x = date, y = vol, colour = factor)) +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = c("Market" = "black", "Size" = "red",
                                 "Value" = "blue", "Momentum" = "orange")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Volatility of each factor over the past year, rolling daily",
       x = NULL, y = "Annualised volatility", colour = NULL) +
  theme_minimal() +
  theme(legend.position = "top", panel.grid.minor = element_blank())
vol_plot

#Chart 4: does the model predict next year's risk? -------------------------------
#Two charts: a scatter for one chosen half life and model (the slider and dropdown in the app),
#and an error curve across all half lives for both models.

backtest = readRDS("data/backtest.rds")

#Chart 4a: predicted against actual volatility for one half life and one model
make_backtest_plot = function(chosen_hl, chosen_model = "Full covariance")
{
  #keep only the chosen half life and model (348 portfolios x 2 test years)
  one_hl = backtest[backtest$half_life == chosen_hl & backtest$model == chosen_model, ]
  
  #panel names that say which years built the model
  one_hl$panel = ifelse(one_hl$test_year == 2024,
                        "Predicting 2024 (built on 2020 to 2023)",
                        "Predicting 2025 (built on 2020 to 2024)")
  
  #draw. Each dot is one portfolio, dashed line = perfect prediction
  ggplot(one_hl, aes(x = predicted, y = actual, colour = type)) +
    geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = "dashed") +
    geom_point(alpha = 0.6) +
    geom_point(data = one_hl[one_hl$type %in% c("Equal weight", "IT basket"), ], size = 4) +
    facet_wrap(~ panel) +
    scale_colour_manual(values = c("Single stock" = "grey60", "Random" = "steelblue",
                                   "Equal weight" = "black", "IT basket" = "firebrick")) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    coord_equal() +
    labs(title = chosen_model, x = "Predicted volatility", y = "Actual volatility", colour = NULL) +
    theme_minimal() +
    theme(legend.position = "top",
          panel.border = element_rect(colour = "grey70", fill = NA),   #a box around each panel
          panel.spacing = unit(1.5, "lines"))                          #space between the two panels
}

#Chart 4b: average percentage error for every half life, both models
#Uses the summary saved by 5_backtest.R, so the chart and the README show the same numbers.
#Single stocks are left out: both models give them identical predictions.
error_data = readRDS("data/summary_multi.rds")

#half life as a label, with Inf shown as "No decay"
error_data$hl_label = factor(ifelse(is.infinite(error_data$half_life), "No decay",
                                    as.character(error_data$half_life)),
                             levels = c("21", "42", "63", "126", "252", "504", "No decay"))

#panel names that say which years built the model
error_data$prediction = ifelse(error_data$test_year == 2024,
                               "Predicting 2024 (built on 2020 to 2023)",
                               "Predicting 2025 (built on 2020 to 2024)")

#draw. One line per model, one panel per test year
error_plot = ggplot(error_data, aes(x = hl_label, y = ape, colour = model, group = model)) +
  geom_line() +
  geom_point(size = 2.5) +
  facet_wrap(~ prediction) +
  scale_colour_manual(values = c("Full covariance" = "black",
                                 "Factor + diagonal" = "firebrick")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Average prediction error by half life, 302 multi-stock portfolios",
       x = "Half life (trading days)", y = "Mean absolute percentage error", colour = NULL) +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey70", fill = NA),
        panel.spacing = unit(1.5, "lines"))


#Portfolio risk ---------------------------------------------------------------
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

#Chart 5: portfolio exposures and risk ------------------------------------------
#Both charts take the user's stocks and weights and use portfolio_risk() above.

#Chart 5a: the portfolio's loading on each factor
make_exposure_plot = function(chosen_stocks, chosen_weights)
{
  #the four exposures from portfolio_risk()
  result = portfolio_risk(chosen_stocks, chosen_weights)
  exposure_data = data.frame(
    factor   = factor(c("Market", "Size", "Value", "Momentum"),
                      levels = c("Market", "Size", "Value", "Momentum")),
    exposure = as.numeric(result$exposure)
  )
  
  #green if the portfolio moves with the factor, red if against.
  exposure_data$colour = ifelse(exposure_data$exposure > 0, "with", "against")
  
  ggplot(exposure_data, aes(x = factor, y = exposure, fill = colour)) +
    geom_col(width = 0.6) +
    geom_hline(yintercept = 0, colour = "grey40") +
    geom_text(aes(label = round(exposure, 2),
                  vjust = ifelse(exposure > 0, -0.5, 1.5))) +
    scale_fill_manual(values = c("with" = "seagreen", "against" = "firebrick"), guide = "none") +    scale_y_continuous(expand = expansion(mult = 0.15)) +
    labs(title = "Portfolio loading on each factor",
         subtitle = "Green: moves with the factor. Red: moves against it.",
         x = NULL, y = "Loading") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank())
}

#Chart 5b: the portfolio's yearly volatility, and where it comes from
make_risk_plot = function(chosen_stocks, chosen_weights)
{
  #Step 1: the volatilities from portfolio_risk()
  result = portfolio_risk(chosen_stocks, chosen_weights)
  risk_data = data.frame(
    part = c("From the four factors",
             "Stock-specific",
             "Total",
             "Total if stocks' own news were unrelated"),
    vol  = c(result$factor_vol, result$specific_vol,
             result$total_vol, result$total_vol_indep)
  )
  risk_data$part = factor(risk_data$part, levels = rev(risk_data$part))   #keep this order, top to bottom
  
  #Step 2: the parts combine as squares, not as a plain sum (variances add, volatilities do not)
  note = paste0("Parts combine as squares: ",
                round(100 * result$factor_vol, 1), "\u00b2 + ",
                round(100 * result$specific_vol, 1), "\u00b2 = ",
                round(100 * result$total_vol, 1), "\u00b2")
  
  #Step 3: draw, with the real total in a darker colour
  ggplot(risk_data, aes(x = vol, y = part, fill = part == "Total")) +
    geom_col(width = 0.6) +
    geom_text(aes(label = scales::percent(vol, accuracy = 0.1)), hjust = -0.2) +
    scale_fill_manual(values = c("TRUE" = "steelblue4", "FALSE" = "skyblue"), guide = "none") +
    scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.2))) +
    labs(title = "Portfolio volatility per year, 2020 to 2025", subtitle = note, x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank())
}