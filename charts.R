library(ggplot2)

estimates = readRDS("data/estimates.rds")
nifty_list = read.csv("data_raw/ind_nifty50list.csv")
panel = readRDS("data/panel.rds")
nifty_list$ticker = paste0(nifty_list$Symbol,".NS")
stocks = merge(estimates$loadings,nifty_list[,c("ticker","Industry")],by = "ticker")

heat_data = data.frame(
  stock = rep(stocks$ticker,4),
  industry = rep(stocks$Industry,4),
  factor = rep(c("Market","Size","Value","Momentum"),each = 46),
  beta = c(stocks$b_mkt,stocks$b_smb,stocks$b_hml,stocks$b_wml)
)
heat_data$factor = factor(heat_data$factor,levels = c("Market","Size","Value","Momentum"))

#no shade for the market column (all betas are positive, it would be all red)
heat_data$shade = heat_data$beta
heat_data$shade[heat_data$factor == "Market"] = NA

heatmap_plot = ggplot(heat_data, aes(x = factor, y = stock, fill = shade)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(beta, 2)), size = 2.7) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                       limits = c(-1, 1), oob = scales::squish, na.value = "grey95") +
  facet_grid(industry ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Factor loadings of 46 NIFTY 50 stocks, 2020 to 2025",
       x = NULL, y = NULL, fill = "Loading") +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0, hjust = 0))
heatmap_plot
#----------------------------------------------------------------

shares = estimates$loadings
shares$market = shares$r2_mkt #explained by market alone
shares$other = shares$r2_4f - shares$r2_mkt #extra explained by size,value and momentum
shares$own = 1 - shares$r2_4f #not explained by any factor (stock specific)

split_data = data.frame(
  stock = rep(shares$ticker,3),
  part = rep(c("Market","Other three factors","Stock-specific"),each=46),
  share = c(shares$market,shares$other,shares$own)
)
split_data$stock = factor(split_data$stock, levels = shares$ticker[order(shares$r2_4f)])
split_data$part = factor(split_data$part,levels = c("Stock-specific", "Other three factors", "Market"))

split_plot = ggplot(split_data, aes(x = share, y = stock, fill = part)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("Market" = "grey30",
                               "Other three factors" = "steelblue",
                               "Stock-specific" = "grey85")) +
  scale_x_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse = TRUE)) +   #legend in the same order as the bars
  labs(title = "Where each stock's daily movement comes from, 2020 to 2025",
       x = "Share of daily variance", y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
split_plot

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