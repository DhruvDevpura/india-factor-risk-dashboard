library(ggplot2)

estimates = readRDS("data/estimates.rds")
nifty_list = read.csv("data_raw/ind_nifty50list.csv")

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
