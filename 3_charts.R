#Builds the three proposal charts from data/panel.rds.
library(ggplot2)
library(dplyr)

panel = readRDS("data/panel.rds")

#Chart 1: Growth of 1 in each IIM-A factor

#Factor columns are the same for every ticker on a date, so one stock (TCS) gives the full series
fac = panel %>%
  filter(ticker == "TCS.NS") %>%
  select(date,mf,smb,hml,wml)

#cumprod(1 + r) turns daily simple returns into a cumulative value.
fac$wml_cum = cumprod(1+fac$wml)
fac$hml_cum = cumprod(1+fac$hml)
fac$smb_cum = cumprod(1+fac$smb)
fac$mf_cum = cumprod(1+fac$mf)

#plot(fac$date, fac$mf_cum, type = "l", ylim = range(...))
#lines(fac$date, fac$smb_cum, col = "red")
#lines(fac$date, fac$hml_cum, col = "blue")
#lines(fac$date, fac$wml_cum, col = "darkgreen")

last = fac[nrow(fac), ]

ggplot(fac, aes(x = date)) +
  geom_hline(yintercept = 1, colour = "grey75", linewidth = 0.3) +
  geom_line(aes(y = mf_cum),  colour = "black",   linewidth = 0.35) +
  geom_line(aes(y = smb_cum), colour = "red", linewidth = 0.35) +
  geom_line(aes(y = hml_cum), colour = "blue", linewidth = 0.35) +
  geom_line(aes(y = wml_cum), colour = "orange", linewidth = 0.35) +
  annotate("text", x = max(fac$date), y = last$mf_cum,  label = sprintf("  MF %.2f",  last$mf_cum),  hjust = 0, size = 3, colour = "black") +
  annotate("text", x = max(fac$date), y = last$smb_cum, label = sprintf("  SMB %.2f", last$smb_cum), hjust = 0, size = 3, colour = "red") +
  annotate("text", x = max(fac$date), y = last$hml_cum, label = sprintf("  HML %.2f", last$hml_cum), hjust = 0, size = 3, colour = "blue") +
  annotate("text", x = max(fac$date), y = last$wml_cum, label = sprintf("  WML %.2f", last$wml_cum), hjust = 0, size = 3, colour = "orange") +
  scale_y_continuous(breaks = seq(0.5, 3, 0.5)) +
  coord_cartesian(clip = "off") +
  labs(title = "Growth of 1 in each IIM-A factor, 2020 to 2025",
       x = NULL, y = "Growth of 1") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.3),
    plot.title         = element_text(face = "bold", size = 12),
    plot.margin        = margin(5, 28, 5, 5)
  )

#Chart 2: Stock vs market excess returns, HUL and Tata Steel

#betas <- sapply(split(panel, panel$ticker),
#function(d) coef(lm(exret ~ mf, data = d))[2])

#Two stocks from opposite ends of the beta range across the 46 constituents
beta_pair = panel[panel$ticker %in% c("HINDUNILVR.NS","TATASTEEL.NS"),]

ggplot(transform(beta_pair,
                 ticker = ifelse(ticker == "HINDUNILVR.NS",
                                 "HINDUNILVR.NS  (beta 0.59)",
                                 "TATASTEEL.NS  (beta 1.39)")),
       aes(x = mf, y = exret)) +
  geom_point(alpha = 0.2, size = 0.7) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", linewidth = 0.6) +
  facet_wrap(~ ticker) +
  labs(title = "Daily excess returns of HUL and Tata Steel against the market",
       x = "Market excess return", y = "Stock excess return") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12))
  

#Chart 3: Share of daily stock movement explained by the market, by year

yr_panel = panel %>%
  mutate(year = format(date,"%Y"))

years = sort(unique(yr_panel$year))
tickers = unique(yr_panel$ticker)

market= rep(NA,length(years))

for( i in 1:length(years))
{
  c2 = rep(NA,length(tickers))
  for( j in 1:length(tickers))
  {
    d = yr_panel[yr_panel$year == years[i] & yr_panel$ticker == tickers[j],]
    c2[j] = cor(d$exret,d$mf)^2
    ##Squared correlation with the market = share of this stock's daily movement that tracks the market
  }
  #Average across the 46 stocks, equal weight
  market[i] = mean(c2)
}
#other = not explained by the market. This includes stock-specific news
#AND anything SMB, HML or WML would explain, so it is not "stock-specific".
yr_share = data.frame(year = years,market= market, other = 1-market)

ggplot(yr_share, aes(x = year, y = market)) +
  geom_col(fill = "skyblue", width = 0.6) +
  geom_text(aes(label = paste0(round(100 * market), "%")),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Share of daily stock movement explained by the market, by year",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid  = element_blank(),
        axis.text.y = element_blank(),
        plot.title  = element_text(face = "bold", size = 12))

