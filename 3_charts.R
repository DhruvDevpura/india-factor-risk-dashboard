library(ggplot2)
library(dplyr)
library(timeDate)
?
panel = readRDS("data/panel.rds")

fac = panel %>%
  filter(ticker == "TCS.NS") %>%
  select(date,mf,smb,hml,wml)

fac$wml_cum = cumprod(1+fac$wml)
fac$hml_cum = cumprod(1+fac$hml)
fac$smb_cum = cumprod(1+fac$smb)
fac$mf_cum = cumprod(1+fac$mf)

#plot(fac$date, fac$mf_cum, type = "l", ylim = range(...))
#lines(fac$date, fac$smb_cum, col = "red")
#lines(fac$date, fac$hml_cum, col = "blue")
#lines(fac$date, fac$wml_cum, col = "darkgreen")

last <- fac[nrow(fac), ]

ggplot(fac, aes(x = date)) +
  geom_hline(yintercept = 1, colour = "grey75", linewidth = 0.3) +
  geom_line(aes(y = mf_cum),  colour = "black",   linewidth = 0.35) +
  geom_line(aes(y = smb_cum), colour = "red", linewidth = 0.35) +
  geom_line(aes(y = hml_cum), colour = "blue", linewidth = 0.35) +
  geom_line(aes(y = wml_cum), colour = "orange", linewidth = 0.35) +
  annotate("text", x = max(fac$date), y = last$mf_cum,  label = "  MF",  hjust = 0, size = 3, colour = "black") +
  annotate("text", x = max(fac$date), y = last$smb_cum, label = "  SMB", hjust = 0, size = 3, colour = "red") +
  annotate("text", x = max(fac$date), y = last$hml_cum, label = "  HML", hjust = 0, size = 3, colour = "blue") +
  annotate("text", x = max(fac$date), y = last$wml_cum, label = "  WML", hjust = 0, size = 3, colour = "orange") +
  scale_y_continuous(breaks = seq(0.5, 3, 0.5)) +
  coord_cartesian(clip = "off") +
  labs(title = "Four facs, one market, very different outcomes",
       x = NULL, y = "Growth of 1") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.3),
    plot.title         = element_text(face = "bold", size = 12),
    plot.margin        = margin(5, 28, 5, 5)
  )

#--------------------------------
# skew_tbl = panel %>%
#   group_by(ticker) %>%
#   summarise(skew = skewness(ret))
# skew_tbl = skew_tbl %>%
#   arrange(skew)
# skew_tbl$ticker = factor(skew_tbl$ticker,levels = skew_tbl$ticker)
# 
# ggplot(skew_tbl, aes(x = skew, y = ticker)) +
#   geom_vline(xintercept = 0, colour = "grey60") +
#   geom_point(colour = ifelse(skew_tbl$skew > 0, "#2e7d4f", "#c0392b"), size = 1.8) +
#   labs(title = "Most stocks show positive skewness",
#        x = "Skewness of daily returns", y = NULL) +
#   theme_minimal(base_size = 10) +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.y = element_blank(),
#         plot.title = element_text(face = "bold", size = 12))


#betas <- sapply(split(panel, panel$ticker),
#function(d) coef(lm(exret ~ mf, data = d))[2])

beta_pair = panel[panel$ticker %in% c("HINDUNILVR.NS","TATASTEEL.NS"),]

ggplot(transform(beta_pair,
                 ticker = ifelse(ticker == "HINDUNILVR.NS",
                                 "HINDUNILVR.NS  (beta 0.59)",
                                 "TATASTEEL.NS  (beta 1.39)")),
       aes(x = mf, y = exret)) +
  geom_point(alpha = 0.2, size = 0.7) +
  geom_smooth(method = "lm", se = FALSE, colour = "#c0392b", linewidth = 0.6) +
  facet_wrap(~ ticker) +
  labs(title = "Same market days, very different responses",
       x = "Market excess return", y = "Stock excess return") +
  theme_minimal()
