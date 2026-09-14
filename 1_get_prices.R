#Download NSE daily prices from Yahoo Finance

library("tidyquant")

#Universe is Nifty 50 list downloaded from NSE
#Download from https://nsearchives.nseindia.com/content/indices/ind_nifty50list.csv
n50_const = read.csv("data_raw/ind_nifty50list.csv")
tickers = paste0(n50_const$Symbol,".NS")
#NSEI is NIFTY 50 index itself, used as a benchmark
tickers = c(tickers,"^NSEI")

fetch_from = "2019-12-20"
#Earlier then window so panel has prior close to compute a return against
fetch_to = "2025-12-31"

prices = tq_get(tickers,get ="stock.prices",from = fetch_from,to = fetch_to)

#Stops if catches a ticker failing to download
stopifnot(length(unique(prices$symbol)) == length(tickers))

write.csv(prices,"data_raw/nse_prices_raw.csv",row.names = FALSE)