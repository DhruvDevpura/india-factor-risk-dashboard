library(tidyquant)
library(dplyr)

panel_from = "2020-01-01"

prices = read.csv("data_raw/nse_prices_raw.csv")
prices$date = as.Date(prices$date)
#3 nifty constituents are listed after Jan 2020 (JIOFIN,ETERNAL,MAXHEALTH)
#Keeping them would force every date before listing out of panel
#so the tickers without full coverage are dropped instead
full_n = max(table(prices$symbol))
keep = names(table(prices$symbol))[table(prices$symbol) == full_n]
#dropped = setdiff(unique(prices$symbol),keep)
prices = prices[prices$symbol %in% keep,]

iima_url = "https://faculty.iima.ac.in/iffm/Indian-Fama-French-Momentum/DATA/2025-12_FourFactors_and_Market_Returns_Daily_SurvivorshipBiasAdjusted.csv"
download.file(iima_url,"data_raw/ff_india_daily_2025_12.csv")

mom_fac = read.csv("data_raw/ff_india_daily_2025_12.csv")
mom_fac$Date = as.Date(mom_fac$Date)
mom_fac = mom_fac[mom_fac$Date >= panel_from,]

#Percentage convert to decimals
#mean(mom_fac$RF)*252 = 5.17%, consistent with RBI 91-day T-bill
mom_fac$RF = mom_fac$RF/100
mom_fac$MF = mom_fac$MF/100
mom_fac$WML = mom_fac$WML/100
mom_fac$HML = mom_fac$HML/100
mom_fac$SMB = mom_fac$SMB/100


NSE = prices[prices$symbol == "^NSEI",]
NSE = NSE[!is.na(NSE$adjusted),]
# 14 Nov 2020 was Diwali Muhurat session
#Yahoo has a row for ^NSEI with no adjusted close
#Dropping the row means the 17 Nov index return is measured against 13 Nov


#Without grouping, the lag runs across the ticker boundary
#and the first return is computed against the previous stock's last price
stocks = prices %>%
  filter(symbol != "^NSEI") %>%
  arrange(symbol,date) %>%
  group_by(symbol) %>%
  mutate(ret = (adjusted / dplyr::lag(adjusted)) - 1) %>%
  ungroup()
#Filter now, as doing it before would leave first day of 2020 with no prior price
stocks = stocks[stocks$date >= panel_from,]

#Same as above done for NSE specifically
NSE = NSE %>%
  arrange(date) %>%
  mutate(nifty_ret = (adjusted / dplyr::lag(adjusted)) - 1) %>%
  select(date,nifty_ret)

NSE = NSE[NSE$date >= panel_from,]

#Yahoo has date and IIMA names it as Date
#Every stock date exists in IIMA so join loses nothing 
#the other way IIMA covers 6 dates the NSE price data doesnt 
panel = inner_join(stocks,mom_fac,by = c("date"="Date"))

#Drops 14 Nov across all tickers, since the index has no return
panel = inner_join(panel,NSE,by="date")

#Calculating and storing the excess return over the T-bill
panel$exret = panel$ret - panel$RF

#Renaming for easier access
panel = panel %>%
  rename(ticker = symbol,rf=RF,mf=MF,smb=SMB,hml=HML,wml=WML) %>%
  select(date,ticker,ret,rf,exret,mf,smb,hml,wml,nifty_ret)
saveRDS(panel,"data/panel.rds")

