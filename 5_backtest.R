panel = readRDS("data/panel.rds")
tickers = unique(panel$ticker)
n_stocks = length(tickers)
panel$year = as.numeric(format(panel$date,"%Y"))
factors = c("mf","smb","hml","wml")

half_lives = c(21,42,63,126,252,504,Inf)
test_years = c(2024,2025)

single = diag(n_stocks) # 46 portfolios : 100% in one stock
equal = rep(1/n_stocks,n_stocks) #equal weight in all stocks
it = rep(0,n_stocks) 
it[tickers %in% c("TCS.NS", "INFY.NS", "WIPRO.NS", "HCLTECH.NS", "TECHM.NS")] = 0.2

set.seed(1)
random_pf = matrix(0,nrow=300,ncol = n_stocks) #300 empty portfolios with all 0 zero weights

for(r in 1:300)
{
  held = sample(1:n_stocks,10) #pick 10 random stocks
  sizes = runif(10) #a random amount for each of them b2n 0 and 1
  random_pf[r,held] = sizes / sum(sizes) #scale so all 10 weights sum upto 1
}

port_weights = rbind(single,equal,it,random_pf)
port_names = c(tickers,"Equal weight","IT basket",paste("Random",1:300))
port_type = c(rep("Single stock",n_stocks),"Equal weight","IT basket",rep("Random", 300))
n_ports = nrow(port_weights) #348

n_rows = length(test_years)*length(half_lives)*n_ports #2x7x348 = 4872

backtest = data.frame(
  test_year = rep(NA,n_rows),
  half_life = rep(NA,n_rows),
  portfolio = rep(NA,n_rows),
  type = rep(NA,n_rows),
  predicted = rep(NA,n_rows),
  actual = rep(NA,n_rows)
)
k = 0 #rows filled so far

#Backtest: one run for every test year and every half life

for(test_year in test_years)
{
  for(hl in half_lives)
  {
    train = panel[panel$year<test_year,]
    test = panel[panel$year == test_year,]
    
    n_train = length(unique(train$date))
    days_ago = (n_train-1):0
    w = 0.5^(days_ago/hl) #half the weight every hl training days
    
    fac_rows = train[train$ticker == tickers[1],]
    fac_rows = fac_rows[order(fac_rows$date),]
    F_train = as.matrix(fac_rows[,factors])
    
    B = matrix(NA,nrow = n_stocks,ncol = 4) #loadings
    E = matrix(NA,nrow = n_train,ncol = n_stocks) #residuals
    
    for( i in 1:n_stocks)
    {
      train_stock = train[train$ticker == tickers[i],]
      train_stock = train_stock[order(train_stock$date),]
      
      l1 = lm(exret ~ mf + smb + hml + wml, data = train_stock,weights = w)
      B[i,] = coef(l1)[factors]
      E[,i] = resid(l1)
    }
    
    factor_cov = cov.wt(F_train,wt = w)$cov #How 4 factors move together 
    resid_cov = cov.wt(E,wt =w)$cov #how to stocks leftover part move together
    factor_part = B %*% factor_cov %*% t(B) #stock risk coming through the factors
    Sigma = factor_part + resid_cov #Total predicted risk
    
    predicted = rep(NA,n_ports) #one slot for each portfolio
    
    for(p in 1:n_ports)
    {
      one_port = port_weights[p,] #this portfolio's 46 weights
      daily_var = as.numeric(t(one_port) %*% Sigma %*% one_port) #its predicted daily variance
      predicted[p] = sqrt(daily_var) * sqrt(252)
    }
    
    n_test = length(unique(test$date))
    test_returns = matrix(NA,nrow=n_test,ncol = n_stocks)
    
    for(i in 1:n_stocks)
    {
      test_stock = test[test$ticker == tickers[i],]
      test_stock = test_stock[order(test_stock$date),]
      test_returns[,i] = test_stock$exret
    }
    
    actual = rep(NA,n_ports)
    
    for(p in 1:n_ports)
    {
      one_port = port_weights[p,]
      daily_ret = test_returns %*% one_port
      actual[p] = sd(daily_ret) * sqrt(252)
    }
    
    rows = (k+1):(k+n_ports) #the next 348 empty rows
    backtest$test_year[rows] = test_year
    backtest$half_life[rows] = hl
    backtest$portfolio[rows] = port_names
    backtest$type[rows] = port_type
    backtest$predicted[rows] = predicted
    backtest$actual[rows] = actual
    k = k+n_ports
  }
}

saveRDS(backtest,"data/backtest.rds")

check = backtest[backtest$test_year == 2025 & backtest$half_life == 252,]
mean(check$predicted / check$actual) #about 1.167

check = backtest[backtest$test_year == 2025 & backtest$half_life == Inf,]
mean(check$predicted / check$actual) #about 1.47

# panel = readRDS("data/panel.rds")
# tickers = unique(panel$ticker)
# n_stocks = length(tickers)
# panel$year = as.numeric(format(panel$date,"%Y"))
# factors = c("mf","smb","hml","wml")
# 
# half_lives = c(21,42,63,126,252,504,Inf)
# test_years = c(2024,2025)
# 
# single = diag(n_stocks) # 46 portfolios : 100% in one stock
# equal = rep(1/n_stocks,n_stocks) #equal weight in all stocks
# it = rep(0,n_stocks) 
# it[tickers %in% c("TCS.NS", "INFY.NS", "WIPRO.NS", "HCLTECH.NS", "TECHM.NS")] = 0.2
# 
# set.seed(1)
# random_pf = matrix(0,nrow=300,ncol = n_stocks) #300 empty portfolios with all 0 zero weights
# 
# for(r in 1:300)
# {
#   held = sample(1:n_stocks,10) #pick 10 random stocks
#   sizes = runif(10) #a random amount for each of them b2n 0 and 1
#   random_pf[r,held] = sizes / sum(sizes) #scale so all 10 weights sum upto 1
# }
# 
# port_weights = rbind(single,equal,it,random_pf)
# port_names = c(tickers,"Equal Weights","IT basket",paste("Random",1:300))
# port_type = c(rep("Single Stock",n_stocks),"Equal Weights","IT basket",rep("Random", 300))
# 
# test_year = 2025
# hl = 252 #half life 
# 
# train = panel[panel$year<test_year,]
# test = panel[panel$year == test_year,]
# 
# n_train = length(unique(train$date))
# days_ago = (n_train-1):0
# w = 0.5^(days_ago/hl) #half the weight every hl training days
# 
# fac_rows = train[train$ticker == tickers[1],]
# fac_rows = fac_rows[order(fac_rows$date),]
# F_train = as.matrix(fac_rows[,factors])
# 
# B = matrix(NA,nrow = n_stocks,ncol = 4) #loadinga
# E = matrix(NA,nrow = n_train,ncol = n_stocks) #residuals
# 
# for( i in 1:n_stocks)
# {
#     train_stock = train[train$ticker == tickers[i],]
#     train_stock = train_stock[order(train_stock$date),]
#     
#     l1 = lm(exret ~ mf + smb + hml + wml, data = train_stock,weights = w)
#     B[i,] = coef(l1)[factors]
#     E[,i] = resid(l1)
# }
# 
# factor_cov = cov.wt(F_train,wt = w)$cov #How 4 factors move together 
# resid_cov = cov.wt(E,wt =w)$cov #how to stocks leftover part move together
# factor_part = B %*% factor_cov %*% t(B) #stock risk coming through the factors
# Sigma = factor_part + resid_cov #Total predicted risk
# 
# predicted = rep(NA,nrow(port_weights)) #one slot for each portfolio
# 
# for(p in 1:nrow(port_weights))
# {
#   one_port = port_weights[p,] #this portfolio's 46 weights
#   daily_var = as.numeric(t(one_port) %*% Sigma %*% one_port) #its predicted daily variance
#   predicted[p] = sqrt(daily_var) * sqrt(252)
#   
# }
# 
# n_test = length(unique(test$date))
# test_returns = matrix(NA,nrow=n_test,ncol = n_stocks)
# 
# for(i in 1:n_stocks)
# {
#   test_stock = test[test$ticker == tickers[i],]
#   test_stock = test_stock[order(test_stock$date),]
#   test_returns[,i] = test_stock$exret
# }
# 
# actual = rep(NA,nrow(port_weights))
# for(p in 1:nrow(port_weights))
# {
#   one_port = port_weights[p,]
#   daily_ret = test_returns %*% one_port
#   actual[k] = sd(daily_ret) * sqrt(252)
# }
