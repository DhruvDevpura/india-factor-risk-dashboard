panel = readRDS("data/panel.rds")
tickers = unique(panel$ticker)
n = length(tickers)

loadings = data.frame(
  ticker = tickers,
  alpha = rep(NA,n),
  b_mkt = rep(NA,n),
  b_smb = rep(NA,n),
  b_hml = rep(NA,n),
  b_wml = rep(NA,n),
  r2_mkt = rep(NA,n),
  r2_4f = rep(NA,n)
)

for(i in 1:n)
{
  curr_stock = panel[panel$ticker == tickers[i],]
  l1 = lm(exret ~ mf + smb + hml + wml, data = curr_stock)
  loadings$alpha[i] = coef(l1)["(Intercept)"]
  loadings$b_mkt[i] = coef(l1)["mf"]
  loadings$b_smb[i] = coef(l1)["smb"]
  loadings$b_hml[i] = coef(l1)["hml"]
  loadings$b_wml[i] = coef(l1)["wml"]
  loadings$r2_4f[i] = summary(l1)$r.squared
  
  l2 = lm(exret ~ mf, data = curr_stock)
  loadings$r2_mkt[i] = summary(l2)$r.squared
}

loadings$mkt_share = loadings$r2_mkt
loadings$other_share = loadings$r2_4f - loadings$r2_mkt
loadings$idio_share = 1 - loadings$r2_4f

panel$year = as.numeric(format(panel$date,"%Y"))
set.seed(1)

years = 2020:2025
B = 1000
factors = c("mf","smb","hml","wml")
n_rows = n*length(years)*length(factors)
yearly = data.frame(
  ticker = rep(NA,n_rows),
  year = rep(NA,n_rows),
  factor = rep(NA,n_rows),
  est = rep(NA,n_rows),
  se = rep(NA,n_rows), #SE for tooltip; lo/hi for ribbon (percentile, allows asymmetry)
  lo = rep(NA,n_rows),
  hi = rep(NA,n_rows)
)
yearly$factor = factor(yearly$factor, levels = factors)
#This fixes the order in legends and facets as mkt, smb, hml, wml
#Without it, ggplot sorts alphabetically: hml, mkt, smb, wml.
k = 0
for(i in 1:n)
{
  for(y in years)
  {
    popn = panel[panel$ticker == tickers[i] & panel$year == y,]
    m = nrow(popn)
    popn_fit = lm(exret ~ mf + smb + hml + wml, data = popn)
    est = coef(popn_fit)[factors]
    
    boot = matrix(NA,nrow = B,ncol = 4)
    for(b in 1:B)
    {
      idx = sample (1:m,m,replace=TRUE)
      fit = lm(exret ~ mf + smb + hml + wml, data = popn[idx,])
      boot[b,] = coef(fit)[factors]
    }
    for(j in 1:4)
    {
      q = quantile(boot[,j],c(0.025,0.975))
      yearly$ticker[k+j] = tickers[i]
      yearly$year[k+j] = y
      yearly$factor[k+j] = factors[j]
      yearly$est[k+j] = est[j]
      yearly$se[k+j] = sd(boot[,j])
      yearly$lo[k+j] = q[1]
      yearly$hi[k+j] = q[2]
    }
    k = k+4
    
  }
}