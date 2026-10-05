# Factor Risk Analytics for Indian equities

A stock's daily return mixes several things at once: the market's move, tilts toward small or value or momentum names, and whatever is specific to the company. Separating those matters because the first parts are exposures anyone can buy cheaply, while only the last is particular to holding that stock, and a portfolio that looks diversified by position count can turn out to be a single factor bet.
This repository builds a daily panel of 46 NIFTY 50 constituents from January 2020 to December 2025, merged with the IIM-Ahmedabad Fama-French and momentum factor library, and estimates factor loadings and risk decompositions from it. It then tests the resulting risk forecasts out of sample on 2024 and 2025.

## Data sources

**IIM-A Fama-French and Momentum Factors, Indian market**

Release 2025-12, survivorship-bias-adjusted daily file.

- URL: https://faculty.iima.ac.in/iffm/Indian-Fama-French-Momentum/DATA/2025-12_FourFactors_and_Market_Returns_Daily_SurvivorshipBiasAdjusted.csv
- Accessed: 14 September 2026
- Cite as: Agarwalla, S. K., Jacob, J. and Varma, J. R. (2013), "Four factor model in Indian equities market", W.P. No. 2013-09-05, IIM Ahmedabad.

**NIFTY 50 constituent list**

- URL: https://nsearchives.nseindia.com/content/indices/ind_nifty50list.csv
- Downloaded: 14 September 2026

Committed as a dated snapshot rather than fetched at runtime: NSE publishes the current constituent list, so a live fetch would silently change the universe after any index rebalance.

**Daily prices**

- Source: Yahoo Finance, retrieved with `tq_get()` from tidyquant
- Fetched: 14 September 2026, for the window 20 December 2019 to 31 December 2025

Tickers are the NIFTY 50 constituent symbols with a `.NS` suffix, plus `^NSEI` (the NIFTY 50 index) as the benchmark series. Adjusted closing prices are used, so returns include dividends and are corrected for splits.
Yahoo does not reliably adjust for demergers; see Exclusions and limitations.

## Reproducing the panel

1. `1_get_prices.R` downloads the Yahoo price series to `data_raw/`.
   It requires network access and is run manually.
2. `2_build_panel.R` downloads the IIM-A factor file to `data_raw/`,
   then computes returns, merges the two sources and writes
   `data/panel.rds`. The IIM-A URL is release-specific (2025-12).
3. `4_estimates.R` reads `data/panel.rds` and writes `data/estimates.rds`.
4. `5_backtest.R` reads `data/panel.rds` and writes `data/backtest.rds`,
   `data/summary_all.rds` and `data/summary_multi.rds`. It fits
   46 regressions for each of 14 runs and takes a few minutes.

`3_charts.R` only builds the charts used in the proposal and is not
needed for any of the outputs above.

The fetch window starts earlier than the analysis window so that the
first day in the panel has a prior close to compute a return against.
Both end dates are pinned rather than left open, so re-running the
scripts does not silently extend the sample.

R 4.6.1. Packages: tidyquant, dplyr, ggplot2.

## Panel

`data/panel.rds`: 68,310 rows, being 46 tickers by 1,485 trading days,
January 2020 to December 2025. Long format, one row per date and ticker.

| column | meaning |
|---|---|
| `date` | trading date |
| `ticker` | Yahoo symbol, e.g. `TCS.NS` |
| `ret` | simple daily return from adjusted prices, decimal |
| `rf` | daily risk-free rate, decimal |
| `exret` | `ret - rf`, the excess return used on the left-hand side |
| `mf` | excess market return, decimal |
| `smb` | size factor (small minus big), decimal |
| `hml` | value factor (high minus low), decimal |
| `wml` | momentum factor (winners minus losers), decimal |
| `nifty_ret` | simple daily return on the NIFTY 50 index, decimal |

All return and factor columns are decimals, not percentages. The IIM-A
file reports factors in percent and is divided by 100 at read time;
Yahoo-derived returns are already decimal. Scaling only one side would
leave the factor loadings intact while inflating the regression
intercept a hundredfold, with nothing in the output to indicate it.

`rf` is a daily holding-period rate, not an annualised yield. Verified
as `mean(rf) * 252 = 5.17%` over 2020 to 2025, consistent with RBI
91-day Treasury Bill levels across that period.

## Exclusions and limitations

**Three constituents dropped for incomplete coverage.** `ETERNAL.NS`,
`JIOFIN.NS` and `MAXHEALTH.NS` have 1,098, 583 and 1,327 observations
respectively against 1,493 for the rest, because each listed after
January 2020. Retaining them would have forced every date before their
listing out of the panel, so the tickers are dropped instead of the
dates. This leaves 47 stocks before exclusion below.

**`TMPV.NS` dropped for a corporate action.** Tata Motors demerged its
commercial-vehicle business with a record date of 14 October 2025, a
spin-off worth about 39.5% of the parent's value. Yahoo's adjusted prices
do not account for it, so the series shows a -40.2% return that day, and
the price history before it belongs to a different company. This leaves
46 stocks.

**14 November 2020 dropped.** This was the Diwali Muhurat session. Yahoo
records a row for `^NSEI` with no adjusted close, although all the
constituent stocks traded and IIM-A reports factors for the date. A
single missing price invalidates two returns, so the index row is
removed and the date leaves the panel through the join. The 17 November
index return is therefore measured against 13 November.

**Six IIM-A dates absent from the price data.** 2020-02-01, 2023-11-12,
2024-01-20, 2024-03-02, 2024-05-18 and 2025-12-31. The first five are
NSE special or Saturday sessions for which Yahoo publishes no daily bar;
the last is an artefact of the fetch end boundary. All are removed by the
inner join on date. The return on the following trading day therefore spans two sessions
while the joined factors cover one. The measured effect on market betas
is about 0.01 at most, and this is not corrected.

**Three small demerger errors left uncorrected.** RELIANCE (20 July 2023),
ITC (6 January 2025) and HINDUNILVR (5 December 2025) each show a
one-day return error of about 1.7 percentage points because Yahoo's
adjustment does not match the exchange's. The measured effect on market
betas is 0.002 or less.

**Survivorship bias.** The universe is the NIFTY 50 constituent list as
published on 14 September 2026, applied to a window running from 2020.
Every name in the panel therefore both survived to the present and
existed at the start of the sample. Stocks that were index constituents
during the period but have since been removed do not appear. This biases
the sample and is not corrected for.

## Estimates

`4_estimates.R` regresses each stock's daily excess return on the four
factors and saves `data/estimates.rds`, a list of four elements: the
factor loadings, yearly loadings with bootstrap intervals (B = 1000),
rolling 252-day factor volatility, and the matrix of regression
residuals.

- The market factor alone explains 30.8% of daily return variance on
  average across the 46 stocks. Adding size, value and momentum raises
  this to 35.3%. Most of a stock's daily movement is specific to the
  company.
- Some value loadings are strongly negative: INFY -0.58 and HINDUNILVR
  -0.54, so both behave like growth stocks.
- Loadings move over time. Each stock's loadings were re-estimated for
  each year from 2020 to 2025, with 95% percentile intervals from 1,000
  bootstrap resamples of days. For 89 of the 184 stock-factor pairs, the
  intervals of at least two of the six years do not overlap.
- Residuals are correlated within a sector. TCS and INFY have a residual
  correlation of 0.50 after all four factors are removed. For the IT
  basket (TCS, INFY, WIPRO, HCLTECH, TECHM at 20% each), annualised
  volatility over 2020 to 2025 is 23.2% with the full residual
  covariance and 19.6% if residuals are treated as independent.

## Out-of-sample check

`5_backtest.R` asks whether the risk the model predicts for a portfolio
matches the risk that portfolio actually showed in the following year.

**Method.** Each test year (2024, 2025) is forecast using only the years
before it, so 2024 is built on 2020 to 2023 and 2025 on 2020 to 2024.
Training days are weighted exponentially, with a half-life of 21, 42,
63, 126, 252 or 504 trading days, or equal weights (half-life infinite).
Each stock is regressed on the four factors with those weights, and the
predicted daily covariance matrix is built in two ways:

| Model | Covariance | What it assumes |
|---|---|---|
| Full covariance | factor part + full residual covariance | Every pair of stocks can be linked beyond the factors |
| Factor + diagonal | factor part + residual variances only | Stocks are linked only through the four factors |

The full-covariance version is algebraically identical to the
exponentially weighted sample covariance of the returns. The regression
residuals are uncorrelated with the factors under the same weights, so
splitting returns into a factor part and a residual part and adding both
back recovers the total exactly. The factor structure only changes the
forecast in the second model, which drops the residual links between
stocks.

Predictions are made for 348 portfolios: each of the 46 stocks alone,
an equal-weighted portfolio, an IT basket (TCS, INFY, WIPRO, HCLTECH,
TECHM at 20% each), and 300 random portfolios of 10 stocks with random
weights (seed 1). Predicted volatility is the square root of w'Σw,
annualised by √252. Actual volatility is the standard deviation of the
portfolio's daily excess returns in the test year, annualised the same
way, with weights held fixed. Each run records the ratio predicted /
actual and the absolute percentage error.

**Results.** The table averages over the 302 portfolios that hold more
than one stock. Single stocks are left out because both models give them
identical predictions: a one-stock portfolio uses only that stock's own
variance, and the models differ only in the links between stocks.

Mean absolute percentage error (lower is better):

| Half-life | 2024 Full | 2024 Factor | 2025 Full | 2025 Factor |
|---|---|---|---|---|
| 21 | **21.3%** | 23.1% | **6.9%** | 7.4% |
| 42 | **23.7%** | 25.1% | 9.6% | 9.6% |
| 63 | **23.0%** | 24.2% | 10.9% | **10.7%** |
| 126 | **14.1%** | 15.5% | 11.0% | **10.7%** |
| 252 | 6.7% | **6.2%** | 16.4% | **15.1%** |
| 504 | 16.8% | **15.2%** | 27.8% | **26.1%** |
| Infinite | 34.4% | **32.5%** | 47.7% | **45.5%** |

Mean ratio of predicted to actual volatility (1 is unbiased):

| Half-life | 2024 Full | 2024 Factor | 2025 Full | 2025 Factor |
|---|---|---|---|---|
| 21 | 0.79 | 0.77 | 1.02 | 1.01 |
| 42 | 0.76 | 0.75 | 1.09 | 1.08 |
| 63 | 0.77 | 0.76 | 1.10 | 1.09 |
| 126 | 0.86 | 0.85 | 1.11 | 1.09 |
| 252 | 1.02 | 1.01 | 1.16 | 1.15 |
| 504 | 1.16 | 1.15 | 1.28 | 1.26 |
| Infinite | 1.34 | 1.32 | 1.48 | 1.46 |

**Choice of half-life.** A half-life of 252 days gave the lowest error
in 2024. Carried forward to 2025, the honest choice, it gives 16.4% for
full covariance. A half-life of 21 days would have given 6.9% in 2025,
but that is known only after seeing 2025, so it is not a usable result.
Equal weighting over-predicts risk in both years, by 1.34 times in 2024
and 1.48 times in 2025 for full covariance.

**Choice of model.** The factor + diagonal model predicts lower risk
than full covariance in all 14 runs, because most residual correlations
are positive and the model sets them to zero. It has the lower error in
8 of the 14 runs, including half-life 252 in both years. Those are the
runs where full covariance was already over-predicting. Where full
covariance under-predicted (2024, half-lives 21 to 126), the factor
model is worse. The lower average error therefore reflects a downward
shift in the forecast, not evidence that the factors describe risk
better.

**IT basket, half-life 252:**

| Year | Actual | Full covariance | Factor + diagonal |
|---|---|---|---|
| 2024 | 19.9% | 20.8% (+4.2%) | 17.5% (-12.2%) |
| 2025 | 20.7% | 20.2% (-2.2%) | 16.5% (-20.2%) |

For a portfolio concentrated in one sector, the factor model understates
risk by 12% to 20%. The residual links between IT stocks, which the four
factors do not capture, are exactly what it removes. Under-prediction is
the more dangerous error for a risk forecast, and the average across
portfolios hides it.

The averages for all 348 portfolios, single stocks included, are in
`data/summary_all.rds`. They point the same way with smaller gaps
between the models.

## Structure

    .
    ├── 1_get_prices.R          downloads raw data (network)
    ├── 2_build_panel.R         builds the panel (network)
    ├── 3_charts.R              builds the proposal charts only
    ├── 4_estimates.R           loadings, bootstrap intervals, factor volatility, residuals
    ├── 5_backtest.R            out-of-sample check, two models, seven half-lives
    ├── charts.R                chart functions used by the dashboard
    ├── data_raw/
    │   ├── ind_nifty50list.csv           NSE constituent snapshot
    │   ├── ff_india_daily_2025_12.csv    IIM-A factor library
    │   └── nse_prices_raw.csv            Yahoo price series
    ├── data/
    │   ├── panel.rds           derived panel, regenerable
    │   ├── estimates.rds       output of 4_estimates.R
    │   ├── backtest.rds        output of 5_backtest.R, one row per run and portfolio
    │   ├── summary_all.rds     averages over all 348 portfolios
    │   └── summary_multi.rds   averages over the 302 multi-stock portfolios
    ├── MDS202613Proposal.Rmd   proposal source
    ├── MDS202613Proposal.pdf   proposal
    └── india-factor-risk-dashboard.Rproj