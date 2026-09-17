# Factor Risk Analytics for Indian equities

A stock's daily return mixes several things at once: the market's move, tilts toward small or value or momentum names, and whatever is specific to the company. Separating those matters because the first parts are exposures anyone can buy cheaply, while only the last is particular to holding that stock, and a portfolio that looks diversified by position count can turn out to be a single factor bet.
This repository builds a daily panel of 46 NIFTY 50 constituents from January 2020 to December 2025, merged with the IIM-Ahmedabad Fama-French and momentum factor library, and estimates factor loadings and risk decompositions from it.

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

## Structure

    .
    ├── 1_get_prices.R          downloads raw data (network)
    ├── 2_build_panel.R         builds the panel (network)
    ├── 3_charts.R              builds the proposal charts
    ├── data_raw/
    │   ├── ind_nifty50list.csv           NSE constituent snapshot
    │   ├── ff_india_daily_2025_12.csv    IIM-A factor library
    │   └── nse_prices_raw.csv            Yahoo price series
    ├── data/
    │   └── panel.rds           derived panel, regenerable
    └── Visual by SR.Rproj