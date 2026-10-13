# Modelling Interval-Valued Data with Applications to Finance

R scripts for the numerical analysis in the manuscript *Modelling Interval-Valued Data with Applications to Finance*.

## Code

- `Code/Uniform.R` — Uniform model
- `Code/Lognormal.R` — Lognormal model
- `Code/Copula.R` — Gumbel copula model

## Data Collection

Historical daily stock-price data were obtained from the [NSE India website](https://www.nseindia.com/). The data were collected on 28 April 2026, before market opening, using the latest available observations up to 27 April 2026.

The analysis considers six stocks: State Bank of India (SBIN), Power Grid Corporation of India (POWERGRID), IRCTC, Reliance Industries (RELIANCE), Tata Consultancy Services (TCS), and HDFC Bank (HDFCBANK).

Four observation periods are considered: 1 month, 3 months, 6 months, and 1 year. The data include the date, daily high, daily low, and previous closing price.

Organise the CSV files locally as follows:

```text
Data/
├── 1m/
├── 3m/
├── 6m/
└── 1y/
```

Each folder should contain the corresponding stock CSV files, named according to the codes used in the scripts: `SB`, `PG`, `IR`, `RL`, `TC`, and `HD`. The raw stock-price data are not included in this repository and must be obtained independently.

## Requirements

The analysis was implemented in R. The `evd` and `VineCopula` packages are required for the copula analysis.

## Running the Code

Set the R working directory to the repository root. Place the data in the directory structure described above and run the scripts in the `Code` folder. The resulting tables are saved in the `Results` folder.
