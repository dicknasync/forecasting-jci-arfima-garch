# Forecasting JCI using ARFIMA-GARCH Model in R
This repository contains an R-based analysis for forecasting the Indonesian Composite Stock Price Index (JCI) using the ARFIMA-GARCH model. This approach effectively captures both long memory dependencies and volatility clustering, ensuring a more accurate time series forecast.

##Overview
Financial time series, including stock indices, often exhibit long-term memory and heteroscedasticity, which standard models may not adequately capture. To address this, we apply:

1. ARFIMA (Autoregressive Fractionally Integrated Moving Average): Captures long-range dependencies.
2. GARCH (Generalized Autoregressive Conditional Heteroskedasticity): Models time-dependent volatility.

This study aims to:
✅ Identify the best ARFIMA-GARCH model for forecasting JCI.
✅ Compare forecasting performance between ARFIMA and ARFIMA-GARCH.
✅ Predict JCI from January 2, 2024, to March 31, 2024.

##Key Findings
- Long memory behavior was detected in JCI data, making ARFIMA a suitable choice.
- The best ARFIMA model selected was ARFIMA(2,0.218,0) with an MAPE error rate of 5.880.
- Due to the presence of heteroscedasticity, an ARFIMA-GARCH approach was applied.
- The final selected model ARFIMA(2,0.218,0)-GARCH(1,1) achieved a superior accuracy with an MAPE of 3.319.
- The forecast indicates an upward trend in JCI throughout 2024, with values ranging from 7276.148 to 7740.125.
- The predicted variance remains stationary, aligning with ARFIMA-GARCH assumptions.
