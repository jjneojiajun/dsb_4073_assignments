# =======================================================
# Problem 1: CryptoCurrencies & R MarkDown
# =======================================================

all_crypto <- read.csv("crypto-markets.csv")

# Structure of the CSV File
str(all_crypto)

# ==============================================================
# Pre-processing
# ==============================================================

# Get the cryptocurrencies that we want
# ==============================================================
# BTC, ETH, XRP, BCH, LTC
# ==============================================================

btc <- all_crypto[all_crypto$symbol == 'BTC',]
eth <- all_crypto[all_crypto$symbol == 'ETH',]
xrp <- all_crypto[all_crypto$symbol == 'XRP',]
bch <- all_crypto[all_crypto$symbol == 'BCH',]

ltc <- all_crypto[all_crypto$symbol == 'LTC',]

# Filter the columns:
# ===============================================================
# Column we need : symbol, date, open, close 
# ===============================================================

btc_final <- data.frame(btc$symbol, btc$date, btc$open, btc$close)
colnames(btc_final) <- c("symbol", "date", "open", "close")

eth_final <- data.frame(eth$symbol, eth$date, eth$open, eth$close)
colnames(eth_final) <- c("symbol", "date", "open", "close")

xrp_final <- data.frame(xrp$symbol, xrp$date, xrp$open, xrp$close)
colnames(xrp_final) <- c("symbol", "date", "open", "close")

bch_final <- data.frame(bch$symbol, bch$date, bch$open, bch$close)
colnames(bch_final) <- c("symbol", "date", "open", "close")

ltc_final <- data.frame(ltc$symbol, ltc$date, ltc$open, ltc$close)
colnames(ltc_final) <- c("symbol", "date", "open", "close")

# Format the date & symbol
# ================================================================
# Current implementation is Factor
# ================================================================
btc_final$date <- as.Date(as.factor(btc_final$date))
btc_final$symbol <- as.character(as.factor(btc_final$symbol))

eth_final$date <- as.Date(as.factor(eth_final$date))
eth_final$symbol <- as.character(as.factor(eth_final$symbol))

xrp_final$date <- as.Date(as.factor(xrp_final$date))
xrp_final$symbol <- as.character(as.factor(xrp_final$symbol))

bch_final$date <- as.Date(as.factor(bch_final$date))
bch_final$symbol <- as.character(as.factor(bch_final$symbol))

ltc_final$date <- as.Date(as.factor(ltc_final$date))
ltc_final$symbol <- as.character(as.factor(ltc_final$symbol))

# ==============================================================
# Sorting the date 
# ==============================================================

btc_sorted_final <- btc_final[btc_final$date >= "2017-04-01" & btc_final$date <= "2018-04-15",]
eth_sorted_final <- eth_final[eth_final$date >= "2017-04-01" & eth_final$date <= "2018-04-15",]
xrp_sorted_final <- xrp_final[xrp_final$date >= "2017-04-01" & xrp_final$date <= "2018-04-15",]
bch_sorted_final <- bch_final[bch_final$date >= "2017-04-01" & bch_final$date <= "2018-04-15",]
ltc_sorted_final <- ltc_final[ltc_final$date >= "2017-04-01" & ltc_final$date <= "2018-04-15",]

# ===============================================================
# Data Preprocessing Done! 
# ===============================================================

library("ggplot2")
library("tseries")
library("forecast")

# ================================================================
# BITCOIN!
# ================================================================

# plot open price
ggplot() + 
  geom_line(data = btc_sorted_final, aes(x = date, y = open, color="Close Price"))

summary(btc_sorted_final$open)

# plot closed price 

ggplot() + 
  geom_line(data = btc_sorted_final, aes(x = date, y = close, color="Close Price"))

summary(btc_sorted_final$close)


# I know that in the month of December 2017 Bitcoin prices surges in a ridiculous rate.

# This affect the whole time series data as I can see the price before that huge spike
# to have a model of its own. Also the prices before let's say 6000 are completely useless
# now. 

btc_latest_model <- btc_sorted_final[btc_sorted_final$date >= "2017-04-15" & btc_sorted_final$date <= "2018-04-15",]
btc_latest_model$open_ma = ma(btc_latest_model$open, order=7) # Weekly Moving Average
btc_latest_model$open_ma30 = ma(btc_latest_model$open, order=30) # Monthly Moving Average

btc_latest_model$close_ma = ma(btc_latest_model$close, order=7) # Weekly Moving Average
btc_latest_model$close_ma30 = ma(btc_latest_model$close, order=30) # Monthly Moving Average

ggplot() + 
  geom_line(data = btc_latest_model, aes(x = date, y = open, color="open prices")) +
  geom_line(data = btc_latest_model, aes(x = date, y = open_ma, color="weekly ma")) +
  geom_line(data = btc_latest_model, aes(x = date, y = open_ma30, color="monthly ma")) 

ggplot() + 
  geom_line(data = btc_latest_model, aes(x = date, y = open, color="open prices")) +
  geom_line(data = btc_latest_model, aes(x = date, y = close_ma, color="weekly ma")) +
  geom_line(data = btc_latest_model, aes(x = date, y = close_ma30, color="monthly ma")) 


# ============================================================================
# Predict Bitcoin Open Price  
# ============================================================================

btc_open_ma <- ts(na.omit(btc_latest_model$open_ma))

# Stationarity
adf.test(btc_open_ma, alternative = "stationary")
# Stop if p-value < 0.05

btcLog <- log(btc_open_ma)
plot(btcLog, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(btcLog)
# Stop if p-value < 0.05

btcLogDiff <- diff(btcLog)
plot(btcLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(btcLogDiff)
# Stop if p-value < 0.05

# ==========================================================
# ACF and PACF 
# ==========================================================

acf(btcLogDiff, main='ACF For BTC Differenced Series')
# q is 5

pacf(btcLogDiff, main='PACF For BTC Differenced Series')
# p is 4

# Arima Based on guessing
arimaFit <- arima(btcLogDiff, order = c(4,0,5))
arimaFit            # check the coefficients
plot(arimaFit)

arimaFitFC <- forecast(btcLogDiff, model = arimaFit, h = 10)
plot(btcLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaFitFC$fitted, type="l", col = "red", lwd = 2)

plot(arimaFitFC)

tsdisplay(residuals(arimaFit), lag.max = 80)


# -------------------------------------------------------
# Find the "optimal" ARIMA parameters

auto.arima(btcLogDiff,                   # the dataset
           seasonal = FALSE,            # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Fit the "optimal" ARIMA(p,d,q) model
arimaOpt <- arima(btcLogDiff, order = c(1,0,1))
arimaOpt
plot(arimaOpt)

# Forecast using optimal ARIMA
# Within the training dataset
arimaOptFC <- forecast(btcLogDiff, model = arimaOpt, h = 10)
plot(btcLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaOptFC$fitted, type="l", col = "red", lwd = 2)
# Beyond the training dataset
plot(arimaOptFC)

# =======================================================
# ARIMA(p,d,q)(P,D,Q)[s] modeling of Seasonal Time Series
plot(btcLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")

auto.arima(btcLogDiff,                   # the dataset
           seasonal = TRUE,             # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Not SEASONAL! :D

# Check the performance/accuracy
tsdisplay(residuals(arimaOpt), lag.max = 80)

# ============================================================================
# Predict Bitcoin Close Price  
# ============================================================================

btc_close_ma <- ts(na.omit(btc_latest_model$close_ma))

# Stationarity
adf.test(btc_close_ma, alternative = "stationary")
# Stop if p-value < 0.05

btcCloseLog <- log(btc_close_ma)
plot(btcCloseLog, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(btcCloseLog)
# Stop if p-value < 0.05

btcCloseLogDiff <- diff(btcCloseLog)
plot(btcCloseLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(btcCloseLogDiff)
# Stop if p-value < 0.05

# ==========================================================
# ACF and PACF 
# ==========================================================

acf(btcCloseLogDiff, main='ACF For BTC Differenced Series')
# q is 2

pacf(btcCloseLogDiff, main='PACF For BTC Differenced Series')
# p is 4

# Arima Based on guessing
arimaFit <- arima(btcCloseLogDiff, order = c(4,0,2))
arimaFit            # check the coefficients
plot(arimaFit)

arimaFitFC <- forecast(btcCloseLogDiff, model = arimaFit, h = 10)
plot(btcCloseLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaFitFC$fitted, type="l", col = "red", lwd = 2)

plot(arimaFitFC)

tsdisplay(residuals(arimaFit), lag.max = 80)


# -------------------------------------------------------
# Find the "optimal" ARIMA parameters

auto.arima(btcCloseLogDiff,                   # the dataset
           seasonal = FALSE,            # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Fit the "optimal" ARIMA(p,d,q) model
arimaOpt <- arima(btcCloseLogDiff, order = c(1,0,1))
arimaOpt
plot(arimaOpt)

# Forecast using optimal ARIMA
# Within the training dataset
arimaOptFC <- forecast(btcCloseLogDiff, model = arimaOpt, h = 10)
plot(btcCloseLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaOptFC$fitted, type="l", col = "red", lwd = 2)
# Beyond the training dataset
plot(arimaOptFC)
tsdisplay(residuals(arimaOpt), lag.max = 80)


# =======================================================
# ARIMA(p,d,q)(P,D,Q)[s] modeling of Seasonal Time Series
plot(btcCloseLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")

auto.arima(btcCloseLogDiff,                   # the dataset
           seasonal = TRUE,             # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Not SEASONAL! :D

# ------------------------------------------------------------
# Ethereum 
# -----------------------------------------------------------

# plot
ggplot() + 
  geom_line(data = eth_sorted_final, aes(x = date, y = open, color="Open Price"))

summary(eth_sorted_final$open)

# I know that in the month of December 2017 Crypto prices surges in a ridiculous rate.

# This affect the whole time series data as I can see the price before that huge spike
# to have a model of its own. Also the prices before let's say 6000 are completely useless
# now. 

eth_latest_model <- eth_sorted_final[eth_sorted_final$date >= "2017-11-15" & eth_sorted_final$date <= "2018-04-15",]
eth_latest_model$open_ma = ma(eth_latest_model$open, order=7) # Weekly Moving Average
eth_latest_model$open_ma30 = ma(eth_latest_model$open, order=30) # Monthly Moving Average

eth_latest_model$close_ma = ma(eth_latest_model$close, order=7) # Weekly Moving Average
eth_latest_model$close_ma30 = ma(eth_latest_model$close, order=30) # Monthly Moving Average

ggplot() + 
  geom_line(data = eth_latest_model, aes(x = date, y = open, color="open Price")) + 
  geom_line(data = eth_latest_model, aes(x = date, y = open_ma, color="weekly moving average") ) + 
  geom_line(data = eth_latest_model, aes(x = date, y = open_ma30, color="weekly moving average") )

# ==================================================================================
# Ethereum Open Price
# ==================================================================================

eth_open_ma <- ts(na.omit(eth_latest_model$open_ma))


# From the Decomposed data, we can see that it is very seasonal, and there's trend is going down

# Stationarity
adf.test(eth_open_ma, alternative = "stationary")
# Stop if p-value < 0.05


# -------------------------------------------------------
# Stationarize the Time Series before fitting ARIMA
# Desirable Stationarity properties are as follows:
# -- Time-independent Variance
# -- Time-independent Mean
# -- Time-independent Autocorrelation

ethLog <- log(eth_open_ma)
plot(ethLog, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(ethLog)

ethLogDiff <- diff(ethLog)
plot(ethLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(ethLogDiff)

# ==========================================================
# ACF and PACF 
# ==========================================================

acf(ethLogDiff, main='ACF For BTC Differenced Series')
# q is 5

pacf(ethLogDiff, main='PACF For BTC Differenced Series')
# p is 4

# Arima Based on guessing
arimaFit <- arima(ethLogDiff, order = c(4,0,5))
arimaFit            # check the coefficients
plot(arimaFit)

arimaFitFC <- forecast(ethLogDiff, model = arimaFit, h = 10)
plot(ethLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaFitFC$fitted, type="l", col = "red", lwd = 2)

plot(arimaFitFC)

tsdisplay(residuals(arimaFit), lag.max = 80)

# Residuals is okay! but the ACF and PACF do have one or two lines coming out!

# -------------------------------------------------------
# Find the "optimal" ARIMA parameters

auto.arima(ethLogDiff,                   # the dataset
           seasonal = FALSE,            # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Fit the "optimal" ARIMA(p,d,q) model
arimaOpt <- arima(ethLogDiff, order = c(3,0,5))
arimaOpt
plot(arimaOpt)

# Forecast using optimal ARIMA
# Within the training dataset
arimaOptFC <- forecast(ethLogDiff, model = arimaOpt, h = 10)
plot(ethLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaOptFC$fitted, type="l", col = "red", lwd = 2)
# Beyond the training dataset
plot(arimaOptFC)

tsdisplay(residuals((arimaOpt), lag.max = 80))

# =======================================================
# ARIMA(p,d,q)(P,D,Q)[s] modeling of Seasonal Time Series
plot(ethLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")

auto.arima(ethLogDiff,                   # the dataset
           seasonal = TRUE,             # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# It is not seasonal! 

# ============================================================================
# Predict ETHEREUM Close Price  
# ============================================================================

eth_close_ma <- ts(na.omit(eth_latest_model$close_ma))

# Stationarity
adf.test(eth_close_ma, alternative = "stationary")
# Stop if p-value < 0.05

ethCloseLog <- log(eth_close_ma)
plot(ethCloseLog, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(ethCloseLog)
# Stop if p-value < 0.05

ethCloseLogDiff <- diff(ethCloseLog)
plot(ethCloseLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(ethCloseLogDiff)
# Stop if p-value < 0.05

# ==========================================================
# ACF and PACF 
# ==========================================================

acf(ethCloseLogDiff, main='ACF For BTC Differenced Series')
# q is 2

pacf(ethCloseLogDiff, main='PACF For BTC Differenced Series')
# p is 4

# Arima Based on guessing
arimaFit <- arima(ethCloseLogDiff, order = c(4,0,2))
arimaFit            # check the coefficients
plot(arimaFit)

arimaFitFC <- forecast(ethCloseLogDiff, model = arimaFit, h = 10)
plot(ethCloseLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaFitFC$fitted, type="l", col = "red", lwd = 2)

plot(arimaFitFC)

tsdisplay(residuals(arimaFit), lag.max = 80)


# -------------------------------------------------------
# Find the "optimal" ARIMA parameters

auto.arima(ethCloseLogDiff,                   # the dataset
           seasonal = FALSE,            # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Fit the "optimal" ARIMA(p,d,q) model
arimaOpt <- arima(ethCloseLogDiff, order = c(1,0,1))
arimaOpt
plot(arimaOpt)

# Forecast using optimal ARIMA
# Within the training dataset
arimaOptFC <- forecast(ethCloseLogDiff, model = arimaOpt, h = 10)
plot(ethCloseLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaOptFC$fitted, type="l", col = "red", lwd = 2)
# Beyond the training dataset
plot(arimaOptFC)
tsdisplay(residuals(arimaOpt), lag.max = 80)


# =======================================================
# ARIMA(p,d,q)(P,D,Q)[s] modeling of Seasonal Time Series
plot(ethCloseLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")

auto.arima(ethCloseLogDiff,                   # the dataset
           seasonal = TRUE,             # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Not SEASONAL! :D

# ------------------------------------------------------------
# Ripple 
# -----------------------------------------------------------

# plot
ggplot() + 
  geom_line(data = xrp_sorted_final, aes(x = date, y = open, color="Open Price"))

summary(xrp_sorted_final$open)

# I know that in the month of December 2017 Crypto prices surges in a ridiculous rate.

# This affect the whole time series data as I can see the price before that huge spike
# to have a model of its own. Also the prices before let's say 6000 are completely useless
# now. 

xrp_latest_model <- xrp_sorted_final[xrp_sorted_final$date >= "2017-04-15" & xrp_sorted_final$date <= "2018-04-15",]
xrp_latest_model$open_ma = ma(xrp_latest_model$open, order=7) # Weekly Moving Average
xrp_latest_model$open_ma30 = ma(xrp_latest_model$open, order=30) # Monthly Moving Average

xrp_latest_model$close_ma = ma(xrp_latest_model$close, order=7)
xrp_latest_model$close_ma30 = ma(xrp_latest_model$close, order=30)


ggplot() + 
  geom_line(data = xrp_latest_model, aes(x = date, y = open, color="open Price")) + 
  geom_line(data = xrp_latest_model, aes(x = date, y = open_ma, color="weekly moving average") ) + 
  geom_line(data = xrp_latest_model, aes(x = date, y = open_ma30, color="weekly moving average") )


# ==================================================================================
# Ripple Open Price
# ==================================================================================

xrp_open_ma <- ts(na.omit(xrp_latest_model$open_ma))

# From the Decomposed data, we can see that it is very seasonal, and there's trend is going down

# Stationarity
adf.test(xrp_open_ma, alternative = "stationary")
# Stop if p-value < 0.05

xrpLog <- log(xrp_open_ma)
plot(xrpLog, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(xrpLog)

xrpLogDiff <- diff(xrpLog)
plot(xrpLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(xrpLogDiff)


# ==========================================================
# ACF and PACF 
# ==========================================================

acf(xrpLogDiff, main='ACF For BTC Differenced Series')
# q is 1

pacf(xrpLogDiff, main='PACF For BTC Differenced Series')
# p is 3

# Arima Based on guessing
arimaFit <- arima(xrpLogDiff, order = c(3,0,1))
arimaFit            # check the coefficients
plot(arimaFit)

arimaFitFC <- forecast(xrpLogDiff, model = arimaFit, h = 10)
plot(xrpLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaFitFC$fitted, type="l", col = "red", lwd = 2)

plot(arimaFitFC)

tsdisplay(residuals(arimaFit), lag.max = 80)

# Residuals is okay! but the ACF and PACF do have one or two lines coming out!

# -------------------------------------------------------
# Find the "optimal" ARIMA parameters

auto.arima(xrpLogDiff,                   # the dataset
           seasonal = FALSE,            # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Fit the "optimal" ARIMA(p,d,q) model
arimaOpt <- arima(xrpLogDiff, order = c(1,0,0))
arimaOpt
plot(arimaOpt)

# Forecast using optimal ARIMA
# Within the training dataset
arimaOptFC <- forecast(xrpLogDiff, model = arimaOpt, h = 10)
plot(xrpLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaOptFC$fitted, type="l", col = "red", lwd = 2)
# Beyond the training dataset
plot(arimaOptFC)

tsdisplay(residuals((arimaOpt), lag.max = 80))

# =======================================================
# ARIMA(p,d,q)(P,D,Q)[s] modeling of Seasonal Time Series
plot(xrpLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")

auto.arima(xrpLogDiff,                   # the dataset
           seasonal = TRUE,             # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# It is not seasonal! 

# ==================================================================================
# Ripple Close Price
# ==================================================================================

xrp_close_ma <- ts(na.omit(xrp_latest_model$close_ma))

# Stationarity
adf.test(xrp_close_ma, alternative = "stationary")
# Stop if p-value < 0.05

xrpCloseLog <- log(xrp_close_ma)
plot(xrpCloseLog, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(xrpCloseLog)
# Stop if p-value < 0.05

xrpCloseLogDiff <- diff(xrpCloseLog)
plot(xrpCloseLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(xrpCloseLogDiff)
# Stop if p-value < 0.05

# ==========================================================
# ACF and PACF 
# ==========================================================

acf(xrpCloseLogDiff, main='ACF For BTC Differenced Series')
# q is 2

pacf(xrpCloseLogDiff, main='PACF For BTC Differenced Series')
# p is 4

# Arima Based on guessing
arimaFit <- arima(xrpCloseLogDiff, order = c(4,0,2))
arimaFit            # check the coefficients
plot(arimaFit)

arimaFitFC <- forecast(xrpCloseLogDiff, model = arimaFit, h = 10)
plot(xrpCloseLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaFitFC$fitted, type="l", col = "red", lwd = 2)

plot(arimaFitFC)

tsdisplay(residuals(arimaFit), lag.max = 80)


# -------------------------------------------------------
# Find the "optimal" ARIMA parameters

auto.arima(xrpCloseLogDiff,                   # the dataset
           seasonal = FALSE,            # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Fit the "optimal" ARIMA(p,d,q) model
arimaOpt <- arima(xrpCloseLogDiff, order = c(1,0,1))
arimaOpt
plot(arimaOpt)

# Forecast using optimal ARIMA
# Within the training dataset
arimaOptFC <- forecast(xrpCloseLogDiff, model = arimaOpt, h = 10)
plot(xrpCloseLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaOptFC$fitted, type="l", col = "red", lwd = 2)
# Beyond the training dataset
plot(arimaOptFC)
tsdisplay(residuals(arimaOpt), lag.max = 80)


# =======================================================
# ARIMA(p,d,q)(P,D,Q)[s] modeling of Seasonal Time Series
plot(xrpCloseLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")

auto.arima(xrpCloseLogDiff,                   # the dataset
           seasonal = TRUE,             # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Not SEASONAL! :D

# ------------------------------------------------------------
# Bitcoin Cash 
# -----------------------------------------------------------

# plot
ggplot() + 
  geom_line(data = bch_sorted_final, aes(x = date, y = open, color="Open Price"))

summary(bch_sorted_final$open)

# I know that in the month of December 2017 Crypto prices surges in a ridiculous rate.

# This affect the whole time series data as I can see the price before that huge spike
# to have a model of its own. Also the prices before let's say 6000 are completely useless
# now. 

bch_latest_model <- bch_sorted_final[bch_sorted_final$date >= "2017-04-15" & bch_sorted_final$date <= "2018-04-15",]
bch_latest_model$open_ma = ma(bch_latest_model$open, order=7) # Weekly Moving Average
bch_latest_model$open_ma30 = ma(bch_latest_model$open, order=30) # Monthly Moving Average

bch_latest_model$close_ma = ma(bch_latest_model$close, order=7) # Weekly Moving Average
bch_latest_model$close_ma30 = ma(bch_latest_model$close, order=30) # Monthly Moving Average

ggplot() + 
  geom_line(data = bch_latest_model, aes(x = date, y = open, color="open Price")) + 
  geom_line(data = bch_latest_model, aes(x = date, y = open_ma, color="weekly moving average") ) + 
  geom_line(data = bch_latest_model, aes(x = date, y = open_ma30, color="weekly moving average") )

bch_open_ma <- ts(na.omit(bch_latest_model$open_ma))

# From the Decomposed data, we can see that it is very seasonal, and there's trend is going down

# Stationarity
adf.test(bch_open_ma, alternative = "stationary")
# Stop if p-value < 0.05

bchLog <- log(bch_open_ma)
plot(bchLog, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(bchLog)

bchLogDiff <- diff(bchLog)
plot(bchLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(bchLogDiff)


# ==========================================================
# ACF and PACF 
# ==========================================================

acf(bchLogDiff, main='ACF For BTC Differenced Series')
# q is 1

pacf(bchLogDiff, main='PACF For BTC Differenced Series')
# p is 4

# Arima Based on guessing
arimaFit <- arima(bchLogDiff, order = c(3,0,3))
arimaFit            # check the coefficients
plot(arimaFit)

arimaFitFC <- forecast(bchLogDiff, model = arimaFit, h = 10)
plot(bchLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaFitFC$fitted, type="l", col = "red", lwd = 2)

plot(arimaFitFC)

tsdisplay(residuals(arimaFit), lag.max = 80)

# Residuals is okay! but the ACF and PACF do have one or two lines coming out!

# -------------------------------------------------------
# Find the "optimal" ARIMA parameters

auto.arima(bchLogDiff,                   # the dataset
           seasonal = FALSE,            # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Fit the "optimal" ARIMA(p,d,q) model
arimaOpt <- arima(bchLogDiff, order = c(2,0,3))
arimaOpt
plot(arimaOpt)

# Forecast using optimal ARIMA
# Within the training dataset
arimaOptFC <- forecast(bchLogDiff, model = arimaOpt, h = 10)
plot(bchLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaOptFC$fitted, type="l", col = "red", lwd = 2)
# Beyond the training dataset
plot(arimaOptFC)

tsdisplay(residuals((arimaOpt), lag.max = 80))

# =======================================================
# ARIMA(p,d,q)(P,D,Q)[s] modeling of Seasonal Time Series
plot(bchLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")

auto.arima(bchLogDiff,                   # the dataset
           seasonal = TRUE,             # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# ==================================================================================
# Bitcoin Cash Close Price
# ==================================================================================

bch_close_ma <- ts(na.omit(bch_latest_model$close_ma))

# Stationarity
adf.test(bch_close_ma, alternative = "stationary")
# Stop if p-value < 0.05

bchCloseLog <- log(bch_close_ma)
plot(bchCloseLog, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(bchCloseLog)
# Stop if p-value < 0.05

bchCloseLogDiff <- diff(bchCloseLog)
plot(bchCloseLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(bchCloseLogDiff)
# Stop if p-value < 0.05

# ==========================================================
# ACF and PACF 
# ==========================================================

acf(bchCloseLogDiff, main='ACF For BTC Differenced Series')
# q is 2

pacf(bchCloseLogDiff, main='PACF For BTC Differenced Series')
# p is 4

# Arima Based on guessing
arimaFit <- arima(bchCloseLogDiff, order = c(4,0,2))
arimaFit            # check the coefficients
plot(arimaFit)

arimaFitFC <- forecast(bchCloseLogDiff, model = arimaFit, h = 10)
plot(bchCloseLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaFitFC$fitted, type="l", col = "red", lwd = 2)

plot(arimaFitFC)

tsdisplay(residuals(arimaFit), lag.max = 80)


# -------------------------------------------------------
# Find the "optimal" ARIMA parameters

auto.arima(bchCloseLogDiff,                   # the dataset
           seasonal = FALSE,            # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Fit the "optimal" ARIMA(p,d,q) model
arimaOpt <- arima(bchCloseLogDiff, order = c(1,0,1))
arimaOpt
plot(arimaOpt)

# Forecast using optimal ARIMA
# Within the training dataset
arimaOptFC <- forecast(bchCloseLogDiff, model = arimaOpt, h = 10)
plot(bchCloseLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaOptFC$fitted, type="l", col = "red", lwd = 2)
# Beyond the training dataset
plot(arimaOptFC)
tsdisplay(residuals(arimaOpt), lag.max = 80)


# =======================================================
# ARIMA(p,d,q)(P,D,Q)[s] modeling of Seasonal Time Series
plot(bchCloseLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")

auto.arima(bchCloseLogDiff,                   # the dataset
           seasonal = TRUE,             # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Not SEASONAL! :D

# ------------------------------------------------------------
# LiteCoin 
# -----------------------------------------------------------

# plot
ggplot() + 
  geom_line(data = ltc_sorted_final, aes(x = date, y = open, color="Open Price"))

summary(ltc_sorted_final$open)

# I know that in the month of December 2017 Crypto prices surges in a ridiculous rate.

# This affect the whole time series data as I can see the price before that huge spike
# to have a model of its own. Also the prices before let's say 6000 are completely useless
# now. 

ltc_latest_model <- ltc_sorted_final[ltc_sorted_final$date >= "2017-04-15" & ltc_sorted_final$date <= "2018-04-15",]
ltc_latest_model$open_ma = ma(ltc_latest_model$open, order=7) # Weekly Moving Average
ltc_latest_model$open_ma30 = ma(ltc_latest_model$open, order=30) # Monthly Moving Average

ltc_latest_model$close_ma = ma(ltc_latest_model$close, order=7) # Monthly Moving Average
ltc_latest_model$close_ma30 = ma(ltc_latest_model$close, order=30) # Monthly Moving Average

ggplot() + 
  geom_line(data = ltc_latest_model, aes(x = date, y = open, color="open Price")) + 
  geom_line(data = ltc_latest_model, aes(x = date, y = open_ma, color="weekly moving average") ) + 
  geom_line(data = ltc_latest_model, aes(x = date, y = open_ma30, color="weekly moving average") )

ltc_open_ma <- ts(na.omit(ltc_latest_model$open_ma))

# From the Decomposed data, we can see that it is very seasonal, and there's trend is going down

# Stationarity
adf.test(ltc_open_ma, alternative = "stationary")
# Stop if p-value < 0.05

ltcLog <- log(ltc_open_ma)
plot(ltcLog, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(ltcLog)

ltcLogDiff <- diff(ltcLog)
plot(ltcLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(ltcLogDiff)


# ==========================================================
# ACF and PACF 
# ==========================================================

acf(ltcLogDiff, main='ACF For BTC Differenced Series')
# q is 2

pacf(ltcLogDiff, main='PACF For BTC Differenced Series')
# p is 3

# Arima Based on guessing
arimaFit <- arima(ltcLogDiff, order = c(3,0,2))
arimaFit            # check the coefficients
plot(arimaFit)

arimaFitFC <- forecast(ltcLogDiff, model = arimaFit, h = 10)
plot(ltcLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaFitFC$fitted, type="l", col = "red", lwd = 2)

plot(arimaFitFC)

tsdisplay(residuals(arimaFit), lag.max = 80)

# Residuals is okay! but the ACF and PACF do have one or two lines coming out!

# -------------------------------------------------------
# Find the "optimal" ARIMA parameters

auto.arima(ltcLogDiff,                   # the dataset
           seasonal = FALSE,            # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Fit the "optimal" ARIMA(p,d,q) model
arimaOpt <- arima(ltcLogDiff, order = c(2,0,1))
arimaOpt
plot(arimaOpt)

# Forecast using optimal ARIMA
# Within the training dataset
arimaOptFC <- forecast(ltcLogDiff, model = arimaOpt, h = 10)
plot(ltcLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaOptFC$fitted, type="l", col = "red", lwd = 2)
# Beyond the training dataset
plot(arimaOptFC)

tsdisplay(residuals((arimaOpt), lag.max = 80))

# =======================================================
# ARIMA(p,d,q)(P,D,Q)[s] modeling of Seasonal Time Series
plot(bchLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")

auto.arima(ltcLogDiff,                   # the dataset
           seasonal = TRUE,             # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# It is not seasonal! 

# ==================================================================================
# Litecoin Close Price
# ==================================================================================

ltc_close_ma <- ts(na.omit(ltc_latest_model$close_ma))

# Stationarity
adf.test(ltc_close_ma, alternative = "stationary")
# Stop if p-value < 0.05

ltcCloseLog <- log(ltc_close_ma)
plot(ltcCloseLog, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(ltcCloseLog)
# Stop if p-value < 0.05

ltcCloseLogDiff <- diff(ltcCloseLog)
plot(ltcCloseLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")
adf.test(ltcCloseLogDiff)
# Stop if p-value < 0.05

# ==========================================================
# ACF and PACF 
# ==========================================================

acf(ltcCloseLogDiff, main='ACF For BTC Differenced Series')
# q is 2

pacf(ltcCloseLogDiff, main='PACF For BTC Differenced Series')
# p is 4

# Arima Based on guessing
arimaFit <- arima(ltcCloseLogDiff, order = c(4,0,2))
arimaFit            # check the coefficients
plot(arimaFit)

arimaFitFC <- forecast(ltcCloseLogDiff, model = arimaFit, h = 10)
plot(ltcCloseLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaFitFC$fitted, type="l", col = "red", lwd = 2)

plot(arimaFitFC)

tsdisplay(residuals(arimaFit), lag.max = 80)


# -------------------------------------------------------
# Find the "optimal" ARIMA parameters

auto.arima(ltcCloseLogDiff,                   # the dataset
           seasonal = FALSE,            # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Fit the "optimal" ARIMA(p,d,q) model
arimaOpt <- arima(ltcCloseLogDiff, order = c(1,0,1))
arimaOpt
plot(arimaOpt)

# Forecast using optimal ARIMA
# Within the training dataset
arimaOptFC <- forecast(ltcCloseLogDiff, model = arimaOpt, h = 10)
plot(ltcCloseLogDiff, type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaOptFC$fitted, type="l", col = "red", lwd = 2)
# Beyond the training dataset
plot(arimaOptFC)
tsdisplay(residuals(arimaOpt), lag.max = 80)


# =======================================================
# ARIMA(p,d,q)(P,D,Q)[s] modeling of Seasonal Time Series
plot(ltcCloseLogDiff, type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")

auto.arima(ltcCloseLogDiff,                   # the dataset
           seasonal = TRUE,             # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported
