#' ---
#' title: "Advanced Hedge Funds Final Project"
#' author: Charles Rogers
#' date: "5/11/2019"
#' ---

library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)
## Creates Currencies Portfolio##
symbols <- c("CROC","DBV","USDU","FXB")

currency('USD')
stock(symbols,
      currency = "USD",
      multiplier = 1)
## Sets up investment strategy parameters
init_date <- "2014-05-01"
start_date <- "2014-05-01"
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000; One Billion Dollars
adjustment <- TRUE
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date, 
           to = end_date,
           adjust = adjustment)
getSymbols(Symbols = "SPY",
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
stock(symbols,
      currency = "USD",
      multiplier = 1)
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"


### Main Strategy Code ###
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date) 
strategy(strategy.st, store = TRUE)

### Indicators and Trading Signals ###
fastSMA=60
slowSMA=180
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fastSMA),
              label = "nFast")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slowSMA),
              label = "nSlow")
#### More Signals and Trading Parameters ###
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")
#### Long/Short Trade Amounts ###

longSize=1000000
shortSize=20000000
### LONG Rules ###

add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = longSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "High",
                          TxnFees = 0,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
### Exit Long ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")
#### SHORT Rules ###

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -shortSize,
                          ordertype = "market",
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = 0,
                          prefer = "Low"),
         type = "enter", 
         label = "EnterSHORT")
### Exit Short ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")
### Stop Loss Rule ###
.stoploss <- .1 # <<<< Modify stop loss as % of entry price here
### Stop loss long ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = T)
## Stop loss short ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = FALSE, 
                          orderside = "short",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled =T)
#### End of Stop Loss code ###

### Gets Results for Signal Strategies ###
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}
results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
for(symbol in symbols) {
  print(symbol)
  chart.Posn(portfolio.st, Symbol = symbol,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
}

### Tabulate Result Stats by Individual Asset and Strategy ###
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats))

### Gets Total Portfolio Results ####
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance")

### Calculates the accumulated portfolio return ###
(end_eq[1237,]-1000000000)/1000000000 

### Calculates sharpe ratio ###
sharperatio_portfolio <- SharpeRatio.annualized(returns, Rf = 0, scale = NA, geometric = TRUE)
sharperatio_portfolio

### Calculates information ratio ###
library(PerformanceAnalytics)
Currencies_returns = Return.calculate(end_eq, method="log")
SPYreturns = Return.calculate(SPY[,4], method="log")
InformationRatio(SPYreturns,Currencies_returns)
Currenciesreturns<- Return.calculate(end_eq, method="log") 


### Creates MANAGED FUTURES Portfolio ###
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)

symbols <- c("FMF","WTMF")
currency('USD')
stock(symbols,
      currency = "USD",
      multiplier = 1)

## Sets up investment strategy parameters ###
init_date <- "2014-05-01"
start_date <- "2014-05-01"
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000; One Billion Dollars
adjustment <- TRUE 
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
stock(symbols,
      currency = "USD",
      multiplier = 1)
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"

### Main Strategy Code ###
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)
strategy(strategy.st, store = TRUE) 

### Signal and Indicators Range can be changed here ###
fastSMA=15
slowSMA=45
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fastSMA),
              label = "nFast")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slowSMA),
              label = "nSlow")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")

### Trade sizes/amounts for long/short ###
longSize=30000000
shortSize=10000000

### LONG Rules ###
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = longSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "High",
                          TxnFees = 0,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
### Exit Long ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")

### Short Rules ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -shortSize,
                          ordertype = "market",
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = 0,
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT") 
### Exit Short ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")
### Add stop loss rule ###
.stoploss <- .1 # <<<< Modify stop loss as % of entry price here

### Stop loss long rules ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = T)
### Stop loss short rules ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit", 
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled =T)
### End of Stop Loss code ###

### Gets results for signal/rules/strategies ###
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}
results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
for(symbol in symbols) {
  print(symbol)
  chart.Posn(portfolio.st, Symbol = symbol,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
}

### Tabulates Result Stats by Individual Asset/ Strategy ###
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats))

### Now get Total Portfolio Results ###
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance")

### Calculates accumulated portfolio return ###
(end_eq[1258,]-1000000000)/1000000000 

### Calculates sharpe ratio ###
sharperatio_portfolio <- SharpeRatio.annualized(returns, Rf = 0, scale = NA, geometric = TRUE)
sharperatio_portfolio

### Calculates information ratio ###
library(PerformanceAnalytics )
getSymbols(Symbols = "SPY",
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)

Managedfutures_returns<- Return.calculate(end_eq, method="log")

### Hedge Fund Replication Strategy ###
### Sets  up packages we need to use below ###
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)

symbols <- c("ALFA","CSM","HDG")

currency('USD')
stock(symbols,
      currency = "USD",
      multiplier = 1)
### Sets up investment strategy parameters ###
init_date <- "2014-05-01"
start_date <- "2014-05-01"
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000
adjustment <- TRUE 
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
stock(symbols,
      currency = "USD",
      multiplier = 1)
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"

### Main strategy code ###
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)
strategy(strategy.st, store = TRUE) 

### Edit Indicators and Signals here ###
# SMA speed can be modified here
fastSMA=30
slowSMA=150
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fastSMA),
              label = "nFast")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slowSMA),
              label = "nSlow")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")

### Trade sizes/amounts for long/short trades ###
longSize=50000000
shortSize=10000000

### LONG Rules
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = longSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "High",
                          TxnFees = 0,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
### Exit Long ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")

### SHORT rules ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -shortSize,
                          ordertype = "market",
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = 0,
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT") 
### Exit Short ###

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")

### Adds stop loss rule ###
.stoploss <- .05 # <<<< Modify stop loss as % of entry price here

### Stop loss long ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = T)

### Stop loss short ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit", 
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled =T)
### End of Stop Loss ###

### Gets results for signal/rules/strategies ###
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}
results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
for(symbol in symbols) {
  print(symbol)
  chart.Posn(portfolio.st, Symbol = symbol,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
}

### Tabulates Result Stats by Individual Asset/ Strategy ###
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats))

### Gets Total Portfolio Results ####
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance")

### Calculates accumulated portfolio return ###
(end_eq[1258,]-1000000000)/1000000000 

### Calculates sharpe ratio ###
sharperatio_portfolio <- SharpeRatio.annualized(returns, Rf = 0, scale = NA, geometric = TRUE)
sharperatio_portfolio

### Calculates information ratio ###
library(PerformanceAnalytics)
getSymbols(Symbols = "SPY",
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
SPYreturns = Return.calculate(SPY[,6], method="log")
InformationRatio(returns,SPYreturns)
Hedgefunds_returns<- Return.calculate(end_eq, method="log")

### Volatility Investing Strategy ###
### Sets up packages we need to use below ###
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)

symbols <- c("SVXY","VIXM","VIIX",
             "VIXY","UVXY","TVIX","ZIV")

currency('USD')
stock(symbols,
      currency = "USD",
      multiplier = 1)

### Sets up investment strategy parameters ###
init_date <- "2014-05-01"
start_date <- "2014-05-01"
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000
adjustment <- TRUE 
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
stock(symbols,
      currency = "USD",
      multiplier = 1)
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"

### Main Code for Strategy ###
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)
strategy(strategy.st, store = TRUE) 

### Signals and Indicators can be changed here ###
fastSMA=60
slowSMA=180
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fastSMA),
              label = "nFast")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slowSMA),
              label = "nSlow")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")

### Trade sizes/amounts for long/short trades ###
longSize=2000000
shortSize=1000000

### LONG Rules ###
### Enter Long ###
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = longSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "High",
                          TxnFees = 0,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
### Exit Long ###

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")

### SHORT Rules ###
### Enter Short ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -shortSize,
                          ordertype = "market",
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = 0,
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT") 
### Exit Short ###

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")
### Stop Loss Trading Rule ###
.stoploss <- .0 # <<<< Modify stop loss as % of entry price here

### Stop loss long ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = T)
### Stop loss short ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit", 
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled =T)
### End of Stop Loss code ###

### Gets results for signal/rules/strategies ###
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}
results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
for(symbol in symbols) {
  print(symbol)
  chart.Posn(portfolio.st, Symbol = symbol,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
}

### Tabulates Result Stats by Individual Asset/Strategy ###
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats))

### Gets Total Portfolio Results ###
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance")

### Calculates accumulated portfolio return ###
(end_eq[1258,]-1000000000)/1000000000 

### Calculates sharpe ratio ###
sharperatio_portfolio <- SharpeRatio.annualized(returns, Rf = 0, scale = NA, geometric = TRUE)
sharperatio_portfolio

### Calculates information ratio ###
library(PerformanceAnalytics)
getSymbols(Symbols = "SPY",
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
SPYreturns = Return.calculate(SPY[,6], method="log")
InformationRatio(returns,SPYreturns)
Volatility_returns<- Return.calculate(end_eq, method="log")


### Active/ Event Driven Strategy###
### Sets up packages we need to use below ###
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)

symbols <- c("IPO","FPX","MNA")

currency('USD')
stock(symbols,
      currency = "USD",
      multiplier = 1)

### Sets up investment strategy parameters ###
init_date <- "2014-05-01" 
start_date <- "2014-05-01"
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000; One Billion Dollars
adjustment <- TRUE
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
stock(symbols,
      currency = "USD",
      multiplier = 1)
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"

### Main code for strategy ###
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st, 
           symbols = symbols,
           initDate = init_date)
strategy(strategy.st, store = TRUE)

### Signals and Indicators can be changed here ###
fastSMA=15
slowSMA=150
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fastSMA),
              label = "nFast")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slowSMA),
              label = "nSlow")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short") 

### Trade Sizes/Amounts for Long/Short Trades ###
longSize=12500000
shortSize=0

### LONG Rules ###
### Enter Long ###
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = longSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "High",
                          TxnFees = 0,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")

### Exit Long ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")

### SHORT Rules ###
### Enter Short ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -shortSize,
                          ordertype = "market",
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = 0, 
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")
### Exit Short ###

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")

### Stop Loss for Trades ###
.stoploss <- .15 # <<<< Modify stop loss as % of entry price here
## Stop loss long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = T)

### Stop loss short ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled =T)
### End of Stop Loss code ###

### Gets results for signal/rules/strategies ###
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}
results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
for(symbol in symbols) {
  print(symbol)
  chart.Posn(portfolio.st, Symbol = symbol,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
}

### Tabulates Result Stats by Individual Asset/ Strategy ###
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats))

### Gets Total Portfolio Results ####
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance") 

### Calculates accumulated portfolio return ###
(end_eq[1258,]-10000000)/10000000

### Calculates sharpe ratio ###
sharperatio_portfolio <- SharpeRatio.annualized(returns, Rf = 0, scale = NA, geometric = TRUE)
sharperatio_portfolio

### Calculates information ratio ###
library(PerformanceAnalytics )
getSymbols(Symbols = "SPY",
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
Eventdriven_returns = Return.calculate(end_eq, method="log")
InformationRatio(returns,Eventdriven_returns)
Eventdriven_returns<- Return.calculate(end_eq, method="log")


### Realestate Hedging Strategy ###
### Set up packages we need to use below ###
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)
symbols <- c("DRN","URE","REZ","ICF","USRT",
             "VNQ","PSR","IYR","FRI","SCHH",
             "RWR","GRI","REM","MORT","REK")

currency('USD')
stock(symbols,
      currency = "USD",
      multiplier = 1)

### Sets up investment strategy parameters ### 
init_date <- "2014-05-01"
start_date <- "2014-05-01"
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000 One Billion Dollars
adjustment <- TRUE
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
getSymbols(Symbols = "SPY",
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
stock(symbols,
      currency = "USD",
      multiplier = 1)
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"

### Main Strategy Code ###
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date) 
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)
strategy(strategy.st, store = TRUE)

### Signals and indicators can be changed here ###
fastSMA=60
slowSMA=180
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fastSMA),
              label = "nFast")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slowSMA),
              label = "nSlow")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long") 
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")

### Trade sizes/ amounts for long/short ###
longSize=350000
shortSize=3000000

### LONG Rules ###
### Enter Long ###
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = longSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "High",
                          TxnFees = 0,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
### Exits Long ###

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")

### SHORT rules ###
### Enter Short ###
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -shortSize,
                          ordertype = "market",
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = 0,
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")
### Exit Short ###

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")

### Adds stop loss rule ###
.stoploss <- .0 # <<<< Modify stop loss as % of entry price here

### Stop loss long ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG", 
         label = "StopLossLONG",
         enabled = T)

### Stop loss short ###
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled =T)
### End of Stop Loss code ###

### Gets results for signal/rules/strategies ###
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}
results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
for(symbol in symbols) {
  print(symbol)
  chart.Posn(portfolio.st, Symbol = symbol,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
}

### Tabulates Result Stats by Individual Asset/ Strategy ###
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats)) 

### Gets Total Portfolio Results ###
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance")

### Calculates accumulated portfolio return ###
(end_eq[1258,]-10000000)/10000000

### Calculates sharpe ratio ###
sharperatio_portfolio <- SharpeRatio.annualized(returns, Rf = 0, scale = NA, geometric = TRUE)
sharperatio_portfolio

### Calculates information ratio ###
library(PerformanceAnalytics)
Realestate_returns = Return.calculate(end_eq, method="log")
SPYreturns = Return.calculate(SPY[,4], method="log")
InformationRatio(SPYreturns,Realestate_returns)
Realestate_returns<- Return.calculate(end_eq, method="log")


### Other assets included in CTA Portfolio with main Strategies ###
symbols <- c("GLD", "IAU", "SLV", "DBC", "USO",
             "PPLT", "DBE", "BNO", "UGAZ", "GLL",
             "PALL", "VXX", "VIXM", "QQQ",
             "EWJ", "EWG", "EWQ", "EWH", "CORN",
             "WEAT", "SOYB")
benchmark="SPY"

### Sets up investment strategy parameters ###
init_date <- "2014-05-01"
start_date <- "2014-05-01" 
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000; One Billion Dollars 

### Gets Market Data ###
### Market Data ####
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = init_date,
           to = end_date,
           adjust = TRUE)
getSymbols(Symbols = benchmark,
           src = "yahoo",
           index.class = "POSIXct",
           from = init_date,
           to = end_date,
           adjust = TRUE)

### Gets Benchmark Data ###
ff3=readRDS("ff3.rds")/100

### Factor Matrix Build ###
### Gets regression coefs and returns time series ###
dRange=paste0(start_date,"/",end_date)
clRange=paste0(init_date,"/",end_date)
clcl=ClCl(SPY)[clRange]
idx=intersect(as.Date(index(clcl)),as.Date(index(ff3)))
fit=lm(clcl[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
rets=clcl
colnames(rets)="SPY"
factorCoefs=data.frame("SPY",fit$coefficients[2],fit$coefficients[3],fit$coefficients[4])
names(factorCoefs)<-c("Symbol","Mkt","SMB","HML")
rownames(factorCoefs)=""

###1 CTA Build One
idx=intersect(as.Date(index(Currenciesreturns)),as.Date(index(ff3)))

### fama french fit ###
fit=lm(Currenciesreturns[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
factorCoefs=rbind(factorCoefs,data.frame(Symbol="CCY",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,Currenciesreturns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"CCY")

###2 CTA Build Two ###
idx=intersect(as.Date(index(Managedfutures_returns)),as.Date(index(ff3)))

### fama french fit ###
fit=lm(Managedfutures_returns[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
factorCoefs=rbind(factorCoefs,data.frame(Symbol="MF",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,Managedfutures_returns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"MF")

###3 CTA Build Three ###
idx=intersect(as.Date(index(Hedgefunds_returns)),as.Date(index(ff3)))

### now fama french fit ###
fit=lm(Hedgefunds_returns[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
factorCoefs=rbind(factorCoefs,data.frame(Symbol="Hedge",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,Hedgefunds_returns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"Hedge")

###4 CTA Build Four ###
idx=intersect(as.Date(index(Volatility_returns)),as.Date(index(ff3)))

### fama french fit ###
fit=lm(Volatility_returns[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
factorCoefs=rbind(factorCoefs,data.frame(Symbol="Volatility",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,Volatility_returns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"Volatility")

###5 CTA Build Five ###
idx=intersect(as.Date(index(Eventdriven_returns)),as.Date(index(ff3)))

### fama french fit ###
fit=lm(Eventdriven_returns[as.Date(idx)]~ff3[as.Date(idx)][,1:3]) 
factorCoefs=rbind(factorCoefs,data.frame(Symbol="Event",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,Eventdriven_returns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"Event")

###6 CTA Build Six ###
idx=intersect(as.Date(index(Realestate_returns)),as.Date(index(ff3)))

### fama french fit ###
fit=lm(Realestate_returns[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
factorCoefs=rbind(factorCoefs,data.frame(Symbol="Real",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,Realestate_returns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"Real")

for(symbol in symbols){
  
  clcl=ClCl(get(symbol))[clRange]
  idx=intersect(as.Date(index(clcl)),as.Date(index(ff3)))
  
  ### fama french fit ####
  fit=lm(clcl[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
  
  factorCoefs=rbind(factorCoefs,data.frame(Symbol=symbol,Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
  rets=cbind(rets,clcl)
  colnames(rets)=c(colnames(rets[,-ncol(rets)]),symbol)
  
}
rownames(factorCoefs)=c()
rets=rets[complete.cases(rets)]

### Optimize ###

### Creates portfolio object ###
pspec <- portfolio.spec(assets=colnames(rets))

### Defines individual constraint objects ###
### Leverage constraint ###
lev_constr <- weight_sum_constraint(min_sum=1, max_sum=1.5)

### Box constraint ###
lo_constr <- box_constraint(assets=pspec$assets, min=c(-15, rep(-10,6),rep(-5,ncol(rets)-7)),
                            max=c(0, rep(10,6), rep(5,ncol(rets)-7)))

### Position limit constraint ###
pl_constr <- position_limit_constraint(assets=pspec$assets,
                                       max_pos=ncol(rets)-1,max_pos_short = 6)

### FF3 exposure constraint ###
### c(MKT,SMB,HML) ###
### MKT cannot go above .1, everything else can change ###
lower <- c(-.2, -1.1, 1.3)
upper <- c(.1, 0.6, 2.0)
mf=as.matrix(factorCoefs[,-1])
rownames(mf)=factorCoefs$Symbol
exp_constr <- factor_exposure_constraint(assets=pspec$assets, B=mf, lower=lower,upper=upper)

### Minimizes Variance ###
var_obj <- portfolio_risk_objective(name="var")

### Maximizes Return ###
ret_obj <- return_objective(name="mean")

### Run optimization on minimum variance portfolio with leverage, long only,and group constraint here ### 
opta <- optimize.portfolio(R=rets, portfolio=pspec,
                           constraints=list(lev_constr, lo_constr, pl_constr,exp_constr),
                           objectives=list(var_obj,ret_obj),
                           maxSR=TRUE,
                           optimize_method="ROI")
### Looks at fit ###
print(opta)

### Gets returns of teh Strategy ###
myStratRets=xts(rets%*%opta$weights,order.by=index(rets))

### Gets Sharpe ###
SharpeRatio(myStratRets)

### Checks ff3 contraint was met in data:mkt <.2 ###
idx=intersect(as.Date(index(myStratRets)),as.Date(index(ff3)))
fit=lm(myStratRets~ff3[as.Date(idx)][,1:3])
summary(fit)

### Gets Sharpe (Note: Annualized) ###
SharpeRatio(myStratRets)

### Accumulated return ###
accumulated_return=sum(myStratRets[,1])
accumulated_return

### Calculartes annualized return for the strategy and benchmark ###
Return.annualized(myStratRets)
Return.annualized(rets[,1])
write.table(myStratRets,file = "daily log return",col.names = "TRUE",row.names = TRUE) 
charts.PerformanceSummary(myStratRets, colorset = bluefocus, main = "Strategy Performance")




