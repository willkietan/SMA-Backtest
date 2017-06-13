#initalisation settings
library(quantmod)
library(quantstrat)
library(plyr)
initdate <- "1999-01-01"
from <- "2003-01-01"
to <- "2016-12-25"
Sys.setenv(TZ = "UTC")
currency("AUD")
getSymbols("asx.ax", from = from, to = to, src = 'yahoo', adjust = TRUE)
asx.ax <- ASX.AX
stock("ASX.AX", currency = "AUD")
stock("asx.ax", currency = "AUD")
tradesize <- 10000
initeq <- 20000
strategy.st <- "asxthreshold"
portfolio.st <- "December 2016 47"
account.st <- "Willkie Tan 47"
rm.strat(strategy.st)
initPortf(portfolio.st, symbols = "ASX.AX", initdate = initdate, currency = "AUD")
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "AUD")
initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store = TRUE)

#indicators
sma60 <- SMA(x = Cl(ASX.AX), n = 60)
sma365 <- SMA(x = Cl(ASX.AX), n = 365)
add.indicator(strategy = strategy.st,
              name = "SMA",
              argument = list(x = quote(Cl(mktdata)), n = 60, label = "sma60"))
add.indicator(strategy = strategy.st,
              name = "SMA",
              argument = list(x = quote(Cl(mktdata)), n = 365,label = "sma365"))
#Signals
add.signal(strategy.st, name = "sigCrossover", arguments = list(columns = c("sma60","sma365"), 
          relationship = "gt"), label = "longentry")

add.signal(strategy.st, name = "sigCrossover", arguments = list(columns = c("sma60","sma365"),
          relationship = "lt"), label = "longexit")

#Rules
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longentry", 
                          sigval = TRUE, 
                          orderqty = tradesize,
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open"),
                        tradeSize = tradesize,
                        maxSize = tradesize,
         type = "enter")
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longexit", 
                          sigval = TRUE, 
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE, 
                          prefer = "Open"),
         tradeSize = tradesize,
         maxSize = tradesize,
         type = "exit")

add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longexit", 
                          sigval = TRUE, 
                          orderqty = tradesize,
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE, 
                          prefer = "Open"),
         tradeSize = tradesize,
         maxSize = tradesize,
         type = "enter")
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longentry", 
                          sigval = TRUE, 
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE, 
                          prefer = "Open"),
         tradeSize = tradesize,
         maxSize = tradesize,
         type = "exit")

#Strategy
mktdata <- cbind(ASX.AX, sma60, sma365)
mktdata_cleaned <- na.omit(mktdata)
mktdata_cleaned <- rename(mktdata_cleaned, c("SMA" = "sma60", "SMA.1" = "sma365"))
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st, mktdata = mktdata_cleaned)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(account.st, daterange)
updateEndEq(account.st)

#Analysis
tstats <- tradeStats(Portfolios = portfolio.st)
tstats$Profit.Factor
plot(Cl(ASX.AX))
lines(sma60, col = "red")
lines(sma365, col = "blue")
print(tstats)