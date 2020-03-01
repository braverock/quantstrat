###############################################################################
# A demo script using a simple MA cross strategy, with trailing stops to be
# used for a test. Thanks to @kvekka for reporting the bug in #116 and for
# contributing this script.
###############################################################################


library(quantstrat)

init_date <- "2017-01-01" 
start_date <- "2017-01-01" 
end_date <- "2018-12-31"
init_equity <- 1e4 # $10,000
.orderqty <- 100
Sys.setenv(TZ = "UTC") 
fast <- 5
slow <- 75
symbols <- symbol <- "AAPL" 
tradesize = 1e4
currency('USD')
# getSymbols.rda("AAPL", dir="quantstrat/data/AAPL.rda")
data(AAPL)
# getSymbols(symbol,from=start_date, to = end_date, auto.assign = T, index.class = "POSIXct", src = 'yahoo')
# for(i in symbols)
#   assign(i, adjustOHLC(get(i),use.Adjusted=TRUE))

stock(symbols, currency = "USD", multiplier = 1)

portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"

rm.strat(strategy.st)
rm.strat(portfolio.st)
rm.strat(account.st)

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

add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fast),
              label = "nFast")

add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slow),
              label = "nSlow")

add.signal(strategy.st,
           name = "sigCrossover",
           arguments = list(column = c("nFast", "nSlow"),
                            relationship = "gt"),
           label = "LONG")

add.signal(strategy.st,
           name = "sigCrossover",
           arguments = list(column = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "SHORT")

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "LONG",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "limit", #market/ limit
                          threshold = 1/100,
                          time.in.force= 24*60*60*2,
                          tmult = T,
                          orderqty = 1000,
                          prefer = "Open", 
                          TxnFees = -10,
                          orderset = "ocolong",
                          replace = F),
         type = "enter",
         enabled = T,
         label = "EnterLONG")


add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "SHORT",
                          sigval = TRUE,
                          orderqty = 'all', 
                          orderside = 'long',
                          ordertype = "market",
                          prefer = "Open",
                          TxnFees = -10,
                          orderset = "ocolong",
                          replace = F),
         type = "exit",
         enabled = T,
         parent = "EnterLONG",
         label = "Exit2SHORT")

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "LONG",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "stoptrailing",
                          orderqty = "all",
                          prefer = "Close", 
                          threshold = 2/100, 
                          time.in.force= 24*60*60*1,
                          tmult = T, 
                          TxnFees = -10,
                          orderset = "ocolong",
                          replace = F),
         type = "chain",
         parent = "EnterLONG",
         enabled = T,
         label = "StopTrailingLONG")


applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(account.st, daterange)
updateEndEq(account.st)
tstats <- t(tradeStats(Portfolios = portfolio.st))
chart.Posn(Portfolio = portfolio.st, Symbol = "AAPL", TA= c("add_SMA(n=fast, col='blue')", "add_SMA(n=slow, col='red')"))

#look at the order book
obook<-getOrderBook('Port.Luxor')
