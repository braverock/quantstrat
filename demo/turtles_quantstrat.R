# Turtle Trading System
# Author: Jasen Mackie

## References - and many thanks to the authors of the below references
## Free PDF explaining the system - https://bigpicture.typepad.com/comments/files/turtlerules.pdf
## The Complete Turtle Trader - https://www.amazon.ca/Complete-TurtleTrader-Investors-Overnight-Millionaires/dp/0061241717
## blotter version authored by Josh Ulrich and Peter Carl - https://github.com/braverock/blotter/blob/master/demo/turtles.R

## System 1 - A shorter-term system based on a 20-day breakout
## System 2 - A simpler long-term system based on a 55-day breakout
##
## A note on breakouts and how they were applied by the Turtles:
## "Turtles always traded at the breakout when it was exceeded during the day,
## and did not wait until the daily close or the open of the following day. 
## In the case of opening gaps, the Turtles would enter positions on the open
## if a market opened through the price of the breakout."
##
## Depending on your market data, entry price will vary. With OHLC data, we
## could enter at the next day Open or Close. With 'AllowMagicalThinking' set
## to TRUE, we are able to enter at the Close on the same day the signal is
## generated. It may even be appropriate in this case.

# required libraries
require(quantstrat)
require(blotter)

Sys.setenv(TZ="UTC")

# Try to clean up in case the demo was run previously
try(rm("turtles_quantstrat","turtles_quantstrat",pos=.blotter),silent=TRUE)
# try(rm("portfolio","account","N","symbol","symbols","ClosePrice","CurrentDate","equity","Units","maxUnits","size","Stop","equity","TxnPrice","initDate","initEq","Posn","verbose"),silent=TRUE)


# Set initial values
initDate="2008-01-01"
initEq=100000
print("Initializing portfolio and account structure")
# Assemble a small portfolio of three stocks
symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
currency("USD")
for(symbol in symbols){
  stock(symbol, currency="USD",multiplier=1)
}

getSymbols(symbols, index.class="POSIXct", from=initDate, src="yahoo")

# Set up a portfolio object and an account object
portfolio = "turtles_quantstrat"
initPortf(name=portfolio,symbols, initDate=initDate)
account = "turtles_quantstrat"
initAcct(name=account,portfolios="turtles_quantstrat", initDate=initDate, initEq=initEq)
initOrders(portfolio=portfolio)
# addPosLimit(portfolio, symbol, startDate, 100, 1 ) #set max pos
allowMagicalThinking <- TRUE # potentially controversial

# Portfolio Parameters
risk_size = 0.01
maxUnits = 4
stratTurtles<- strategy(portfolio)
mult_N <- 0.5

ATRperiod = 20

# TODO: System 1 Indicators

# TODO: System 1 Signals

# TODO: System 1 Rules

# TODO: position sizing and risk management

# TODO: System 2 short entry and exit

## System 2 Indicators
# ATR
stratTurtles <- add.indicator(strategy=stratTurtles, name="ATR",
                             arguments=list(HLC=quote(HLC(mktdata)), n=ATRperiod),
                             label="atrX")
# Long Breakout
stratTurtles <- add.indicator(strategy = stratTurtles,
                              name = "runMax",
                              arguments = list(x=quote(Hi(mktdata)[,1]), n=55),
                              label= "runMax55" )
# Long Exit
stratTurtles <- add.indicator(strategy = stratTurtles,
                              name = "runMin",
                              arguments = list(x=quote(Lo(mktdata)[,1]), n=20),
                              label= "runMin20")
# Short Breakout
stratTurtles <- add.indicator(strategy = stratTurtles,
                              name = "runMin",
                              arguments = list(x=quote(Lo(mktdata)[,1]), n=55),
                              label= "runMin55")
# Short Exit
stratTurtles <- add.indicator(strategy = stratTurtles,
                              name = "runMax",
                              arguments = list(x=quote(Hi(mktdata)[,1]), n=20),
                              label= "runMax20")

# # Test Indicators
# test <- applyIndicators(stratTurtles, mktdata=OHLC(XLF))
# head(test)

## System 2 Signals
# Long Entry Signal
stratTurtles <- add.signal(strategy = stratTurtles,
                           name="sigCrossover",
                           arguments = list(columns=c("High", "runMax55"), relationship="gte"),
                           label="Hi.gte.runMax55")
# Long Exit Signal
stratTurtles <- add.signal(strategy = stratTurtles,
                           name="sigCrossover",
                           arguments = list(column=c("Low", "runMin20"),relationship="lte"),
                           label="Lo.lte.runMin20")

# Short Entry Signal
stratTurtles <- add.signal(strategy = stratTurtles,
                           name="sigCrossover",
                           arguments = list(column=c("Low", "runMin55"),relationship="lte"),
                           label="Lo.lte.runMin55")
# Short Exit Signal
stratTurtles <- add.signal(strategy = stratTurtles,
                           name="sigCrossover",
                           arguments = list(columns=c("High", "runMax20"), relationship="gte"),
                           label="Hi.gte.runMax20")


# # Test Signals
# testSignals <- applySignals(stratTurtles, mktdata=mktdata)
# head(testSignals)

# System 2 Order Size function
# 'N' is the 20-day EMA of the True Range, or more commonly the ATR
# True Range = max(H-L, H-PDC, PDC-L)
# 'N' = (19 x PDN + TR)/20
# Volatility Adjusted Position Units
# "The Turtles built positions in pieces which we called Units. Units were sized
# so that 1 N represented 1% of the account equity."
# Unit = (1% of Account) / (N x Dollars per Point)
OrdQty2 <- function(data, timestamp, orderqty, ordertype, orderside,
                    portfolio, symbol, prefer="Close",
                    integerQty=TRUE, size=risk_size,
                    ...) {
  if(getPosQty("turtles_quantstrat", symbol, timestamp) == 0){
    
    if(prefer=="Close") {
      price <- as.numeric()
    } else {
      price <- as.numeric(Op(mktdata[timestamp,]))
    }
    sharesToTransact <- (initEq * size) / (mktdata$atr.atrX[timestamp])
    if (orderside=="short") {
      #qty <- -dollarsToTransact / price
      qty <- -sharesToTransact
    } else {
      qty <- sharesToTransact
    }
    if(integerQty) {
      qty <- trunc(qty)
    }
    return(qty)
  }
}

## System 2 Rules
# Long Entry
stratTurtles <- add.rule(strategy = stratTurtles,
                         name='ruleSignal',
                         arguments = list(sigcol="Hi.gte.runMax55",sigval=TRUE, osFUN=OrdQty2,
                                          ordertype='market', orderside='long', prefer="open"),
                         type='enter')
# Long Exit
stratTurtles <- add.rule(strategy = stratTurtles,
                         name='ruleSignal',
                         arguments = list(sigcol="Lo.lte.runMin20",sigval=TRUE, orderqty='all',
                                          ordertype='market', orderside='long', prefer="open"),
                         type='exit')

# Short Entry
stratTurtles <- add.rule(strategy = stratTurtles,
                         name='ruleSignal',
                         arguments = list(sigcol="Lo.lte.runMin55",sigval=TRUE, osFUN=OrdQty2,
                                          ordertype='market', orderside='short', prefer="open"),
                         type='enter')
# Short Exit
stratTurtles <- add.rule(strategy = stratTurtles,
                         name='ruleSignal',
                         arguments = list(sigcol="Hi.gte.runMax20",sigval=TRUE, orderqty='all',
                                          ordertype='market', orderside='short', prefer="open"),
                         type='exit')


# Run backtest
start_t<-Sys.time()
out<-applyStrategy(strategy=stratTurtles , portfolios=portfolio, allowMagicalThinking = allowMagicalThinking)
end_t<-Sys.time()
print(end_t-start_t)

# Update portfolio
start_t<-Sys.time()
updatePortf(Portfolio='turtles_quantstrat',Dates=paste('::',as.Date(Sys.time()),sep=''))
updateAcct(name='turtles_quantstrat')
end_t<-Sys.time()
print("trade blotter portfolio update:")
print(end_t-start_t)

# Print charts
if (require(quantmod)) {
  for(symbol in symbols){
    dev.new()
    chart.Posn(Portfolio='turtles_quantstrat',Symbol=symbol)
  }
}

if(require(PerformanceAnalytics)){
  charts.PerformanceSummary(PortfReturns('turtles_quantstrat'),main='Turtle Demo Instrument Return on Equity',geometric=FALSE)
}

getEndEq(account)

ret1 <- PortfReturns('turtles_quantstrat')
ret1$total <- rowSums(ret1)

print(last(cumsum(ret1$total)))
plot.xts(main = "Daily Portfolio Returns", ret1$total)
plot.xts(main = "Cumulative Portfolio Return", cumsum(ret1$total))

getSymbols("SPY", src='yahoo', index.class=c("POSIXt","POSIXct"), from='1999-01-01')
# SPY<-to.monthly(SPY, indexAt='lastof')  
SPY.ret<-Return.calculate(SPY$SPY.Adjusted)
dev.new()
charts.PerformanceSummary(cbind(ret1$total,SPY.ret), geometric=FALSE, wealth.index=TRUE)

# Trade Stats
turtles_quantstrat.Stats <- t(tradeStats('turtles_quantstrat'))
View(turtles_quantstrat.Stats)
