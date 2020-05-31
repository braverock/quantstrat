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
symbols = c("XLF", "XLP", "XLE")#, "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
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

stratTurtles<- strategy(portfolio)

# TODO: System 1 Indicators

# TODO: System 1 Signals

# TODO: System 1 Rules

# TODO: position sizing and risk management

# TODO: System 2 short entry and exit

# System 2 Indicators
stratTurtles <- add.indicator(strategy = stratTurtles,
                              name = "runMax",
                              arguments = list(x=quote(Hi(mktdata)), n=55),
                              label= "runMax55" )
stratTurtles <- add.indicator(strategy = stratTurtles,
                              name = "runMin",
                              arguments = list(x=quote(Lo(mktdata)[,1]), n=55),
                              label= "runMin55")

# System 2 Signals
stratTurtles <- add.signal(strategy = stratTurtles,
                           name="sigCrossover",
                           arguments = list(columns=c("High", "runMax55"), relationship="gte"),
                           label="Hi.gte.runMax55")
stratTurtles <- add.signal(strategy = stratTurtles,
                           name="sigCrossover",
                           arguments = list(column=c("Low", "runMin55"),relationship="lte"),
                           label="Lo.lte.runMin55")

# System 2 Rules
stratTurtles <- add.rule(strategy = stratTurtles,
                         name='ruleSignal',
                         arguments = list(sigcol="Hi.gte.runMax55",sigval=TRUE, orderqty=100,
                                          ordertype='market', orderside='long', prefer="open"),
                         type='enter')
stratTurtles <- add.rule(strategy = stratTurtles,
                         name='ruleSignal',
                         arguments = list(sigcol="Lo.lte.runMin55",sigval=TRUE, orderqty='all',
                                          ordertype='market', orderside='long', prefer="open")
                         ,type='exit')

# Run backtest
start_t<-Sys.time()
out<-applyStrategy(strategy=stratTurtles , portfolios=portfolio, allowMagicalThinking = allowMagicalThinking)
end_t<-Sys.time()
print(end_t-start_t)

# Update portfolio
start_t<-Sys.time()
updatePortf(Portfolio='turtles_quantstrat',Dates=paste('::',as.Date(Sys.time()),sep=''))
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

getEndEq(account,Sys.time())
