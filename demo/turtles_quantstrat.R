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
initDate="2010-01-01"
initEq=100000
print("Initializing portfolio and account structure")
# Assemble a small portfolio of three stocks randomly from a sample of 9 - this is a torture set after all
# symbols = sample(c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU"), 3, replace = FALSE)
symbols = "XLP"
currency("USD")
for(symbol in symbols){
  stock(symbol, currency="USD",multiplier=1)
}

getSymbols(symbols, index.class="POSIXct", from=initDate, src="yahoo")

# Set up a portfolio object and an account object
portfolio.st='turtles_quantstrat'
account.st='turtles_quantstrat'

initPortf(portfolio.st,symbols)
initAcct(account.st,portfolios=portfolio.st,initEq = initEq)
initOrders(portfolio=portfolio.st)

strategy.st<-portfolio.st
# define the strategy
strategy(strategy.st, store=TRUE)

# portfolio = "turtles_quantstrat"
# initPortf(name=portfolio,symbols, initDate=initDate)
# account = "turtles_quantstrat"
# initAcct(name=account,portfolios="turtles_quantstrat", initDate=initDate, initEq=initEq)
# initOrders(portfolio=portfolio)
# addPosLimit(portfolio, symbol, startDate, 100, 1 ) #set max pos
allowMagicalThinking <- TRUE # potentially controversial

# Portfolio Parameters
risk_size = 0.01
maxUnits = 4 # not used unless we level into positions
# stratTurtles<- strategy(portfolio)
# strategy(stratTurtles, store=TRUE)
mult_N <- 0.5 # not used unless we level into positions 
# boPer <- 55 # Breakout Period, System 1 uses 55 days
# exPer <- 20 # Exit Period, System 1 uses 20 days
stMult <- 2 # Stop Multiple, System 1 uses 2xN
init_boPer <- 55 # initial Breakout Period
init_exPer <- 20 # initial Exit Period

ATRperiod = 20

# TODO: System 1 Indicators

# TODO: System 1 Signals

# TODO: System 1 Rules

## System 2 Indicators
# ATR (for position sizing)
stratTurtles <- add.indicator(strategy.st, name="ATR",
                             arguments=list(HLC=quote(HLC(mktdata)), n=ATRperiod),
                             label="atrX")
# 2x ATR (for Stops)
Threshold <- function(data=mktdata, n, side) {
  Threshold <- stMult * ATR(HLC(data), n) * side
}

###############################################################################
# create custom indicator for MOper
boPer <- function(data = mktdata, n = init_boPer){
  boPer <- xts(rep(n,nrow(data)), index(data))
}

stratTurtles <- add.indicator(strategy.st,
              name = 'boPer',
              arguments = list(data, init_boPer),
              label='boPer')

exPer <- function(data = mktdata, n = init_exPer){
  exPer <- xts(rep(n,nrow(data)), index(data))
}

stratTurtles <- add.indicator(strategy.st,
              name = 'exPer',
              arguments = list(data, init_exPer),
              label='exPer')

###############################################################################

# Long Breakout
stratTurtles <- add.indicator(strategy.st,
                              name = "runMax",
                              arguments = list(x=quote(Hi(mktdata)[,1]),
                                               n = quote(mktdata[1,last(grep('boPer',x = colnames(mktdata)))])),
                              label= "runMax55" )
# Long Breakout (for Singal Analysis in apply.paramset.signal.analysis)
boN <- 20 # Breakout lookback period (boN)
stratTurtles <- add.indicator(strategy.st,
                              name = "runMax",
                              arguments = list(x=quote(Hi(mktdata)[,1]),
                                               boN = boN),
                              label= "runMaxSigAn" )
# Long Exit
stratTurtles <- add.indicator(strategy.st,
                              name = "runMin",
                              arguments = list(x=quote(Lo(mktdata)[,1]),
                                               n = quote(mktdata[1,last(grep('exPer',x = colnames(mktdata)))])),
                              label= "runMin20")
# Long Stop
stratTurtles <- add.indicator(strategy.st, name="Threshold",
                              arguments=list(HLC=quote(HLC(data)), n=ATRperiod, side = 1),
                              label="long2N")

# Short Breakout
stratTurtles <- add.indicator(strategy.st,
                              name = "runMin",
                              arguments = list(x=quote(Lo(mktdata)[,1]),
                                               n = quote(mktdata[1,last(grep('boPer',x = colnames(mktdata)))])),
                              label= "runMin55")
# Short Breakout (for Signal Analysis in apply.paramset.signal.analysis)
boN <- 20 # Breakout lookback period (boN)
stratTurtles <- add.indicator(strategy.st,
                              name = "runMin",
                              arguments = list(x=quote(Lo(mktdata)[,1]),
                                               boN = boN),
                              label= "runMinSigAn" )
# Short Exit
stratTurtles <- add.indicator(strategy.st,
                              name = "runMax",
                              arguments = list(x=quote(Hi(mktdata)[,1]),
                                               n = quote(mktdata[1,last(grep('exPer',x = colnames(mktdata)))])),
                              label= "runMax20")
# Short Stop
stratTurtles <- add.indicator(strategy.st, name="Threshold",
                              arguments=list(HLC=quote(HLC(data)), n=ATRperiod, side = -1),
                              label="short2N")

# Test Indicators
# test <- applyIndicators(stratTurtles, mktdata=OHLC(XLV))
# head(test)

## System 2 Signals
# Long Entry Signal
stratTurtles <- add.signal(strategy.st,
                           name="sigCrossover",
                           arguments = list(columns=c("High", "runMax55"), relationship="gte"),
                           label="Hi.gte.runMax55")
# Long Entry Signal for Signal Analysis
stratTurtles <- add.signal(strategy.st,
                           name="sigCrossover",
                           arguments = list(columns=c("High", "runMaxSigAn"), relationship="gte"),
                           label="Hi.gte.runMaxboN")
# Long Exit Signal
stratTurtles <- add.signal(strategy.st,
                           name="sigCrossover",
                           arguments = list(column=c("Low", "runMin20"),relationship="lte"),
                           label="Lo.lte.runMin20")

# Short Entry Signal
stratTurtles <- add.signal(strategy.st,
                           name="sigCrossover",
                           arguments = list(column=c("Low", "runMin55"),relationship="lte"),
                           label="Lo.lte.runMin55")
# Short Entry Signal for Signal Analysis
stratTurtles <- add.signal(strategy.st,
                           name="sigCrossover",
                           arguments = list(columns=c("Low", "runMinSigAn"), relationship="lte"),
                           label="Lo.lte.runMinboN")
# Short Exit Signal
stratTurtles <- add.signal(strategy.st,
                           name="sigCrossover",
                           arguments = list(columns=c("High", "runMax20"), relationship="gte"),
                           label="Hi.gte.runMax20")


# Test Signals
# testSignals <- applySignals(stratTurtles, mktdata=mktdata)
# head(testSignals)

## Run Signal Analysis Study
# Buy Signal Column Label to analyze
buy.signal.label = 'Hi.gte.runMaxboN'
# Desired Parameter Pool
.boN = seq(20,50,10)
# Add distribution
# add.distribution(strategy.st,
#                  paramset.label = 'signal_analysis',
#                  component.type = 'indicator',
#                  component.label = 'runMaxSigAn',
#                  variable = list(n = .boN),
#                  label = 'boPerBuySigAn'
# )
# 
# sa_buy <- apply.paramset.signal.analysis(
#   strategy.st,
#   paramset.label='signal_analysis',
#   portfolio.st=portfolio.st,
#   sigcol = buy.signal.label,
#   sigval = 1,
#   on=NULL,
#   forward.days=20,
#   cum.sum=TRUE,
#   include.day.of.signal=FALSE,
#   obj.fun=signal.obj.slope,
#   decreasing=TRUE,
#   verbose=TRUE)
# 
# signal.plot(sa_buy$sigret.by.asset$XLP)
# distributional.boxplot(signal=sa_buy$sigret.by.asset$XLP$paramset.55,
#                        x.val=seq(1, 20, 1),val=10,ylim=c(-20, 20),
#                        xlim=c(0, 20),mai=c(1,1,0.3,0.5),h=0)

# Sell Signal Column Label to analyze
sell.signal.label = 'Lo.lte.runMinboN'
# Add distribution - will override previous distribution
add.distribution(strategy.st,
                 paramset.label = 'signal_analysis',
                 component.type = 'indicator',
                 component.label = 'runMinSigAn', 
                 variable = list(n = .boN),
                 label = 'boPerSellSigAn'
)

sa_sell <- apply.paramset.signal.analysis(
  strategy.st, 
  paramset.label='signal_analysis', 
  portfolio.st=portfolio.st, 
  sigcol = sell.signal.label,
  sigval = 1,
  on=NULL,
  forward.days=20,
  cum.sum=TRUE,
  include.day.of.signal=FALSE,
  obj.fun=signal.obj.slope,
  decreasing=TRUE,
  verbose=TRUE)

signal.plot(sa_sell$sigret.by.asset$XLP)
distributional.boxplot(signal=sa_sell$sigret.by.asset$XLP$paramset.55,
                       x.val=seq(1, 20, 1),val=10,ylim=c(-20, 20),
                       xlim=c(0, 20),mai=c(1,1,0.3,0.5),h=0)

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
  if(getPosQty(portfolio.st, symbol, timestamp) == 0){
    
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
stratTurtles <- add.rule(strategy.st,
                         name='ruleSignal',
                         arguments = list(sigcol="Hi.gte.runMax55",sigval=TRUE, osFUN=OrdQty2,
                                          ordertype='market', orderside='long', prefer="open"),
                         type='enter')
# Long Exit
stratTurtles <- add.rule(strategy.st,
                         name='ruleSignal',
                         arguments = list(sigcol="Lo.lte.runMin20",sigval=TRUE, orderqty='all',
                                          ordertype='market', orderside='long', prefer="open"),
                         type='exit')
# Long Stop
stratTurtles <- add.rule(strategy.st,
                         name='ruleSignal',
                         arguments = list(sigcol="Hi.gte.runMax55",sigval=TRUE, orderqty='all',
                                          ordertype='stoplimit', threshold="atr.long2N", orderside='long', prefer="open"),
                         type='risk')

# Short Entry
stratTurtles <- add.rule(strategy.st,
                         name='ruleSignal',
                         arguments = list(sigcol="Lo.lte.runMin55",sigval=TRUE, osFUN=OrdQty2,
                                          ordertype='market', orderside='short', prefer="open"),
                         type='enter')
# Short Exit
stratTurtles <- add.rule(strategy.st,
                         name='ruleSignal',
                         arguments = list(sigcol="Hi.gte.runMax20",sigval=TRUE, orderqty='all',
                                          ordertype='market', orderside='short', prefer="open"),
                         type='exit')
# Short Stop
stratTurtles <- add.rule(strategy.st,
                         name='ruleSignal',
                         arguments = list(sigcol="Lo.lte.runMin55",sigval=TRUE, orderqty='all',
                                          ordertype='stoplimit', threshold="atr.short2N", orderside='short', prefer="open"),
                         type='risk')

# Run backtest
start_t<-Sys.time()
out<-applyStrategy(strategy.st , portfolios=portfolio.st, allowMagicalThinking = allowMagicalThinking)
end_t<-Sys.time()
print(end_t-start_t)

# Update portfolio
start_t<-Sys.time()
updatePortf(Portfolio=portfolio.st,Dates=paste('::',as.Date(Sys.time()),sep=''))
updateAcct(name=account.st)
end_t<-Sys.time()
print("trade blotter portfolio update:")
print(end_t-start_t)

# Print charts
if (require(quantmod)) {
  for(symbol in symbols){
    dev.new()
    chart.Posn(Portfolio=portfolio.st,Symbol=symbol)
  }
}

if(require(PerformanceAnalytics)){
  charts.PerformanceSummary(PortfReturns(account.st),main='Turtle Demo Instrument Return on Equity',geometric=FALSE)
}

getSymbols("SPY", src='yahoo', index.class=c("POSIXt","POSIXct"), from='1999-01-01')
# SPY<-to.monthly(SPY, indexAt='lastof')  
SPY.ret<-Return.calculate(SPY$SPY.Adjusted)
ret1 <- PortfReturns(account.st)
if (require(quantmod)) {
  for(i in 1:length(symbols)){
    dev.new()
    charts.PerformanceSummary(cbind(ret1[,i],SPY.ret), geometric=FALSE, wealth.index=TRUE)
  }
}

getEndEq(account)

ret1 <- PortfReturns(account.st)
ret1$total <- rowSums(ret1)

print(last(cumsum(ret1$total)))
plot.xts(main = "Daily Portfolio Returns", ret1$total)
plot.xts(main = "Cumulative Portfolio Return", cumsum(ret1$total))

# Trade Stats
turtles_quantstrat.Stats <- t(tradeStats(portfolio.st))
View(turtles_quantstrat.Stats)

# Parameter Optimization
# set up for parallel computation
require(parallel)
require(doParallel)
registerDoParallel()

# Set parameters
.boPer = (21:55)
.exPer = (10:20)
# .nsamples = 200 
#for random parameter sampling, 
# less important if you're using doParallel or doMC

### Period paramset
add.distribution(stratTurtles,
                 paramset.label = 'paramset',
                 component.type = 'indicator',
                 component.label = 'boPer', #this is the label given to the indicator in the strat
                 variable = list(n = .boPer),
                 label = 'boPer'
)

add.distribution(stratTurtles,
                 paramset.label = 'paramset',
                 component.type = 'indicator',
                 component.label = 'exPer', #this is the label given to the indicator in the strat
                 variable = list(n = .exPer),
                 label = 'exPer'
)

# add.distribution.constraint(strategy.st,
#                             paramset.label = 'MA',
#                             distribution.label.1 = 'nFAST',
#                             distribution.label.2 = 'nSLOW',
#                             operator = '<',
#                             label = 'MA'
# )

.paramaudit <- new.env()
ps_start <- Sys.time()
paramset.results  <- apply.paramset(stratTurtles, 
                                    paramset.label='paramset', 
                                    portfolio.st=portfolio.st, 
                                    account.st=account.st,
                                    #                          nsamples=.nsamples,
                                    audit=.paramaudit,
                                    store=FALSE,
                                    verbose=FALSE)
ps_end   <- Sys.time()
cat("Running the parameter search (apply.paramset): \n ")
print(ps_end-ps_start)
cat("Total trials:",.strategy$turtles_quantstrat$trials,"\n")

plot(paramset.results$cumPL[-1,], major.ticks = 'years', grid.ticks.on = 'years')


