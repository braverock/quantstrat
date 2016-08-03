require(quantstrat)

suppressWarnings(rm("order_book.RSI",pos=.strategy))
suppressWarnings(rm("account.RSI","portfolio.RSI",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratRSI","startDate","initEq",'start_t','end_t'))

# Initialize a strategy object
stratRSI <- strategy("RSI")

n=2

# Add an indicator
stratRSI <- add.indicator(strategy = stratRSI, name = "RSI", arguments = list(price = quote(getPrice(mktdata))), label="RSI")

# There are two signals:
# The first is when RSI is greater than 90
stratRSI <- add.signal(strategy = stratRSI, name="sigThreshold",arguments = list(threshold=70, column="RSI",relationship="gt", cross=TRUE),label="RSI.gt.70")
# The second is when RSI is less than 10
stratRSI <- add.signal(strategy = stratRSI, name="sigThreshold",arguments = list(threshold=30, column="RSI",relationship="lt",cross=TRUE),label="RSI.lt.30")

# There are two rules:
#'## we use osMaxPos to put trade on in layers, or to a maximum position. 
# The first is to sell when the RSI crosses above the threshold
stratRSI <- add.rule(strategy = stratRSI, name='ruleSignal', arguments = list(sigcol="RSI.gt.70", sigval=TRUE, orderqty=-1000, ordertype='market', orderside='short', pricemethod='market', replace=FALSE, osFUN=osMaxPos), type='enter', path.dep=TRUE)
stratRSI <- add.rule(strategy = stratRSI, name='ruleSignal', arguments = list(sigcol="RSI.lt.30", sigval=TRUE, orderqty='all', ordertype='market', orderside='short', pricemethod='market', replace=FALSE), type='exit', path.dep=TRUE)
# The second is to buy when the RSI crosses below the threshold
stratRSI <- add.rule(strategy = stratRSI, name='ruleSignal', arguments = list(sigcol="RSI.lt.30", sigval=TRUE, orderqty= 1000, ordertype='market', orderside='long', pricemethod='market', replace=FALSE, osFUN=osMaxPos), type='enter', path.dep=TRUE)
stratRSI <- add.rule(strategy = stratRSI, name='ruleSignal', arguments = list(sigcol="RSI.gt.70", sigval=TRUE, orderqty='all', ordertype='market', orderside='long', pricemethod='market', replace=FALSE), type='exit', path.dep=TRUE)

#add changeable parameters
# add level in/out

# add trailing entry

# add trailing exit?

currency("USD")
currency("EUR")
symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
for(symbol in symbols){ # establish trade-able instruments
    stock(symbol, currency="USD",multiplier=1)
}
getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='1998-01-01')


# you can test with something like this:
# applySignals(strategy=stratRSI, mktdata=applyIndicators(strategy=stratRSI, mktdata=symbols[1]))

startDate='1997-12-31'
initEq=100000
port.st<-'RSI' #use a string here for easier changing of parameters and re-trying

initPortf(port.st, symbols=symbols)
initAcct(port.st, portfolios=port.st, initEq=initEq)
initOrders(portfolio=port.st)
for(symbol in symbols){ addPosLimit(port.st, symbol, startDate, 300, 3 ) } #set max pos 

print("setup completed")

# Process the indicators and generate trades
start_t<-Sys.time()
out<-try(applyStrategy(strategy=stratRSI , portfolios=port.st, parameters=list(n=2) ) )
end_t<-Sys.time()
print("Strategy Loop:")
print(end_t-start_t)

# look at the order book
#print(getOrderBook(port.st))

start_t<-Sys.time()
updatePortf(Portfolio=port.st,Dates=paste('::',as.Date(Sys.time()),sep=''))
end_t<-Sys.time()
print("trade blotter portfolio update:")
print(end_t-start_t)

# hack for new quantmod graphics, remove later
themelist<-chart_theme()
themelist$col$up.col<-'lightgreen'
themelist$col$dn.col<-'pink'
for(symbol in symbols){
    dev.new()
    chart.Posn(Portfolio=port.st,Symbol=symbol,theme=themelist)
    plot(add_RSI(n=2))
}

ret1 <- PortfReturns(port.st)
ret1$total <- rowSums(ret1)
#ret1

if("package:PerformanceAnalytics" %in% search() || require("PerformanceAnalytics",quietly=TRUE)) {
    dev.new()
    charts.PerformanceSummary(ret1$total,geometric=FALSE,wealth.index=TRUE)
}


###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2012
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
