# Initialize a strategy object
stratRSI <- strategy("RSI")

# Add an indicator
stratRSI <- add.indicator(strategy = stratRSI, name = "RSI", arguments = list(price = quote(getPrice(mktdata)), n=2), label="RSI")

# There are two signals:
# The first is when RSI is greater than 90
stratRSI <- add.signal(strategy = stratRSI, name="sigThreshold",arguments = list(data=quote(mktdata),threshold=90, column="RSI",relationship="gt", cross=TRUE),label="RSI.gt.90")
# The second is when RSI is less than 10
stratRSI <- add.signal(strategy = stratRSI, name="sigThreshold",arguments = list(data=quote(mktdata),threshold=10, column="RSI",relationship="lt",cross=TRUE),label="RSI.lt.10")

# There are two rules:
#'## we would Use osMaxPos to put trade on in layers, or to a maximum position. 
# The first is to sell when the RSI crosses above the threshold
stratRSI <- add.rule(strategy = stratRSI, name='ruleSignal', arguments = list(data=quote(mktdata), sigcol="RSI.gt.90", sigval=TRUE, orderqty=-1000, ordertype='market', orderside='short', pricemethod='market'), type='enter', path.dep=TRUE)
stratRSI <- add.rule(strategy = stratRSI, name='ruleSignal', arguments = list(data=quote(mktdata), sigcol="RSI.lt.10", sigval=TRUE, orderqty='all', ordertype='market', orderside='short', pricemethod='market'), type='exit', path.dep=TRUE)
# The second is to buy when the RSI crosses below the threshold
stratRSI <- add.rule(strategy = stratRSI, name='ruleSignal', arguments = list(data=quote(mktdata), sigcol="RSI.lt.10", sigval=TRUE, orderqty= 1000, ordertype='market', orderside='long', pricemethod='market'), type='enter', path.dep=TRUE)
stratRSI <- add.rule(strategy = stratRSI, name='ruleSignal', arguments = list(data=quote(mktdata), sigcol="RSI.gt.90", sigval=TRUE, orderqty='all', ordertype='market', orderside='long', pricemethod='market'), type='enter', path.dep=TRUE)

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


# you can test with something like this:
# applySignals(strategy=stratRSI, mktdata=applyIndicators(strategy=stratRSI, mktdata=symbols[1]))


initDate='2009-02-06'
initEq=100000
port.st<-'RSISPX' #use a string here for easier changing of parameters and re-trying

initPortf(port.st, symbols=symbols, initDate=initDate)
initAcct(port.st, portfolios=port.st, initDate=initDate)
initOrders(portfolio=port.st, initDate=initDate)

print("setup completed")

# Process the indicators and generate trades
portfolios=c("RSISPX","RSIGDAXI","RSIboth")
start_t<-Sys.time()
out<-try(applyStrategy(strategy=stratRSI , portfolios=c(port.st)))
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
themelist$up.col<-'lightgreen'
themelist$down.col<-'pink'
for(symbol in symbols){
    dev.new()
    chart.Posn(Portfolio=port.st,Symbol=symbol,theme=themelist)
    plot(add_RSI(n=2))
}
