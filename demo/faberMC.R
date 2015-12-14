# This is a very simple trend following strategy for testing the results of:
# Faber, Mebane T., "A Quantitative Approach to Tactical Asset Allocation." 
# Journal of Risk Management (Spring 2007). 
# The article proposes a very simple quantitative market-timing model.  They 
# test the model in sample on the US stock market since 1900 before testing
# it out-of-sample in twenty other markets.

# The article discusses a 200-day simple moving average, which is proposed
# in Jeremy Seigel's book "Stocks for the Long Run" for timing the DJIA.  He 
# concludes that a simple market timing strategy improves the absolute and
# risk adjusted returns over a buy-and-hold strategy.  After all transaction
# costs are included, the timing strategy falls short on the absolute return,
# but still provides a better risk-adjusted return.  Siegel also tests timing on  
# the Nasdaq composite since 1972 and finds better absolute and risk adjusted
# returns.

# The article implements a simpler version of the 200-day SMA, opting for a
# 10-month SMA.  Monthly data is more easily available for long periods of time,
# and the lower granularity should translate to lower transaction costs.  

# The rules of the system are relatively simple:
# - Buy when monthly price > 10-month SMA
# - Sell and move to cash when monthly price < 10-month SMA

# 1. All entry and exit prices are on the day of the signal at the close.
# 2. All data series are total return series including dividends, updated monthly. 
#    For the purposes of this demo, we only use price returns.
# 3. Cash returns are estimated with 90-day commercial paper.  Margin rates for
#    leveraged models are estimated with the broker call rate.  Again, for the
#    purposes of this demo, we ignore interest and leverage.
# 4. Taxes, commissions, and slippage are excluded.

# This simple strategy is different from well-known trend-following systems in
# three respects.  First, there's no shorting.  Positions are converted to cash on
# a 'sell' signal, rather than taking a short position. Second, the entire position
# is put on at trade inception.  No assumptions are made about increasing position
# size as the trend progresses.  Third, there are no stops.  If the trend reverts
# quickly, this system will wait for a sell signal before selling the position.

# Data
# Instead of using total returns data, this demo uses monthly data for the SP500
# downloaded from Yahoo Finance.  We'll use about 10 years of data, starting at 
# the beginning of 1998.

# Load required libraries
require(quantstrat)

# Try to clean up in case the demo was run previously
suppressWarnings(rm("account.faber","account.faberMC","portfolio.faber","portfolio.combMC", 
                        "portfolio.GDAXI", "portfolio.GSPC", "portfolio.N225",pos=.blotter))
suppressWarnings(rm("ltaccount","ltportfolio","ClosePrice","CurrentDate","equity","stratFaber","startDate","initEq","Posn","UnitSize","verbose"))
suppressWarnings(rm("order_book.faber","order_book.combMC", "order_book.GDAXI", "order_book.GSPC", "order_book.N225", pos=.strategy))

# Set initial values
startDate='2000-01-01'
initEq=100000

# Set up instruments with FinancialInstruments package
symbols = c("^GSPC", "^N225", "^GDAXI")

currency("USD")
currency("JPY")
currency("EUR")

#get the currencies
USDJPY<-getPrice(to.monthly(getSymbols("JPY=X",auto.assign=FALSE),indexAt='lastof',drop.time=TRUE))
EURUSD<-getPrice(to.monthly(getSymbols("EURUSD=X",auto.assign=FALSE),indexAt='lastof',drop.time=TRUE))
colnames(USDJPY)<-"USDJPY"
colnames(EURUSD)<-"EURUSD"

getSymbols(symbols,from=startDate)
#takes out the carat
symbols = c("GSPC", "N225", "GDAXI")


stock(symbols[1], currency="USD",multiplier=1)
stock(symbols[2], currency="JPY",multiplier=1)
stock(symbols[3], currency="EUR",multiplier=1)
# to do this truly correctly, we'd use the futures contracts, which are tradable

for(symbol in symbols) {
    x<-get(symbol)
    x<-to.monthly(x,indexAt='lastof',drop.time=TRUE)
    indexFormat(x)<-'%Y-%m-%d'
    colnames(x)<-gsub("x",symbol,colnames(x))
    assign(symbol,x)
	initPortf(symbol, symbols=symbol, currency=getInstrument(symbol)$currency)
	initOrders(portfolio=symbol)
}


initAcct('faberMC', portfolios=symbols, currency="USD")

# Initialize portfolio and account

print("setup completed")

# Initialize a strategy object
stratFaber <- strategy("faber")

# Add an indicator
stratFaber <- add.indicator(strategy = stratFaber, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")

# There are two signals:
# The first is when monthly price crosses over the 10-month SMA
stratFaber <- add.signal(stratFaber,name="sigCrossover",arguments = list(columns=c("Close","SMA10"),relationship="gte"),label="Cl.gt.SMA")
# The second is when the monthly price crosses under the 10-month SMA
stratFaber <- add.signal(stratFaber,name="sigCrossover",arguments = list(columns=c("Close","SMA10"),relationship="lt"),label="Cl.lt.SMA")

# There are two rules:
# The first is to buy when the price crosses above the SMA
stratFaber <- add.rule(stratFaber, name='ruleSignal', arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=1000, ordertype='market', orderside='long', pricemethod='market'), type='enter', path.dep=TRUE)
# The second is to sell when the price crosses below the SMA
stratFaber <- add.rule(stratFaber, name='ruleSignal', arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all', ordertype='market', orderside='long', pricemethod='market'), type='exit', path.dep=TRUE)

# Process the indicators and generate trades
start_t<-Sys.time()
out<-try(applyStrategy(strategy=stratFaber , portfolios=symbols))
end_t<-Sys.time()
print("Strategy Loop:")
print(end_t-start_t)

# look at the order book
#print(getOrderBook('faber'))

start_t<-Sys.time()
for(symbol in symbols) {
	updatePortf(Portfolio=symbol,Dates=paste('::',as.Date(Sys.time()),sep=''))
}
end_t<-Sys.time()
print("trade blotter portfolio update:")
print(end_t-start_t)

#and a combined portfolio
initPortf('combMC', symbols=symbols, currency="USD")
initOrders(portfolio= 'combMC')
comb.out<-applyStrategy(strategy=stratFaber , portfolios='combMC')
updatePortf(Portfolio='combMC',Dates=paste('::',as.Date(Sys.time()),sep=''))

# hack for new quantmod graphics, remove later
themelist<-chart_theme()
themelist$col$up.col<-'lightgreen'
themelist$col$dn.col<-'pink'
for(symbol in symbols){
    dev.new()
    chart.Posn(Portfolio=symbol,Symbol=symbol,theme=themelist)
    plot(add_SMA(n=10,col='darkgreen', on=1))
}


###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2012
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, 
# Jeffrey Ryan, Joshua Ulrich, and Garrett See
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: faber.R 371 2010-08-12 20:18:09Z braverock $
#
###############################################################################
