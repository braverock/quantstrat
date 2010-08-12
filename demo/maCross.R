#########################################################################################################################################################################
#A simple moving average strategy to evaluate trade efficiency
#checks on SMA of 50 days and SMA of 200 days
#Author: R. Raghuraman("raghu"), Brian Peterson
#########################################################################################################################################################################

require(quantstrat)
try(rm("order_book.macross",pos=.strategy),silent=TRUE)
try(rm("account.macross","portfolio.macross",pos=.blotter),silent=TRUE)
try(rm("account.st","portfolio.st","stock.str","macross","initDate","initEq",'start_t','end_t'),silent=TRUE)
stock.str='AAPL' # what are we trying it on
currency('USD')
stock(stock.str,currency='USD',multiplier=1)
initDate='1999-12-31'
initEq=1000000
portfolio.st='macross'
account.st='macross'
initPortf(portfolio.st,symbols=stock.str, initDate=initDate)
initAcct(account.st,portfolios=portfolio.st, initDate=initDate)
initOrders(portfolio=portfolio.st,initDate=initDate)

stratMACROSS<- strategy(portfolio.st)

stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n=50),label= "ma50" )
stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n=200),label= "ma200")

stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(columns=c("ma50","ma200"),relationship="gt"),label="ma50.gt.ma200")
stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(column=c("ma50","ma200"),relationship="lt"),label="ma50.lt.ma200")

stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty=100, ordertype='market', orderside='long'),type='enter')
stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.lt.ma200",sigval=TRUE, orderqty=-100, ordertype='market', orderside='long'),type='exit')

# if you want a long/short Stops and Reverse MA cross strategy, you'd add two more rules for the short side:

# stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.lt.ma200",sigval=TRUE, orderqty=-100, ordertype='market', orderside='short'),type='enter')
# stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty=100, ordertype='market', orderside='short'),type='exit')

getSymbols(stock.str,from=initDate)
start_t<-Sys.time()
out<-try(applyStrategy(strategy=stratMACROSS , portfolios=portfolio.st))
end_t<-Sys.time()
end_t-start_t
updatePortf(Portfolio='macross',Dates=paste('::',as.Date(Sys.time()),sep=''))
chart.Posn(Portfolio='macross',Symbol=stock.str)
add_SMA(n=50 , on=1,col='blue')
add_SMA(n=200, on=1)

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: faber.R 369 2010-08-12 11:44:00Z braverock $
#
###############################################################################