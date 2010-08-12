# Simple MACD strategy
#
# MACD may be used in many ways, this will demonstrate a trend indicator.
# 
# traditionally, when the MACD signal crosses zero, this indicated a establishment of a positive trend
#
# we'll buy on positive treshold crossover of the 'signal' column, and sell on negative threshold crossover
# 
# Author: brian
###############################################################################


require(quantstrat)
try(rm("order_book.macd",pos=.strategy),silent=TRUE)
try(rm("account.macd","portfolio.macd",pos=.blotter),silent=TRUE)
try(rm("account.st","portfolio.st","stock.str","s","initDate","initEq",'start_t','end_t'),silent=TRUE)

stock.str='AAPL' # what are we trying it on

#MA parameters for MACD
fastMA = 12 
slowMA = 26 
signalMA = 9
maType="EMA"

currency('USD')
stock(stock.str,currency='USD',multiplier=1)

initDate='2006-12-31'
initEq=1000000
portfolio.st='macd'
account.st='macd'

initPortf(portfolio.st,symbols=stock.str, initDate=initDate)
initAcct(account.st,portfolios=portfolio.st, initDate=initDate)
initOrders(portfolio=portfolio.st,initDate=initDate)


stratMACD <- strategy(portfolio.st)

stratMACD <- add.indicator(strategy = stratMACD, name = "MACD", arguments = list(x=quote(Cl(mktdata))) )

stratMACD <- add.signal(strategy = stratMACD,name="sigThreshold",arguments = list(column="signal",relationship="gt",threshold=0,cross=TRUE),label="signal.gt.zero")
stratMACD <- add.signal(strategy = stratMACD,name="sigThreshold",arguments = list(column="signal",relationship="lt",threshold=0,cross=TRUE),label="signal.lt.zero")

stratMACD <- add.rule(strategy = stratMACD,name='ruleSignal', arguments = list(sigcol="signal.gt.zero",sigval=TRUE, orderqty=100, ordertype='market', orderside='long', threshold=NULL),type='enter')
stratMACD <- add.rule(strategy = stratMACD,name='ruleSignal', arguments = list(sigcol="signal.gt.zero",sigval=TRUE, orderqty=-100, ordertype='stoplimit', orderside='long', threshold=.95,tmult=TRUE),type='risk')
stratMACD <- add.rule(strategy = stratMACD,name='ruleSignal', arguments = list(sigcol="signal.lt.zero",sigval=TRUE, orderqty='all', ordertype='market', orderside='long', threshold=NULL),type='exit')

getSymbols(stock.str,from=initDate)
start_t<-Sys.time()
out<-try(applyStrategy(strategy=stratMACD , portfolios=portfolio.st,parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType)))
end_t<-Sys.time()
end_t-start_t
updatePortf(Portfolio=portfolio.st,Dates=paste('::',as.Date(Sys.time()),sep=''))
chart.Posn(Portfolio=portfolio.st,Symbol=stock.str)
plot(add_MACD(fast=fastMA, slow=slowMA, signal=signalMA,maType="EMA"))

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
##############################################################################