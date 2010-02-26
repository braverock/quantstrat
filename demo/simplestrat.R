require(quantstrat)
try(rm("order_book.simplestrat",pos=.strategy),silent=TRUE)
try(rm("account.simplestrat","portfolio.simplestrat",pos=.blotter),silent=TRUE)
try(rm("account.st","portfolio.st","IBM","s","initDate","initEq",'start_t','end_t'),silent=TRUE)

currency('USD')
stock('IBM',currency='USD',multiplier=1)

initDate='1997-12-31'
initEq=1000000

portfolio.st='simplestrat'
account.st='simplestrat'

initPortf(portfolio.st,symbols='IBM', initDate=initDate)
initAcct(account.st,portfolios='simplestrat', initDate=initDate)
initOrders(portfolio=portfolio.st,initDate=initDate)

s <- strategy("simplestrat")
#s <- add.indicator(strategy = s, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")
s <- add.indicator(strategy = s, name = "BBands", arguments = list(HLC = quote(HLC(mktdata)), sd = 2,maType=quote(SMA)))


#if you wanted to manually apply a signal function for demonstration
#cbind(IBM.mod,sigComparison(label="Close.gt.Open",data=IBM.inds,columns=c("Close","Open"),">"))
#cbind(IBM.mod,sigComparison(label="Adjusted.gt.SMA",data=IBM.inds,columns=c("Adjusted","SMA10"),">"))

#do it properly and add it to the strategy:
#s<- add.signal(s,name="sigComparison",arguments = list(data=quote(mktdata),columns=c("Close","Open"),relationship="gt"),label="Cl.gt.Op")
s<- add.signal(s,name="sigCrossover",arguments = list(data=quote(mktdata),columns=c("Close","up"),relationship="gt"),label="Cl.gt.UpperBand")
s<- add.signal(s,name="sigCrossover",arguments = list(data=quote(mktdata),columns=c("Close","dn"),relationship="lt"),label="Cl.lt.LowerBand")

#IBM.sigs<-applySignals(s,mktdata=IBM.inds)

# lets add some rules
s 
s <- add.rule(s,name='ruleSignal', arguments = list(data=quote(mktdata),sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-100, ordertype='market', orderside=NULL, threshold=NULL),type='enter')
s <- add.rule(s,name='ruleSignal', arguments = list(data=quote(mktdata),sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty= 100, ordertype='market' , orderside=NULL, threshold=NULL),type='enter')
#TODO add thresholds and stop-entry and stop-exit handling to test

getSymbols("IBM")
start_t<-Sys.time()
out<-try(applyStrategy(strategy='s' , portfolios='simplestrat'))
# look at the order book
#getOrderBook('simplestrat')
end_t<-Sys.time()
end_t-start_t
updatePortf(Portfolio='simplestrat',Dates=paste('::',as.Date(Sys.time()),sep=''))
chart.Posn(Portfolio='simplestrat',Symbol='IBM',theme='white')
addBBands(on=1,sd=2,n=10)
###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
