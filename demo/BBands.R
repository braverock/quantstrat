require(quantstrat)
try(rm("order_book.bbands",pos=.strategy),silent=TRUE)
try(rm("account.bbands","portfolio.bbands",pos=.blotter),silent=TRUE)
try(rm("account.st","portfolio.st","stock.str","s","initDate","initEq",'start_t','end_t'),silent=TRUE)

# some things to set up here
stock.str='IBM' # what are we trying it on

# we'll pass these 
SD = 2 # how many standard deviations, traditionally 2
N = 20 # how many periods for the moving average, traditionally 20


currency('USD')
stock(stock.str,currency='USD',multiplier=1)

initDate='2006-12-31'
initEq=1000000

portfolio.st='bbands'
account.st='bbands'

initPortf(portfolio.st,symbols=stock.str, initDate=initDate)
initAcct(account.st,portfolios='bbands', initDate=initDate)
initOrders(portfolio=portfolio.st,initDate=initDate)

s <- strategy("bbands")

#one indicator
s <- add.indicator(strategy = s, name = "BBands", arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'))


#if you wanted to manually apply a signal function for demonstration
#cbind(IBM.mod,sigComparison(label="Close.gt.Open",data=IBM.inds,columns=c("Close","Open"),">"))
#cbind(IBM.mod,sigComparison(label="Adjusted.gt.SMA",data=IBM.inds,columns=c("Adjusted","SMA10"),">"))

#add signals:
s<- add.signal(s,name="sigCrossover",arguments = list(data=quote(mktdata),columns=c("Close","up"),relationship="gt"),label="Cl.gt.UpperBand")
s<- add.signal(s,name="sigCrossover",arguments = list(data=quote(mktdata),columns=c("Close","dn"),relationship="lt"),label="Cl.lt.LowerBand")
s<- add.signal(s,name="sigCrossover",arguments = list(data=quote(mktdata),columns=c("High","Low","mavg"),relationship="op"),label="Cross.Mid")

# lets add some rules
s 
s <- add.rule(s,name='ruleSignal', arguments = list(data=quote(mktdata),sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-100, ordertype='market', orderside=NULL, threshold=NULL),type='enter')
s <- add.rule(s,name='ruleSignal', arguments = list(data=quote(mktdata),sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty= 100, ordertype='market', orderside=NULL, threshold=NULL),type='enter')
s <- add.rule(s,name='ruleSignal', arguments = list(data=quote(mktdata),sigcol="Cross.Mid",sigval=TRUE, orderqty= 'all', ordertype='market', orderside=NULL, threshold=NULL),type='exit')
#s <- add.rule(s,name='ruleSignal', arguments = list(data=quote(mktdata),sigcol="Lo.gt.UpperBand",sigval=TRUE, orderqty= 'all', ordertype='market', orderside=NULL, threshold=NULL),type='exit')
#s <- add.rule(s,name='ruleSignal', arguments = list(data=quote(mktdata),sigcol="Hi.lt.LowerBand",sigval=TRUE, orderqty= 'all', ordertype='market', orderside=NULL, threshold=NULL),type='exit')
#TODO add thresholds and stop-entry and stop-exit handling to test

getSymbols(stock.str,from=initDate)
start_t<-Sys.time()
out<-try(applyStrategy(strategy='s' , portfolios='bbands',parameters=list(sd=SD,n=N)))

# look at the order book
#getOrderBook('bbands')
end_t<-Sys.time()
end_t-start_t
updatePortf(Portfolio='bbands',Dates=paste('::',as.Date(Sys.time()),sep=''))
chart.Posn(Portfolio='bbands',Symbol=stock.str)
plot(add_BBands(on=1,sd=SD,n=N))
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
