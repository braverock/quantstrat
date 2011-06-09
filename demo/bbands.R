require(quantstrat)
suppressWarnings(rm("order_book.bbands",pos=.strategy))
suppressWarnings(rm("account.bbands","portfolio.bbands",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratBBands","initDate","initEq",'start_t','end_t'))

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

stratBBands <- strategy("bbands")

#one indicator
stratBBands <- add.indicator(strategy = stratBBands, name = "BBands", arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'))


#add signals:
stratBBands <- add.signal(stratBBands,name="sigCrossover",arguments = list(columns=c("Close","up"),relationship="gt"),label="Cl.gt.UpperBand")
stratBBands <- add.signal(stratBBands,name="sigCrossover",arguments = list(columns=c("Close","dn"),relationship="lt"),label="Cl.lt.LowerBand")
stratBBands <- add.signal(stratBBands,name="sigCrossover",arguments = list(columns=c("High","Low","mavg"),relationship="op"),label="Cross.Mid")

# lets add some rules
stratBBands <- add.rule(stratBBands,name='ruleSignal', arguments = list(sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-100, ordertype='market', orderside=NULL, threshold=NULL),type='enter')
stratBBands <- add.rule(stratBBands,name='ruleSignal', arguments = list(sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty= 100, ordertype='market', orderside=NULL, threshold=NULL),type='enter')
stratBBands <- add.rule(stratBBands,name='ruleSignal', arguments = list(sigcol="Cross.Mid",sigval=TRUE, orderqty= 'all', ordertype='market', orderside=NULL, threshold=NULL),type='exit')

#alternately, to exit at the opposite band, the rules would be...
#stratBBands <- add.rule(stratBBands,name='ruleSignal', arguments = list(data=quote(mktdata),sigcol="Lo.gt.UpperBand",sigval=TRUE, orderqty= 'all', ordertype='market', orderside=NULL, threshold=NULL),type='exit')
#stratBBands <- add.rule(stratBBands,name='ruleSignal', arguments = list(data=quote(mktdata),sigcol="Hi.lt.LowerBand",sigval=TRUE, orderqty= 'all', ordertype='market', orderside=NULL, threshold=NULL),type='exit')

#TODO add thresholds and stop-entry and stop-exit handling to test

getSymbols(stock.str,from=initDate)
start_t<-Sys.time()
out<-try(applyStrategy(strategy=stratBBands , portfolios='bbands',parameters=list(sd=SD,n=N)) )

# look at the order book
#getOrderBook('bbands')
end_t<-Sys.time()
print("strat execution time:")
print(end_t-start_t)

start_t<-Sys.time()
updatePortf(Portfolio='bbands',Dates=paste('::',as.Date(Sys.time()),sep=''))
end_t<-Sys.time()
print("updatePortf execution time:")
print(end_t-start_t)

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
