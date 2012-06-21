#!/usr/bin/Rscript --no-save
#
# Rocema strategy, demonstrating among others:
# * using ternaryindicator that returns -1 (short), 0 (cash) or 1 (long)
# * using ordersets to handle simultaenous stop-loss and take-profit order, in
#   combination with market entry order and market exit order
# * using prefer='Open' on next bar
#
# tested with blotter svn r1057
#
# JH, June 2012

.ema = 19
.roc = 45
.trend = 118

.tplong = 3.0
.tpshort = -3.0

.sllong = -5.0
.slshort = 5.0

#.timespan='T08:00:00/T12:59:00'
.timespan=NULL

.TxnFees=-1.9

#.subset='2011-12-01::2012-01-31'
#.subset='2011'
.subset='2011-01'

initDate = '2011-01-01'
initEq = 10000

p = 'rocema'
a = 'IB'

###############################################################################

RocSys = function(x, nEMA, nROC, nTREND)
{
  rocema = ROC(EMA(x, nEMA), nROC)
  trend = EMA(x, nTREND)

  signal = 
    ifelse(rocema>0 & lag(rocema)<=0, ifelse(x>trend,  1, 0), 
    ifelse(rocema<0 & lag(rocema)>=0, ifelse(x<trend, -1, 0), 
    NA)
  )
  
  signal <- na.locf(signal)
  
  return(signal)
}

###############################################################################

require(quantstrat)

getSymbols('GBPUSD')
GBPUSD <- align.time(to.period(GBPUSD, 'minutes', 15), 900)
GBPUSD <- GBPUSD[.subset]

###############################################################################

initPortf(p, symbols='GBPUSD', initDate=initDate, currency="EUR")
initAcct(a, portfolios=p, initDate=initDate, currency="EUR")

initOrders(p, initDate=initDate)

###############################################################################

s <- strategy(p)

#

s <- add.indicator(s, 'RocSys', arguments=list(x=quote(Cl(mktdata))), label='myrocsys')

#

s <- add.signal(s, 'sigThreshold', arguments = list(column="myrocsys", relationship="eq", threshold=0, cross=TRUE), label='cash')
s <- add.signal(s, 'sigThreshold', arguments = list(column="myrocsys", relationship="gt", threshold=0, cross=TRUE), label='long')
s <- add.signal(s, 'sigThreshold', arguments = list(column="myrocsys", relationship="lt", threshold=0, cross=TRUE), label='short')

#

s <- add.rule(s,
	'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=FALSE,
		TxnFees=.TxnFees,
		orderside='long' ,
		orderqty='all',
		ordertype='market',
		orderset='ocolong'
	), 
	type='exit',
	label='ExitLONG2SHORT')
s <- add.rule(s,
	'ruleSignal',
	arguments=list(sigcol='cash' , sigval=TRUE,
		replace=FALSE,
		TxnFees=.TxnFees,
		orderside='long' ,
		orderqty='all',
		ordertype='market',
		orderset='ocolong'
	), 
	type='exit',
	label='ExitLONG2CASH')

s <- add.rule(s,
	'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		TxnFees=.TxnFees,
		orderside='short',
		orderqty='all',
		ordertype='market',
		orderset='ocoshort'
	), 
	type='exit',
	label='ExitSHORT2LONG')
s <- add.rule(s,
	'ruleSignal',
	arguments=list(sigcol='cash' , sigval=TRUE,
		replace=FALSE,
		TxnFees=.TxnFees,
		orderside='short',
		orderqty='all',
		ordertype='market',
		orderset='ocoshort'
	), 
	type='exit',
	label='ExitSHORT2CASH')

s <- add.rule(s,
	'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		TxnFees=.TxnFees,
		orderside='long' ,
		orderqty='all',
		ordertype='limit',
		orderset='ocolong', threshold=.tplong
	),
	type='exit',
	label='TakeProfitLONG')
s <- add.rule(s,
	'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		TxnFees=.TxnFees,
		orderside='long' ,
		orderqty='all',
		ordertype='stoplimit',
		orderset='ocolong', threshold=.sllong
	), 
	type='exit',
	label='StopLossLONG')
s <- add.rule(s,
	'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		TxnFees=.TxnFees,
		orderside='long', 
		orderqty=1    ,
		ordertype='market'
	),
	type='enter',
	label='EnterLONG')

s <- add.rule(s,
	'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=FALSE,
		TxnFees=.TxnFees,
		orderside='short',
		orderqty='all',
		ordertype='limit',
	orderset='ocoshort', threshold=.tpshort
),
	type='exit',
	label='TakeProfitSHORT')
s <- add.rule(s,
	'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=FALSE,
		TxnFees=.TxnFees,
		orderside='short',
		orderqty='all',
		ordertype='stoplimit',
	orderset='ocoshort', threshold=.slshort), 
	type='exit',
	label='StopLossSHORT')
s <- add.rule(s,
	'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=FALSE,
		TxnFees=.TxnFees,
		orderside='short',
		orderqty=-1   ,
		ordertype='market'
	),
	type='enter',
	label='EnterSHORT')

applyStrategy(s, p, parameters=list(nEMA=.ema,nROC=.roc,nTREND=.trend), verbose = FALSE, prefer='Open')

chart.Posn(p, "GBPUSD")

print(getOrderBook(p))

txns <- getTxns(p, 'GBPUSD')
txns

cat('Net profit:', sum(txns$Net.Txn.Realized.PL), '\n')

