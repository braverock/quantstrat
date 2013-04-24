#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1230
#
# From Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#

options(width = 240)
#Sys.setenv(TZ="GMT")

##### PLACE DEMO AND TEST DATES HERE #################
#
#if(isTRUE(options('in_test')$in_test))
#  # use test dates
#  {initDate="2011-01-01" 
#  endDate="2012-12-31"   
#  } else
#  # use demo defaults
#  {initDate="1999-12-31"
#  endDate=Sys.Date()}

.from='2002-10-21'
#.to='2008-07-04'
.to='2002-10-31'

###

source('luxor.include.R')
source('luxor.getSymbols.R')

### blotter

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')

### quantstrat

initOrders(portfolio.st, initDate=initDate)

### define strategy

addPosLimit(
            portfolio=portfolio.st,
            symbol='GBPUSD',
            timestamp=initDate,
            maxpos=.orderqty)

strategy(strategy.st, store=TRUE)

### indicators

add.indicator(strategy.st, name = "SMA",
	arguments = list(
		x = quote(Cl(mktdata)[,1]),
		n = .fast
	),
	label="nFast"
)

add.indicator(strategy.st, name="SMA",
	arguments = list(
		x = quote(Cl(mktdata)[,1]),
		n = .slow
	),
	label="nSlow"
)

### signals

add.signal(strategy.st, name='sigCrossover',
	arguments = list(
		columns=c("nFast","nSlow"),
		relationship="gte"
	),
	label='long'
)

add.signal(strategy.st, name='sigCrossover',
	arguments = list(
		columns=c("nFast","nSlow"),
		relationship="lt"
	),
	label='short'
)

### rules ############

### stop-loss

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='stoplimit',
		tmult=TRUE,
		threshold=.stoploss,
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocolong'
	),
	type='chain', parent='EnterLONG',
	label='StopLossLONG',
	storefun=FALSE
)

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='short' , sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='stoplimit',
		tmult=TRUE,
		threshold=.stoploss,
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocoshort'
	),
	type='chain', parent='EnterSHORT',
	label='StopLossSHORT',
	storefun=FALSE
)

### stop-trailing

if(TRUE)
{
add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='stoptrailing',
		tmult=TRUE,
		threshold=.stoptrailing,
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocolong'
	),
	type='chain', parent='EnterLONG',
	label='StopTrailingLONG',
	storefun=FALSE
)

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='short' , sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='stoptrailing',
		tmult=TRUE,
		threshold=.stoptrailing,
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocoshort'
	),
	type='chain', parent='EnterSHORT',
	label='StopTrailingSHORT',
	storefun=FALSE
)
}

### take-profit

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='limit',
		tmult=TRUE,
		threshold=.takeprofit,
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocolong'
	),
	type='chain', parent='EnterLONG',
	label='TakeProfitLONG',
	storefun=FALSE
)

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='short' , sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='limit',
		tmult=TRUE,
		threshold=.takeprofit,
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocoshort'
	),
	type='chain', parent='EnterSHORT',
	label='TakeProfitSHORT',
	storefun=FALSE
)

### 

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=TRUE,
		orderside='short',
		ordertype='market',
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocoshort'
	),
	type='exit',
	timespan = .timespan,
	label='Exit2LONG',
	storefun=FALSE
)

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=TRUE,
		orderside='long' ,
		ordertype='market',
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocolong'
	),
	type='exit',
	timespan = .timespan,
	label='Exit2SHORT',
	storefun=FALSE
)

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long' ,
		ordertype='stoplimit',
		prefer='High',
		threshold=.threshold,
		TxnFees=0,
		orderqty=+.orderqty,
		osFUN=osMaxPos,
		orderset='ocolong'
	),
	type='enter',
	timespan = .timespan,
	label='EnterLONG',
	storefun=FALSE
)

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='stoplimit',
		prefer='Low',
		threshold=.threshold,
		TxnFees=0,
		orderqty=-.orderqty,
		osFUN=osMaxPos,
		orderset='ocoshort'
	),
	type='enter',
	timespan = .timespan,
	label='EnterSHORT',
	storefun=FALSE
)

#

###############################################################################

applyStrategy(strategy.st, portfolio.st, verbose = FALSE)
#applyStrategy(strategy.st, p, prefer='Open', verbose = FALSE)

updatePortf(portfolio.st, Symbols='GBPUSD', ,Dates=paste('::',as.Date(Sys.time()),sep=''))

###############################################################################

chart.Posn(portfolio.st, "GBPUSD")

print(getOrderBook(portfolio.st))

#txns <- getTxns(portfolio.st, 'GBPUSD')
#txns
##txns$Net 
#cat('Net profit:', sum(txns$Net.Txn.Realized.PL), '\n')

print(tradeStats(portfolio.st, 'GBPUSD'))

save.strategy(strategy.st)

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################
