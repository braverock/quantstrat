#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1123
#
# From Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Paragraph 3.4: inserting an intraday time filter

options(width = 240)
#Sys.setenv(TZ="GMT")

.fast = 1
.slow = 44

.qty=100000
.th=0.0005
.txn=-30
.timespan = 'T08:00/T12:00'

##### PLACE THIS BLOCK AHEAD OF DATE INITS IN DEMO SCRIPT ######
# if(!exists('in_test') || !isTRUE(in_test)){
#     initDate='2005-12-31' # ensure this is demo default
#     endDate=Sys.Date()    # ensure this is demo default
# }
################################################################

initDate = '2002-10-21'
.from='2002-10-21'
#.to='2008-07-04'
#.to='2003-12-31'
.to='2002-10-31'
#.to='2002-12-31'
#.from='2006-01-01'
#.to='2006-12-31'
#.from='2007-01-01'
#.to='2007-12-31'

####

s = 'luxor'
p = 'forex'
a = 'IB1'

###

require(quantstrat)

currency(c('GBP', 'USD'))

exchange_rate(c('GBPUSD'), tick_size=0.0001)

setSymbolLookup.FI(system.file('extdata',package='quantstrat'), 'GBPUSD')

###

getSymbols('GBPUSD', from=.from, to=.to, verbose=FALSE)
GBPUSD = to.minutes30(GBPUSD)
GBPUSD = align.time(to.minutes30(GBPUSD), 1800)

###

initPortf(p, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(a, portfolios=p, initDate=initDate, currency='USD')

###

initOrders(p, initDate=initDate)

### strategy ######################################################################

addPosLimit(
            portfolio=p,
            symbol='GBPUSD',
            timestamp=initDate,
            maxpos=.qty)

strategy(s, store=TRUE)

### indicators

add.indicator(s, name = "SMA",
	arguments = list(
		x = quote(Cl(mktdata)),
		n = .fast
	),
	label="nFast"
)

add.indicator(s, name="SMA",
	arguments = list(
		x = quote(Cl(mktdata)),
		n = .slow
	),
	label="nSlow"
)

### signals

add.signal(s, name = 'sigCrossover',
	arguments = list(
		columns=c("nFast","nSlow"),
		relationship="gte"
	),
	label='long'
)

add.signal(s, name = 'sigCrossover',
	arguments = list(
		columns=c("nFast","nSlow"),
		relationship="lt"
	),
	label='short'
)

### rules

add.rule(s, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=TRUE,
		orderside='short',
		ordertype='market',
		TxnFees=.txn,
		orderqty='all',
		orderset='ocoshort'
	),
	type='exit',
	timespan = .timespan,
	label='Exit2LONG'
)

add.rule(s, name = 'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=TRUE,
		orderside='long' ,
		ordertype='market',
		TxnFees=.txn,
		orderqty='all',
		orderset='ocolong'
	),
	type='exit',
	timespan = .timespan,
	label='Exit2SHORT')

add.rule(s, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long' ,
		ordertype='stoplimit',
		prefer='High',
		threshold=.th,
		TxnFees=0,
		orderqty=+.qty,
		osFUN=osMaxPos,
		orderset='ocolong'
	),
	type='enter',
	timespan = .timespan,
	label='EnterLONG'
)

add.rule(s, name = 'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='stoplimit',
		prefer='Low',
		threshold=-.th,
		TxnFees=0,
		orderqty=-.qty,
		osFUN=osMaxPos,
		orderset='ocoshort'
	),
	type='enter',
	timespan = .timespan,
	label='EnterSHORT'
)

#

###############################################################################

applyStrategy(s, p, verbose = FALSE)
#applyStrategy(s, p, prefer='Open', verbose = FALSE)

updatePortf(p, Symbols='GBPUSD', ,Dates=paste('::',as.Date(Sys.time()),sep=''))

###############################################################################

chart.Posn(p, "GBPUSD")

print(getOrderBook(p))

#txns <- getTxns(p, 'GBPUSD')
#txns
##txns$Net 
#cat('Net profit:', sum(txns$Net.Txn.Realized.PL), '\n')

print(tradeStats(p, 'GBPUSD'))

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################
