#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1123
#
# From Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#

.fast = 6
.slow = 44

.qty=100000
.th=0.0005
.txn=-30
.timespan = 'T08:00/T12:00'
.timespan = 'T00:00/T23:59'

.stoploss=0.001
.stoptrailing=0.0015
.takeprofit=0.003

####

require(quantstrat)

s <- 'luxor'

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

### rules ############

### stop-loss

add.rule(s, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='stoplimit',
		tmult=TRUE,
		threshold=.stoploss,
		TxnFees=.txn,
		orderqty='all',
		orderset='ocolong'
	),
	type='chain',
	parent='EnterLONG',
	label='StopLossLONG',
	storefun=FALSE
)

add.rule(s, name = 'ruleSignal',
	arguments=list(sigcol='short' , sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='stoplimit',
		tmult=TRUE,
		threshold=.stoploss,
		TxnFees=.txn,
		orderqty='all',
		orderset='ocoshort'
	),
	type='chain',
	parent='EnterSHORT',
	label='StopLossSHORT',
	storefun=FALSE
)

### stop-trailing

add.rule(s, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='stoptrailing',
		tmult=TRUE,
		threshold=.stoptrailing,
		TxnFees=.txn,
		orderqty='all',
		orderset='ocolong'
	),
	type='chain',
	parent='EnterLONG',
	label='StopTrailingLONG',
	storefun=FALSE
)

add.rule(s, name = 'ruleSignal',
	arguments=list(sigcol='short' , sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='stoptrailing',
		tmult=TRUE,
		threshold=.stoptrailing,
		TxnFees=.txn,
		orderqty='all',
		orderset='ocoshort'
	),
	type='chain',
	parent='EnterSHORT',
	label='StopTrailingSHORT',
	storefun=FALSE
)

### take-profit

add.rule(s, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='limit',
		tmult=TRUE,
		threshold=.takeprofit,
		TxnFees=.txn,
		orderqty='all',
		orderset='ocolong'
	),
	type='chain',
	parent='EnterLONG',
	label='TakeProfitLONG',
	storefun=FALSE
)

add.rule(s, name = 'ruleSignal',
	arguments=list(sigcol='short' , sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='limit',
		tmult=TRUE,
		threshold=.takeprofit,
		TxnFees=.txn,
		orderqty='all',
		orderset='ocoshort'
	),
	type='chain',
	parent='EnterSHORT',
	label='TakeProfitSHORT',
	storefun=FALSE
)

### normal exits

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
	label='Exit2LONG',
	storefun=FALSE
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
	label='Exit2SHORT',
	storefun=FALSE
)

### entries

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
	label='EnterLONG',
	storefun=FALSE
)

add.rule(s, name = 'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='stoplimit',
		prefer='Low',
		threshold=.th,
		TxnFees=0,
		orderqty=-.qty,
		osFUN=osMaxPos,
		orderset='ocoshort'
	),
	type='enter',
	timespan = .timespan,
	label='EnterSHORT',
	storefun=FALSE
)

###

save.strategy('luxor')
