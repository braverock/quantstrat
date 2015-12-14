#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - May 2013
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tomasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Advanced luxor strategy implementation including exit management using ordersets and orderchains

require(quantstrat)

source(paste0(path.package("quantstrat"),"/demo/luxor.include.R"))

### define strategy

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

# normal exit rules

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
	label='Exit2LONG'
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
	label='Exit2SHORT'
)

# normal entry rules

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
	label='EnterLONG'
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
	label='EnterSHORT'
)

### parameter sets

# SMA

add.distribution(strategy.st,
	paramset.label = 'SMA',
	component.type = 'indicator',
	component.label = 'nFast',
	variable = list(n = .FastSMA),
	label = 'nFAST'
)

add.distribution(strategy.st,
	paramset.label = 'SMA',
	component.type = 'indicator',
	component.label = 'nSlow',
	variable = list(n = .SlowSMA),
	label = 'nSLOW'
)

add.distribution.constraint(strategy.st,
	paramset.label = 'SMA',
	distribution.label.1 = 'nFAST',
	distribution.label.2 = 'nSLOW',
	operator = '<',
	label = 'SMA'
)

# stop-loss

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='stoplimit', tmult=TRUE, threshold=quote(.stoploss),
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocolong'
	),
	type='chain', parent='EnterLONG',
	label='StopLossLONG',
	enabled=FALSE
)

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='short' , sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='stoplimit', tmult=TRUE, threshold=quote(.stoploss),
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocoshort'
	),
	type='chain', parent='EnterSHORT',
	label='StopLossSHORT',
	enabled=FALSE
)

add.distribution(strategy.st,
	paramset.label = 'StopLoss',
	component.type = 'chain',
	component.label = 'StopLossLONG',
	variable = list(threshold = .StopLoss),
	label = 'StopLossLONG'
)

add.distribution(strategy.st,
	paramset.label = 'StopLoss',
	component.type = 'chain',
	component.label = 'StopLossSHORT',
	variable = list(threshold = .StopLoss),
	label = 'StopLossSHORT'
)

add.distribution.constraint(strategy.st,
	paramset.label = 'StopLoss',
	distribution.label.1 = 'StopLossLONG',
	distribution.label.2 = 'StopLossSHORT',
	operator = '==',
	label = 'StopLoss'
)

# stop-trailing

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='stoptrailing', tmult=TRUE, threshold=quote(.stoptrailing),
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocolong'
	),
	type='chain', parent='EnterLONG',
	label='StopTrailingLONG',
	enabled=FALSE
)

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='short' , sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='stoptrailing', tmult=TRUE, threshold=quote(.stoptrailing),
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocoshort'
	),
	type='chain', parent='EnterSHORT',
	label='StopTrailingSHORT',
	enabled=FALSE
)

add.distribution(strategy.st,
	paramset.label = 'StopTrailing',
	component.type = 'chain',
	component.label = 'StopTrailingLONG',
	variable = list(threshold = .StopTrailing),
	label = 'StopTrailingLONG'
)

add.distribution(strategy.st,
	paramset.label = 'StopTrailing',
	component.type = 'chain',
	component.label = 'StopTrailingSHORT',
	variable = list(threshold = .StopTrailing),
	label = 'StopTrailingSHORT'
)

add.distribution.constraint(strategy.st,
	paramset.label = 'StopTrailing',
	distribution.label.1 = 'StopTrailingLONG',
	distribution.label.2 = 'StopTrailingSHORT',
	operator = '==',
	label = 'StopTrailing'
)

# take-profit

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='limit', tmult=TRUE, threshold=quote(.takeprofit),
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocolong'
	),
	type='chain', parent='EnterLONG',
	label='TakeProfitLONG',
	enabled=FALSE
)

add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='short' , sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='limit', tmult=TRUE, threshold=quote(.takeprofit),
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocoshort'
	),
	type='chain', parent='EnterSHORT',
	label='TakeProfitSHORT',
	enabled=FALSE
)

add.distribution(strategy.st,
	paramset.label = 'TakeProfit',
	component.type = 'chain',
	component.label = 'TakeProfitLONG',
	variable = list(threshold = .TakeProfit),
	label = 'TakeProfitLONG'
)

add.distribution(strategy.st,
	paramset.label = 'TakeProfit',
	component.type = 'chain',
	component.label = 'TakeProfitSHORT',
	variable = list(threshold = .TakeProfit),
	label = 'TakeProfitSHORT'
)

add.distribution.constraint(strategy.st,
	paramset.label = 'TakeProfit',
	distribution.label.1 = 'TakeProfitLONG',
	distribution.label.2 = 'TakeProfitSHORT',
	operator = '==',
	label = 'TakeProfit'
)

# Walk Forward Analysis

add.distribution(strategy.st,
	paramset.label = 'WFA',
	component.type = 'indicator',
	component.label = 'nFast',
	variable = list(n = .FastWFA),
	label = 'nFAST'
)

add.distribution(strategy.st,
	paramset.label = 'WFA',
	component.type = 'indicator',
	component.label = 'nSlow',
	variable = list(n = .SlowWFA),
	label = 'nSLOW'
)

add.distribution.constraint(strategy.st,
	paramset.label = 'WFA',
	distribution.label.1 = 'nFAST',
	distribution.label.2 = 'nSLOW',
	operator = '<',
	label = 'WFA'
)

###############################################################################

save.strategy(strategy.st)
