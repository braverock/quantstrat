#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012, revised April 2013
#
# Tested and found to work correctly using blotter r1420

require(quantstrat)

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

###

.FastSMA = (1:20)
.SlowSMA = (30:80)

.StopLoss = seq(0.1, 2.0, length.out=20)/100
.StopTrailing = seq(0.1, 2.0, length.out=20)/100
.TakeProfit = seq(0.1, 2.0, length.out=20)/100

strategy.st <- 'luxor'

###

load.strategy(strategy.st)

### SMA paramset

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

add.constraint(strategy.st,
	paramset.label = 'SMA',
	distribution.label.1 = 'nFAST',
	distribution.label.2 = 'nSLOW',
	operator = '<',
	label = 'SMA'
)

### Stop Loss paramset

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

add.constraint(strategy.st,
	paramset.label = 'StopLoss',
	distribution.label.1 = 'StopLossLONG',
	distribution.label.2 = 'StopLossSHORT',
	operator = '==',
	label = 'StopLoss'
)

### Stop Trailing paramset

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

add.constraint(strategy.st,
	paramset.label = 'StopTrailing',
	distribution.label.1 = 'StopTrailingLONG',
	distribution.label.2 = 'StopTrailingSHORT',
	operator = '==',
	label = 'StopTrailing'
)

### Take Profit paramset

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

add.constraint(strategy.st,
	paramset.label = 'TakeProfit',
	distribution.label.1 = 'TakeProfitLONG',
	distribution.label.2 = 'TakeProfitSHORT',
	operator = '==',
	label = 'TakeProfit'
)

###

save.strategy(strategy.st)

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################
