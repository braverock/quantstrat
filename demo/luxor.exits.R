#!/usr/bin/Rscript --vanilla

.FastSMA = (1:20)
.SlowSMA = (30:80)

.StopLoss = seq(0.1, 2.0, length.out=20)/100

.StopTrailing = seq(0.1, 2.0, length.out=20)/100

.TakeProfit = seq(0.1, 2.0, length.out=20)/100

#require(quantstrat)

source('luxor.orderchains.R')

s<-getStrategy('luxor')

### SMA paramset

s<-add.distribution(s,
	paramset.label = 'SMA',
	component.type = 'indicator',
	component.label = 'nFast',
	variable = list(n = .FastSMA),
	label = 'nFAST'
)

s<-add.distribution(s,
	paramset.label = 'SMA',
	component.type = 'indicator',
	component.label = 'nSlow',
	variable = list(n = .SlowSMA),
	label = 'nSLOW'
)

s<-add.constraint(s,
	paramset.label = 'SMA',
	distribution.label.1 = 'nFAST',
	distribution.label.2 = 'nSLOW',
	operator = '<',
	label = 'SMA'
)

### Stop Loss paramset

s<-add.distribution(s,
	paramset.label = 'StopLoss',
	component.type = 'chain',
	component.label = 'StopLossLONG',
	variable = list(threshold = .StopLoss),
	label = 'StopLossLONG'
)

s<-add.distribution(s,
	paramset.label = 'StopLoss',
	component.type = 'chain',
	component.label = 'StopLossSHORT',
	variable = list(threshold = .StopLoss),
	label = 'StopLossSHORT'
)

s<-add.constraint(s,
	paramset.label = 'StopLoss',
	distribution.label.1 = 'StopLossLONG',
	distribution.label.2 = 'StopLossSHORT',
	operator = '==',
	label = 'StopLoss'
)

### Stop Trailing paramset

s<-add.distribution(s,
	paramset.label = 'StopTrailing',
	component.type = 'chain',
	component.label = 'StopTrailingLONG',
	variable = list(threshold = .StopTrailing),
	label = 'StopTrailingLONG'
)

s<-add.distribution(s,
	paramset.label = 'StopTrailing',
	component.type = 'chain',
	component.label = 'StopTrailingSHORT',
	variable = list(threshold = .StopTrailing),
	label = 'StopTrailingSHORT'
)

s<-add.constraint(s,
	paramset.label = 'StopTrailing',
	distribution.label.1 = 'StopTrailingLONG',
	distribution.label.2 = 'StopTrailingSHORT',
	operator = '==',
	label = 'StopTrailing'
)

### Take Profit paramset

s<-add.distribution(s,
	paramset.label = 'TakeProfit',
	component.type = 'chain',
	component.label = 'TakeProfitLONG',
	variable = list(threshold = .TakeProfit),
	label = 'TakeProfitLONG'
)

s<-add.distribution(s,
	paramset.label = 'TakeProfit',
	component.type = 'chain',
	component.label = 'TakeProfitSHORT',
	variable = list(threshold = .TakeProfit),
	label = 'TakeProfitSHORT'
)

s<-add.constraint(s,
	paramset.label = 'TakeProfit',
	distribution.label.1 = 'TakeProfitLONG',
	distribution.label.2 = 'TakeProfitSHORT',
	operator = '==',
	label = 'TakeProfit'
)

