#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012, revised April 2013
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)

require(quantstrat)

source(paste0(path.package("quantstrat"),"/demo/luxor.include.R"))

###

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

add.distribution.constraint(strategy.st,
	paramset.label = 'SMA',
	distribution.label.1 = 'nFAST',
	distribution.label.2 = 'nSLOW',
	operator = '<',
	label = 'SMA'
)

### Timespan paramset

add.distribution(strategy.st,
	paramset.label = 'Timespan',
	component.type = 'enter',
	component.label = 'EnterLONG',
	variable = list(timespan = .timespans),
	label = 'EnterLong'
)

add.distribution(strategy.st,
	paramset.label = 'Timespan',
	component.type = 'enter',
	component.label = 'EnterSHORT',
	variable = list(timespan = .timespans),
	label = 'EnterShort'
)

add.distribution(strategy.st,
	paramset.label = 'Timespan',
	component.type = 'exit',
	component.label = 'Exit2LONG',
	variable = list(timespan = .timespans),
	label = 'ExitLong'
)

add.distribution(strategy.st,
	paramset.label = 'Timespan',
	component.type = 'exit',
	component.label = 'Exit2SHORT',
	variable = list(timespan = .timespans),
	label = 'ExitShort'
)

add.distribution.constraint(strategy.st,
	paramset.label = 'Timespan',
	distribution.label.1 = 'EnterLong',
	distribution.label.2 = 'EnterShort',
	operator = '==',
	label = 'EnterTimespan'
)

add.distribution.constraint(strategy.st,
	paramset.label = 'Timespan',
	distribution.label.1 = 'ExitLong',
	distribution.label.2 = 'ExitShort',
	operator = '==',
	label = 'ExitTimespan'
)

add.distribution.constraint(strategy.st,
	paramset.label = 'Timespan',
	distribution.label.1 = 'EnterLong',
	distribution.label.2 = 'ExitShort',
	operator = '==',
	label = 'EnterExitTimespan'
)

###

save.strategy(strategy.st)
