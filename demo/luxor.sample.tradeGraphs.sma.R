#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012, revised April 2013
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# 3D SMA graph example

require(quantstrat)
require(rgl)

### load 'stats' back into .GlobalEnv

load(paste0(
	path.package('quantstrat'),
	'/data/luxor.parameters.1-10.30-55.RData')
)

### show trade graphs from stats

tradeGraphs (
	stats = stats,
	free.params = c("Param.indicator.1.nFast", "Param.indicator.2.nSlow"),
	statistics = c("Net.Trading.PL", "maxDrawdown", "Avg.Trade.PL", "Num.Trades", "Profit.Factor"),
	title = 'Luxor SMA Parameter Scan'
)
