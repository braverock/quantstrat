#!/usr/bin/Rscript --vanilla

require(quantstrat)

load('../data/luxor.250.RData')

tradeGraphs (
	stats = stats,
	free.params = c("Param.indicator.1.nFast", "Param.indicator.2.nSlow"),
	fixed.params = NULL,
	statistics = c("Net.Trading.PL", "maxDrawdown", "Avg.Trade.PL", "Num.Trades", "Profit.Factor"),
	title = 'Luxor'
)

