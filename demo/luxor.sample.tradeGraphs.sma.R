#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012, revised April 2013
#
# Tested and found to work correctly using blotter r1420
#
# From Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)

require(quantstrat)

### load 'stats' back into .GlobalEnv

load('../data/luxor.parameters.1-10.30-55.RData')

### show trade graphs from stats

tradeGraphs (
	stats = stats,
	free.params = c("Param.indicator.1.nFast", "Param.indicator.2.nSlow"),
	statistics = c("Net.Trading.PL", "maxDrawdown", "Avg.Trade.PL", "Num.Trades", "Profit.Factor"),
	title = 'Luxor SMA Parameter Scan'
)

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

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################
