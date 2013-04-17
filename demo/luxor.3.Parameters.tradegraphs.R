#!/usr/bin/Rscript --vanilla

require(quantstrat)

load('../data/luxor.parameters.1-10.30-55.RData')

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
