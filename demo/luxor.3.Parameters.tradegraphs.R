#!/usr/bin/Rscript --vanilla

require(quantstrat)

load('../data/luxor.parameters.1-10.30-55.RData')

tradeGraphs (
	stats = stats,
	free.params = c("Param.indicator.1.nFast", "Param.indicator.2.nSlow"),
	fixed.params = NULL,
	statistics = c("Net.Trading.PL", "maxDrawdown", "Avg.Trade.PL", "Num.Trades", "Profit.Factor"),
	title = 'Luxor SMA Parameter Scan'
)

##### PLACE THIS BLOCK AHEAD OF DATE INITS IN DEMO SCRIPT ######
# if(!exists('in_test') || !isTRUE(in_test)){
#     initDate='2005-12-31' # ensure this is demo default
#     endDate=Sys.Date()    # ensure this is demo default
# }
################################################################

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################
