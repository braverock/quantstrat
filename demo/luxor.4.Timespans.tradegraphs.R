#!/usr/bin/Rscript --vanilla

require(quantstrat)

load('../data/luxor.timespan.24x24.2002-2008.RData')

names(stats)[names(stats)=='testPackListPRL[[k]]$parameters']<-'timespan'

stats$tmp = strsplit(as.character(stats$timespan),'/')

stats$from<-sapply(stats$tmp,FUN='[',1)
stats$to<-sapply(stats$tmp,FUN='[',2)

stats$start<-as.numeric(gsub('T([0-9]+):[0-9]+',x=stats$from,'\\1'))
stats$stop<-(as.numeric(gsub('T([0-9]+):[0-9]+',x=stats$to,'\\1'))+1)%%24

# trading data is in EST (GMT-4): move 4 hours to adjust to GMT
#stats$start<-(stats$start+4)%%24
#stats$stop<-(stats$stop+4)%%24

tradeGraphs(
	stats,
	free.params=c('start','stop'),
	statistics=c('Net.Trading.PL','maxDrawdown',"Avg.Trade.PL",'Num.Trades',"Profit.Factor"),
	title = 'Luxor Intraday TimeWindow Scan'
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
