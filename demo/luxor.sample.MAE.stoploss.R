#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Figure 3.11: MAE graph of Luxor system

require('blotter')

data('luxor-p066', package='quantstrat', envir=.blotter)

currency(c('GBP', 'USD'))
exchange_rate(c('GBPUSD'), tick_size=0.0001)

chart.ME(
	 Portfolio='luxor',
	 Symbol='GBPUSD',
	 type='MAE',
	 scale='cash'
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
