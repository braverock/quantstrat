#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1143
#
# From Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Figure 3.11: MAE graph of Luxor system

require('blotter')

data('luxor-p066', package='quantstrat', envir=.blotter)

currency(c('GBP', 'USD'))
exchange_rate(c('GBPUSD'), tick_size=0.0001)

chart.ME('luxor', 'GBPUSD', type='MAE', scale='cash')

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
