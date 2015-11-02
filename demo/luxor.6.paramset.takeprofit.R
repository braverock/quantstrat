#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# compute TakeProfit percentage for various paramsets

require(quantstrat)

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

source(paste0(path.package("quantstrat"),"/demo/luxor.include.R"))
source(paste0(path.package("quantstrat"),"/demo/luxor.getSymbols.R"))

###

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
addPosLimit(
            portfolio=portfolio.st,
            symbol='GBPUSD',
            timestamp=initDate,
            maxpos=.orderqty)

initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')

###

initOrders(portfolio.st, initDate=initDate)

load.strategy(strategy.st)

### BEGIN uncomment lines to activate StopLoss and/or StopTrailing and/or TakeProfit rules

enable.rule('luxor', 'chain', 'StopLoss')
#enable.rule('luxor', 'chain', 'StopTrailing')
enable.rule('luxor', 'chain', 'TakeProfit')

### END uncomment lines to activate StopLoss and/or StopTrailing and/or TakeProfit rules
require(foreach)
#registerDoSEQ()

if (!"doMC" %in% installed.packages()[,1]) {
    install.packages("doMC")
}
require(doMC)
registerDoMC(cores=8)

#require(doParallel)
#registerDoParallel(cores=2)

#require(doRedis)
#registerDoRedis('jobs')

############################

results <- apply.paramset(strategy.st, paramset.label='TakeProfit', portfolio.st=portfolio.st, account.st=account.st, nsamples=80, verbose=TRUE)

stats <- results$tradeStats

View(t(stats))

plot(100*stats$TakeProfitLONG, stats$Net.Trading.PL, type='b', xlab='TakeProfit %', ylab='Net.Trading.PL', main='Luxor')

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################
