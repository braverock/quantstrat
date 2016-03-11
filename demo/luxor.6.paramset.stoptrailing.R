#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# compute StopTrailing percentage for various paramsets

require(quantstrat)

source(paste0(path.package("quantstrat"),"/demo/luxor.include.R"))
source(paste0(path.package("quantstrat"),"/demo/luxor.getSymbols.R"))

###

initPortf(portfolio.st, symbols='GBPUSD', currency='USD')
addPosLimit(
            portfolio=portfolio.st,
            symbol='GBPUSD',
            timestamp=startDate,
            maxpos=.orderqty)

initAcct(account.st, portfolios=portfolio.st, currency='USD')

###

initOrders(portfolio.st)

load.strategy(strategy.st)

### BEGIN uncomment lines to activate StopLoss and/or StopTrailing and/or TakeProfit rules

enable.rule('luxor', 'chain', 'StopLoss')
enable.rule('luxor', 'chain', 'StopTrailing')
#enable.rule('luxor', 'chain', 'TakeProfit')

### END uncomment lines to activate StopLoss and/or StopTrailing and/or TakeProfit rules
require(foreach)
#registerDoSEQ()

if (!"doMC" %in% installed.packages()[,1]) {
    install.packages("doMC")
}
require(doMC)
registerDoMC(cores=2)

#require(doParallel)
#registerDoParallel(cores=2)

#require(doRedis)
#registerDoRedis('jobs')

############################

results <- apply.paramset(strategy.st, paramset.label='StopTrailing', portfolio.st=portfolio.st, account.st=account.st, nsamples=80, verbose=TRUE)

stats <- results$tradeStats

print(t(stats))

plot(100*stats$StopTrailingLONG, stats$Net.Trading.PL, type='b', xlab='StopTrailing %', ylab='Net.Trading.PL', main='Luxor')
