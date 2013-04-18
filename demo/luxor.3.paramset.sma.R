#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012, revised April 2013
#
# Tested and found to work correctly using blotter r1420
#
# From Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Paragraph 3.2: luxor with $30 slippage and transaction costs

options(width = 240)
Sys.setenv(TZ='UTC')

.nsamples=80

###

initDate = '2002-10-21'

####

strategy.st = 'luxor'
portfolio.st = 'forex'
account.st = 'IB1'

source('luxor.symbols.R')

### blotter

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')

### quantstrat

initOrders(portfolio.st, initDate=initDate)

load.strategy(strategy.st)

### doMC

require(doMC)
registerDoMC(cores=8)

#require(doParallel)
#registerDoParallel(cores=2)

#require(doRedis)
#registerDoRedis('jobs')

results <- apply.paramset(strategy.st, paramset.label='SMA', portfolio.st=portfolio.st, account.st=account.st, nsamples=.nsamples, verbose=TRUE)

###

stats <- results$tradeStats

save(stats, file='luxor.3.optimize.sma.RData')

print(results$tradeStats)

