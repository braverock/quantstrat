#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1230
#
# From Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# compute StopTrailing percentage for various paramsets

require(quantstrat)

options(width = 240)
Sys.setenv(TZ='UTC')

.qty=100000

.fast = 10
.slow = 30

.qty=100000
.th=0.0005
.txn=0

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

initDate = '2002-10-21'

source('luxor.symbols.R')

###

strategy.st = 'luxor'
portfolio.st = 'forex'
account.st = 'IB1'

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
addPosLimit(
            portfolio=portfolio.st,
            symbol='GBPUSD',
            timestamp=initDate,
            maxpos=.qty)

initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')

###

initOrders(portfolio.st, initDate=initDate)

load.strategy(strategy.st)

############################

require(foreach)
#registerDoSEQ()

require(doMC)
registerDoMC(cores=2)

#require(doParallel)
#registerDoParallel(cores=2)

#require(doRedis)
#registerDoRedis('jobs')

############################

results <- apply.paramset(strategy.st, paramset.label='StopTrailing', portfolio.st=portfolio.st, account.st=account.st, nsamples=80, verbose=TRUE)

print(results$tradeStats)

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################
