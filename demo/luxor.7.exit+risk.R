#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - April 2013
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Paragraph 3.5: determination of appropriate exit and risk management

require(quantstrat)

source(paste0(path.package("quantstrat"),"/demo/luxor.include.R"))

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

source(paste0(path.package("quantstrat"),"/demo/luxor.getSymbols.R"))

### blotter

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')

### quantstrat

load.strategy('luxor')

### BEGIN uncomment lines to activate StopLoss and/or StopTrailing and/or TakeProfit rules

#enable.rule('luxor', 'chain', 'StopLoss')
#enable.rule('luxor', 'chain', 'StopTrailing')
#enable.rule('luxor', 'chain', 'TakeProfit')

### END uncomment lines to activate StopLoss and/or StopTrailing and/or TakeProfit rules

addPosLimit(
            portfolio=portfolio.st,
            symbol='GBPUSD',
            timestamp=initDate,
            maxpos=.orderqty)

initOrders(portfolio.st, initDate=initDate)

applyStrategy(strategy.st, portfolio.st, prefer='Open')

View(getOrderBook(portfolio.st)[[portfolio.st]]$GBPUSD)

###############################################################################

updatePortf(portfolio.st, Symbols='GBPUSD', Dates=paste('::',as.Date(Sys.time()),sep=''))

chart.Posn(portfolio.st, "GBPUSD")

###############################################################################

View(t(tradeStats(portfolio.st, 'GBPUSD')))

###############################################################################

print(tradeQuantiles('forex', 'GBPUSD'))

dev.new()

### Uncomment to choose appropriate MAE of MFE graph

chart.ME(portfolio.st, 'GBPUSD', scale='percent', type='MAE')
dev.new()
chart.ME(portfolio.st, 'GBPUSD', scale='percent', type='MFE')

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################
