#!/usr/bin/Rscript --vanilla
#
# Bumblebee trading system
# copyright (c) 2009-2012, Algorithm Alpha, LLC
# Licensed GPL-2
#
##### PLACE DEMO DATES HERE #################

startDate="1999-12-31"

############################# DEFINE VARIABLES ##############################

sym           = 'GLD'
port          = 'bug'
acct          = 'colony'
initEq        = 100000
fast          = 10
slow          = 30
sd            = 0.5

############################# GET DATA ######################################

suppressMessages(require(quantstrat))
getSymbols(sym, from=startDate, index.class=c("POSIXt","POSIXct"))

############################# INITIALIZE ####################################

currency('USD')
stock(sym ,currency='USD', multiplier=1)
initPortf(port, sym)
initAcct(acct, port, initEq=initEq)
initOrders(port)
bee = strategy(port)

############################# MAX POSITION LOGIC ############################

addPosLimit(
            portfolio=port,
            symbol=sym, 
            timestamp=startDate,  
            maxpos=100)


############################# INDICATORS ####################################

bee <- add.indicator( 
                     strategy  = bee, 
                     name      = 'BBands', 
                     arguments = list(HLC=quote(HLC(mktdata)), 
                                      n=slow, 
                                      sd=sd))

bee <- add.indicator(
                     strategy  = bee, 
                     name      = 'SMA', 
                     arguments = list(x=quote(Cl(mktdata)), 
                                      n=fast),
                     label     = 'fast' )

############################# SIGNALS #######################################

bee <- add.signal(
                  strategy  = bee,
                  name      = 'sigCrossover',
                  arguments = list(columns=c('fast','dn'), 
                                   relationship='lt'),
                  label     = 'fast.lt.dn')

bee <- add.signal(
                  strategy  = bee,
                  name      = 'sigCrossover',
                  arguments = list(columns=c('fast','up'),
                                   relationship='gt'),
                  label     = 'fast.gt.up')

############################# RULES #########################################

bee <- add.rule(
                strategy  = bee,
                name      = 'ruleSignal',
                arguments = list(sigcol    = 'fast.gt.up',
                                 sigval    = TRUE,
                                 orderqty  = 100,
                                 ordertype = 'market',
                                 orderside = 'long',
                                 osFUN     = 'osMaxPos'),

                type      = 'enter',
                label     = 'EnterLONG')

bee <- add.rule(
                strategy  = bee,
                name      = 'ruleSignal',
                arguments = list(sigcol    = 'fast.lt.dn',
                                 sigval    = TRUE,
                                 orderqty  = 'all',
                                 ordertype = 'market',
                                 orderside = 'long'),
                type      = 'exit',
                label     = 'ExitLONG')

bee <- add.rule(
                strategy  = bee,
                name      = 'ruleSignal',
                arguments = list(sigcol     = 'fast.lt.dn',
                                  sigval    = TRUE,
                                  orderqty  =  -100,
                                  ordertype = 'market',
                                  orderside = 'short',
                                  osFUN     = 'osMaxPos'),
                type      = 'enter',
                label     = 'EnterSHORT')

bee <- add.rule(
                strategy  = bee,
                name      = 'ruleSignal',
                arguments = list(sigcol     = 'fast.gt.up',
                                 sigval     = TRUE,
                                 orderqty   = 'all',
                                 ordertype  = 'market',
                                 orderside  = 'short'),
                type      = 'exit',
                label     = 'ExitSHORT')

############################# APPLY STRATEGY ################################

applyStrategy(bee, port, prefer='Open', verbose=FALSE)

############################# UPDATE ########################################

updatePortf(port, sym, Date=paste('::',as.Date(Sys.time()),sep=''))
updateAcct(acct)

########################### USEFUL CONTAINERS #############################

stratStats   = tradeStats(port)
stratReturns = PortfReturns(acct)

############################# EXAMPLE STATS #################################

cat('Profit Factor for bumblebee is: ', stratStats$Profit.Factor, '\n')

suppressMessages(require(PerformanceAnalytics))

cat('Sortino Ratio for bumblebee is: ', SortinoRatio(stratReturns), '\n')

