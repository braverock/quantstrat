#!/usr/bin/Rscript --vanilla

# yellow.R
#
# long only simple moving average crossover
#
# SPX daily data is from 1/1/1970 to 12/31/1972
#
# copyright (c) 2009-2013, Algorithm Alpha, LLC
# Licensed GPL-2
#
################### LOAD QUANTSTRAT #################

require(quantstrat)

###################### LOAD DATA ######################

data(spx)

############################# DEFINE VARIABLES ##############################

port          = 'yellowPort'
acct          = 'yellowAcct'
initEq        = 1e6
initDate      = '1969-12-31'
fast          = 10 
slow          = 30

############################# INITIALIZE ####################################

currency('USD')
stock('spx' ,currency='USD', multiplier=1)
initPortf(port, 'spx', initDate=initDate)
initAcct(acct, port, initEq=initEq, initDate=initDate)
initOrders(port, initDate=initDate )
yellow = strategy(port)

############################# INDICATORS ####################################

yellow <- add.indicator(
                     strategy  = yellow, 
                     name      = 'SMA', 
                     arguments = list(x=quote(Cl(mktdata)), 
                                      n=fast),
                     label     = 'fast' )

yellow <- add.indicator(
                     strategy  = yellow, 
                     name      = 'SMA', 
                     arguments = list(x=quote(Cl(mktdata)), 
                                      n=slow),
                     label     = 'slow' )

############################# SIGNALS #######################################

yellow <- add.signal(
                  strategy  = yellow,
                  name      = 'sigCrossover',
                  arguments = list(columns=c('fast','slow'), 
                                   relationship='lt'),
                  label     = 'fast.lt.slow')

yellow <- add.signal(
                  strategy  = yellow,
                  name      = 'sigCrossover',
                  arguments = list(columns=c('fast','slow'),
                                   relationship='gt'),
                  label     = 'fast.gt.slow')

############################# RULES #########################################

yellow <- add.rule(
                strategy  = yellow,
                name      = 'ruleSignal',
                arguments = list(sigcol    = 'fast.gt.slow',
                                 sigval    = TRUE,
                                 orderqty  = 100,
                                 ordertype = 'market',
                                 orderside = 'long'),

                type      = 'enter',
                label     = 'EnterLONG')
yellow <- add.rule(
                strategy  = yellow,
                name      = 'ruleSignal',
                arguments = list(sigcol    = 'fast.lt.slow',
                                 sigval    = TRUE,
                                 orderqty  = 'all',
                                 ordertype = 'market',
                                 orderside = 'long'),
                type      = 'exit',
                label     = 'ExitLONG')

############################# APPLY STRATEGY ################################

applyStrategy(yellow, port, prefer='Open', verbose=FALSE)
#applyStrategy(yellow, port, verbose=FALSE)

############################# UPDATE ########################################

updatePortf(port, 'spx', Date=paste('::',as.Date(Sys.time()),sep=''))

########################### CONTAINERS CALLED IN TESTING #####################

book = getOrderBook(port)
stats = tradeStats(port)
