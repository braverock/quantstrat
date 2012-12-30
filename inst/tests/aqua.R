#!/usr/bin/Rscript --vanilla
#
# aqua.R
#
# this produces trade statistics that can be compared 
# with results from other frameworks
#
# the data is SPX daily data from 1/1/1970 to 12/31/1972
#
# copyright (c) 2009-2012, Algorithm Alpha, LLC
# Licensed GPL-2
#
################### LOAD QUANTSTRAT #################

require(quantstrat)

###################### LOAD DATA ######################

data(spx)

############### TIMEZONE HOUSEKEEPING #########################

local_TZ = Sys.getenv("TZ")
Sys.unsetenv("TZ")

############################# DEFINE VARIABLES ##############################

port          = 'aquaPort'
acct          = 'aquaAcct'
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
aqua = strategy(port)

############################# INDICATORS ####################################

aqua <- add.indicator(
                     strategy  = aqua, 
                     name      = 'SMA', 
                     arguments = list(x=quote(Cl(mktdata)), 
                                      n=fast),
                     label     = 'fast' )

aqua <- add.indicator(
                     strategy  = aqua, 
                     name      = 'SMA', 
                     arguments = list(x=quote(Cl(mktdata)), 
                                      n=slow),
                     label     = 'slow' )

############################# SIGNALS #######################################

aqua <- add.signal(
                  strategy  = aqua,
                  name      = 'sigCrossover',
                  arguments = list(columns=c('fast','slow'), 
                                   relationship='lt'),
                  label     = 'fast.lt.slow')

aqua <- add.signal(
                  strategy  = aqua,
                  name      = 'sigCrossover',
                  arguments = list(columns=c('fast','slow'),
                                   relationship='gt'),
                  label     = 'fast.gt.slow')

############################# RULES #########################################

aqua <- add.rule(
                strategy  = aqua,
                name      = 'ruleSignal',
                arguments = list(sigcol    = 'fast.gt.slow',
                                 sigval    = TRUE,
                                 orderqty  = 100,
                                 ordertype = 'market',
                                 orderside = 'long'),

                type      = 'enter',
                label     = 'EnterLONG')
aqua <- add.rule(
                strategy  = aqua,
                name      = 'ruleSignal',
                arguments = list(sigcol    = 'fast.lt.slow',
                                 sigval    = TRUE,
                                 orderqty  = 'all',
                                 ordertype = 'market',
                                 orderside = 'long'),
                type      = 'exit',
                label     = 'ExitLONG')

############################# APPLY STRATEGY ################################

applyStrategy(aqua, port, prefer='Open', verbose=FALSE)

############################# UPDATE ########################################

updatePortf(port, 'spx', Date=paste('::',as.Date(Sys.time()),sep=''))

########################### CONTAINERS CALLED IN TESTING #####################

book = getOrderBook(port)

############################### RESET TZ TO ORIGINAL SETTING ########

Sys.setenv(TZ = local_TZ)
