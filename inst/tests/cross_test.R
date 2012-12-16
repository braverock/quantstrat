#!/usr/bin/Rscript --vanilla
#
# cross_test.R
#
# this produces trade statistics that can be compared 
# with results from other frameworks
#
# copyright (c) 2009-2012, Algorithm Alpha, LLC
# Licensed GPL-2
#
################### LOAD QUANTSTRAT #################

suppressMessages(require(quantstrat))

###################### LOAD DATA ######################

data('spx')

############################# DEFINE VARIABLES ##############################

sym           = spx
port          = 'test_port'
acct          = 'test_acct'
initEq        = 100000
initDate      = '1969-12-31'
fast          = 3 
slow          = 8

############################# INITIALIZE ####################################

currency('USD')
stock('sym' ,currency='USD', multiplier=1)
initPortf(port, 'sym', initDate=initDate)
initAcct(acct, port, initEq=initEq, initDate=initDate)
initOrders(port, initDate=initDate )
cross_test = strategy(port)

############################# MAX POSITION LOGIC ############################
# 
 addPosLimit(
             portfolio=port,
             symbol='sym', 
             timestamp=initDate,  
             maxpos=100)
# 
# 
############################# INDICATORS ####################################

cross_test <- add.indicator(
                     strategy  = cross_test, 
                     name      = 'SMA', 
                     arguments = list(x=quote(Cl(mktdata)), 
                                      n=fast),
                     label     = 'fast' )

cross_test <- add.indicator(
                     strategy  = cross_test, 
                     name      = 'SMA', 
                     arguments = list(x=quote(Cl(mktdata)), 
                                      n=slow),
                     label     = 'slow' )

############################# SIGNALS #######################################

cross_test <- add.signal(
                  strategy  = cross_test,
                  name      = 'sigCrossover',
                  arguments = list(columns=c('fast','slow'), 
                                   relationship='lt'),
                  label     = 'fast.lt.slow')

cross_test <- add.signal(
                  strategy  = cross_test,
                  name      = 'sigCrossover',
                  arguments = list(columns=c('fast','slow'),
                                   relationship='gt'),
                  label     = 'fast.gt.slow')

############################# RULES #########################################

cross_test <- add.rule(
                strategy  = cross_test,
                name      = 'ruleSignal',
                arguments = list(sigcol    = 'fast.gt.slow',
                                 sigval    = TRUE,
                                 orderqty  = 100,
                                 ordertype = 'market',
                                 orderside = 'long',
                                 osFUN     = 'osMaxPos'),

                type      = 'enter',
                label     = 'EnterLONG')

cross_test <- add.rule(
                strategy  = cross_test,
                name      = 'ruleSignal',
                arguments = list(sigcol    = 'fast.lt.slow',
                                 sigval    = TRUE,
                                 orderqty  = 'all',
                                 ordertype = 'market',
                                 orderside = 'long'),
                type      = 'exit',
                label     = 'ExitLONG')

cross_test <- add.rule(
                strategy  = cross_test,
                name      = 'ruleSignal',
                arguments = list(sigcol     = 'fast.lt.slow',
                                  sigval    = TRUE,
                                  orderqty  =  -100,
                                  ordertype = 'market',
                                  orderside = 'short',
                                  osFUN     = 'osMaxPos'),
                type      = 'enter',
                label     = 'EnterSHORT')

cross_test <- add.rule(
                strategy  = cross_test,
                name      = 'ruleSignal',
                arguments = list(sigcol     = 'fast.gt.slow',
                                 sigval     = TRUE,
                                 orderqty   = 'all',
                                 ordertype  = 'market',
                                 orderside  = 'short'),
                type      = 'exit',
                label     = 'ExitSHORT')

############################# APPLY STRATEGY ################################

applyStrategy(cross_test, port, prefer='Open', verbose=FALSE)

############################# UPDATE ########################################

updatePortf(port, 'sym', Date=paste('::',as.Date(Sys.time()),sep=''))

########################### USEFUL CONTAINERS #############################

resultsToCompare = tradeStats(port)

