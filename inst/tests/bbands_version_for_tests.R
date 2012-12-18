#################### CLEANUP PREVIOUS TEST ######################

suppressWarnings(rm(list=ls(.strategy), pos=.strategy))
suppressWarnings(rm(list=ls(.blotter), pos=.blotter))
suppressWarnings(rm(list=ls()))

################### LOAD QUANTSTRAT #################

suppressMessages(require(quantstrat))

###################### LOAD DATA ######################

data('spx')

###################### DEFINE VARIABLES #################

SD = 2 
N = 20
currency('USD')
stock('spx', currency='USD', multiplier=1)
initDate='1969-12-31'
initEq=1000000
portfolio.st='bbands'
account.st='bbands'

############################ INITIALIZE AND POSITION LOGIC ################

initPortf(portfolio.st,symbols='spx', initDate=initDate)
initAcct(account.st,portfolios='bbands', initDate=initDate)
initOrders(portfolio=portfolio.st,initDate=initDate)
addPosLimit(portfolio.st, 'spx', initDate, 200, 2 ) #set max pos
stratBBands <- strategy("bbands")

############################ INDICATOR ############################

stratBBands <- add.indicator(strategy = stratBBands,
                             name = "BBands", 
                             arguments = list(HLC = quote(HLC(mktdata)), 
                                              maType='SMA'))


############################ SIGNALS ##############################

stratBBands <- add.signal(stratBBands,
                          name="sigCrossover",
                          arguments = list(columns=c("Close","up"),
                                           relationship="gt"),
                          label="Cl.gt.UpperBand")

stratBBands <- add.signal(stratBBands,
                          name="sigCrossover",
                          arguments = list(columns=c("Close","dn"),
                                           relationship="lt"),
                          label="Cl.lt.LowerBand")

stratBBands <- add.signal(stratBBands,
                          name="sigCrossover",
                          arguments = list(columns=c("High","Low","mavg"),
                                           relationship="op"),
                          label="Cross.Mid")

############################# RULES ###############################


stratBBands <- add.rule(stratBBands,
                        name='ruleSignal', 
                        arguments = list(sigcol="Cl.gt.UpperBand",
                                         sigval=TRUE, 
                                         orderqty=-100, 
                                         ordertype='market', 
                                         orderside=NULL, 
                                         threshold=NULL,
                                         osFUN=osMaxPos),
                        type='enter')

stratBBands <- add.rule(stratBBands,
                        name='ruleSignal', 
                        arguments = list(sigcol="Cl.lt.LowerBand",
                                         sigval=TRUE, 
                                         orderqty= 100, 
                                         ordertype='market', 
                                         orderside=NULL, 
                                         threshold=NULL,
                                         osFUN=osMaxPos),
                        type='enter')

stratBBands <- add.rule(stratBBands,
                        name='ruleSignal', 
                        arguments = list(sigcol="Cross.Mid",
                                         sigval=TRUE, 
                                         orderqty= 'all', 
                                         ordertype='market', 
                                         orderside=NULL, 
                                         threshold=NULL,
                                         osFUN=osMaxPos),
                        type='exit')

######################## APPLY STRAT  ###################################################

out<-try(applyStrategy(strategy=stratBBands , portfolios='bbands',parameters=list(sd=SD,n=N), verbose=FALSE))

updatePortf(Portfolio='bbands',Dates=paste('::',as.Date(Sys.time()),sep=''))
