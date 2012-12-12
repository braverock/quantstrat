require(quantstrat)
suppressWarnings(rm("order_book.bbands",pos=.strategy))
suppressWarnings(rm("account.bbands","portfolio.bbands",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratBBands","initDate","initEq",'start_t','end_t'))

###################### LOAD TTRC ######################

data('ttrc')
TTRC = xts(ttrc[,-1],ttrc[,1])

###################### DEFINE VARIABLES #################
SD = 2 
N = 20
currency('USD')
stock('TTRC', currency='USD', multiplier=1)
initDate='1984-12-31'
initEq=1000000
portfolio.st='bbands'
account.st='bbands'

############################ INITIALIZE AND POSITION LOGIC ################

initPortf(portfolio.st,symbols='TTRC', initDate=initDate)
initAcct(account.st,portfolios='bbands', initDate=initDate)
initOrders(portfolio=portfolio.st,initDate=initDate)
addPosLimit(portfolio.st, 'TTRC', initDate, 200, 2 ) #set max pos
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

out<-try(applyStrategy(strategy=stratBBands , portfolios='bbands',parameters=list(sd=SD,n=N)) )

updatePortf(Portfolio='bbands',Dates=paste('::',as.Date(Sys.time()),sep=''))
