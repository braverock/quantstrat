# Parameter demo for MACD
###############################################################################

require(foreach,quietly=TRUE)
require(iterators)
require(quantstrat)

demo('macd',ask=FALSE)

# example parallel initialization for doParallel. this or doMC, or doRedis are 
# most probably preferable to doSMP
#require(doParallel)
#registerDoParallel() # by default number of physical cores -1


#please run macd demo before all these...

#retrieve the strategy from the environment, since the 'macd' strategy uses store=TRUE
strategy.st <- 'macd'

### Set up Parameter Values
.FastSMA = (1:20)
.SlowSMA = (30:80)
.nsamples = 10 #for random parameter sampling, less important if you're using doParallel or doMC


### SMA paramset

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = '_', #this is the label given to the indicator in the strat
                 variable = list(n = .FastSMA),
                 label = 'nFAST'
)

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = '_', #this is the label given to the indicator in the strat
                 variable = list(n = .SlowSMA),
                 label = 'nSLOW'
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'SMA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'SMA'
)


###

results <- apply.paramset(strategy.st, 
                          paramset.label='SMA', 
                          portfolio.st=portfolio.st, 
                          account.st=account.st, 
                          nsamples=.nsamples, 
                          verbose=TRUE)

stats <- results$tradeStats

print(stats)

##### PLACE THIS BLOCK AHEAD OF DATE INITS IN DEMO SCRIPT ######
# if(!exists('in_test') || !isTRUE(in_test)){
#     initDate='2005-12-31' # ensure this is demo default
#     endDate=Sys.Date()    # ensure this is demo default
# }
################################################################

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################
