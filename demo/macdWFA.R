# Walk Forward demo for MACD
###############################################################################

require(foreach,quietly=TRUE)
require(iterators)
require(quantstrat)

# run the macd demo in this session to set things up
# use source rather than demo so that everything will be local in this session
source(system.file('demo/macd.R',package='quantstrat'),echo = TRUE)

# example parallel initialization for doParallel. this or doMC, or doRedis are 
# most probably preferable to doSMP
#require(doParallel)
#registerDoParallel() # by default number of physical cores -1


#please run macd demo before all these...

#retrieve the strategy from the environment, since the 'macd' strategy uses store=TRUE
strategy.st <- 'macd'

### Set up Parameter Values
.FastMA = (1:10)
.SlowMA = (5:25)
.nsamples = 15 #for random parameter sampling, less important if you're using doParallel or doMC


### MA paramset

add.distribution(strategy.st,
                 paramset.label = 'MA',
                 component.type = 'indicator',
                 component.label = '_', #this is the label given to the indicator in the strat
                 variable = list(n = .FastMA),
                 label = 'nFAST'
)

add.distribution(strategy.st,
                 paramset.label = 'MA',
                 component.type = 'indicator',
                 component.label = '_', #this is the label given to the indicator in the strat
                 variable = list(n = .SlowMA),
                 label = 'nSLOW'
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'MA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'MA'
)


###

wfresults <- walk.forward(strategy.st, 
                        paramset.label = 'MA', 
                        portfolio.st = portfolio.st, 
                        account.st = account.st, 
                        nsamples = .nsamples,
                        period = 'months',
                        k.training = 36,
                        k.testing = 12,
                        verbose =TRUE,
                        anchored = TRUE,
                        audit.prefix = 'macdWFA',
                        include.insamples = TRUE,
                        savewf = FALSE
                        )
                        
wfa.stats <- wfresults$tradeStats

print(wfa.stats)

# TODO FIXME
chart.forward(wfresults)

# your dates will vary, check your working directory
# chart.forward.training('macdWFA.AAPL.20070103T000000.20091231T000000.RData')
