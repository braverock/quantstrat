# Parameter example for BBands demo
###############################################################################

require(foreach,quietly=TRUE)
require(iterators)
require(quantstrat)

# example parallel initialization for doParallel. this or doMC, or doRedis are 
# most probably preferable to doSMP
#require(doParallel)
#registerDoParallel() # by default number of physical cores -1

demo('bbands',ask=FALSE)
strategy.st='bbands'

### User Set up pf parameter ranges to test
.nlist  = 10:40
.sdlist = 1:4

# number of random samples of the parameter distribution to use for random run
.nsamples = 10 

add.distribution(strategy.st,
                 paramset.label = 'BBparams',
                 component.type = 'indicator',
                 component.label = 'BBands', #this is the label given to the indicator in the strat
                 variable = list(n = .nlist),
                 label = 'nFAST'
)

add.distribution(strategy.st,
                 paramset.label = 'BBparams',
                 component.type = 'indicator',
                 component.label = 'BBands', #this is the label given to the indicator in the strat
                 variable = list(sd = .sdlist),
                 label = 'nSLOW'
)


results <- apply.paramset(strategy.st, 
                          paramset.label='BBparams', 
                          portfolio.st=portfolio.st, 
                          account.st=account.st, 
                          nsamples=.nsamples, 
                          verbose=TRUE)

stats <- results$tradeStats

print(stats)



##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2012
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: bbands.R 1097 2012-07-01 00:30:39Z braverock $
#
###############################################################################
