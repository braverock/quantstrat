# Parameter demo for MACD demo
# 
# Author: Yu Chen
###############################################################################

require(foreach,quietly=TRUE)
require(quantstrat)

demo('macd',ask=FALSE)

# example parallel initialization for doParallel. this or doMC, or doRedis are 
# most probably preferable to doSMP
#require(doParallel)
#registerDoParallel() # by default number of physical cores -1


#please run macd demo before all these...

#retrieve the strategy from the environment, since the 'macd' strategy uses store=TRUE
stratMACD<-getStrategy('macd')

paramStructure<-getParameterTable(stratMACD)

rm(tPD2)


# Just provide leagal values and use random sampling.
# Make nFast and nSlow over lap from 20 to 30 to test the constrains later.
# First call don't need to input tPD2, the funciton will initial one and return it.

tPD2<-setParameterDistribution(type='indicator',indexnum=1,distribution=list(nFast=(10:30)),label='nFast')

# Initial one tPD2 and pass it also works as following two lines:
## tPD2<-setParameterDistribution() 
## tPD2<-setParameterDistribution(type='indicator',indexnum=1,distribution=list(nFast=(10:30)),label='nFast')

tPD2<-setParameterDistribution(tPD2,component.type='indicator',indexnum=1,distribution=list(nSlow=(20:40)),label='nSlow')
tPD2<-setParameterDistribution(tPD2,component.type='signal',indexnum=1,distribution=list(relationship=c('gt','gte')),label='sig1.gtgte')

# Put constrains to the nFast and nSlow parameters.
pConstraint2<-setParameterConstraint(constraintLabel='macdPC',paramList=c('nFast','nSlow'),relationship='lt')



#testPackList2<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,method='random',sampleSize=3,parameterConstrains=pConstraint2)
#system.time(testPackList2<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,method='random',sampleSize=3,parameterConstrains=pConstraint2))


laststpar.rnd<-system.time(
		testPackListPL<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,method='random',sampleSize=20,parameterConstraints=pConstraint2)
)
#laststpar.rnd

# Please run either random mode or expand mode, since the latter run will overwrite the objects in .blotter.
#laststpar.exp<-system.time(
#		testPackListPL<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,method='random',sampleSize=20,parameterConstrains=pConstraint2)
#)
if(verbose >=1) print(laststpar.exp)

#examine the stats from this parameter run:
if(verbose >=1) print(testPackListPL$statsTable)

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
