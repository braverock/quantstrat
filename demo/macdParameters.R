# Parameter demo for MACD demo
# 
# Author: Yu Chen
###############################################################################

require(foreach,quietly=TRUE)
require(quantstrat)

demo('macd',ask=FALSE)

# example parallel initialization for doSMP, doParallel, doMC, or doRedis are 
# most probably preferable to doSMP
#require(doSMP)
#workers <- startWorkers(2)
#registerDoSMP(workers)

#please run macd demo before all these...
paramStructure<-getParameterTable(stratMACD)

rm(tPD2)


# Just provide leagal values and use random sampling.
# Make nFast and nSlow over lap from 20 to 30 to test the constrains later.
# First call don't need to input tPD2, the funciton will initial one and return it.

tPD2<-setParameterDistribution(type='indicator',indexnum=1,distribution=list(nFast=(10:30)),label='nFast')

# Initial one tPD2 and pass it also works as following two lines:
## tPD2<-setParameterDistribution() 
## tPD2<-setParameterDistribution(type='indicator',indexnum=1,distribution=list(nFast=(10:30)),label='nFast')

tPD2<-setParameterDistribution(tPD2,type='indicator',indexnum=1,distribution=list(nSlow=(20:40)),label='nSlow')
tPD2<-setParameterDistribution(tPD2,type='signal',indexnum=1,distribution=list(relationship=c('gt','gte')),label='sig1.gtgte')

# Put constrains to the nFast and nSlow parameters.
pConstraint2<-setParameterConstraint(constraintLabel='macdPC',paramList=c('nFast','nSlow'),relationship='lt')



#testPackList2<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,method='random',sampleSize=3,parameterConstrains=pConstraint2)
#system.time(testPackList2<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,method='random',sampleSize=3,parameterConstrains=pConstraint2))


laststpar.rnd<-system.time(
		testPackListPL<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,method='random',sampleSize=20,parameterConstraints=pConstraint2)
)
laststpar.rnd

# Please run either random mode or expand mode, since the latter run will overwrite the objects in .blotter.
#laststpar.exp<-system.time(
#		testPackListPL<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,method='random',sampleSize=20,parameterConstrains=pConstraint2)
#)
#laststpar.exp

#
#stopWorkers(workers)
#rmSessions(all=TRUE)





