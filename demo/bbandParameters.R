# Parameter example for BBands demo
# 
# Author: Yu Chen
###############################################################################

require(foreach,quietly=TRUE)
require(quantstrat)

demo('bbands',ask=FALSE)
#the user should load a parallel backend for foreach before running, 
# or this will run in single threaded mode

#please run bbands demo before all these...
paramStructure<-getParameterTable(stratBBands)

#tPD<-setParameterDistribution() need no more for initial object. 

#Do expand test
#tPD<-setParameterDistribution(tPD,'indicator',indexnum=1,distribution=list(sd=(1:3)))
#tPD<-setParameterDistribution(tPD,'signal',indexnum=2,distribution=list(sd=sample(1:10, size=1, replace=FALSE)))
#tPD<-setParameterDistribution(tPD,'signal',indexnum=3,distribution=list(n=sample(1:10, size=1, replace=FALSE)))
#
##update the 3rd slot by using psindex
#tPD<-setParameterDistribution(tPD,'signal',indexnum=2,distribution=list(n=c(20,30)),psindex=3)
#testPackList<-applyParameter(strategy=stratBBands,portfolios='bbands',parameterPool=tPD,method='expand')




#tPD

#debug(applyParameter)
#undebug(applyParameter)


# Just provide leagal values and use random sampling.
tPD<-setParameterDistribution(tPD,'indicator',indexnum=1,distribution=list(sd=(1:3)),weight=c(.25,.25,.5),label='sd')
tPD<-setParameterDistribution(tPD,'signal',indexnum=2,distribution=list(relationship=c("lt","lte")),label='rel')
#tPD<-setParameterDistribution(tPD,'signal',indexnum=2,distribution=list(relationship=c("lte")))
tPD<-setParameterDistribution(tPD,'indicator',indexnum=1,distribution=list(n=20:30),label='n')

#pConstr<-setParameterConstraint()
pConstraint<-setParameterConstraint(constraintLabel='PC1',paramList=c('sd','n'),relationship='gt')

testPackList<-applyParameter(strategy=stratBBands,portfolios=portfolio.st,parameterPool=tPD,method='random',sampleSize=2,parameterConstraints=pConstraint)


