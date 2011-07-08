# TODO: Add comment
# 
# Author: CCD
###############################################################################



#please run macd demo before all these...
paramStructure<-getParameterTable(stratMACD)

tPD2<-setParameterDistribution() 
tPD2

# Just provide leagal values and use random sampling.
tPD2<-setParameterDistribution(tPD2,'indicator',indexnum=1,distribution=list(nFast=(10:30)),label='nFast')
#make them over lap from 20 to 30 to test.
tPD2<-setParameterDistribution(tPD2,'indicator',indexnum=1,distribution=list(nSlow=(20:40)),label='nSlow')
tPD2<-setParameterDistribution(tPD2,'signal',indexnum=1,distribution=list(relationship=c('gt','gte')),label='sig1.gtgte')

pConstraint2<-setParameterConstraint(constraintLabel='macdPC',paramList=c('nFast','nSlow'),relationship='lt')

testPackList2<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,method='random',sampleSize=3,parameterConstrains=pConstraint2)


