# TODO: Add comment
# 
# Author: Yu Chen
###############################################################################



#please run bbands demo before all these...

tPD<-setParameterDistribution() 

#Do expand test
#tPD<-setParameterDistribution(tPD,'indicator',indexnum=1,distribution=list(sd=(1:3)))
#tPD<-setParameterDistribution(tPD,'signal',indexnum=2,distribution=list(sd=sample(1:10, size=1, replace=FALSE)))
#tPD<-setParameterDistribution(tPD,'signal',indexnum=3,distribution=list(n=sample(1:10, size=1, replace=FALSE)))
#
##update the 3rd slot by using psindex
#tPD<-setParameterDistribution(tPD,'signal',indexnum=2,distribution=list(n=c(20,30)),psindex=3)
#testPackList<-applyParameter(strategy=stratBBands,portfolios='bbands',parameterPool=tPD,method='expand')




tPD
#debug(applyParameter)
#undebug(applyParameter)


# Just provide leagal values and use random sampling.
tPD<-setParameterDistribution(tPD,'indicator',indexnum=1,distribution=list(sd=(1:3)),weight=c(.25,.25,.5))
tPD<-setParameterDistribution(tPD,'signal',indexnum=2,distribution=list(sd=1:10),weight=1:10)
tPD<-setParameterDistribution(tPD,'signal',indexnum=3,distribution=list(n=20:30))

testPackList<-applyParameter(strategy=stratBBands,portfolios='bbands',parameterPool=tPD,method='random',sampleSize=5)


