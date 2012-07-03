#!/usr/bin/Rscript --vanilla

#verbose = 0
verbose = 1

#require(foreach,quietly=TRUE)
require(doMC)

require(quantstrat)

portfolio.st = 'forex'

source('luxor.R')

s<-getStrategy('luxor')

parameterTable<-getParameterTable(s)
#parameterTable

tPD2<-setParameterDistribution(type = 'indicator', indexnum = 1, distribution = list(nFast = (1:20)), label = 'nFast')
tPD2<-setParameterDistribution(tPD2, type = 'indicator', indexnum = 2, distribution = list(nSlow = (21:50)), label = 'nSlow')

pConstraint2<-setParameterConstraint(constraintLabel='luxorPC',paramList=c('nFast','nSlow'),relationship='lt')

registerDoMC(cores=2)

# Please run either random mode or expand mode, since the latter run will overwrite the objects in .blotter.
laststpar.rnd<-system.time(
		testPackListPL<-applyParameter(strategy=s,portfolios=portfolio.st,parameterPool=tPD2,method='random',sampleSize=20,parameterConstraints=pConstraint2)
)
if(verbose >=1) laststpar.rnd

# Please run either random mode or expand mode, since the latter run will overwrite the objects in .blotter.
#laststpar.exp<-system.time(
#		testPackListPL<-applyParameter(strategy=s,portfolios=portfolio.st,parameterPool=tPD2,method='expand',sampleSize=20,parameterConstrains=pConstraint2)
#)
#if(verbose >=1) print(laststpar.exp)

#examine the stats from this parameter run:
if(verbose >=1) print(testPackListPL$statsTable)

