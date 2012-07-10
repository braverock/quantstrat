#!/usr/bin/Rscript --vanilla

#verbose = 0
verbose = 1

#method='expand'
method='random'
.sampleSize=20

.fastRange=(1:20)
.slowRange=(21:80)

###############################################################################

#require(foreach,quietly=TRUE)
require(doMC)
require(quantstrat)

portfolio.st = 'forex'

source('luxor.R')

s<-getStrategy('luxor')

parameterTable<-getParameterTable(s)
#parameterTable

tPD2<-setParameterDistribution(type = 'indicator', indexnum = 1, distribution = list(nFast = .fastRange), label = 'nFast')
tPD2<-setParameterDistribution(tPD2, type = 'indicator', indexnum = 2, distribution = list(nSlow = .slowRange), label = 'nSlow')

pConstraint2<-setParameterConstraint(constraintLabel='luxorPC',paramList=c('nFast','nSlow'),relationship='lt')

registerDoMC(cores=2)

if(method == 'random')
{
	laststpar.rnd<-system.time(
			testPackListPL<-applyParameter(
				strategy=s,
				portfolios=portfolio.st,
				parameterPool=tPD2,
				method='random',
				sampleSize=.sampleSize,
				parameterConstraints=pConstraint2
			)
	)
	if(verbose >=1) laststpar.rnd
}

if(method == 'expand')
{
	laststpar.exp<-system.time(
			testPackListPL<-applyParameter(
				strategy=s,
				portfolios=portfolio.st,
				parameterPool=tPD2,
				method='expand',
				parameterConstraints=pConstraint2
			)
	)
	if(verbose >=1) print(laststpar.exp)
}

#examine the stats from this parameter run:
if(verbose >=1) print(testPackListPL$statsTable)

