#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1123
#
# From Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Paragraph 3.3 Variation of the input parameters: optimisation and stability diagrams
#    uses luxor.3.R

#verbose = 0
verbose = 1

method='expand'
#method='random'
#.sampleSize=8

#.fastRange=(1:20)
#.slowRange=(21:80)

.fastRange=(41:45)
.slowRange=(41:45)

###############################################################################

require(foreach,quietly=TRUE)
require(doMC)
# example parallel initialization for doParallel. this or doMC, or doRedis are 
# most probably preferable to doSMP
#require(doParallel)
#registerDoParallel() # by default number of physical cores -1

require(quantstrat)

portfolio.st = 'forex'

source('luxor.3.R')

s<-getStrategy('luxor')

#parameterTable<-getParameterTable(s)
#parameterTable
#stop()

tPD2<-setParameterDistribution(strategy=s, component.type='indicator', component.label='nFast', distribution = list(nFast = .fastRange), label = 'nFast')
tPD2<-setParameterDistribution(tPD2, strategy=s, component.type='indicator', component.label='nSlow', distribution = list(nSlow = .slowRange), label = 'nSlow')

pConstraint2<-setParameterConstraint(constraintLabel='luxorPC',paramList=c('nFast','nSlow'),relationship='lt')

registerDoMC(cores=2)

if(method == 'random')
{
	laststpar.rnd<-system.time(
			scan.results<-applyParameter(
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
			scan.results<-applyParameter(
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
if(verbose >=1) print(scan.results$statsTable)

stats <- scan.results$statsTable
save(stats, file="luxor.parameters.RData")


##### PLACE DEMO AND TEST DATES HERE #################
#
#if(isTRUE(options('in_test')$in_test))
#  # use test dates
#  {initDate="2011-01-01" 
#  endDate="2012-12-31"   
#  } else
#  # use demo defaults
#  {initDate="1999-12-31"
#  endDate=Sys.Date()}

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################
