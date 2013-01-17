#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1123
#
# From Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Paragraph 3.4: inserting an intraday time filter
#    uses luxor.4.R

source('luxor.4.R')

#verbose = 0
verbose = 1

method='expand'
#method='random'
#.sampleSize=20

# generate 24x24h ISO8601 timespan vector

.timespans.start<-paste(sprintf("T%02d",0:23),':00',sep='')
.timespans.stop<-paste(sprintf("T%02d",0:23),':59',sep='')

.timespans<-outer(.timespans.start, .timespans.stop, FUN=paste, sep='/')

# in order to run the full 24x24 hour scan above, comment out the following line:
.timespans<-c('T06:00/T10:00', 'T07:00/T11:00', 'T08:00/T12:00', 'T09:00/T13:00', 'T10:00/T14:00', 'T11:00/T15:00', 'T12:00/T16:00')

###############################################################################

require(doMC)
require(foreach,quietly=TRUE)
# example parallel initialization for doParallel. this or doMC, or doRedis are 
# most probably preferable to doSMP
#require(doParallel)
#registerDoParallel() # by default number of physical cores -1

require(quantstrat)

portfolio.st = 'forex'

s<-getStrategy('luxor')

parameterTable<-getParameterTable(s)

tPD2<-setParameterDistribution(strategy=s, component.type='enter', component.label='EnterLONG', distribution = list(timespan = .timespans), label = 'Timespan')

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
				parameterConstraints=NULL
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
				parameterConstraints=NULL
			)
	)
	if(verbose >=1) print(laststpar.exp)
}

#examine the stats from this parameter run:
if(verbose >=1) print(scan.results$statsTable)

stats <- scan.results$statsTable
save(stats, file="luxor.timespan.RData")

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
