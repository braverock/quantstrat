###

options(width = 240)
#options(warn=1)

Sys.setenv(TZ="UTC")

###

startDate = '2002-10-21'

.from=startDate

#.to='2008-07-04'
.to='2002-10-31'

###

strategy.st = 'luxor'
portfolio.st = 'forex'
account.st = 'IB'

###

.orderqty = 100000
.threshold = 0.0005
.txnfees = -6		# round-trip fee

### Distributions for paramset analysis

.nsamples=80

.FastSMA = (1:20)
.SlowSMA = (30:80)

.StopLoss = seq(0.05, 2.4, length.out=48)/100
.StopTrailing = seq(0.05, 2.4, length.out=48)/100
.TakeProfit = seq(0.1, 4.8, length.out=48)/100

.FastWFA = c(1, 3, 5, 7, 9)
.SlowWFA = c(42, 44, 46)

# generate 24x24h ISO8601 timespan vector 
# this includes non-ISO8601 elements (e.g. "T01:00/T00:59")
# that are discarded at processing

.timespans.start<-paste(sprintf("T%02d",0:23),':00',sep='')
.timespans.stop<-paste(sprintf("T%02d",0:23),':59',sep='')

.timespans<-outer(.timespans.start, .timespans.stop, FUN=paste, sep='/')

# in order to run the full 24x24 hour scan above, comment out the following line:
.timespans<-c('T06:00/T10:00', 'T07:00/T11:00', 'T08:00/T12:00', 'T09:00/T13:00', 'T10:00/T14:00', 'T11:00/T15:00', 'T12:00/T16:00')

### Actual arameters

.fast = 6
.slow = 44

#.timespan = 'T09:00/T13:00'
#.timespan = 'T00:00/T23:59'
.timespan = NULL

.stoploss <- 0.40/100
.stoptrailing <- 0.8/100
.takeprofit <- 2.0/100

suppressWarnings(rm(list = c(paste("account", account.st, sep='.'), paste("portfolio", portfolio.st, sep='.')), pos=.blotter))
suppressWarnings(rm(list = c(strategy.st, paste("order_book", portfolio.st, sep='.')), pos=.strategy))
