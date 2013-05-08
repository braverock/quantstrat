###

options(width = 240)
#options(warn=1)

Sys.setenv(TZ="UTC")

###

initDate = '2002-10-21'
#initDate = '2003-01-01'

.from=initDate

#.to='2008-07-04'
.to='2002-10-31'
#.to='2003-07-31'

###

strategy.st = 'luxor'
portfolio.st = 'forex'
account.st = 'IB1'

###

.orderqty = 100000
.threshold = 0.0005
.txnfees = -6		# round-trip fee

### Distributions for paramset analysis

.nsamples=80

.FastSMA = (1:20)
.SlowSMA = (30:80)

#.StopLoss = seq(0.02, 0.48, length.out=24)/100
#.StopLoss = seq(0.01, 0.24, length.out=24)/100
.StopLoss = seq(0.002, 0.048, length.out=24)/100
.StopTrailing = seq(0.05, 0.4, length.out=8)/100
.TakeProfit = seq(0.1, 1.0, length.out=10)/100

.FastWFA = c(1, 3, 5, 7, 9)
.SlowWFA = c(42, 44, 46)

# generate 24x24h ISO8601 timespan vector

.timespans.start<-paste(sprintf("T%02d",0:23),':00',sep='')
.timespans.stop<-paste(sprintf("T%02d",0:23),':59',sep='')

.timespans<-outer(.timespans.start, .timespans.stop, FUN=paste, sep='/')

# in order to run the full 24x24 hour scan above, comment out the following line:
.timespans<-c('T06:00/T10:00', 'T07:00/T11:00', 'T08:00/T12:00', 'T09:00/T13:00', 'T10:00/T14:00', 'T11:00/T15:00', 'T12:00/T16:00')

### Actual arameters

.fast = 6
.slow = 44

#.timespan = 'T08:00/T12:00'
.timespan = 'T00:00/T23:59'

.stoploss <- 0.0025
.stoptrailing <- 0.008
.takeprofit <- 0.019

