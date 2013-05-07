###

options(width = 240)
Sys.setenv(TZ="UTC")

###

initDate = '2002-10-21'

.from=initDate

.to='2008-07-04'
#.to='2002-12-31'
#.to='2002-10-31'

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

.StopLoss = seq(0.02, 0.48, length.out=24)/100
.StopTrailing = seq(0.05, 0.4, length.out=8)/100
.TakeProfit = seq(0.1, 1.0, length.out=10)/100

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

