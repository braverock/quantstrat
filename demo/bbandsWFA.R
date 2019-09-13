# Walk Forward demo - see Issue #101
#
# https://github.com/braverock/quantstrat/issues/101
#
# Author: Daniel Hanson (@QuantDevHacks)
###############################################################################

library(quantstrat)

stock.st = c("SPY")
currency("USD")
stock(stock.st, currency="USD",multiplier=1)
Sys.setenv(TZ="UTC") 
initDate = '2011-12-31'
startDate = '2012-01-01'
endDate = '2017-12-31'    
initEq=1e6
tradeSize = initEq/10
getSymbols(stock.st,from=startDate,to=endDate,index.class="POSIXct",adjust=TRUE,src='yahoo')

strat.st <- "bbands"
suppressWarnings(rm.strat(strat.st))
strategy(strat.st, store=TRUE)

add.indicator(strat.st, name = "BBands",
              arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'), label='BBands')

add.signal(strat.st, name="sigCrossover",
           arguments=list(columns=c("Close","up"),relationship="gt"),
           label="Cl.gt.UpperBand")

add.signal(strat.st, name="sigCrossover",
           arguments=list(columns=c("Close","dn"),relationship="lt"),
           label="Cl.lt.LowerBand")

add.signal(strat.st, name="sigCrossover",
           arguments=list(columns=c("High","Low","mavg"),relationship="op"),
           label="Cross.Mid")

add.rule(strategy = strat.st, name='ruleSignal',
         arguments=list(sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-100,
                        ordertype='market', orderside=NULL, threshold=NULL),
         type='enter', label = "Enter.Short")

add.rule(strategy = strat.st, name='ruleSignal',
         arguments=list(sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty=100,
                        ordertype='market', orderside=NULL, threshold=NULL),
         type='enter', label = "Enter.Long")

add.rule(strategy = strat.st, name='ruleSignal',
         arguments=list(sigcol="Cross.Mid",sigval=TRUE, orderqty= 'all',
                        ordertype='market', orderside=NULL, threshold=NULL),
         type='exit', label = "Exit.All")

add.distribution(strat.st,
                 paramset.label = 'BBOPT',
                 component.type = 'indicator',
                 component.label = 'BBands',
                 variable = list(n = seq(10,30,by=10)),     # 2 steps total
                 label = 'n'
)

add.distribution(strat.st,
                 paramset.label = 'BBOPT',
                 component.type = 'indicator',
                 component.label = 'BBands',
                 variable = list(sd = seq(1,3,by=1)),    # 2 steps total
                 label = 'sd'
)

suppressWarnings(rm.strat("opt"))
initPortf(name="opt", stock.st, initDate=initDate)
initAcct(name="opt", portfolios="opt",
         initDate=initDate, initEq=initEq)
initOrders(portfolio="opt", initDate=initDate)

# These will also be used after running WFA, so 
# define once here:
yrsTrain <- 3     
yrsTest <- 1

results <- walk.forward(
  strategy.st=strat.st,
  paramset.label='BBOPT',
  portfolio.st="opt",
  account.st="opt",
  period='years',
  k.training=yrsTrain,
  k.testing=yrsTest,
  nsamples=0,
  audit.prefix='wfa',
  anchored=FALSE,
  verbose=TRUE,
  savewf=TRUE
)

# Now, we look at a plot of each of the OOS test periods using the
# respective optimization in each in-sample training periods.
# What is not clear, however, is what the blue curve in the training
# period represents: a) which training period optimization period it
# represents (or is it something else), and b) why does it start from
# a nonzero position at the outset?
chart.forward(results)  # 'results' is the output *R environment* from
# the walk.forward(.) function call above.