# Work Flow:
# Example Code 1 for Running Signal Analysis in Quantstrat.
# System: A simple moving average strategy for evaluating signal
# Author: Michael Guan
###########################################################################

# Load Packages:
require(iterators)
require(quantstrat)
require(gamlss.util)  # depends on gamlss

###########################################################################
# Configure Date Time Settings
ttz<-Sys.getenv('TZ')
Sys.setenv(TZ='UTC')

suppressWarnings(rm("order_book.macross",pos=.strategy))
suppressWarnings(rm("account.macross","portfolio.macross",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","strategy.st",'start_t','end_t'))

###########################################################################
# Data
startDate="1999-12-31"
stock.str=c('XLY','XLF','XLP','XLI','RTH','XLV','XLK','XLE','IYT')
currency('USD')
stock(stock.str,currency='USD',multiplier=1)
getSymbols(stock.str,from=startDate,src = 'yahoo')
for(i in stock.str)
  assign(i, adjustOHLC(get(i),use.Adjusted=TRUE))

###########################################################################
# Account, Portfolio, Strategy Initialization
initEq=1000000
portfolio.st='macross'
account.st='macross'
initPortf(portfolio.st,symbols=stock.str)
initAcct(account.st,portfolios=portfolio.st, initEq=initEq)
initOrders(portfolio=portfolio.st)

strategy.st<- strategy(portfolio.st)


#Indicator
strategy.st <- add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n=50),label= "ma50" )
strategy.st <- add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x=quote(Cl(mktdata)[,1]), n=200),label= "ma200")

#Signals
strategy.st <- add.signal(strategy = strategy.st,name="sigCrossover",arguments = list(columns=c("ma50","ma200"), relationship="gte"), label="ma50.gt.ma200")
strategy.st <- add.signal(strategy = strategy.st,name="sigCrossover",arguments = list(column=c("ma50","ma200"),relationship="lt"), label="ma50.lt.ma200")

###########################################################################

# Signal Column Label to Analyze
signal.label = 'ma50.gt.ma200'

# Desired Parameter Pool
.FastSMA = seq(1,5,1)
.SlowSMA = seq(5,20,5)

strategy.st<-add.distribution(strategy.st,
                              paramset.label = 'SMA',
                              component.type = 'indicator',
                              component.label = 'ma50',
                              variable = list(n = .FastSMA),
                              label = 'nFAST')

strategy.st<-add.distribution(strategy.st,
                              paramset.label = 'SMA',
                              component.type = 'indicator',
                              component.label = 'ma200',
                              variable = list(n = .SlowSMA),
                              label = 'nSLOW')

# Constraint: nFast < nSlow
strategy.st<-add.distribution.constraint(strategy.st,
                                         paramset.label = 'SMA',
                                         distribution.label.1 = 'nFAST',
                                         distribution.label.2 = 'nSLOW',
                                         operator = '<',
                                         label = 'SMA')


# Daily Signal With Post Daily Return Analysis
results =apply.paramset.signal.analysis(strategy.st, 
                                        paramset.label='SMA', 
                                        portfolio.st, 
                                        sigcol = signal.label,
                                        sigval = 1,
                                        on=NULL,
                                        forward.days=50,
                                        cum.sum=TRUE,
                                        include.day.of.signal=F,
                                        obj.fun=signal.obj.slope,
                                        decreasing=T)

distributional.boxplot(signal=results$sigret.by.asset$IYT$paramset.5.20,
                       x.val=seq(1, 50, 5),val=10,ylim=c(-20, 20),
                       xlim=c(0, 50),mai=c(1,1,0.3,0.5),h=0)

signal.plot(results$sigret.by.asset$XLE, rows=5, columns = 4)
beanplot.signals(results$sigret.by.asset$XLE, rows=5, columns = 4)
signal.path.plot(results$sigret.by.asset$IYT$paramset.5.20)


# Daily Signal With Post Weekly Return Analysis
results.w =apply.paramset.signal.analysis(strategy.st, 
                                          paramset.label='SMA', 
                                          portfolio.st, 
                                          sigcol = signal.label,
                                          sigval = 1,
                                          on='weeks',
                                          forward.days=10,
                                          cum.sum=TRUE,
                                          include.day.of.signal=F,
                                          obj.fun=signal.obj.slope,
                                          decreasing=T)

distributional.boxplot(signal=results.w$sigret.by.asset$IYT$paramset.5.20,
                       x.val=seq(1, 10, 2),val=10,ylim=c(-20, 20),
                       xlim=c(0, 10),mai=c(1,1,0.3,0.5),h=0)

signal.plot(results.w$sigret.by.asset$XLE, rows=5, columns = 4)
beanplot.signals(results.w$sigret.by.asset$XLE, rows=5, columns = 4)


# Daily Signal With Post Monthly Return Analysis
results.m =apply.paramset.signal.analysis(strategy.st, 
                                          paramset.label='SMA', 
                                          portfolio.st, 
                                          sigcol = signal.label,
                                          sigval = 1,
                                          on='months',
                                          forward.days=5,
                                          cum.sum=TRUE,
                                          include.day.of.signal=F,
                                          obj.fun=signal.obj.slope,
                                          decreasing=T)

distributional.boxplot(signal=results.m$sigret.by.asset$IYT$paramset.5.20,
                       x.val=seq(1, 5, 1),val=10,ylim=c(-30, 30),
                       xlim=c(0, 5),mai=c(1,1,0.3,0.5),h=0)

signal.plot(results.m$sigret.by.asset$XLE, rows=5, columns = 4)
beanplot.signals(results.m$sigret.by.asset$XLE, rows=5, columns = 4)

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2015
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson,
# Jeffrey Ryan, Joshua Ulrich, and Garrett See
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
###############################################################################
