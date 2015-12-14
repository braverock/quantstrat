# Work Flow:
# Example Code 2 for Running Signal Analysis in Quantstrat.
# System: A simple RSI strategy for evaluating signals.
# Author: Michael Guan
###########################################################################

# Load Packages:
require(iterators)
require(quantstrat)
require(gamlss.util)  # depends on gamlss

suppressWarnings(rm("order_book.RSI",pos=.strategy))
suppressWarnings(rm("account.RSI","portfolio.RSI",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratRSI","startDate","initEq",'start_t','end_t'))

#Parameters
n=2

#Data
currency("USD")
currency("EUR")
symbols = c("SPY")
for(symbol in symbols){ # establish trade-able instruments
  stock(symbol, currency="USD",multiplier=1)
  getSymbols(symbol,src='yahoo')
}

# Initialize Account, Portfolio, Strategy
stratRSI <- strategy("RSI")

startDate='1997-12-31'
initEq=100000
port.st<-'RSI' #use a string here for easier changing of parameters and re-trying

initPortf(port.st, symbols=symbols)
initAcct(port.st, portfolios=port.st, initEq=initEq)
initOrders(portfolio=port.st)
for(symbol in symbols){ addPosLimit(port.st, symbol, startDate, 300, 3 ) } #set max pos 

# Indicator
stratRSI <- add.indicator(strategy = stratRSI, name = "RSI", arguments = list(price = quote(getPrice(mktdata)),n=n), label="RSI")

# There are two signals:
# The first is when RSI is greater than 90
stratRSI <- add.signal(strategy = stratRSI, name="sigThreshold",arguments = list(threshold=70, column="RSI",relationship="gt", cross=TRUE),label="RSI.gt.70")
# The second is when RSI is less than 10
stratRSI <- add.signal(strategy = stratRSI, name="sigThreshold",arguments = list(threshold=30, column="RSI",relationship="lt",cross=TRUE),label="RSI.lt.30")

#########################################################################
#Signal Analysis

#Entry Signal colname Label
signal.label = 'RSI.lt.30'

.n = seq(2,10,1)

strategy.st<-add.distribution(stratRSI,
                              paramset.label = 'RSI',
                              component.type = 'indicator',
                              component.label = 'RSI',
                              variable = list(n = .n),
                              label = 'nRSI')


# Run Study
results =apply.paramset.signal.analysis(strategy.st, 
                                        paramset.label='RSI', 
                                        port.st, 
                                        sigcol = signal.label,
                                        sigval = 1,
                                        on=NULL,
                                        forward.days=10,
                                        cum.sum=TRUE,
                                        include.day.of.signal=F,
                                        obj.fun=signal.obj.slope,
                                        decreasing=T,
                                        mktdata=NULL,
                                        verbose=TRUE)


# Plot Paramset Combined Barchart [Subset list to plot a sub portion if too large]
signal.plot(results$sigret.by.asset$SPY, rows=2, columns = 5)

# Distributional Box Plot via gamlss
distributional.boxplot(results$sigret.by.asset$SPY$paramset.2)

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

