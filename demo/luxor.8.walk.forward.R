#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - April 2013
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Paragraph 3.5: determination of appropriate exit and risk management

source('luxor.include.R')
initDate <- '2003-01-01'
.from <- initDate
.to <- '2003-12-31'

source('luxor.getSymbols.R')

### foreach and doMC

require(foreach)
require(doMC)
registerDoMC(cores=8)

### blotter

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD', initEq=100000)

### quantstrat

require(quantstrat)

initOrders(portfolio.st, initDate=initDate)

load.strategy(strategy.st)

enable.rule(strategy.st, 'chain', 'StopLoss')
#enable.rule(strategy.st, 'chain', 'StopTrailing')
enable.rule(strategy.st, 'chain', 'TakeProfit')

addPosLimit(
            portfolio=portfolio.st,
            symbol='GBPUSD',
            timestamp=initDate,
            maxpos=.orderqty)

### objective function

ess <- function(a.st, p.st)
{
    require(robustbase, quietly=TRUE)
    require(PerformanceAnalytics, quietly=TRUE)

    portfolios.st <- ls(pos=.blotter, pattern=paste('portfolio', p.st, '[0-9]*',sep='.'))

    pr <- PortfReturns(Account = a.st, Portfolios=portfolios.st)

    my.es <- ES(R=pr, clean='boudt')

    return(my.es)
}

my.obj.func <- function(x)
{
    #return(which(max(x$Net.Trading.PL) == x$Net.Trading.PL))
    #return(which(max(x$GBPUSD) == x$GBPUSD))
    return(which(max(x$tradeStats$Net.Trading.PL/x$user.func$GBPUSD) == x$tradeStats$Net.Trading.PL/x$user.func$GBPUSD))
}

### walk.forward

r <- walk.forward(strategy.st, paramset.label='WFA', portfolio.st=portfolio.st, account.st=account.st, period='months', k.training=3, k.testing=1, obj.func=my.obj.func, obj.args=list(x=quote(result$apply.paramset)), user.func=ess, user.args=list('a.st'=account.st, 'p.st'=portfolio.st), audit.prefix='wfa.ples', anchored=FALSE, verbose=TRUE)

### analyse

pdf(paste('GBPUSD', .from, .to, 'pdf', sep='.'))
chart.Posn(portfolio.st)
dev.off()

ts <- tradeStats(portfolio.st)
save(ts, file=paste('GBPUSD', .from, .to, 'RData', sep='.'))

