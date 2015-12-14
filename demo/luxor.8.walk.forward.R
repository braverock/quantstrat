#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - April 2013
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Paragraph 3.7 walk forward analysis

require(quantstrat)

source(paste0(path.package("quantstrat"),"/demo/luxor.include.R"))
source(paste0(path.package("quantstrat"),"/demo/luxor.getSymbols.R"))
source(paste0(path.package("quantstrat"),"/demo/luxor.5.strategy.ordersets.R"))

### foreach and doMC

require(foreach)
require(doMC)
registerDoMC(cores=8)

### robustbase and PerformanceAnalytics

if (!requireNamespace("robustbase", quietly=TRUE))
  stop("package 'robustbase' required, but not installed")
if (!requireNamespace("PerformanceAnalytics", quietly=TRUE))
  stop("package 'PerformanceAnalytics' required, but not installed")

### blotter

initPortf(portfolio.st, symbols='GBPUSD', currency='USD')
initAcct(account.st, portfolios=portfolio.st, currency='USD', initEq=100000)

### quantstrat

initOrders(portfolio.st)

load.strategy(strategy.st)

enable.rule(strategy.st, 'chain', 'StopLoss')
#enable.rule(strategy.st, 'chain', 'StopTrailing')
enable.rule(strategy.st, 'chain', 'TakeProfit')

addPosLimit(
            portfolio=portfolio.st,
            symbol='GBPUSD',
            timestamp=startDate,
            maxpos=.orderqty)

### objective function

ess <- function(account.st, portfolio.st)
{
    require(robustbase, quietly=TRUE)
    require(PerformanceAnalytics, quietly=TRUE)

    portfolios.st <- ls(pos=.blotter, pattern=paste('portfolio', portfolio.st, '[0-9]*',sep='.'))
    pr <- PortfReturns(Account = account.st, Portfolios=portfolios.st)

    my.es <- ES(R=pr, clean='boudt')

    return(my.es)
}

my.obj.func <- function(x)
{
    # pick one of the following objective functions (uncomment)

    #return(max(x$tradeStats$Max.Drawdown) == x$tradeStats$Max.Drawdown)

    #return(max(x$tradeStats$Net.Trading.PL) == x$tradeStats$Net.Trading.PL)

    return(max(x$user.func$GBPUSD.DailyEndEq) == x$user.func$GBPUSD.DailyEndEq)
}

### walk.forward

r <- walk.forward(strategy.st,
                  paramset.label='WFA',
                  portfolio.st=portfolio.st,
                  account.st=account.st,
                  period='days',
                  k.training=3,
                  k.testing=1,
                  obj.func=my.obj.func,
                  obj.args=list(x=quote(result$apply.paramset)),
                  user.func=ess,
                  user.args=list('account.st'=account.st, 'portfolio.st'=portfolio.st),
                  audit.prefix='wfa',
                  anchored=FALSE,
                  verbose=TRUE)

### analyse

pdf(paste('GBPUSD', .from, .to, 'pdf', sep='.'))
chart.Posn(portfolio.st)
dev.off()

ts <- tradeStats(portfolio.st)
save(ts, file=paste('GBPUSD', .from, .to, 'RData', sep='.'))

