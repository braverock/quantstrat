###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2012
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: parameters.R 1218 2012-10-11 20:47:44Z opentrades $
#
###############################################################################
#
# Authors: Jan Humme
#
###############################################################################

#' Rolling Walk Forward Analysis
#' 
#' A wrapper for apply.paramset() and applyStrategy(), implementing a Rolling Walk Forward Analysis (WFA).
#'
#' walk.forward executes a strategy on a portfolio, while
#' rolling a re-optimization of one of the strategies parameter sets during a specified time period (training window), then selecting an optimal
#' parameter combination from the parameter set using an obj function, then applying the selected parameter combo to the next out-of-sample
#' time period immediately following the training window (testing window). Once completed,
#' the training window is shifted forward by a time period equal to the testing window size, and the process is repeated. 
#' WFA stops when there are insufficient data left for a full testing window.
#'
#' For a complete description, see Jaekle&Tomasini chapter 6.
#' 
#' @param portfolio.st the name of the portfolio object
#' @param account.st the name of the account object
#' @param strategy.st the name of the strategy object
#' @param paramset.label a label uniquely identifying within the strategy the paramset to be tested
#' @param period the period unit, as a character string, eg. 'days' or 'months'
#' @param k.training the number of periods to use for training, eg. '3' months
#' @param nsamples the number of sample param.combos to draw from the paramset for training; 0 means all samples (see also apply.paramset)
#' @param audit.prefix prefix to generate filenames for storage of audit data
#' @param k.testing the number of periods to use for testing, eg. '1 month'
#' @param obj.func a user provided function returning the best param.combo from the paramset, based on training results; defaults to 'max'
#' @param obj.args a user provided argument to obj.func, defaults to quote(tradeStats.list$Net.Trading.PL)
#' @param ... optional parameters to pass to apply.paramset()
#' @param verbose dumps a lot of info during the run if set to TRUE, defaults to FALSE
#'
#' @return a list consisting of a slot containing detailed results for each training + testing period, as well as the portfolio and the tradeStats() for the portfolio
#'
#' @seealso \code{\link{applyStrategy}} \code{\link{apply.paramset}} \code{\link{endpoints}} \code{\link{tradeStats}}
#'
#' @author Jan Humme
#'
#' @export

walk.forward <- function(strategy.st, paramset.label, portfolio.st, account.st,
    period, k.training, nsamples=0, audit.prefix=NULL, k.testing,
    obj.func=function(x){which(x==max(x))},
    obj.args=list(x=quote(tradeStats.list$Net.Trading.PL)),
    ..., verbose=FALSE)
{
    must.have.args(match.call(), c('portfolio.st', 'strategy.st', 'paramset.label', 'k.training'))

    strategy <- must.be.strategy(strategy.st)
    must.be.paramset(strategy, paramset.label)

    portfolio <- getPortfolio(portfolio.st)

    results <- list()

    for(symbol.st in names(portfolio$symbols))
    {
        symbol <- get(symbol.st)

        ep <- endpoints(symbol, on=period)

        k <- 1; while(TRUE)
        {
            result <- list()

            # start and end of training window
            training.start <- ep[k] + 1
            training.end   <- ep[k + k.training]

            # stop if training.end is beyond last data
            if(is.na(training.end))
                break

            training.timespan <- paste(index(symbol[training.start]), index(symbol[training.end]), sep='/')

            if(!missing(k.testing) && k.testing>0)
            {
                # start and end of testing window
                testing.start <- ep[k + k.training] + 1
                testing.end   <- ep[k + k.training + k.testing]

                # stop if testing.end is beyond last data
                if(is.na(testing.end))
                    break

                testing.timespan <- paste(index(symbol[testing.start]), index(symbol[testing.end]), sep='/')
            }

            result$training.timespan <- training.timespan

            print(paste('=== training', paramset.label, 'on', training.timespan))

            .audit <- NULL
            if(!is.null(audit.prefix))
                .audit <- new.env()

            # run backtests on training window
            result$apply.paramset <- apply.paramset(strategy.st=strategy.st, paramset.label=paramset.label,
                portfolio.st=portfolio.st, account.st=account.st,
                mktdata=symbol[training.timespan], nsamples=nsamples,
                calc='slave', audit=.audit, verbose=verbose, ...=...)

            tradeStats.list <- result$apply.paramset$tradeStats

            if(!missing(k.testing) && k.testing>0)
            {
                if(!is.function(obj.func))
                    stop(paste(obj.func, 'unknown obj function', sep=': '))

                # select best param.combo
                param.combo.nr <- do.call(obj.func, obj.args)
                param.combo <- tradeStats.list[param.combo.nr, 1:grep('Portfolio', names(tradeStats.list)) - 1]

                assign('obj.func', obj.func, envir=.audit)
                assign('param.combo.nr', param.combo.nr, envir=.audit)
                assign('param.combo', param.combo, envir=.audit)

                # configure strategy to use selected param.combo
                strategy <- quantstrat:::install.param.combo(strategy, param.combo, paramset.label)

                result$testing.timespan <- testing.timespan
                result$param.combo.nr <- param.combo.nr
                result$param.combo <- param.combo
                result$strategy <- strategy

                print(paste('=== testing param.combo', param.combo.nr, 'on', testing.timespan))
                print(param.combo)

                # run backtest using selected param.combo
                applyStrategy(strategy, portfolios=portfolio.st, mktdata=symbol[testing.timespan])
            }
            else
            {
                if(is.null(tradeStats.list))
                    warning(paste('no trades in training window', training.timespan, '; skipping test'))

                k <- k + 1
            }

            if(!is.null(audit.prefix))
            {
                save(.audit, file=paste(audit.prefix, index(symbol[training.start]), index(symbol[training.end]), 'RData', sep='.'))

                .audit <- NULL
            }

            results[[k]] <- result

            k <- k + k.testing
        }
    }
    updatePortf(portfolio.st, Dates=paste('::',as.Date(Sys.time()),sep=''))

    results$portfolio <- portfolio
    results$tradeStats <- tradeStats(portfolio.st)

    return(results)
} 
