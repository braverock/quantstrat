###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2015
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
#
# Authors: Jan Humme, Brian G. Peterson
#
###############################################################################

#' Rolling Walk Forward Analysis
#' 
#' A wrapper for apply.paramset() and applyStrategy(), implementing a Rolling Walk Forward Analysis (WFA).
#'
#' walk.forward executes a strategy on a portfolio, while rolling a
#' re-optimization of one of the strategies parameter sets during a specified
#' time period (training window), then selecting an optimal parameter
#' combination from the parameter set using an obj function, then applying the
#' selected parameter combo to the next out-of-sample time period immediately
#' following the training window (testing window). Once completed, the training
#' window is shifted forward by a time period equal to the testing window size,
#' and the process is repeated. WFA stops when there are insufficient data left
#' for a full testing window.
#'
#' Note that walk.forward will generate out of sample (OOS) transactions using
#' the chosen parameter set into the portfolio designated by portfolio.st. So
#' walk.forward shoud be supplied with a 'clean' portfolio environment to avoid 
#' issues such as out of order transactions. 
#' 
#' @param portfolio.st the name of the portfolio object
#' @param account.st the name of the account object
#' @param strategy.st the name of the strategy object
#' @param paramset.label a label uniquely identifying within the strategy the paramset to be tested
#' @param period the period unit, as a character string, eg. 'days' or 'months'
#' @param k.training the number of periods to use for training, eg. '3' months
#' @param nsamples the number of sample param.combos to draw from the paramset for training; 0 means all samples (see also apply.paramset)
#' @param audit.prefix prefix to generate filenames for storage of audit data. For each training set, a separate file is created, containing an enviroment called .audit, with all in-sample portfolios and orderbooks as well as information as to which param.combos were evaluated, and the result of the objective function. In addition, a special file is generated that contains portfolio and orderbook for the concatenated testing param.combos as selected by the objective function, plus (optionally) complete in-sample portfolios and orderbooks for reference (see include.insamples)
#' @param k.testing the number of periods to use for testing, eg. '1 month'
#' @param obj.func a user provided function returning the best param.combo from the paramset, based on training results; defaults to 'max'
#' @param obj.args a user provided argument to obj.func, defaults to quote(tradeStats.list$Net.Trading.PL)
#' @param anchored whether to use a fixed start for the training window (TRUE), or a sliding start (FALSE); defaults to FALSE
#' @param include.insamples will optionally run a full backtest for each param.combo in the paramset, and add the resulting in-sample portfolios and orderbooks to the file '<prefix>.results.RData'; default TRUE
#' @param ... optional parameters to pass to apply.paramset()
#' @param verbose dumps a lot of info during the run if set to TRUE, defaults to FALSE
#' @param savewf boolean, if TRUE, the default, saves audit information on training and testing periods to working directory for later analysis
#'
#' @return a list consisting of a slot containing detailed results for each training + testing period, as well as the portfolio and the tradeStats() for the portfolio
#'
#' @references Tomasini, E. and Jaekle, U. Trading Systems. 2009. Chapter 6
#' @seealso 
#'     \code{\link{applyStrategy}} ,
#'     \code{\link{apply.paramset}} ,
#'     \code{\link{chart.forward}} ,
#'     \code{\link{chart.forward.training}} ,  
#'     \code{\link{endpoints}} ,
#'     \code{\link{tradeStats}}
#'
#' @author Jan Humme, Brian G. Peterson
#'
#' @export
walk.forward <- function(  strategy.st
                         , paramset.label
                         , portfolio.st
                         , account.st
                         , period
                         , k.training
                         , nsamples=0
                         , audit.prefix=NULL
                         , k.testing
                         , obj.func=function(x){which(x==max(x))}
                         , obj.args=list(x=quote(tradeStats.list$Net.Trading.PL))
                         , anchored=FALSE
                         , include.insamples=TRUE
                         , ...
                         , verbose=FALSE
                         , savewf=TRUE
                         )
{
    must.have.args(match.call(), c('portfolio.st', 'strategy.st', 'paramset.label', 'k.training'))

    strategy <- must.be.strategy(strategy.st)
    must.be.paramset(strategy, paramset.label)

    portfolio <- .getPortfolio(portfolio.st)

    results <- list()

    # assuming that timespans for all portfolio symbols are same, so ok to use 1st symbol to calculate end points
    symbol.st <- first(ls(portfolio$symbols))
    symbol.data <- get(symbol.st)

    ep <- endpoints(symbol.data, on=period)

    total.start <- ep[1]
    total.timespan <- paste(index(symbol.data[total.start]), '', sep='/', index(last(symbol.data)))

    old.param.combo <- NULL
    
    k <- 1
    
    # now loop over training and testing periods, collecting output
    while(TRUE)
    {
        result <- list()
        
        # start and end of training window
        if(anchored){ training.start <- ep[1]
        } else {
          training.start <- ep[k] + 1
        }
        training.end   <- ep[k + k.training]

        # stop if training.end is beyond last data
        if(is.na(training.end))
            break

        training.timespan <- paste(index(symbol.data[training.start]), index(symbol.data[training.end]), sep='/')

        if(!missing(k.testing) && k.testing>0)
        {
            # start and end of testing window
            testing.start <- ep[k + k.training] + 1
            testing.end   <- ep[k + k.training + k.testing]

            # stop if testing.end is beyond last data
            if(is.na(testing.end))
                break

            testing.timespan <- paste(index(symbol.data[testing.start]), index(symbol.data[testing.end]), sep='/')
        }

        result$training.timespan <- training.timespan

        print(paste('=== training', paramset.label, 'on', training.timespan))

        .audit <- new.env()

        # run backtests on training window
        result$apply.paramset <- apply.paramset(  strategy.st=strategy.st
                                                , paramset.label=paramset.label
                                                , portfolio.st=portfolio.st
                                                , account.st=account.st
                                                , mktdata=symbol.data
                                                , rule.subset=training.timespan
                                                , nsamples=nsamples
                                                , calc='slave'
                                                , audit=.audit
                                                , verbose=verbose
                                                , ...=...
                                                )

        tradeStats.list <- result$apply.paramset$tradeStats

        if(!missing(k.testing) && k.testing>0)
        {
            if(!is.function(obj.func))
                stop(paste(obj.func, 'unknown obj function', sep=': '))

            # select best param.combo
            param.combo.idx <- try(do.call(obj.func, obj.args))
            if(length(param.combo.idx) == 0 || class(param.combo.idx)=="try-error"){
              if(is.null(old.param.combo)){
                stop('obj.func() returned empty result')
              } else {
                param.combo<-old.param.combo
                param.combo.nr<-row.names(old.param.combo)
                warning('obj.func() returned empty result')
                print('using param.combo:')
                print(param.combo)
              }
            } else {
              if(length(param.combo.idx)>1){
                # choose the last row because expand.grid in paramsets will make 
                # the last row the row with the largest parameter values, roughly 
                # equivalent to highest stability of data usage, 
                # or lowest degrees of freedom
                param.combo.idx <- last(param.combo.idx)
              }
              param.combo <- tradeStats.list[param.combo.idx, 1:grep('Portfolio', names(tradeStats.list)) - 1]
              param.combo.nr <- row.names(tradeStats.list)[param.combo.idx]
            }
              
            old.param.combo<-param.combo
            
            if(!is.null(.audit))
            {
                assign('obj.func', obj.func, envir=.audit)
                assign('param.combo.idx', param.combo.idx, envir=.audit)
                assign('param.combo.nr', param.combo.nr, envir=.audit)
                assign('param.combo', param.combo, envir=.audit)
            }

            # configure strategy to use selected param.combo
            strategy <- install.param.combo(strategy, param.combo, paramset.label)

            result$testing.timespan <- testing.timespan

            print(paste('=== testing param.combo', param.combo.nr, 'on', testing.timespan))
            print(param.combo)

            # run backtest using selected param.combo
            # NOTE, this will generate OOS transactions in the portfolio identified,
            # so strart with a clean portfolio environment.
            applyStrategy(strategy
                          , portfolios=portfolio.st
                          , mktdata=symbol.data
                          , rule.subset=testing.timespan
                          , ...)
        } else {
            if(is.null(tradeStats.list))
                warning(paste('no trades in training window', training.timespan))
        }

        if(!is.null(.audit))
        {
            iso.format <- "%Y%m%dT%H%M%S"
            time.range <- paste(format(index(symbol.data[training.start]), iso.format),
                                format(index(symbol.data[training.end]), iso.format), sep=".")
            if(savewf){
              save(.audit, file = paste(audit.prefix, symbol.st, time.range, "RData", sep="."))
            }
        }

        results[[k]] <- result

        k <- k + k.testing # move the beginning of the next training set forward
    }
    #updatePortf(portfolio.st, Dates=paste('::',as.Date(Sys.time()),sep=''))
    updatePortf(portfolio.st, Dates=total.timespan, sep='')

    results$tradeStats <- tradeStats(portfolio.st)
    #results$portfolio <- portfolio

    if(!is.null(audit.prefix))
    {
        .audit <- new.env()

        portfolio <- getPortfolio(portfolio.st)
        orderbook <- getOrderBook(portfolio.st)
        account <- getAccount(account.st)

        put.portfolio(portfolio.st, portfolio, envir=.audit)
        put.orderbook(portfolio.st, orderbook, envir=.audit)
        put.account(account.st, account, envir=.audit)

        assign('tradeStats', results$tradeStats, envir=.audit)

        if(include.insamples)
        {
          # copy the paramset results in here too
          .audit$apply.paramset <- result$apply.paramset # this is only the last set, needs fixing
        }
        if(savewf){
          save(.audit, file=paste(audit.prefix, 'results', 'RData', sep='.'))
        }
    }
    return(results)
} 
