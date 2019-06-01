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
#' A wrapper for \code{\link{apply.paramset}} and \code{\link{applyStrategy}},
#' implementing a Rolling Walk Forward Analysis (WFA).
#'
#' walk.forward executes a strategy on a portfolio, while rolling a
#' re-optimization of one of the strategies parameter sets during a specified
#' time period (training window), then selecting an optimal parameter
#' combination from the parameter set using an obj function, then applying the
#' selected parameter combo to the next out-of-sample time period immediately
#' following the training window (testing window). Once completed, the training
#' window is shifted forward by a time period equal to the testing window size,
#' and the process is repeated. The final testing window may be shorter than the
#' full testing window, if the length of the time series does not allow a full 
#' testing window.
#' 
#' 'anchored' walk forward forces all training windows to start on the first
#' observation of the market data.  This can be useful if the indicators make
#' use of all the data, e.g. for a risk metric such as a volatility estimator,
#' for a regime model, or for a long-memory process of some sort.  If
#' \code{anchored=TRUE} and you have specified \code{k.training}, then the
#' performance of each paramset will only be evaluated on the rolling training 
#' window, even though larger (anchored) periods are used for input calculations.  
#'
#' Note that walk.forward will generate out of sample (OOS) transactions using
#' the chosen parameter set into the portfolio designated by portfolio.st. So
#' walk.forward shoud be supplied with a 'clean' portfolio environment to avoid 
#' issues such as out of order transactions. 
#'
#' The \code{psgc} argument is a tradeoff between memory efficiency and speed.
#' \R does garbage collection promarily when it is running low on RAM, but this 
#' automatic detection works poorly in parallel processes.  If TRUE, the default,
#' \code{walk.proward} and \code{\link{apply.paramset}} will call \code{gc()}
#' at key points to limit RAM usage.  For small tests, this is probably 
#' unecessary and will only slow the test.  For large tests, even on substantial
#' hardware, it may be the difference between completing the test and crashing \R.
#'   
#' @param portfolio.st the name of the portfolio object
#' @param account.st the name of the account object
#' @param strategy.st the name of the strategy object
#' @param paramset.label a label uniquely identifying within the strategy the paramset to be tested
#' @param period the period unit, as a character string, eg. 'days' or 'months'
#' @param k.training the number of periods to use for training, eg. '3' months
#' @param nsamples the number of sample param.combos to draw from the paramset for training; 0 means all samples (see also apply.paramset)
#' @param audit.prefix default NULL.  if not NULL, \code{walk.forward} will store the audit environment from each training period, containing an enviroment called .audit, with all in-sample portfolios and orderbooks as well as information as to which param.combos were evaluated, and the result of the objective function. In addition, a special file is generated that contains portfolio and orderbook for the concatenated testing param.combos as selected by the objective function, plus (optionally) complete in-sample portfolios and orderbooks for reference (see include.insamples)
#' @param k.testing the number of periods to use for testing, eg. '1 month'
#' @param obj.func a user provided function returning the best param.combo from the paramset, based on training results; defaults to 'max'
#' @param obj.args a user provided argument to obj.func, defaults to quote(tradeStats.list$Net.Trading.PL)
#' @param anchored whether to use a fixed start for the training window (TRUE), or a sliding start (FALSE); defaults to FALSE
#' @param include.insamples will optionally run a full backtest for each param.combo in the paramset, and add the resulting in-sample portfolios and orderbooks to the audit environment; default TRUE
#' @param ... optional parameters to pass to apply.paramset()
#' @param verbose dumps a lot of info during the run if set to TRUE, defaults to FALSE
#' @param savewf boolean, default FALSE. if TRUE, saves audit information on training and testing periods to working directory for later analysis
#' @param saveenv boolean, default FALSE. if TRUE, save the paramset environment information for each trial, and not just the tradeStats and chosen paramset
#' @param psgc boolean, if TRUE, the default, run gc() in each worker session to conserve RAM.
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
#'     \code{\link[blotter]{tradeStats}}
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
                         , savewf=FALSE
                         , saveenv=FALSE
                         , psgc=TRUE
                         )
{
    must.have.args(match.call(), c('portfolio.st', 'strategy.st', 'paramset.label', 'k.training'))

    strategy <- must.be.strategy(strategy.st)
    must.be.paramset(strategy, paramset.label)

    portfolio <- .getPortfolio(portfolio.st)

    test.portfolio.st <- paste0("test.", portfolio.st)
    # orig.portfolio.st <- portfolio.st

    .safety <- new.env()
    
    # initialize test portfolios
    clone.portfolio(paste0(portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE,target_envir=.safety)
    clone.orderbook(paste0(portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE,target_envir=.safety)
    
    results <- new.env()

    # assuming that timespans for all portfolio symbols are same, so ok to use 1st symbol to calculate end points
    symbol.st <- first(ls(portfolio$symbols))
    symbol.data <- get(symbol.st)

    ep <- endpoints(symbol.data, on=period)

    total.start <- ep[1]
    total.timespan <- paste(index(symbol.data[total.start]), '', sep='/', index(last(symbol.data)))

    # construct the subsets to use for training/testing
    training.end.v   <- ep[c(k.training+1,(k.training+1)+cumsum(rep(k.testing,as.integer((length(ep)-k.training)/k.testing))))]

    if( is.na(last(training.end.v)) ) {
      training.end.v <- training.end.v[-length(training.end.v)]
    }
    
    training.start.v <- c(1,1+ep[cumsum(rep(k.testing,as.integer((length(ep)-k.training)/k.testing))) + 1])
    
    testing.start.v  <- 1+training.end.v
    
    if(last(testing.start.v)>length(index(symbol.data))){
      # last training period ends when the data ends, so remove it
      testing.start.v <- testing.start.v[-length(testing.start.v)]
    }

    testing.end.v    <- c(training.end.v[-1],last(ep))
    
    if(last(training.start.v)>=last(testing.start.v)){
      # remove the last training period
      training.start.v <- training.start.v[-length(training.start.v)]
      training.end.v <- training.end.v[-length(training.end.v)]
    }

    if(last(training.end.v)>=last(testing.end.v)){
      # remove the last training period
      training.start.v <- training.start.v[-length(training.start.v)]
      training.end.v   <- training.end.v[-length(training.end.v)]
    } 

    if(length(training.start.v)>length(testing.start.v)){
      # more training periods than testing periods
      training.start.v <- training.start.v[-length(training.start.v)]
      # training.end.v   <- training.end.v[-length(training.end.v)]
      # TODO: give the user an option to train anyway, and save these as 'production' parameters
    }

    if(length(testing.start.v)>length(training.start.v)){
      # more testing periods than training periods
      testing.start.v <- testing.start.v[-length(testing.start.v)]
      testing.end.v   <- testing.end.v[-length(testing.end.v)]
    }
    
    if(length(testing.end.v)>length(testing.start.v)){
      #we have an extra ending, remove it
      testing.end.v   <- testing.end.v[-length(testing.end.v)]
    }
    
    #construct the vectors of dates
    training.start   <- index(symbol.data[training.start.v])
    if(anchored || anchored=='anchored' || anchored=='rolling.subset'){
      training.start.v <- rep(1,length(training.start.v))
      training.start   <- rep(index(symbol.data[1]),length(training.start.v))
    }
    training.end     <- index(symbol.data[training.end.v])
    testing.start    <- index(symbol.data[testing.start.v])
    testing.end      <- index(symbol.data[testing.end.v])
    

    if(anchored || anchored=='anchored' || anchored=='rolling.subset'){
      perf.start.v     <- training.start.v
      perf.start       <- index(symbol.data[training.start.v])
    } else {
      perf.start <- perf.start.v  <- rep(NA,length(training.start.v))
    }
    
    
    wf.subsets       <- data.frame( training.start=training.start
                                  , training.end=training.end
                                  , testing.start=testing.start
                                  , testing.end=testing.end
                                  , perf.start=perf.start
                                  , training.start.ep=training.start.v
                                  , training.end.ep=training.end.v
                                  , testing.start.ep=testing.start.v
                                  , testing.end.ep=testing.end.v
                                  , perf.start.ep=perf.start.v
                                  )

    result <- new.env()

    # set up our control variables
    old.param.combo <- NULL
    
    # now loop over training and testing periods, collecting output
    # do the traditional rolling method, computationally expensive
    for(i in 1:nrow(wf.subsets))
    {
      result <- new.env()
      if(!is.null(audit.prefix) || saveenv){
        .audit <- new.env()
      } else {
        .audit=NULL
      }
      
      training.timespan <- paste(wf.subsets[i,'training.start'], wf.subsets[i,'training.end'], sep='/')
      testing.timespan  <- paste(wf.subsets[i,'testing.start'], wf.subsets[i,'testing.end'], sep='/')

      #choose the perf.subset to use for apply.paramsets
      if(anchored || anchored=='anchored' ){
        perf.subset  <- paste(wf.subsets[i,'perf.start'], wf.subsets[i,'training.end'], sep='/')
      } else {
        perf.subset <- training.timespan
      }
      
      t.start <- wf.subsets[i,'training.start']
      t.end   <- wf.subsets[i,'training.end']
      
      result$training.timespan <- training.timespan
      result$testing.timespan  <- testing.timespan
      
      print(paste('=== training', paramset.label, 'on', training.timespan))
      
      
      # run backtests on training window
      result$apply.paramset <- apply.paramset( strategy.st=strategy.st
                                             , paramset.label=paramset.label
                                             , portfolio.st=portfolio.st
                                             , account.st=account.st
                                             , mktdata=symbol.data
                                             , rule.subset=training.timespan
                                             , nsamples=nsamples
                                             , calc='slave'
                                             , audit=.audit
                                             #, verbose=verbose
                                             , perf.subset=perf.subset
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
        
        result$testing.param.combo <- param.combo
        result$testing.param.combo.idx <- param.combo.idx
        
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
        
        # put the original portfolio back in the .blotter env
        clone.portfolio(paste0("test.",portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE,src_envir=.safety)
        clone.orderbook(paste0("test.",portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE,src_envir=.safety)
        
        # run backtest using selected param.combo
        # NOTE, this will generate OOS transactions in the portfolio identified,
        # so strart with a clean portfolio environment.
        applyStrategy( strategy
                     , portfolios=test.portfolio.st
                     # , portfolios=portfolio.st
                     , mktdata=symbol.data
                     , rule.subset=testing.timespan
                     , ...
                     )
        

        # now copy the original portfolio out of the way, from .blotter to .safety env
        clone.portfolio(paste0("test.",portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE, target_envir=.safety)
        clone.orderbook(paste0("test.",portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE, target_envir=.safety)
        
      } else {
        if(is.null(tradeStats.list))
          warning(paste('no trades in training window', training.timespan))
      }

      iso.format <- "%Y%m%dT%H%M%S"
      time.range <- paste(format(index(symbol.data[t.start]), iso.format),
                          format(index(symbol.data[t.end]), iso.format), sep=".")
      
      if(!is.null(.audit) && !is.null(audit.prefix)){
        
        result$audit      <- .audit
        
        if(savewf){
          filestr<-paste(audit.prefix, symbol.st, time.range, "RData", sep=".")
          if(verbose) cat('Saving .audit env in file: ',filestr,'\n')
          save(.audit, file = filestr)
        }
      }
      
      if(is.null(results[[time.range]]))  results[[time.range]] <- new.env()

      #store some reduced form information to use later
      results[[time.range]][['testing.param.combo']] <- result$testing.param.combo
      results[[time.range]][['testing.timespan']] <- result$testing.timespan
      results[[time.range]][['training.timespan']] <- result$training.timespan
      results[[time.range]]$tradeStats <- result$apply.paramsets$tradeStats
      if(saveenv){
        results[[time.range]]$audit <- .audit
      }

    } # end full rolling training/testing loop

    if(include.insamples){
      # run apply.paramset on the entire period
      if(!is.null(.audit)){
        # only keep the debug auditing information if we are 
        # keeping it for the rest of the simulation
        .insampleaudit <- new.env()
      } else {
        .insampleaudit <- NULL
      }
      results$insample.apply.paramset <- 
        apply.paramset( strategy.st=strategy.st
                        , paramset.label=paramset.label
                        , portfolio.st=portfolio.st
                        , account.st=account.st
                        , mktdata=symbol.data
                        , nsamples=nsamples
                        , calc='slave'
                        , audit=.insampleaudit
                        #, verbose=verbose
                        , ...=...
        )
    }
    
    # orig.portfolio.st holds our OOS txns, so lets clone that portfolio to
    # another portfolio named test.portfolio.st which we will use as the OOS
    # portfolio in chart.
    clone.portfolio(paste0("test.",portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE,src_envir=.safety)
    clone.orderbook(paste0("test.",portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE,src_envir=.safety)
    #updatePortf(portfolio.st, Dates=paste('::',as.Date(Sys.time()),sep=''))
    updatePortf(test.portfolio.st, Dates=total.timespan, sep='')
    
    results$tradeStats <- tradeStats(test.portfolio.st)
    #results$portfolio <- portfolio

    iso.format <- "%Y%m%dT%H%M%S"
    tfs <- format(index(symbol.data[t.start]), iso.format)
    tfe <- format(index(symbol.data[t.end]), iso.format)
    time.range <- paste(tfs, tfe , sep=".")

    results$blotter    <- .blotter
    results$strategy   <- .strategy
    results$wf.subsets <- wf.subsets
    
    results$portfolio.st <- portfolio.st
    
    results$testing.parameters <- NULL
    for (tp in ls(pattern='*.[0-9]+',pos=results)){
      tr <- cbind(results[[tp]][['testing.param.combo']], 
                  results[[tp]][['testing.timespan']])
      if(is.null(results$testing.parameters)){
        results$testing.parameters <- tr
      } else {
        results$testing.parameters <- rbind(results$testing.parameters, tr)
      }
    }
    colnames(results$testing.parameters)[ncol(results$testing.parameters)] <- 'testing.timespan'
    
    if(!is.null(.audit) && !is.null(audit.prefix))
    {
      results$audit      <- .audit
    }
    
    if(savewf){
      filestr<-paste(audit.prefix, symbol.st, time.range,"Results","RData", sep=".")
      cat('\n','Saving final results env in file: ',filestr,'\n')
      save(results, file = filestr)
    }
    
    return(results)
}
