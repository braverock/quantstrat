###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2023
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
#
# Authors: Jan Humme, Brian Peterson
#
# This code is inspired by  earlier work by Yu Chen and Brian Peterson
#
###############################################################################
#
# This code uses the following terminology:
#
# component.type: indicator, signal or order/enter/exit/chain-type rule, identified by a component.label
#
# constraint: a restriction applying to 2 distributions
#
# distribution: a range of values to be applied to a particular strategy parameter, identified by the tuple
# (component.type, component.label, variable.name)
#
# parameter: a variable argument in a strategy component
#
# paramset: a set of parameter distributions and constraints, identified by a paramset.label
#
# param.combo: an expanded distribution
#
# param.values: the set of values to be applied to a parameter
#
###############################################################################

# TODO: fix put.portfolio() to use environments
# TODO: fix expand.grid
# TODO: "and" multiple constraints i.o. "or"

#' clone a portfolio object, potentially stripping all history
#'
#' creates a copy of a portfolio, stripping all history (transactions etc)
#' 
#' @param portfolio.st string identifying the source portfolio
#' @param cloned.portfolio.st string naming the target portfolio
#' @param strip.history boolean, default TRUE, whether to remove all transaction and portfolio history, keeping only the structure (e.g. assets) 
#' @param src_envir environment to get the portfolio to be cloned from, default \code{.blotter}
#' @param target_envir environment to put the cloned portfolio in, passed to \code{\link[blotter]{put.portfolio}}
clone.portfolio <- function(  portfolio.st
                            , cloned.portfolio.st
                            , strip.history=TRUE
                            , src_envir=.blotter
                            , target_envir=.blotter)
{
    #must.have.args(match.call(), c('portfolio.st', 'cloned.portfolio.st'))

    # get a *copy* of the portfolio, not the pointer to the environment
    portfolio <- getPortfolio(portfolio.st, envir = src_envir)

    if(strip.history==TRUE)
    {
        for(symbol in ls(portfolio$symbols))
        {
            portfolio$symbols[[symbol]]$txn <- portfolio$symbols[[symbol]]$txn[1,]

            xts.tables <- grep('(^posPL|txn|PosLimit)',names(portfolio$symbols[[symbol]]), value=TRUE)
            for(xts.table in xts.tables)
                portfolio$symbols[[symbol]][[xts.table]] <- portfolio$symbols[[symbol]][[xts.table]][1,]
        }
        portfolio$summary <- portfolio$summary[1,]
    }
    
    put.portfolio(as.character(cloned.portfolio.st), portfolio, envir = target_envir)

    return(cloned.portfolio.st)
}

#' clone a orderbook object, potentially stripping all history
#'
#' creates a copy of a orderbook, stripping all history (orders etc)
#' 
#' @param orderbook.st string identifying the source orderbook
#' @param cloned.orderbook.st string naming the target orderbook
#' @param strip.history boolean, default TRUE, whether to remove all orderbook history, keeping only the structure (e.g. assets) 
#' @param src_envir environment to get the source orderbook from, default \code{.strategy}
#' @param target_envir environment to put the cloned orderbook in, passed to \code{\link{put.orderbook}}
clone.orderbook <- function(  orderbook.st
                            , cloned.orderbook.st
                            , strip.history=TRUE
                            ,src_envir=.strategy
                            ,target_envir=.strategy)
{
    #must.have.args(match.call(), c('orderbook.st', 'cloned.orderbook.st'))

    orderbook <- getOrderBook(orderbook.st,envir = src_envir)

    i <- 1  # TODO: find index number by name
    names(orderbook)[i] <- cloned.orderbook.st

    if(strip.history == TRUE)
    {
        for(symbol in names(orderbook[[orderbook.st]]))
            orderbook[[orderbook.st]][[symbol]] <- orderbook[[orderbook.st]][[symbol]][1,]
    }

    put.orderbook(cloned.orderbook.st, orderbook,envir = target_envir)
}

### local functions ############################################################

must.be.paramset <- function(strategy, paramset)
{
    if(!(paramset %in% names(strategy$paramsets)))
        stop(paste(paramset, ': no such paramset in strategy', strategy$name))
}

create.paramset <- function(strategy, paramset.label)
{
    strategy$paramsets[[paramset.label]] <- list()
    strategy$paramsets[[paramset.label]]$distributions <- list()
    strategy$paramsets[[paramset.label]]$constraints <- list()

    strategy
}

#' given a distributions object expand all options to a full factorial representation
#'
#' @param distributions distributions slot from a strtaegy object
#'
#' @return expanded grid of all distributions as a `data.frame` by distribution labels  
#'
#' @seealso expand.grid
expand.distributions <- function(distributions)
{
  param.values <- list()
  
  for(distribution.label in names(distributions))
  {
    param.values[[distribution.label]] <-
      coredata(distributions[[distribution.label]]$variable.dist)
  }
  expand.grid(param.values)
}

apply.constraints <- function(constraints, distributions, param.combos)
{
    for(constraint in constraints)
    {
        operator <- constraint$operator

        distribution.name.1 <- constraint$distributions[[1]]
        distribution.name.2 <- constraint$distributions[[2]]

        variable.name.1 <- names(distributions[[distribution.name.1]]$variable)
        variable.name.2 <- names(distributions[[distribution.name.2]]$variable)

        result <- do.call(operator, list(param.combos[,distribution.name.1], param.combos[,distribution.name.2]))

        param.combos <- param.combos[which(result),]
    }
    param.combos
}

select.samples <- function(nsamples, param.combos)
{
    nsamples <- min(nsamples, nrow(param.combos))

    param.combos <- param.combos[sample(nrow(param.combos), size=nsamples),,drop=FALSE]
    
    if(NCOL(param.combos) == 1)
        param.combos <- param.combos[order(param.combos),,drop=FALSE]
    else
        param.combos <- param.combos[with(param.combos,order(param.combos[,1],param.combos[,2])),]

    param.combos
}

#' insert a specific parameter combo into a strategy object
#' 
#' In order to test \code{\link{applyStrategy}} with a specific parameter 
#' combination, it is necessary to insert those parameters into a copy of the 
#' strategy specification object.
#' 
#' This internal, non-exported function examines the paramset specification, 
#' and then uses that to insert the chosen parameters into a copy of the strategy
#' object.  It will search the strategy object, component by component, and 
#' attempt to locate the components named in the paramset, so that the individual 
#' parameter values may be changed.
#'
#' @param strategy strategy object
#' @param param.combo single parameter combination to be inserted
#' @param paramset.label label for the paramset to use to determine slot locations inside the strategy object
install.param.combo <- function(strategy, param.combo, paramset.label)
{
    if (is.null(dim(param.combo))) {
        stop("'param.combo' must have a dim attribute")
    }
  
    if (nrow(param.combo)>1) {
      # choose the last row because expand.grid in paramsets will generally make 
      # the last row the row with the largest parameter values, roughly 
      # equivalent to highest stability of data usage, 
      # or lowest degrees of freedom
      param.combo <- param.combo[nrow(param.combo),] 
      warning('"param.combo" passed to "install.param.combo" has more than one row, was this intentional?')
    }

    for(param.label in colnames(param.combo))
    {
        distribution <- strategy$paramsets[[paramset.label]]$distributions[[param.label]]

        component.type <- distribution$component.type
        component.label <- distribution$component.label

        found <- FALSE
        switch(component.type,
            indicator =,
            signal =
            {
                # indicator and signal slots in strategy list use plural name for some reason:
                ctype <- paste(component.type,'s',sep='') 

                n <- length(strategy[[ctype]])

                for(index in 1:n)
                {
                  for(c in 1:length(component.label)){
                    if(strategy[[ctype]][[index]]$label == component.label[c])
                    {
                      strategy[[ctype]][[index]]$arguments[[distribution$variable[c]]] <- param.combo[,param.label]
                      
                      found <- TRUE
                    }
                  }
                }
            },
            order =,
            enter =,
            exit =,
            chain =
            {
                n <- length(strategy$rules[[component.type]])

                for(index in 1:n)
                {
                  for(c in 1:length(component.label)){
                    if(strategy$rules[[component.type]][[index]]$label == component.label[c])
                    {
                        variable.name <- distribution$variable[c]
                        if(variable.name %in% c('timespan'))
                            strategy$rules[[component.type]][[index]][[variable.name[c]]] <- as.character(param.combo[,param.label])
                        else
                            strategy$rules[[component.type]][[index]]$arguments[[variable.name[c]]] <- param.combo[,param.label]

                        found <- TRUE
                    }
                  }
                }
            }
        )
        if(!found) stop(paste(component.label, ': no such ', component.type, ' rule in strategy ', strategy$name, sep=''))
    }
    return(strategy)
}

### exported functions ############################################################

#' Delete a paramset from a strategy
#' 
#' Delete a paramset from a strategy, including its distributions and constraints.
#' 
#' @param strategy the name of the strategy object
#' @param paramset.label a label uniquely identifying the paramset within the strategy
#' @param store indicates whether to store the strategy in the .strategy environment
#'
#' @author Jan Humme
#' @seealso 
#'     \code{\link{add.distribution}}, 
#'     \code{\link{add.distribution.constraint}}, 
#'     \code{\link{apply.paramset}}
#' @export
delete.paramset <- function(strategy, paramset.label, store=TRUE)
{
    must.have.args(match.call(), c('strategy', 'paramset.label'))

    if(!is.strategy(strategy))
    {
        strategy <- must.be.strategy(strategy)
        store <- TRUE
    }

    if(!is.null(strategy$paramsets[[paramset.label]])) {
        strategy$paramsets[[paramset.label]] <- NULL
    } else {
        warning("strategy ", sQuote(strategy$name), " does not have a paramset ",
                sQuote(paramset.label), " to delete. Aborting.", immediate.=TRUE)
    }

    if(store)
    {
        put.strategy(strategy)
        return(strategy$name)
    }
    return(strategy)
}

#' Adds a distribution to a paramset in a strategy
#' 
#' Creates a distribution in paramset, where a distribution consists of the name of a variable in
#' a strategy component plus a range of values for this variable.
#' 
#' In the original version of this function `variable` was defined as a named 
#' list that contained the distribution directly. e.g.
#' 
#' ` variable = list(mVAR = 1:50)`
#' 
#' In order to more efficiently support equality constraints, we have optionally
#' separated `variable` and `variable.dist`:
#' 
#' `variable = 'mVAR',`
#' `variable.dist = 1:50`
#' 
#' and for an equality-constrained distribution:
#' 
#' `variable=c('mVAR','mOtherVAR'),`
#' `variable.dist=1:50`
#' 
#' The old formulation is still supported for backwards compatibility if 
#' `variable.dist=NULL`, the default.
#' 
#' Variables that should be equal, or equality-constrained, should be defined by
#' creating a vector of the same length for `component.label` and `variable`. 
#' There is currently no error checking for getting this wrong, please be careful, patches welcome.
#' The equal-length vectors will then apply the same distribution to one or more
#' `variable`s in the same `component.type`.  This is far more efficient than 
#' testing the constraints utilizing `apply.constraints`, and ensures the  
#' equality of the parameters.
#' 
#' @param strategy the name of the strategy object to add the distribution to
#' @param paramset.label a label uniquely identifying the paramset within the strategy
#' @param component.type one of c('indicator', 'signal', 'order', 'enter', 'exit', 'chain')
#' @param component.label a label identifying the component. must be unique per component type
#' @param variable the name of the variable in the component, and optionally, it's distribution
#' @param variable.dist the distribution to be applied to variable
#' @param label a label uniquely identifying the distribution within the paramset
#' @param weight vector
#' @param store indicates whether to store the strategy in the .strategy environment
#'
#' @author Jan Humme, Brian Peterson
#' @seealso 
#'     \code{\link{add.distribution.constraint}}, 
#'     \code{\link{delete.paramset}}, 
#'     \code{\link{apply.paramset}}
#' @export
add.distribution <- function(strategy, paramset.label, 
                             component.type, component.label, 
                             variable, variable.dist=NULL, 
                             weight=NULL, 
                             label, 
                             store=TRUE)
{
    must.have.args(match.call(), c('strategy', 'paramset.label', 'component.type', 'component.label', 'variable', 'label'))

    if(!is.strategy(strategy))
    {
        strategy <- must.be.strategy(strategy)
        store <- TRUE
    }
    if(is.null(variable.dist)){
      variable.dist<-variable[[1]]
      variable<-names(variable)[1]
    }

    new_distribution <- list()
    new_distribution$component.type <- component.type
    new_distribution$component.label <- component.label
    new_distribution$variable <- variable
    new_distribution$variable.dist <- coredata(variable.dist)
    new_distribution$weight <- weight

    if(!(paramset.label %in% names(strategy$paramsets)))
        strategy <- create.paramset(strategy, paramset.label)

    if(label %in% names(strategy$paramsets[[paramset.label]]$distributions)) {
        fmt <- paste("add.distribution replacing previously defined",
                     "distribution %s in paramset %s for strategy %s.")
        msg <- sprintf(fmt, sQuote(label), sQuote(paramset.label), sQuote(strategy$name))
        warning(msg, immediate.=TRUE, call.=FALSE)
    }

    strategy$paramsets[[paramset.label]]$distributions[[label]] <- new_distribution

    #increment trials
    strategy$trials <- strategy$trials+1
    
    if(store)
    {
        put.strategy(strategy)
        return(strategy$name)
    }
    return(strategy)
}

#' Adds a constraint on 2 distributions within a paramset
#' 
#' Creates a constraint on 2 distributions in a paramset, i.e. a restriction limiting the allowed
#' combinations from the ranges for distribution 1 and distribution 2.
#' 
#' @param strategy the name of the strategy object to add the constraint to
#' @param paramset.label a label uniquely identifying the paramset within the strategy
#' @param distribution.label.1 a label identifying the first distribution
#' @param distribution.label.2 a label identifying the second distribution
#' @param operator an operator specifying the relational constraint between the 2 distributions
#' @param label a label uniquely identifying the constraint within the paramset
#' @param store indicates whether to store the strategy in the .strategy environment
#'
#' @author Jan Humme
#' @seealso 
#'     \code{\link{add.distribution}}, 
#'     \code{\link{delete.paramset}}, 
#'     \code{\link{apply.paramset}}
#' @export
add.distribution.constraint <- function(strategy, paramset.label, distribution.label.1, distribution.label.2, operator, label, store=TRUE)
{
    must.have.args(match.call(), c('strategy', 'paramset.label', 'distribution.label.1', 'distribution.label.2', 'operator', 'label'))

    if(!is.strategy(strategy))
    {
        strategy <- must.be.strategy(strategy)
        store <- TRUE
    }

    new_constraint <- list()
    new_constraint$distributions <- list(distribution.label.1, distribution.label.2)
    new_constraint$operator <- operator

    if(!(paramset.label %in% names(strategy$paramsets)))
        strategy <- create.paramset(strategy, paramset.label)

    if(label %in% names(strategy$paramsets[[paramset.label]]$constraints)) {
        fmt <- paste("add.distribution.constraint replacing previously defined",
                     "constraint %s in paramset %s for strategy %s.")
        msg <- sprintf(fmt, sQuote(label), sQuote(paramset.label), sQuote(strategy$name))
        warning(msg, immediate.=TRUE, call.=FALSE)
    }

    strategy$paramsets[[paramset.label]]$constraints[[label]] <- new_constraint

    #increment trials
    strategy$trials <- strategy$trials+1
    
    if(store)
    {
        put.strategy(strategy)
        return(strategy$name)
    }
    return(strategy)
}

#' Apply a paramset to the strategy
#'
#' This function will run \code{\link{applyStrategy}} on \code{portfolio.st},
#' once for each parameter combination as specified by the parameter
#' distributions and constraints in the paramset. Results are gathered and
#' returned as a list containing a slot for each parameter combination.
#'
#' apply.paramset uses the foreach package to start the runs for each parameter
#' combination, and as such allows for parallel processing. It is up to the
#' caller to load and register an appropriate backend, eg. doMC, doParallel or
#' doRedis.
#' 
#' Note that we will attempt to pass dots through to most other called functions. 
#' This could include arguments such as the \code{tradeDef} argument for
#' \code{\link[blotter]{tradeStats}}, or additional arguments to be passed to 
#' \code{\link{applyStrategy}}
#'  
#' It is also worth discussing the \code{nsamples} argument.  This option will 
#' randomly sample from the total parameter space.  It is quite useful for testing
#' your parameterization distributions.  It is not always terribly useful for 
#' real tests,even for a large parameter space.  Use of this sampling methodology,
#' if there are not enough samples, will make parameter surface analysis
#' challenging, for example, because there may be 'voids' in any randomly chosen
#' parameter space.  Also, if \code{apply.paramset} is called via \code{\link{walk.forward}}
#' with \code{nsamples}, then the sampled parameter sets will be different for
#' each training period, as the sampling methodology is independent.  This 
#' latter issue could be addressed by passing \code{paramsets} instead, so if 
#' you must use a parameter subset (e.g. one generated via some optimization 
#' algorithm, or to use a constand sample for all training periods), then
#' passing \code{paramsets} should be preferred to passing \code{nsamples}.
#' 
#' @param strategy.st the name of the strategy object
#' @param paramset.label a label uniquely identifying the paramset within the strategy
#' @param portfolio.st the name of the portfolio
#' @param account.st the name of the account
#' @param mktdata optional xts mktdata object, will be passed unchanged to applyStrategy
#' @param nsamples if > 0 then take a sample of only size nsamples from the paramset
#' @param user.func an optional user-supplied function to be run for each param.combo at the end, either on the slave or on the master (see calc)
#' @param user.args user-supplied list of arguments for user.func
#' @param calc 'slave' to run \code{\link[blotter]{updatePortf}} and \code{\link[blotter]{tradeStats}} on the slave and return all portfolios and orderbooks as a list: higher parallelization but more data transfer between master and slave; 'master' to have \code{\link[blotter]{updatePortf}} and \code{\link[blotter]{tradeStats}} run at the master and return all portfolios and orderbooks in the .blotter and .strategy environments resp: less parallelization but also less data transfer between slave and master; default is 'slave'
#' @param packages a vector specifying names of R packages to be loaded by the slave, default NULL
#' @param audit a user-specified environment to store a copy of all portfolios, orderbooks and other data from the tests, or NULL to trash this information
#' @param verbose return full information, in particular the .blotter environment, default FALSE
#' @param paramsets a user-sepcified (sub)set of paramsets to run
#' @param ... any other passthru parameters
#' @param rule.subset ISO-8601 subset for period to execute rules over, default NULL (will use all dates)
#' @param perf.subset ISO-8601 subset for period to examine performance over, default NULL (will use all dates)
#' @param store indicates whether to store the strategy in the .strategy environment
#' @param psgc boolean, if TRUE, the default, will force \code{\link[base]{gc}} garbage collection periodically in workers to conserve RAM
#' @author Jan Humme, Brian Peterson
#' @seealso 
#'     \code{\link{add.distribution}}, 
#'     \code{\link{add.distribution.constraint}}, 
#'     \code{\link{delete.paramset}}
#' @importFrom iterators iter
#' @export
apply.paramset <- function(strategy.st
                           , paramset.label
                           , portfolio.st
                           , ...
                           , account.st
                           , mktdata=NULL
                           , nsamples=0
                           , user.func=NULL
                           , user.args=NULL
                           , calc='slave'
                           , audit=NULL
                           , packages=NULL
                           , verbose=FALSE
                           , paramsets
                           , rule.subset=NULL
                           , perf.subset=NULL
                           , psgc=TRUE
                           , store=TRUE
                           )
{
    must.have.args(match.call(), c('strategy.st', 'paramset.label', 'portfolio.st'))

    strategy <- must.be.strategy(strategy.st)
    must.be.paramset(strategy, paramset.label)

    if(!is.null(audit)) must.be.environment(audit)

    portfolio <- .getPortfolio(portfolio.st)
    account <- getAccount(account.st)
    orderbook <- getOrderBook(portfolio.st)

    distributions <- strategy$paramsets[[paramset.label]]$distributions
    constraints <- strategy$paramsets[[paramset.label]]$constraints

    if(missing(paramsets))
    {
        param.combos <- expand.distributions(distributions)
        param.combos <- apply.constraints(constraints, distributions, param.combos)
        rownames(param.combos) <- NULL  # reset rownames
        if(nsamples > 0)
            param.combos <- select.samples(nsamples, param.combos)
    } else {
        param.combos <- paramsets
    }
    
    #increment trials
    strategy$trials <- strategy$trials+nrow(param.combos)
    if(store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    
    # This is work-around for a buglet in iterators:::getIterVal.dataframeiter
    # Convert param.combos to matrix if it's only one column, else the
    # iterator will drop the data.frame dimensions, resulting in a vector
    if(ncol(param.combos) == 1) {
        param.combos <- as.matrix(param.combos)
        rownames(param.combos) <- seq_len(nrow(param.combos))
    }

    env.functions <- c('clone.portfolio', 'clone.orderbook', 'install.param.combo')
    env.instrument <- as.list(FinancialInstrument:::.instrument)
    symbols <- names(getPortfolio(portfolio.st)$symbols)

#    if(is.null(audit))
#        .audit <- new.env()
#    else
#        .audit <- audit

    if(!is.null(audit)) .audit <- audit
    
    combine.results <- function(...)
    {
        args <- list(...)

        results <- new.env(parent=emptyenv(),size=length(args))
        results$error <-list()
        results$cumPL <- xts()
        
        for(i in 1:length(args))
        {
            r <- args[[i]]
            
            #check for error
            if(class(r)=='try-error' || any(class(r)=='error')){
              results$error[[i]]<-r
            } else { #process normally
              if(!is.null(audit)){
                # move portfolio from slave returned list into .blotter environment
                put.portfolio(r$portfolio.st, r$portfolio, envir=.audit)
                #r$portfolio <- NULL
                
                # move orderbook from slave returned list into .strategy environment
                put.orderbook(r$portfolio.st, r$orderbook, envir=.audit)
                #r$orderbook <- NULL
              }
              
              if(calc == 'master' || is.null(r$tradeStats) )
              {
                # calculate tradeStats on portfolio
                updatePortf(r$portfolio.st, Symbols = symbols, ...)
                r$tradeStats <- tradeStats(r$portfolio.st,Dates = perf.subset, ...)

                r$cumPL <- cumsum(r$portfolio.st[,'Net.Trading.PL'])
                if(!is.null(perf.subset)) r$cumPL <- r$cumPL[perf.subset]
                colnames(r$cumPL) <- portfolio.st

                # run user specified function, if they provided one
                if(!is.null(user.func) && !is.null(user.args))
                  r$user.func <- do.call(user.func, user.args)
              }
              
              results[[r$portfolio.st]] <- r
              
              # add copy of tradeStats to summary list for convenience
              if(!is.null(r$tradeStats) ){
                if(nrow(r$tradeStats)==0){
                  tmpnames <- colnames(r$tradeStats)
                  if(nrow(r$tradeStats)==0) { # no trades returned in this param.combo
                    print(paste0("No transactions returned for param.combo ", i, " out of ", length(args)))
                    next # jump to next param.combo
                  }
                  r$tradeStats <- data.frame(r$portfolio.st,t(rep(0,length(tmpnames)-1)))
                  colnames(r$tradeStats) <- tmpnames
                }
                if(is.null(results$tradeStats)){
                  results$tradeStats <- cbind(r$param.combo, r$tradeStats)
                } else {
                  results$tradeStats <- rbind(results$tradeStats, cbind(r$param.combo, r$tradeStats))
                }
              }

              # add copy of dailyStats to summary list for convenience
              if(!is.null(r$dailyStats) ){
                if(nrow(r$dailyStats)==0){
                  tmpnames <- colnames(r$dailyStats)
                  r$dailyStats <- data.frame(r$portfolio.st,t(rep(0,length(tmpnames)-1)))
                  colnames(r$dailyStats) <- tmpnames
                }
                if(is.null(results$dailyStats)){
                  results$dailyStats <- cbind(r$param.combo, r$dailyStats)
                } else {
                  results$dailyStats <- rbind(results$dailyStats, cbind(r$param.combo, r$dailyStats))
                }
              }
              
              if(!is.null(r$cumPL)){
                results$cumPL <- cbind(results$cumPL,r$cumPL)
              }
              
              # add copy of user.func results to summary list for convenience
              if(!is.null(r$user.func)){
                results$user.func <- rbind(results$user.func, cbind(r$param.combo, r$user.func))
              }
            } #end non-error results block
        } # end loop over data returned by foreach
        return(results)
    } # end fn combine.results

    # create foreach object
    fe <- foreach( param.combo=iter(param.combos,by='row')
                  ,.verbose=verbose, .errorhandling='pass'
                  ,.packages=c('quantstrat', packages)
                  ,.combine=combine.results
                  , .multicombine=TRUE
                  , .maxcombine=max(2,nrow(param.combos))
                  ,.export=c(env.functions, symbols)
                  , ...
                 )
    # remove all but the param.combo iterator before calling %dopar%
    # this allows us to pass '...' through foreach to the expression
    fe$args <- fe$args[1]
    fe$argnames <- fe$argnames[1]
    # now call %dopar%
    results <- fe %dopar%
    {
        if(psgc) gc()
      
        param.combo.num <- rownames(param.combo)
        #print(paste("Processing param.combo", param.combo.num))
        #print(param.combo)

        # doSEQ and doMC make all environments available to the slave, but
        # doRedis only provides the .GlobalEnv, so we erase both .blotter
        # and .strategy environments to make sure that envs are clean
        # regardless of backend
        #
        # also, environments persist in each slave, so data may be accumulating
        # for each transition through the foreach loop
        #
        if(!getDoSeqRegistered())
        {
            rm(list=ls(pos=.blotter), pos=.blotter)
            rm(list=ls(pos=.strategy), pos=.strategy)
        }

        list2env(env.instrument, envir=FinancialInstrument:::.instrument)

        for (sym in symbols)
          assign(sym, get(sym), .GlobalEnv)
        
        put.portfolio(portfolio.st, portfolio)
        put.account(account.st, account)
        put.orderbook(portfolio.st, orderbook)
        put.strategy(strategy)

        result <- new.env()
        result$param.combo <- param.combo
        result$portfolio.st <- paste(portfolio.st, "train", param.combo.num, sep='.')

        clone.portfolio(portfolio.st, result$portfolio.st)
        clone.orderbook(portfolio.st, result$portfolio.st)

        if(exists('redisGetContext'))
        {
            # assume we are using a doRedis parallel backend
            # store the context, and close the connection
            # patch to prevent timeout on large data sets
            #
            # thanks to Kent Hoxsey for this workaround

            redisContext <- redisGetContext()
            redisClose()
        }
        
        strategy <- install.param.combo(strategy, param.combo, paramset.label)
        applyStrategy(strategy
                      , portfolios=result$portfolio.st
                      , mktdata=mktdata
                      , rule.subset=rule.subset
                      , ...)

        if(exists('redisContext'))
        {
            # assume redisContext contains preserved context
            # restore doRedis connection
            #
            # thanks to Kent Hoxsey for this workaround

            redisConnect(host=redisContext$host)
        }

        if(calc == 'slave'  || calc == 'worker')
        {
            updatePortf(result$portfolio.st, ...)
            result$tradeStats <- tradeStats(result$portfolio.st, Dates=perf.subset, ...)
            result$dailyStats <- dailyStats(result$portfolio.st, Dates=perf.subset, perSymbol = FALSE, method='moment', ...)
            
            result$cumPL <- cumsum(.getPortfolio(result$portfolio.st)[['summary']][,'Net.Trading.PL'])
            if(!is.null(perf.subset)) result$cumPL <- result$cumPL[perf.subset]
            colnames(result$cumPL) <- portfolio.st
            
            if(!is.null(user.func) && !is.null(user.args))
                result$user.func <- do.call(user.func, user.args)
            
            if(is.null(audit)){
              # clean up portfolio and orderbook on worker processes
              #rm(list=c(paste('portfolio', portfolio.st, sep='.'), paste('account', portfolio.st, sep='.')),envir=.blotter)
              #rm(list=c(paste('order_book', portfolio.st, sep='.')),envir=.strategy)
            } else {
              result$portfolio <- getPortfolio(result$portfolio.st)
              result$orderbook <- getOrderBook(result$portfolio.st)
            }
            
        } else {
          result$portfolio <- getPortfolio(result$portfolio.st)
          result$orderbook <- getOrderBook(result$portfolio.st)
        }

        # portfolio name has param.combo rowname in suffix, so
        # print param.combo number for diagnostics
        # print(paste("Returning results for param.combo", param.combo.num))

        return(result)
    }

    #make sure we preserve the param combo name in cumPL
    if(!is.null(results$tradeStats)){
      if(nrow(results$tradeStats)>0 && nrow(results$tradeStats)==ncol(results$cumPL)){
         colnames(results$cumPL) <- rownames(results$tradeStats)
      }
    }
    
    if(is.null(audit) && calc=='master'){
      .audit <- .blotter
    } else if(!is.null(audit)) {
      assign('distributions', distributions, envir=.audit)
      assign('constraints', constraints, envir=.audit)
      assign('paramset.label', paramset.label, envir=.audit)
      assign('param.combos', param.combos, envir=.audit)
      assign('tradeStats', results$tradeStats, envir=.audit)
      assign('dailyStats', results$dailyStats, envir=.audit)
      assign('cumPL',results$cumPL, envir=.audit)
      assign('user.func', results$user.func, envir=.audit)
      assign('foreach.errors', results$error, envir=.audit)
    }

    
    if(psgc) gc()
    return(results)
}
