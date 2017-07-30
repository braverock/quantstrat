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
# Authors: Jan Humme, Brian Peterson
#
# This code is a based on earlier work by Yu Chen
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

# creates a copy of a portfolio, stripping all history (transactions etc)

clone.portfolio <- function(portfolio.st, cloned.portfolio.st, strip.history=TRUE)
{
    #must.have.args(match.call(), c('portfolio.st', 'cloned.portfolio.st'))

    portfolio <- .getPortfolio(portfolio.st)

    if(strip.history==TRUE)
    {
        for(symbol in ls(portfolio$symbols))
        {
            portfolio$symbols[[symbol]]$txn <- portfolio$symbols[[symbol]]$txn[1,]

            xts.tables <- grep('(^posPL|txn)',names(portfolio$symbols[[symbol]]), value=TRUE)
            for(xts.table in xts.tables)
                portfolio$symbols[[symbol]][[xts.table]] <- portfolio$symbols[[symbol]][[xts.table]][1,]
        }
        portfolio$summary <- portfolio$summary[1,]
    }
    put.portfolio(as.character(cloned.portfolio.st), portfolio)

    return(cloned.portfolio.st)
}

# creates a copy of an orderbook, stripping all orders

clone.orderbook <- function(portfolio.st, cloned.portfolio.st, strip.history=TRUE)
{
    #must.have.args(match.call(), c('portfolio.st', 'cloned.portfolio.st'))

    orderbook <- getOrderBook(portfolio.st)

    i <- 1  # TODO: find index number by name
    names(orderbook)[i] <- cloned.portfolio.st

    if(strip.history == TRUE)
    {
        for(symbol in names(orderbook[[portfolio.st]]))
            orderbook[[portfolio.st]][[symbol]] <- orderbook[[portfolio.st]][[symbol]][1,]
    }

    put.orderbook(cloned.portfolio.st, orderbook)
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

expand.distributions <- function(distributions)
{
    param.values <- list()

    for(distribution.name in names(distributions))
    {
        variable.name <- names(distributions[[distribution.name]]$variable)

        param.values[[distribution.name]] <-
            distributions[[distribution.name]]$variable[[variable.name]]
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

install.param.combo <- function(strategy, param.combo, paramset.label)
{
    if (is.null(dim(param.combo))) {
        stop("'param.combo' must have a dim attribute")
    }

    for(param.label in colnames(param.combo))
    {
        distribution <- strategy$paramsets[[paramset.label]]$distributions[[param.label]]

        component.type <- distribution$component.type
        component.label <- distribution$component.label
        variable.name <- names(distribution$variable)

        found <- FALSE
        switch(component.type,
            indicator =,
            signal =
            {
                # indicator and signal slots in strategy list use plural name for some reason:
                components.type <- paste(component.type,'s',sep='') 

                n <- length(strategy[[components.type]])

                for(index in 1:n)
                {
                    if(strategy[[components.type]][[index]]$label == component.label)
                    {
                        strategy[[components.type]][[index]]$arguments[[variable.name]] <- param.combo[,param.label]

                        found <- TRUE
                        break
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
                    if(strategy$rules[[component.type]][[index]]$label == component.label)
                    {
                        if(variable.name %in% c('timespan'))
                            strategy$rules[[component.type]][[index]][[variable.name]] <- as.character(param.combo[,param.label])
                        else
                            strategy$rules[[component.type]][[index]]$arguments[[variable.name]] <- param.combo[,param.label]

                        found <- TRUE
                        break
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
#' @param strategy the name of the strategy object to add the distribution to
#' @param paramset.label a label uniquely identifying the paramset within the strategy
#' @param component.type one of c('indicator', 'signal', 'order', 'enter', 'exit', 'chain')
#' @param component.label a label identifying the component. must be unique per component type
#' @param variable the name of the variable in the component
#' @param label a label uniquely identifying the distribution within the paramset
#' @param weight vector
#' @param store indicates whether to store the strategy in the .strategy environment
#'
#' @author Jan Humme
#' @seealso 
#'     \code{\link{add.distribution.constraint}}, 
#'     \code{\link{delete.paramset}}, 
#'     \code{\link{apply.paramset}}
#' @export
add.distribution <- function(strategy, paramset.label, component.type, component.label, variable, weight=NULL, label, store=TRUE)
{
    must.have.args(match.call(), c('strategy', 'paramset.label', 'component.type', 'component.label', 'variable', 'label'))

    if(!is.strategy(strategy))
    {
        strategy <- must.be.strategy(strategy)
        store <- TRUE
    }

    new_distribution <- list()
    new_distribution$component.type <- component.type
    new_distribution$component.label <- component.label
    new_distribution$variable <- variable
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
#' This function will run applyStrategy() on portfolio.st, once for each parameter combination as specified by
#' the parameter distributions and constraints in the paramset. Results are gathered and returned as a list
#' containing a slot for each parameter combination.
#'
#' apply.paramset uses the foreach package to start the runs for each parameter combination, and as such allows
#' for parallel processing. It is up to the caller to load and register an appropriate backend, eg. doMC,
#' doParallel or doRedis.
#' 
#' @param strategy.st the name of the strategy object
#' @param paramset.label a label uniquely identifying the paramset within the strategy
#' @param portfolio.st the name of the portfolio
#' @param account.st the name of the account
#' @param mktdata optional xts mktdata object, will be passed unchanged to applyStrategy
#' @param nsamples if > 0 then take a sample of only size nsamples from the paramset
#' @param user.func an optional user-supplied function to be run for each param.combo at the end, either on the slave or on the master (see calc)
#' @param user.args user-supplied list of arguments for user.func
#' @param calc 'slave' to run updatePortfolio() and tradesStats() on the slave and return all portfolios and orderbooks as a list: higher parallelization but more data transfer between master and slave; 'master' to have updatePortf() and tradeStats() run at the master and return all portfolios and orderbooks in the .blotter and .strategy environments resp: less parallelization but also less data transfer between slave and master; default is 'slave'
#' @param packages a vector specifying names of R packages to be loaded by the slave, default NULL
#' @param audit a user-specified environment to store a copy of all portfolios, orderbooks and other data from the tests, or NULL to trash this information
#' @param verbose return full information, in particular the .blotter environment, default FALSE
#' @param paramsets a user-sepcified (sub)set of paramsets to run
#' @param ... any other passthru parameters
#' @param rule.subset ISO-8601 subset for period to execute rules over, default NULL
#' @param store indicates whether to store the strategy in the .strategy environment
#'
#' @author Jan Humme
#' @seealso 
#'     \code{\link{add.distribution.constraint}}, 
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

    if(is.null(audit))
        .audit <- new.env()
    else
        .audit <- audit

    combine.results <- function(...)
    {
        args <- list(...)

        results <- list()
        results$error <-list()
        for(i in 1:length(args))
        {
            r <- args[[i]]
            
            #check for error
            if(class(r)=='try-error' || any(class(r)=='error')){
              results$error[[i]]<-r
            } else { #process normally
              # move portfolio from slave returned list into .blotter environment
              put.portfolio(r$portfolio.st, r$portfolio, envir=.audit)
              #r$portfolio <- NULL
              
              # move orderbook from slave returned list into .strategy environment
              put.orderbook(r$portfolio.st, r$orderbook, envir=.audit)
              #r$orderbook <- NULL
              
              if(calc == 'master' || is.null(r$tradeStats) )
              {
                # calculate tradeStats on portfolio
                updatePortf(r$portfolio.st, Symbols = symbols, ...)
                r$tradeStats <- tradeStats(r$portfolio.st)
                
                # run user specified function, if they provided one
                if(!is.null(user.func) && !is.null(user.args))
                  r$user.func <- do.call(user.func, user.args)
              }
              
              results[[r$portfolio.st]] <- r
              
              # add copy of tradeStats to summary list for convenience
              if(!is.null(r$tradeStats) ){
                if(nrow(r$tradeStats)==0){
                  tmpnames <- colnames(r$tradeStats)
                  r$tradeStats <- data.frame(r$portfolio.st,t(rep(0,length(tmpnames)-1)))
                  colnames(r$tradeStats) <- tmpnames
                }
                results$tradeStats <- rbind(results$tradeStats, cbind(r$param.combo, r$tradeStats))
              }
              
              # add copy of user.func results to summary list for convenience
              if(!is.null(r$user.func)){
                results$user.func <- rbind(results$user.func, cbind(r$param.combo, r$user.func))
              }
            } #end non-error results block
        } # end loop over results
        dummy <- 1
        return(results)
    } # end fn combine.results

    # create foreach object
    fe <- foreach(param.combo=iter(param.combos,by='row'),
        .verbose=verbose, .errorhandling='pass',
        .packages=c('quantstrat', packages),
        .combine=combine.results, .multicombine=TRUE, .maxcombine=max(2,nrow(param.combos)),
        .export=c(env.functions, symbols), ...)
    # remove all but the param.combo iterator before calling %dopar%
    # this allows us to pass '...' through foreach to the expression
    fe$args <- fe$args[1]
    fe$argnames <- fe$argnames[1]
    # now call %dopar%
    results <- fe %dopar%
    {
        param.combo.num <- rownames(param.combo)
        print(paste("Processing param.combo", param.combo.num))
        print(param.combo)

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

        result <- list()
        result$param.combo <- param.combo
        result$portfolio.st <- paste(portfolio.st, param.combo.num, sep='.')

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

        if(calc == 'slave')
        {
            updatePortf(result$portfolio.st, ...)
            result$tradeStats <- tradeStats(result$portfolio.st)

            if(!is.null(user.func) && !is.null(user.args))
                result$user.func <- do.call(user.func, user.args)
        }
        result$portfolio <- getPortfolio(result$portfolio.st)
        result$orderbook <- getOrderBook(result$portfolio.st)

        # portfolio name has param.combo rowname in suffix, so
        # print param.combo number for diagnostics
        print(paste("Returning results for param.combo", param.combo.num))

        return(result)
    }

    #increment trials
    strategy$trials <- strategy$trials+nrow(param.combos)
    if(store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    
    if(is.null(audit) && calc=='master'){
      .audit <- .blotter
    } else {
      assign('distributions', distributions, envir=.audit)
      assign('constraints', constraints, envir=.audit)
      assign('paramset.label', paramset.label, envir=.audit)
      assign('param.combos', param.combos, envir=.audit)
      assign('tradeStats', results$tradeStats, envir=.audit)
      assign('user.func', results$user.func, envir=.audit)
      assign('foreach.errors', results$error, envir=.audit)
    }

    return(results)
}

