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
# Authors: Yu Chen, Jan Humme
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

# creates a copy of a portfolio, stripping all history (transactions etc)

clone.portfolio <- function(portfolio.st, cloned.portfolio.st, strip.history=TRUE)
{
    #must.have.args(match.call(), c('portfolio.st', 'cloned.portfolio.st'))

    portfolio <- getPortfolio(portfolio.st)

    if(strip.history==TRUE)
    {
        for(symbol in names(portfolio$symbols))
        {
            portfolio$symbols[[symbol]]$txn <- portfolio$symbols[[symbol]]$txn[1,]

            xts.tables <- grep('(^posPL|txn)',names(portfolio$symbols[[symbol]]))
            for(xts.table in xts.tables)
                portfolio$symbols[[symbol]][[xts.table]] <- portfolio$symbols[[symbol]][[xts.table]][1,]
        }
        portfolio$summary <- portfolio$summary[1,]
    }
    assign(paste("portfolio", as.character(cloned.portfolio.st), sep='.'), portfolio, envir=.blotter)

    return(cloned.portfolio.st)
}

# creates a copy of an orderbook, stripping all orders

clone.orderbook <- function(portfolio.st, cloned.portfolio.st, strip.history=TRUE)
{
    #must.have.args(match.call(), c('portfolio.st', 'cloned.portfolio.st'))

    order.book <- getOrderBook(portfolio.st)

    i <- 1  # TODO: find index number by name
    names(order.book)[i] <- cloned.portfolio.st

    if(strip.history == TRUE)
    {
        for(symbol in names(order.book[[portfolio.st]]))
            order.book[[portfolio.st]][[symbol]] <- order.book[[portfolio.st]][[symbol]][1,]
    }

    assign(paste("order_book", cloned.portfolio.st, sep='.'), order.book, envir=.strategy)
}

### local functions ############################################################

must.be.paramset <- function(strategy, paramset)
{
    if(!(paramset %in% names(strategy$paramsets)))
        stop(paste(paramset, ': not a paramset in strategy', strategy$name))
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

    param.combos <- param.combos[sample(nrow(param.combos), size=nsamples),]
    param.combos <- param.combos[with(param.combos,order(param.combos[,1],param.combos[,2])),]

    param.combos
}

install.param.combo <- function(strategy, param.combo, paramset.label)
{
    for(param.label in names(param.combo))
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
                        strategy[[components.type]][[component.label]]$arguments[[variable.name]] <- param.combo[[param.label]]
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
                        strategy$rules[[component.type]][[index]]$arguments[[variable.name]] <- param.combo[[param.label]]
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
#'
#' @author Jan Humme
#' @export
#' @seealso \code{\link{add.constraint}}, \code{\link{add.constraint}}, \code{\link{apply.paramset}}

delete.paramset <- function(strategy, paramset.label, store=TRUE)
{
    must.have.args(match.call(), c('strategy', 'paramset.label'))

    if(!is.strategy(strategy))
    {
        strategy <- must.be.strategy(strategy)
        store <- TRUE
    }

    if(!is.null(strategy$paramsets[[paramset.label]]))
        strategy$paramsets[[paramset.label]] <- NULL

    if(store)
    {
        store.strategy(strategy)
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
#'
#' @author Jan Humme
#' @export
#' @seealso \code{\link{add.constraint}}, \code{\link{delete.paramset}}, \code{\link{apply.paramset}}

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

    strategy$paramsets[[paramset.label]]$distributions[[label]] <- new_distribution

    if(store)
    {
        store.strategy(strategy)
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
#'
#' @author Jan Humme
#' @export
#' @seealso \code{\link{add.distribution}}, \code{\link{delete.paramset}}, \code{\link{apply.paramset}}

add.constraint <- function(strategy, paramset.label, distribution.label.1, distribution.label.2, operator, label, store=TRUE)
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

    strategy$paramsets[[paramset.label]]$constraints[[label]] <- new_constraint

    if(store)
    {
        store.strategy(strategy)
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
#' @param strategy the name of the strategy object
#' @param paramset.label a label uniquely identifying the paramset within the strategy
#' @param portfolio.st a string variable
#' @param nsamples if > 0 then take a sample of only size nsamples from the paramset
#' @param verbose return full information, in particular the .blotter environment, default FALSE
#'
#' @author Jan Humme
#' @export
#' @seealso \code{\link{add.constraint}}, \code{\link{add.constraint}}, \code{\link{delete.paramset}}

apply.paramset <- function(strategy.st, paramset.label, portfolio.st, nsamples=0, verbose=FALSE)
{
    require(foreach, quietly=TRUE)
    require(iterators, quietly=TRUE)

    must.have.args(match.call(), c('strategy.st', 'paramset.label', 'portfolio.st'))

    strategy <- must.be.strategy(strategy.st)
    must.be.paramset(strategy, paramset.label)

    portfolio <- getPortfolio(portfolio.st)
    symbols <- names(portfolio$symbols)

    distributions <- strategy$paramsets[[paramset.label]]$distributions
    constraints <- strategy$paramsets[[paramset.label]]$constraints

    param.combos <- expand.distributions(distributions)
    param.combos <- apply.constraints(constraints, distributions, param.combos)
    if(nsamples > 0)
        param.combos <- select.samples(nsamples, param.combos)

    env.functions <- c('clone.portfolio', 'clone.orderbook', 'install.param.combo')
    env.blotter <- as.list(.blotter)
    env.instrument <- as.list(FinancialInstrument:::.instrument)
    env.strategy <- as.list(.strategy)

    symbol.list <- as.list(.getSymbols)
    symbol.names <- names(.getSymbols)

    results <- foreach(param.combo=iter(param.combos,by='row'), .packages='quantstrat',
        .export=c(env.functions, 'env.blotter', 'env.instrument', 'env.strategy', 'symbol.list', symbol.names)) %dopar%
    {
        if(verbose) print(param.combo)

        # environment data accumulate with each transition through the foreach loop, so must be cleaned
        rm(list=ls(pos=.blotter), pos=.blotter)
        rm(list=ls(pos=.strategy), pos=.strategy)
        rm(list=ls(pos=FinancialInstrument:::.instrument), pos=FinancialInstrument:::.instrument)

        gc(verbose=verbose)

        .getSymbols <- as.environment(symbol.list)
        for(symbol in symbol.names)
            assign(symbol, eval(as.name(symbol)), .GlobalEnv)

        list2env(env.blotter, envir=.blotter)
        list2env(env.instrument, envir=FinancialInstrument:::.instrument)
        list2env(env.strategy, envir=.strategy)

        result <- list()
        result$param.combo <- param.combo
        result$portfolio.st <- paste(portfolio.st, '.', rownames(param.combo), sep='')

        clone.portfolio(portfolio.st, result$portfolio.st)
        clone.orderbook(portfolio.st, result$portfolio.st)

        strategy <- install.param.combo(strategy, param.combo, paramset.label)

        applyStrategy(strategy, portfolios=result$portfolio.st, verbose=TRUE)
        updatePortf(result$portfolio.st, Dates=paste('::',as.Date(Sys.time()),sep=''))

        result$tradeStats <- tradeStats(result$portfolio.st)

        if(verbose) result$blotter <- as.list(.blotter)

        return(result)
    }

    for(result in results)
    {
        results$tradeStats <- rbind(results$tradeStats, cbind(result$param.combo, result$tradeStats))
    }

    if(verbose)
    {
        results$distributions <- distributions
        results$constraints <- constraints
    }
    return(results)
}

