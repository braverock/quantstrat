
must.have.args <- function(supplied.args, mandatory.args)
{
    msg <- ': argument(s) missing in call to function '

    missing.args <- NULL

    for(arg in mandatory.args)
    {
        if(length(grep(paste('^',arg,'$',sep=''), names(as.list(supplied.args)))) == 0)
        {
            if(is.null(missing.args))
                missing.args <- arg
            else
                missing.args <- paste(missing.args, ', ', arg)
        }
    }
    if(length(missing.args) > 0)
    {
        funcname <- as.character(sys.call(-1)[[1]])

        stop(paste(missing.args, msg, funcname, sep=''))
    }
}

must.be.environment <- function(e)
{
    if(!is.environment(e))
        stop(paste(e, ': not an environment', sep=''))
}

must.be.strategy <- function(strategy)
{
    if(!is.strategy(strategy))
    {
        strategy<-try(getStrategy(strategy))

        if(inherits(strategy,"try-error"))
            stop(paste(strategy, ': not a strategy'))
    }
    return(strategy)
}

must.be.portfolio <- function(portfolio)
{
    if(!is.portfolio(portfolio))
    {
        portfolio<-try(.getPortfolio(portfolio))

        if(inherits(portfolio,"try-error"))
            stop(paste(portfolio, ': not a portfolio'))
    }
}

modify.args <- function(formals, arglist, ..., dots=FALSE)
{
    # avoid evaluating '...' to make things faster
    dots.names <- eval(substitute(alist(...)))

    if(missing(arglist))
        arglist <- NULL
    arglist <- c(arglist, dots.names)

    # see 'S Programming' p. 67 for this matching

    # nothing to do if arglist is empty; return formals
    if(!length(arglist))
        return(formals)

    argnames <- names(arglist)
    if(!is.list(arglist) && !is.null(argnames) && !any(argnames == ""))
        stop("'arglist' must be a *named* list, with no names == \"\"")

    .formals  <- formals
    onames <- names(.formals)

    pm <- pmatch(argnames, onames, nomatch = 0L)
    #if(any(pm == 0L))
    #    message(paste("some arguments stored for", fun, "do not match"))
    names(arglist[pm > 0L]) <- onames[pm]
    .formals[pm] <- arglist[pm > 0L]

    # include all elements from arglist if function formals contain '...'
    if(dots && !is.null(.formals$...)) {
        dotnames <- names(arglist[pm == 0L])
        .formals[dotnames] <- arglist[dotnames]
        #.formals$... <- NULL  # should we assume we matched them all?
    }

    .formals
}

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
# $Id: rules.R 1452 2013-05-04 22:39:39Z opentrades $
#
###############################################################################
