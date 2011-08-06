###############################################################################
# 
# File for initialization (and possibly wrap-up) of strategy runs
#
###############################################################################

initStrategy <- function(strategy=NULL, portfolio=NULL, symbols=NULL, getSymbols=TRUE, initPortf=TRUE, initAcct=TRUE, initOrders=TRUE, unique=TRUE) {
    # basic idea is to do all the common set-up stuff
    # create portfolio, account, orderbook
    
    # additionally, we should put an initialization slot in the strategy and have 
    # an add.init function (like add.indicator, etc) that could have 
    # arbitrary user-defined initialization functions added to the initialization steps
    
}

add.init <- function(strategy, name, arguments, parameters=NULL, label=NULL, ..., enabled=TRUE, indexnum=NULL, store=FALSE) {
    if(!is.strategy(strategy)) stop("You must pass in a strategy object to manipulate")
    tmp_init<-list()
    tmp_init$name<-name
    if(is.null(label)) label = paste(name,"ind",sep='.')
    tmp_init$label<-label
    tmp_init$enabled=enabled
    if (!is.list(arguments)) stop("arguments must be passed as a named list")
    tmp_init$arguments<-arguments
    if(!is.null(parameters)) tmp_init$parameters = parameters
    if(length(list(...))) tmp_init<-c(tmp_init,list(...))
    
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$inits)+1
    tmp_init$call<-match.call()
    class(tmp_init)<-'strat_init'
    
    strategy$inits[[indexnum]]<-tmp_init
    
    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
}

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2011
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
