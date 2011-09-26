###############################################################################
# 
# File for initialization (and possibly wrap-up) of strategy runs
#
###############################################################################

#' run standard and custom strategy initialization functions 
#' @param strategy object of type \code{strategy} to initialize data/containers for
#' @param portfolio 
#' @param symbols 
#' @param get.Symbols TRUE/FALSE, default TRUE: 
#' @param init.Portf TRUE/FALSE, default TRUE: 
#' @param init.Acct TRUE/FALSE, default TRUE: 
#' @param init.Orders TRUE/FALSE, default TRUE: 
#' @param unique TRUE/FALSE, default TRUE: 
#' @param dots any other passtrhrough parameters
#' @author Garrett See, Brian Peterson
#' @export
initStrategy <- function(strategy, portfolio, symbols, get.Symbols=TRUE, init.Portf=TRUE, init.Acct=TRUE, init.Orders=TRUE, unique=TRUE,...) {
    # basic idea is to do all the common set-up stuff
    # create portfolio, account, orderbook
    #if any 'symbols' are not defined as instruments, we'll make a basic instrument
    if(isTRUE(get.Symbols)){
        getsyms <- NULL #symbols that aren't in .GlobalEnv that we'll have to get
        for (sym in symbols) {
            if(!is.instrument(getInstrument(sym,silent=TRUE))) {
                instrument.auto(sym, currency=currency)
            }   
            tmp <- try(get(sym,pos=env),silent=TRUE)
            #test for is.xts here?
            if (inherits(tmp, 'try-error')) getsyms <- c(getsyms, sym)
        }
        if (!is.null(getsyms)) getSymbols(getsyms,from=initDate) #get the data that didn't exist in env
    }
    if(isTRUE(init.Portf)){
        initPortf(name=portfolio, symbols=symbols, currency=currency, ...=...)
    }
    if(isTRUE(init.Acct)){
        if(hasArg(account)) account<-account else account<-portfolio
        initAcct(name=account, portfolios=portfolio, currency=currency, ...=...)
    }
    if(isTRUE(init.Orders)){
        initOrders(portfolio=portfolio, symbols=symbols, ...=...)
    }

    # additionally, we should put an initialization slot in the strategy and have 
    # an add.init function (like add.indicator, etc) that could have 
    # arbitrary user-defined initialization functions added to the initialization steps
    
    #now do whatrever else the user stuck in this init slot...
    for (init_o in strategy$init){
        if(!is.function(get(init_o$name))){
            message(paste("Skipping initialization function",init_o$name,"because there is no function by that name to call"))
            next()      
        }
        
        if(!isTRUE(init_o$enabled)) next()
        
        # see 'S Programming p. 67 for this matching
        fun<-match.fun(init_o$name)
        
        .formals  <- formals(fun)
        onames <- names(.formals)
        
        pm <- pmatch(names(init_o$arguments), onames, nomatch = 0L)
        #if (any(pm == 0L))
        #    warning(paste("some arguments stored for",init_o$name,"do not match"))
        names(init_o$arguments[pm > 0L]) <- onames[pm]
        .formals[pm] <- init_o$arguments[pm > 0L]       
        
        # now add arguments from parameters
        if(length(parameters)){
            pm <- pmatch(names(parameters), onames, nomatch = 0L)
            names(parameters[pm > 0L]) <- onames[pm]
            .formals[pm] <- parameters[pm > 0L]
        }
        
        #now add dots
        if (length(nargs)) {
            pm <- pmatch(names(nargs), onames, nomatch = 0L)
            names(nargs[pm > 0L]) <- onames[pm]
            .formals[pm] <- nargs[pm > 0L]
        }
        .formals$... <- NULL
        
        do.call(fun,.formals)
    }            
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
    
    strategy$init[[indexnum]]<-tmp_init
    
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
