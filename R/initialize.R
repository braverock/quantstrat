###############################################################################
# 
# File for initialization (and possibly wrap-up) of strategy runs
#
###############################################################################

#' run standard and custom strategy initialization functions 
#' 
#' \code{initStrategy} will run a series of common initialization functions at the 
#' beginning of an \code{\link{applyStrategy}} call.
#' 
#' \describe{
#'      \item{get.Symbols}{if TRUE, will call \code{\link[quantmod]{getSymbols}} 
#'                          on all symbols included in the \code{symbols} vector}
#'      \item{init.Portf}{if TRUE, will call \code{\link[blotter]{initPortf}} 
#'                          to initialize the portfolio object}
#'      \item{init.Acct}{if TRUE, will call \code{\link[blotter]{initAcct}} 
#'                          to initialize the account object}
#'      \item{init.Orders}{if TRUE, will call \code{\link{initOrders}} 
#'                          to initialize the order book for this test}
#'      \item{unique}{not yet implemented, will force a unique portfolio and account name 
#'                          if the portfolio, account, or order book already exist}
#' }
#'
#' If used in conjuction with \code{initBySymbol}, \code{get.Symbols} should be \code{FALSE}.
#' 
#' @param strategy object of type \code{strategy} to initialize data/containers for
#' @param portfolio portfolio
#' @param symbols symbols
#' @param parameters named list of parameters to be applied during evaluation of the strategy, default NULL
#' @param get.Symbols TRUE/FALSE, default FALSE
#' @param init.Portf TRUE/FALSE, default TRUE 
#' @param init.Acct TRUE/FALSE, default TRUE 
#' @param init.Orders TRUE/FALSE, default TRUE 
#' @param unique TRUE/FALSE, default TRUE
#' @param \dots any other passthrough parameters
#' @author Garrett See, Brian Peterson
#' @export
#' @seealso \code{\link{applyStrategy}}, \code{\link{add.init}},  
initStrategy <- function(strategy,
                         portfolio,
                         symbols,
                         parameters   = NULL,
                         get.Symbols  = FALSE,
                         init.Portf   = TRUE,
                         init.Acct    = TRUE,
                         init.Orders  = TRUE,
                         unique       = TRUE,
                         ...) {
    # basic idea is to do all the common set-up stuff
    # create portfolio, account, orderbook

    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object or the name of an object of type 'strategy'.")
        store=TRUE    
    } 

    #set default values that will break the initialization
    if(!hasArg(currency)){
        if(!is.null(strategy$currency)) currency <- strategy$currency
        else currency<-'USD'
    } 
        
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
        if (!is.null(getsyms)) getSymbols(getsyms,from=initDate, ...=...) #get the data that didn't exist in env
    }
    
    if(isTRUE(init.Portf) & !isTRUE(is.portfolio(portfolio))){
        if(hasArg(portfolio)) portfolio<-portfolio else portfolio<-strategy$name 
        
        #TODO FIXME implment unique here
        
        initPortf(name=portfolio, symbols=symbols, currency=currency, ...=...)
    }
    
    if(isTRUE(init.Acct)){
        if(hasArg(account)) account<-account else account<-portfolio
        if(!isTRUE(is.account(account))) initAcct(name=account, portfolios=portfolio, currency=currency, ...=...)
    }
    
    if(isTRUE(init.Orders)){
        initOrders(portfolio=portfolio, symbols=symbols, ...=...)
    }

    # arbitrary user-defined initialization functions added to the initialization steps    
    # now do whatever else the user stuck in this init slot...
    for (init_o in strategy$init){
        if(is.function(init_o$name)) {
            init_oFun <- init_o$name
        } else {
            if(exists(init_o$name, mode="function")) {
                init_oFun <- get(init_o$name, mode="function")
            } else {
                message("Skipping initialization function ", init_o$name,
                        " because there is no function by that name to call.")
            }
        }

        if(!isTRUE(init_o$enabled)) next()
        
        # replace default function arguments with init_o$arguments
        .formals <- formals(init_o$name)
        .formals <- modify.args(.formals, init_o$arguments, dots=TRUE)
        # now add arguments from parameters
        .formals <- modify.args(.formals, parameters, dots=TRUE)
        # now add dots
        .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
        # remove ... to avoid matching multiple args
        .formals$`...` <- NULL
        
        do.call(init_oFun, .formals)
    }
}

#' add arbitrary initialization functions to a strategy
#' 
#' \code{\link{initStrategy}} will run a series of common initialization functions at the 
#' beginning of an \code{\link{applyStrategy}} call.  This function allows the user to 
#' add arbitrary initialization functions to the sequence.
#' 
#' These arbitrary functions will be added to the \code{init} slot of the strategy object
#' and when \code{applyStrategy} is evaluated, the arbitrary initialization functions will
#' be evaluated after the standardized functions.
#' 
#' For example, if your strategy uses a synthetic basket instrument, you could use this 
#' initialization slot to add a custom constructor to build the basket instrument time 
#' series and modify the symbols slot(s) of the strategy and portfolio.
#' 
#' @param strategy an object (or the name of an object) of type 'strategy' to add the init function definition to
#' @param name name of the init, must correspond to an R function
#' @param arguments named list of default arguments to be passed to an init function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition,default NULL, only needed if you need special names to avoid argument collision
#' @param label arbitrary text label for init output, default NULL
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the init is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific init, the index number in the $init list to update
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @return if \code{strategy} was the name of a strategy, the name. It it was a strategy, the updated strategy. 
#' @export
add.init <- function(strategy, name, arguments, parameters=NULL, label=NULL, ..., enabled=TRUE, indexnum=NULL, store=FALSE) {
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object or the name of an object of type 'strategy'.")
        store=TRUE    
    } 
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

#' Run standard and custom symbol initialization functions
#'
#' \code{initSymbol} will load a symbol and run user-defined functions to pre-process the symbol's data
#' before constructing indicators.
#'
#' The custom initialization must be defined as named list containing
#' \describe{
#'   \item{name}{function name}
#'   \item{argument}{list of arguments}
#'   \item{enabled}{TRUE or FALSE}
#' }
#' and included as the slot \code{init_symbol} of the strategy object. 
#' 
#' @param strategy an object (or the name of an object) of type 'strategy' to add the init function definition to
#' @param symbol   symbol
#' @param \dots any other passthrough parameters

#' @export
initSymbol <- function(strategy, symbol, ...){
    getSymbols(symbol, env = .GlobalEnv)

    ## run user-defined initialization function contained in the strategy slot init_symbol
    init_s <- strategy$init_symbol

    if(is.function(init_s$name)) {
        init_sFun <- init_s$name
    } else {
        if(exists(init_s$name, mode="function")) {
            init_sFun <- get(init_s$name, mode="function")
        } else {
            message("Initialization function ", init_s$name, " not found. Skipping")
            return()
        }
    }

    if(!isTRUE(init_s$enabled)) return()

    ## (from initStrategy)
    ## replace default function arguments with init_s$arguments
    .formals <- formals(init_s$name)
    .formals <- modify.args(.formals, init_s$arguments, dots=TRUE)
    ## now add dots
    .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
    ## remove ... to avoid matching multiple args
    .formals$`...` <- NULL
    
    do.call(init_sFun, .formals)
}



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
