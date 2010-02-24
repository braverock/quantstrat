#' add a rule to a strategy
#' 
#' Rules will be processed in a very particular manner, so it bears going over.
#' 
#' First, rules are either path dependent or non-path-dependent.  Path dependent rules 
#' will be processed in every time increment for the \code{mktdata} passed into
#' \code{\link{applyStrategy}}.  Non path dependent rules will likely be quite rare in real life, 
#' and will be applied after indicators and signals, and before path-dependent rules are processed.
#' 
#' All rules have a \code{type}.  These may be any of:
#' \itemize{
#'   \item{risk}{ rules that check and react to risk of positions, may stop all other rule execution temporarily or permanently}
#'   \item{order}{ rules for order processing of any open orders at time t, always path-dependent}
#'   \item{rebalance}{ rules executed specifically in a portfolio context, unnecessary in univariate strategies}
#'   \item{exit}{ rules to determine whether to exit a position}
#'   \item{enter}{ rules to determine whether to enter or increase a position}
#' }  
#' The rules will be executed by type, in the order listed above.  
#' Multiple rules of each type may be defined, as with signals and indicators, 
#' they will be executed in order by index number with any other rules sharing the same 
#' type.
#' 
#' The rule execution order was constructed because path-dependent rules may modify   
#' the ability of rules that have not fired yet to be evaluated.  For example, a 
#' risk rule may flatten (close out) an entire position and put new orders 
#' on hold, effectively stopping all further execution of the strategy.  
#' Another example would be a rebalancing rule function that would enter 
#' orders to rebalance the portfolio, and would hold other strategy processing 
#' until the rebalancing period was over.
#' 
#' We anticipate that rules will be the portion of a strategy most likely to 
#' not have suitable template code included with this package, as every strategy 
#' and environment are different, especially in this respect.  
#' We will attempt to provide enough examples and generic rules to give strategy
#' authors a place to start.
#'    
#' @param strategy an object of type 'strategy' to add the rule to
#' @param name name of the rule, must correspond to an R function
#' @param arguments default arguments to be passed to an rule function when executed
#' @param label arbitrary text label for rule output, NULL default will be converted to '<name>.rule'
#' @param type one of "risk","order","rebalance","exit","entry", see Details
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the rule is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific rule, the index number in the $rules[type] list to update
#' @param path.dep TRUE/FALSE whether rule is path dependent, default TRUE, see Details 
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @export
add.rule <- function(strategy, name, arguments, label=NULL, type=c(NULL,"risk","order","rebalance","exit","entry"), ..., enabled=TRUE, indexnum=NULL, path.dep=TRUE, store=FALSE) {
    if(!is.strategy(strategy)) stop("You must pass in a strategy object to manipulate")
    type=type[1]
    if(is.null(type)) stop("You must specify a type")
    tmp_rule<-list()
    tmp_rule$name<-name
    tmp_rule$type<-type
    tmp_rule$enabled<-enabled
    if (!is.list(arguments)) stop("arguments must be passed as a named list")
    if(is.null(label)) label = paste(name,"rule",sep='.')
    tmp_rule$label<-label
    tmp_rule$arguments<-arguments
    tmp_rule$path.dep<-path.dep
    tmp_rule$call<-match.call()
    class(tmp_rule)<-'trade_rule'
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$rules[[type]])+1
    strategy$rules[[type]][[indexnum]]<-tmp_rule
    
    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
}

#' apply the rules in the strategy to arbitrary market data 
#' 
#' In typical usage, this function will be called via \code{\link{applyStrategy}}.  
#' In this mode, this function will be called twice, once with \code{path.dep=FALSE} 
#' and then again in stepping over the time indexes of the mktdata object.
#' 
#' Individual rule functions may need to use <<- to place \code{hold} and \code{holdtill}
#' variables into play.  These would be mosrt likely implemented by risk rules.
#' 
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param strategy an object of type 'strategy' to add the rule to
#' @param mktdata an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information
#' @param Dates default NULL, list of time stamps to iterate over, ignored if \code{path.dep=FALSE}
#' @param indicators if indicator output is not contained in the mktdata object, it may be passed separately as an xts object or a list.
#' @param signals if signal output is not contained in the mktdata object, it may be passed separately as an xts object or a list.
#' @param ... any other passthru parameters
#' @param path.dep TRUE/FALSE whether rule is path dependent, default TRUE, see Details 
#' @seealso \code{\link{add.rule}} \code{\link{applyStrategy}} 
#' @export
applyRules <- function(portfolio, symbol, strategy, mktdata, Dates=NULL, indicators=NULL, signals=NULL,  ..., path.dep=TRUE) {
    # TODO check for symbol name in mktdata using Josh's code:
    # symbol <- strsplit(colnames(mktdata)[1],"\\.")[[1]][1]
    
    # TODO handle indicator and signal lists as well as indicators/signals that were cbound to mktdata
    
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object of type 'strategy'.")
    } 
    ret <- NULL
    nargs <-list(...)
    if(length(nargs)==0) nargs=NULL
    if (length('...')==0 | is.null('...')) {
        rm('...')
        nargs=NULL
    }
    
    ruleProc <- function (ruletypelist,timestamp=NULL, path.dep, ...){
        for (rule in ruletypelist){
            #TODO check to see if they've already been calculated
            if (!rule$path.dep==path.dep) next()
            if(!is.function(get(rule$name))){
                if(!is.function(get(paste("sig",rule$name,sep='.')))){
                    message(paste("Skipping rule",rule$name,"because there is no function by that name to call"))
                    next()      
                } else {
                    rule$name<-paste("sig",rule$name,sep='.')
                }
            }
            
            if(!isTRUE(rule$enabled)) next()
            
            # see 'S Programming p. 67 for this matching
            fun<-match.fun(rule$name)
            
            .formals  <- formals(fun)
            onames <- names(.formals)
            rule$arguments$timestamp=timestamp
            pm <- pmatch(names(rule$arguments), onames, nomatch = 0L)
            if (any(pm == 0L))
                warning(paste("some arguments stored for",rule$name,"do not match"))
            names(rule$arguments[pm > 0L]) <- onames[pm]
            .formals[pm] <- rule$arguments[pm > 0L]
            #now add dots
            if (length(nargs)) {
                pm <- pmatch(names(nargs), onames, nomatch = 0L)
                names(nargs[pm > 0L]) <- onames[pm]
                .formals[pm] <- nargs[pm > 0L]
            }
            .formals$... <- NULL
            
            tmp_val<-do.call(fun,.formals)
            if(is.null(names(tmp_val)) & ncol(tmp_val)==1) names(tmp_val)<-rule$label
            if (nrow(mktdata)==nrow(tmp_val) | length(mktdata)==length(tmp_val)) {
                # the rule returned a time series, so we'll name it and cbind it
                mktdata<-cbind(mktdata,tmp_val)
            } else {
                # the rule returned something else, add it to the ret list
                if(is.null(ret)) ret<-list()
                ret[[rule$name]]<-tmp_val
            }
            #print(tmp_val)
        } #end rules loop
        mktdata <<- mktdata
        ret <<- ret
        hold <<- hold
    } # end sub process function

    #TODO FIXME we should probably do something more sophisticated, but this should work
    if(isTRUE(path.dep) & is.null(Dates)) Dates=time(mktdata) # should this be index() instead?
    if(!isTRUE(path.dep)) Dates=''

    hold=FALSE
    holdtill=first(time(Dates))-1 # TODO FIXME make holdtill default more robust?
    
    for(d in 1:length(Dates)){ # d is a date slot counter
        # I shouldn't have to do this but we lose the class for the element 
        # when we do for(date in Dates)
        timestamp=Dates[d]    
        
        # check to see if we need to release a hold
        if(isTRUE(hold) & holdtill<timestamp){
            hold=FALSE
            holdtill=NULL
        }
        for ( type in names(strategy$rules)){
            switch( type ,
                    pre = {
                        if(length(strategy$rules[[type]])>=1){
                            ruleProc(strategy$rules$pre,timestamp=timestamp, path.dep=path.dep)    
                        }
                    },
                    risk = {
                        if(length(strategy$rules$risk)>=1){
                            ruleProc(strategy$rules$risk,timestamp=timestamp, path.dep=path.dep)    
                        }       
                    },
                    order = {
                        if(isTRUE(hold)) next()
                        if(length(strategy$rules[[type]])>=1) {
                            ruleProc(strategy$rules[[type]],timestamp=timestamp, path.dep=path.dep)
                        } else {
                            #(mktdata, portfolio, symbol, timestamp, slippageFUN=NULL)
                            ruleOrderProc(portfolio=portfolio, symbol=symbol, mktdata=mktdata, timestamp=timestamp)
                        }
                    },
                    rebalance =, exit = , enter = {
                        if(isTRUE(hold)) next()    
                        if(length(strategy$rules[[type]])>=1) {
                            ruleProc(strategy$rules$risk,timestamp=timestamp, path.dep=path.dep)
                        }      
                    },
                    post = {
                        #TODO do we processfor hold here, or not?
                        if(length(strategy$rules$post)>=1) {
                            ruleProc(strategy$rules$post,timestamp=timestamp, path.dep=path.dep)    
                        }
                    }
            ) # end switch            
        } #end type loop
    } # end dates loop
    
    mkdata<<-mktdata
    if(is.null(ret)) {
        return(mktdata)
    }
    else return(ret)
}

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
