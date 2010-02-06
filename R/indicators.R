
#' add an indicator to a strategy
#' @param strategy an object of type 'strategy' to add the indicator to
#' @param name name of the indicator, must correspond to an R function
#' @param arguments default arguments to be passed to an indicator function when executed
#' @param label arbitrary text label for indicator output, NULL default will be converted to '<name>.ind'
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the indicator is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific indicator, the index number in the $indicators list to update
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @export
add.indicator <- function(strategy, name, arguments, label=NULL, ..., enabled=TRUE, indexnum=NULL, store=FALSE) {
    if(!is.strategy(strategy)) stop("You must pass in a strategy object to manipulate")
    tmp_indicator<-list()
    tmp_indicator$name<-name
    if(is.null(label)) label = paste(name,"ind",sep='.')
    tmp_indicator$label<-label
    tmp_indicator$enabled=enabled
    if (!is.list(arguments)) stop("arguments must be passed as a named list")
    tmp_indicator$arguments<-arguments
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$indicators)+1
    tmp_indicator$call<-match.call()
    strategy$indicators[[indexnum]]<-tmp_indicator
    
    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
}

#' apply the indicators in the strategy to arbitrary market data
#' @param strategy an object of type 'strategy' to add the indicator to
#' @param mktdata an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats
#' @param ... any other passthru parameters
#' @export
applyIndicators <- function(strategy, mktdata, ...) {
    #TODO add Date subsetting
    
    # TODO check for symbol name in mktdata using Josh's code:
    # symbol <- strsplit(colnames(mktdata)[1],"\\.")[[1]][1]
    
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object of type 'strategy'.")
    } 
    ret <- NULL
    nargs <-list(...)
    if(length(nargs)==0) nargs=NULL
    if (length('...')==0 | is.null('...')) {
        #rm('...')
        nargs=NULL
    }
    
    for (indicator in strategy$indicators){
        #TODO check to see if they've already been calculated

        if(!is.function(get(indicator$name))){
            if(!is.function(get(paste("sig",indicator$name,sep='.')))){
                message(paste("Skipping indicator",indicator$name,"because there is no function by that name to call"))
                next()      
            } else {
                indicator$name<-paste("ind",indicator$name,sep='.')
            }
        }
        
        if(!isTRUE(indicator$enabled)) next()
        
        # see 'S Programming p. 67 for this matching
        fun<-match.fun(indicator$name)
        .formals  <- formals(fun)
        onames <- names(.formals)
        
        pm <- pmatch(names(indicator$arguments), onames, nomatch = 0L)
        if (any(pm == 0L))
            warning(paste("some arguments stored for",indicator$name,"do not match"))
        names(indicator$arguments[pm > 0L]) <- onames[pm]
        .formals[pm] <- indicator$arguments[pm > 0L]
        #now add arguments from dots
        if (length(nargs)) {
            pm <- pmatch(names(nargs), onames, nomatch = 0L)
            names(nargs[pm > 0L]) <- onames[pm]
            .formals[pm] <- nargs[pm > 0L]
        }
        .formals$... <- NULL
        
        tmp_val<-do.call(fun,.formals)
        if(is.null(names(tmp_val)) & ncol(tmp_val)==1) names(tmp_val)<-indicator$label
        if (nrow(mktdata)==nrow(tmp_val) | length(mktdata)==length(tmp_val)) {
            # the indicator returned a time series, so we'll name it and cbind it
            mktdata<-cbind(mktdata,tmp_val)
        } else {
            # the indicator returned something else, add it to the ret list
            if(is.null(ret)) ret<-list()
            ret[[indicator$name]]<-tmp_val
        }
        #print(tmp_val)
    } #end indicators loop
    if(is.null(ret)) return(mktdata)
    else return(list(mktdata=mktdata,indicator_ret=ret))
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
