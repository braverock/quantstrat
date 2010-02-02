#' constructor for objects of type 'strategy'
#' @param name character string naming the strategy
#' @param ... any other passthru parameters
#' @param assets optional list of assets to apply the strategy to
#' @param constraints optional portfolio constraints object matching assets
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @export
strategy <- function(name, ..., assets=NULL, constraints=NULL ,store=FALSE)
{ # modeled on GPL R-Forge pkg roi by Stefan Thuessel,Kurt Hornik,David Meyer
    
    if(!is.null(assets)){
        if(is.numeric(assets)){
            if (length(assets) == 1) {
                nassets=assets
                #we passed in a number of assets, so we need to create the vector
                message("assuming equal weighted seed portfolio")
                assets<-rep(1/nassets,nassets)
            } else {
                nassets = length(assets)
            }
            # and now we may need to name them
            if (is.null(names(assets))) {
                for(i in 1:length(assets)){
                    names(assets)[i]<-paste("Asset",i,sep=".")
                }
            }
        }
        if(is.character(assets)){
            nassets=length(assets)
            assetnames=assets
            message("assuming equal weighted seed portfolio")
            assets<-rep(1/nassets,nassets)
            names(assets)<-assetnames  # set names, so that other code can access it,
            # and doesn't have to know about the character vector
        }
        # if assets is a named vector, we'll assume it is current weights
    }
       
    ## now structure and return
    strat<-structure(
                    list(
                            name= name,
                            assets = assets,
                            indicators = list(),
                            signals = list(),
                            rules = list(),
                            constraints = NULL,
                            call = match.call()
                    ),
                    class=c("strategy")
            )
    if(store) assign(strat$name,strat,envir=as.environment(.strategy))
    else return(strat)
}

#' apply the strategy to arbitrary market data
#' @param strategy an object of type 'strategy' to add the indicator to
#' @param mktdata an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats
#' @param ... any other passthru parameters
#' @export
applyStrategy <- function(strategy,mktdata, ... ) {
    #TODO add Date subsetting
    
    ret<-list()
    
	if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object of type 'strategy'.")
    } 
    
    #loop over indicators
    ret$indicators <- applyIndicators(strategy,mktdata, ... )
     
    #loop over signal generators
    
    #loop over rules 
    
    return(ret)
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
        rm('...')
        nargs=NULL
    }
    
    for (indicator in strategy$indicators){
        #TODO check to see if they've already been calculated
        if(!is.function(get(indicator$name))) {
            message(paste("Skipping indicator",indicator$name,"because there is no function by that name to call"))
            next()
        }
        
        # see 'S Programming p. 67 for this matching
        fun<-match.fun(indicator$name)
        .formals  <- formals(fun)
        onames <- names(.formals)
        
        pm <- pmatch(names(indicator$arguments), onames, nomatch = 0L)
        if (any(pm == 0L))
            warning(paste("some arguments stored for",indicator$name,"do not match"))
        names(indicator$arguments[pm > 0L]) <- onames[pm]
        .formals[pm] <- indicator$arguments[pm > 0L]
        #now add dots
        if (length(nargs)) {
            pm <- pmatch(names(nargs), onames, nomatch = 0L)
            names(nargs[pm > 0L]) <- onames[pm]
            .formals[pm] <- nargs[pm > 0L]
        }
        .formals$... <- NULL
        
        formals(fun) <- .formals
        tmp_val<-do.call(fun,.formals)
        if(is.null(names(tmp_val)) & ncol(tmp_val)==1) names(tmp_val)<-indicator$name
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

#' test to see if object is of type 'strategy'
#' @param x object to be tested
#' @export
is.strategy <- function( x ) {
    inherits( x, "strategy" )
}

#' retrieve strategy from the container environment
#' @param x string name of object to be retrieved
#' @export
getStrategy <- function(x){
    tmp_strat<-get(as.character(x),pos=.strategy, inherits=TRUE)
    if( inherits(tmp_strat,"try-error") | !is.strategy(tmp_strat) ) {
        warning(paste("Strategy",x," not found, please create it first."))
        return(FALSE)
    } else {
        if(is.strategy(tmp_strat)) return(tmp_strat) else return(NULL)
    }
}

.onLoad <- function(lib, pkg) {
    if(!exists('.strategy'))
        .strategy <<- new.env()
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
# $Id: strategy.R 217 2010-01-29 18:10:53Z braverock $
#
###############################################################################
