#' constructor for objects of type 'strategy'
#' 
#' variables passed in dots will be added to the strategy object, and may 
#' be used by initialization and wrapup functions, as well as
#' indicators, signals, and rules.
#' 
#' @param name character string naming the strategy
#' @param ... any other passthru parameters
#' @param assets optional list of assets to apply the strategy to, should normally be defined in the portfolio, not here
#' @param constraints optional portfolio constraints object matching assets
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @export
#' @seealso \code{\link{applyStrategy}}
strategy <- function(name, ..., assets=NULL, constraints=NULL ,store=FALSE)
{ # originally modeled on framework code in GPL R-Forge pkg roi by Stefan Thuessel,Kurt Hornik,David Meyer
    
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
    rules<-list()
    rules$order<-list()
    ## now structure and return
    strat<-structure(
                    list(
                            name= name,
                            assets = assets,
                            indicators = list(),
                            signals = list(),
                            rules = rules,
                            constraints = NULL,
                            init =list(),
                            wrapup = list(),
                            call = match.call()
                    ),
                    class=c("strategy")
            )
     
    arg<-list(...)        
    if(length(arg)>=1) {
        strat <- c(strat,arg)  
        #the c() function loses our class attribute, annoyingly
        class(strat)<-'strategy'
    }
            
    if(store) assign(strat$name,strat,envir=as.environment(.strategy))
    else return(strat)
}

#' apply the strategy to arbitrary market data
#' 
#' This function is the wrapper that holds together the execution of a strategy.
#' 
#' After the straetgy object has been created, it may be applied to any 
#' combination of symbols and parameters.
#' 
#' The symbols to be utilized will be defined in one of two ways, either by
#' specifying a name of a portfolio that has already been initialized 
#' with the \code{portfolios} argument, or be specifying a 
#' \code{symbols} argument in  addition to setting \code{initStrat=TRUE}.
#' 
#' \code{applyStrategy} will use the \R core function \code{\link{get}} 
#' to load market data for each symbol during stategy evaluation unless 
#' the user passes \code{mktdata} in the call to \code{applyStrategy}
#'
#'  
#' @param strategy an object of type 'strategy' to add the indicator to
#' @param portfolios a list of portfolios to apply the strategy to
#' @param mktdata an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats, default NULL
#' @param parameters named list of parameters to be applied during evaluation of the strategy, default NULL
#' @param ... any other passthru parameters
#' @param verbose if TRUE, return output list
#' @param symbols character vector identifying symbols to initialize a portfolio for, default NULL
#' @param initStrat whether to use (experimental) initialization code, default FALSE
#' @param updateStrat whether to use (experimental) wrapup code, default FALSE
#' @export
#' @seealso \code{\link{strategy}},  \code{\link{applyIndicators}}, 
#'  \code{\link{applySignals}}, \code{\link{applyRules}},
#'  \code{\link{initStrategy}}, 
applyStrategy <- function(strategy , portfolios, mktdata=NULL , parameters=NULL, ..., verbose=TRUE, symbols=NULL, initStrat=FALSE, updateStrat=FALSE ) {
    #TODO add Date subsetting
    #TODO add saving of modified market data
    
    ret<-list()
    
	if (!is.strategy(strategy)) {
    	strategy<-try(getStrategy(strategy))
    	if(inherits(strategy,"try-error"))
    	    stop ("You must supply an object of type 'strategy'.")
    } 
	
    if (missing(mktdata)) load.mktdata=TRUE else load.mktdata=FALSE
	
    for (portfolio in portfolios) {
        
		# TODO call to initStrategy will go here!
        if(isTRUE(initStrat)) initStrategy(strategy=strategy, portfolio, symbols, ...=...)
        
   		ret[[portfolio]]<-list() # this is slot [[i]] which we will use later
        pobj<-getPortfolio(portfolio)
        symbols<-names(pobj$symbols)
        sret<-list()
        for (symbol in symbols){
            if(isTRUE(load.mktdata)) mktdata <- get(symbol)

            #loop over indicators
            sret$indicators <- applyIndicators(strategy=strategy , mktdata=mktdata , parameters=parameters, ... )
            #this should be taken care of by the mktdata<<-mktdata line in the apply* fn
            if(inherits(sret$indicators,"xts") & nrow(mktdata)==nrow(sret$indicators)){
                mktdata<-sret$indicators
            }
            
            #loop over signal generators
            sret$signals <- applySignals(strategy=strategy, mktdata=mktdata, sret$indicators, parameters=parameters, ... )
            #this should be taken care of by the mktdata<<-mktdata line in the apply* fn
            if(inherits(sret$signals,"xts") & nrow(mktdata)==nrow(sret$signals)){
                mktdata<-sret$signals    
            }
            
            #loop over rules  
            sret$rules<-list()
			
			## only fire nonpath/pathdep when true 
			## TODO make this more elegant
			pd <- FALSE
			for(i in 1:length(strategy$rules)){	if(length(strategy$rules[[i]])!=0){z <- strategy$rules[[i]]; if(z[[1]]$path.dep==TRUE){pd <- TRUE}}}
				
            sret$rules$nonpath<-applyRules(portfolio=portfolio, symbol=symbol, strategy=strategy, mktdata=mktdata, Dates=NULL, indicators=sret$indicators, signals=sret$signals, parameters=parameters,  ..., path.dep=FALSE)
			
			## Check for open orders
			rem.orders <- suppressWarnings(getOrders(portfolio=portfolio, symbol=symbol, status="open")) #, timespan=timespan, ordertype=ordertype,which.i=TRUE)
			if(nrow(rem.orders)>0){pd <- TRUE}
            if(pd==TRUE){sret$rules$pathdep<-applyRules(portfolio=portfolio, symbol=symbol, strategy=strategy, mktdata=mktdata, Dates=NULL, indicators=sret$indicators, signals=sret$signals, parameters=parameters,  ..., path.dep=TRUE)}

			ret[[portfolio]][[symbol]]<-sret
		}
        
        # TODO call to updateStrategy will go here!
        if(isTRUE(updateStrat)) updateStrategy(strategy, portfolio, Symbols=symbols, ...=...)
        
    }
    
    if(verbose) return(ret)
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
# Copyright (c) 2009-2011
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
