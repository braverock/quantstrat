
#' add an indicator to a strategy
#'
#' Indicators are typically standard technical or statistical analysis outputs, 
#' such as moving averages, bands, or pricing models.
#'
#' Indicators are always path-independent, and should be constructed from vectorized functions where possible.
#'
#' Indicators are applied before signals and rules, and the output of indicators 
#' may be used as inputs to construct signals or fire rules.
#'
#' \code{arguments} and \code{parameters} are named lists that describe the arguments to be passed to nthe indicator function.
#' \code{arguments} is for defining any non-default arguments to be passed to the function named in the \code{name} of the indicator.  
#' For example, the \code{x} argument to a moving average function may be defined as \code{x=quote(Cl(mktdata))}
#' 
#' If you look at the demo scripts, you'll notice that we often use \code{quote(mktdata)} in setting up indicators, signals, or rules.
#' This tells \R to delay evaluation via \code{quote()}, and to use the special variable \code{mktdata}. 
#' 
#' \code{mktdata} is typically created internally to \code{quantstrat} by looking in the global environment for 
#' a time series of prices or returns. mktdata may also contain other data you've manipulated outside quatstrat, 
#' though where possible you should use quantstrat to contain all the logic for the strategy, to aid in maintenance and modifications.
#' 
#' The use of \code{quote()} tells R to not evaluate what's inside the quote until the function is evaluated later.  
#' By the time that code is evaluated, \code{mktdata} will be populated with the correct price information based on the contents of whatever portfolio you are evaluating the strategy on. 
#'
#' \code{parameters} is another named list, and normally will not be needed.  
#' If you have multiple indicator, signal, or rule functions share the that 
#' \emph{both} share the same argument names \emph{and} will need to have 
#' different values passed to those arguments as defined parameters at apply-time,
#' then you may need to give them unique names so that delayed evaluation can
#' sort it all out for you at apply-time.  
#' We will endeavor to get an example of named parameters into the demo scripts.
#'
#' @param strategy an object of type 'strategy' to add the indicator to
#' @param name name of the indicator, must correspond to an R function
#' @param arguments default arguments to be passed to an indicator function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition,default NULL, only needed if you need special names to avoid argument collision
#' @param label arbitrary text label for indicator output, NULL default will be converted to '<name>.ind'
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the indicator is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific indicator, the index number in the $indicators list to update
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @export
#' @seealso \code{\link{quote}}
add.indicator <- function(strategy, name, arguments, parameters=NULL, label=NULL, ..., enabled=TRUE, indexnum=NULL, store=FALSE) {
    if(!is.strategy(strategy)) stop("You must pass in a strategy object to manipulate")
    tmp_indicator<-list()
    tmp_indicator$name<-name
    if(is.null(label)) label = paste(name,"ind",sep='.')
    tmp_indicator$label<-label
    tmp_indicator$enabled=enabled
    if (!is.list(arguments)) stop("arguments must be passed as a named list")
    tmp_indicator$arguments<-arguments
	if(!is.null(parameters)) tmp_indicator$parameters = parameters
	if(length(list(...))) tmp_indicator<-c(tmp_indicator,list(...))
	
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$indicators)+1
    tmp_indicator$call<-match.call()
	class(tmp_indicator)<-'strat_indicator'
	
    strategy$indicators[[indexnum]]<-tmp_indicator
    
    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
}

#' apply the indicators in the strategy to arbitrary market data
#' @param strategy an object of type 'strategy' to add the indicator to
#' @param mktdata an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats
#' @param parameters named list of parameters to be applied during evaluation of the strategy
#' @param ... any other passthru parameters
#' @export
applyIndicators <- function(strategy, mktdata, parameters=NULL, ...) {
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
				# now add arguments from parameters
				if(length(parameters)){
					pm <- pmatch(names(parameters), onames, nomatch = 0L)
					names(parameters[pm > 0L]) <- onames[pm]
					.formals[pm] <- parameters[pm > 0L]
				}
				
				
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
		
		# now add arguments from parameters
		if(length(parameters)){
			pm <- pmatch(names(parameters), onames, nomatch = 0L)
			names(parameters[pm > 0L]) <- onames[pm]
			.formals[pm] <- parameters[pm > 0L]
		}
		
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
    mktdata<<-mktdata
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
