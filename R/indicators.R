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
#' \code{arguments} and \code{parameters} are named lists that describe the arguments to be passed to the indicator function.
#' \code{arguments} is for defining any non-default arguments to be passed to the function named in the \code{name} of the indicator.  
#' For example, the \code{x} argument to a moving average function may be defined as \code{x=quote(Cl(mktdata))}
#' 
#' If you look at the demo scripts, you'll notice that we often use \code{quote(mktdata)} in setting up indicators, signals, or rules.
#' This tells \R to delay evaluation via \code{quote()}, and to use the special variable \code{mktdata}. 
#' 
#' \code{mktdata} is typically created internally to \code{quantstrat} by looking in the global environment for 
#' a time series of prices or returns. mktdata may also contain other data you've manipulated outside quantstrat, 
#' though where possible you should use quantstrat to contain all the logic for the strategy, 
#' to aid in maintenance and modifications.
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
#' if \code{label} is not supplied,  NULL default will be converted to '<name>.ind' unless
#' there already exists an indicator with that label in which case it will be appended
#' with a number (i.e. '<name>.ind.2', '<name>.ind.3', etc.).
#' If the indicator function returns multiple columns, the label will be 
#' \code{\link{paste}}'d to the end of either the returned column names or the 
#' respective column number when applying it to \code{mktdata}.
#' 
#' @param strategy an object (or the name of an object) type 'strategy' to add the indicator to
#' @param name name of the indicator function -- must correspond to an R function
#' @param arguments default arguments to be passed to an indicator function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition,default NULL, only needed if you need special names to avoid argument collision
#' @param label arbitrary text label for indicator output.  This will also be used as the
#' name of the indicator list when it is stored.  NULL default will be converted to '<name>.ind'
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the indicator is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific indicator, the \code{label} or the index number in the $indicators list to update.
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @return if \code{strategy} was the name of a strategy, the name. It it was a strategy, the updated strategy. 
#' @seealso 
#' \code{\link{quote}}
#' \code{\link{applyIndicators}}
#' \code{\link{add.signal}}
#' \code{\link{add.rule}}
#' @examples
#' \dontrun{
#' strategy("example", store=TRUE)
#' getSymbols("SPY", src='yahoo')
#' add.indicator('example', 'SMA', arguments=list(x=quote(Ad(SPY)), n=20))
#' str(getStrategy('example')$indicators)
#' out <- applyIndicators('example', SPY)
#' tail(out)
#' }
#' @export
add.indicator <- function(  strategy
                          , name
                          , arguments
                          , parameters=NULL
                          , label=NULL
                          , ...
                          , enabled=TRUE
                          , indexnum=NULL
                          , store=FALSE
                          ) 
{
  if (!is.strategy(strategy)) {
    strategy<-try(getStrategy(strategy))
    if(inherits(strategy,"try-error"))
      stop ("You must supply an object or the name of an object of type 'strategy'.")
    store=TRUE    
  } 
  tmp_indicator<-list()
  tmp_indicator$name<-name
  # if we have a 'label', that will be the name of the indicator, if it already exists, 
  #     it will be overwritten.  If label is NULL the indicator name will be "<name>.ind" 
  #     unless that already exists in which case we will append that with a number. 
  if(is.null(label)) {
    label <- paste(name,"ind",sep='.')
    gl <- grep(label, names(strategy$indicators))
    if (!identical(integer(0), gl)) label <- paste(label, length(gl)+1, sep=".")
  }   
  tmp_indicator$label<-label
  tmp_indicator$enabled=enabled
  if (!is.list(arguments)) stop("arguments must be passed as a named list")
  tmp_indicator$arguments<-arguments
  if(!is.null(parameters)) tmp_indicator$parameters = parameters
  if(length(list(...))) tmp_indicator<-c(tmp_indicator,list(...))
  
  #if(!hasArg(indexnum) || (hasArg(indexnum) && is.null(indexnum))) indexnum = length(strategy$indicators)+1
  indexnum <- if (!is.null(indexnum)) {indexnum} else label 
  
  tmp_indicator$call<-match.call()
  class(tmp_indicator)<-'strat_indicator'
  
  strategy$indicators[[indexnum]]<-tmp_indicator
  
  #increment trials
  strategy$trials <- strategy$trials+1
  
  if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
  else return(strategy)
  strategy$name
}

#' apply the indicators in the strategy to arbitrary market data
#' 
#' \code{applyIndicators} will take the \code{mktdata} object, 
#' and will apply each indicator specified in the strategy definition to it.
#' 
#' If the indicator function returns an xts object or a vector of the same length 
#' as mktdata, the columns created by the indicator function will be added to the 
#' mktdata object via \code{\link{cbind}}. 
#' 
#' If the indicator function returns multiple columns, the label will be 
#' \code{\link{paste}}'d to the end of either the returned column names or the 
#' respective column number when applying it to \code{mktdata}.
#' 
#' If the indicator returns some more complexobject, it will be added to a list 
#' in the $indicators slot inside the \code{\link{applyStrategy}} execution frame.
#' If you want your indicators to return a more complex object, 
#' (such as a model specification and output from a regression), be advised
#' that your signal generator, and potentially your rule function, will need
#' to understand, anticipate, and know how to manipulate the internal strategy frame.
#'   
#' @param strategy an object of type 'strategy' to add the indicator to
#' @param mktdata an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats
#' @param parameters named list of parameters to be applied during evaluation of the strategy
#' @param ... any other passthru parameters
#' @return \code{mktdata} with indicators colums added.
#' @examples
#' \dontrun{
#' strategy("example", store=TRUE)
#' getSymbols("SPY", src='yahoo')
#' add.indicator('example', 'SMA', arguments=list(x=quote(Ad(SPY)), n=20))
#' str(getStrategy('example')$indicators)
#' out <- applyIndicators('example', SPY)
#' tail(out)
#' }
#' @export
applyIndicators <- function(strategy, mktdata, parameters=NULL, ...) {
    #TODO add Date subsetting
    
    # TODO check for symbol name in mktdata using Josh's code:
    # symbol <- strsplit(colnames(mktdata)[1],"\\.")[[1]][1]

    # ensure no duplicate index values in mktdata
    if(any(diff(.index(mktdata)) == 0)) {
        warning("'mktdata' index contains duplicates; calling 'make.index.unique'")
        mktdata <- make.index.unique(mktdata)
    }
    
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object of type 'strategy'.")
    } 
    ret <- NULL

    # First, delete any colums in mktdata that correspond to indicators we're about
    # to (re)calculate and cbind.
    omit <- unique(do.call(c, lapply(names(strategy$indicators), grep, colnames(mktdata))))
    cidx <- 1:NCOL(mktdata)
    keep <- cidx[!cidx %in% omit]
    mktdata <- mktdata[, keep]
    
    for (indicator in strategy$indicators){
        if(is.function(indicator$name)) {
            indFun <- indicator$name
        } else {
            if(exists(indicator$name, mode="function")) {
                indFun <- get(indicator$name, mode="function")
            } else {
                ind.name <- paste("ind", indicator$name, sep=".")
                if(exists(ind.name, mode="function")) {
                    indFun <- get(ind.name, mode="function")
                    indicator$name <- ind.name
                } else {
                    message("Skipping indicator ", indicator$name,
                            " because there is no function by that name to call")
                    next
                }
            }
        }

        if(!isTRUE(indicator$enabled)) next()
        
        # replace default function arguments with indicator$arguments
        .formals <- formals(indicator$name)
        .formals <- modify.args(.formals, indicator$arguments, dots=TRUE)
        # now add arguments from parameters
        .formals <- modify.args(.formals, parameters, dots=TRUE)
        # now add dots
        .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
        # remove ... to avoid matching multiple args
        .formals$`...` <- NULL
        
        tmp_val <- do.call(indFun, .formals)
		
		#add label
        if(is.null(colnames(tmp_val)))
            colnames(tmp_val) <- seq(ncol(tmp_val))
        if(!identical(colnames(tmp_val),indicator$label)) 
			colnames(tmp_val) <- paste(colnames(tmp_val),indicator$label,sep='.')

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
# Copyright (c) 2009-2015
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
