
#' add a signal to a strategy
#' 
#' This adds a signal definition to a strategy object. 
#' 
#' Signals denote times at which the strategy \emph{may} want to 
#' take action.  Common signals types from the literature include 
#' crossovers, thresholds, or other interactions between your \code{mktdata}
#' and your indicators.
#' 
#' if \code{label} is not supplied,  NULL default will be converted to '<name>.sig'
#' if the signal function returns one named column, we use that, and ignore the label.  
#' If the signal function returns multiple columns, the label will be 
#' \code{\link{paste}}'d to either the returned column names or the 
#' respective column number.
#' 
#' @param strategy an object (or the name of an object) of type 'strategy' to add the signal to
#' @param name name of the signal, must correspond to an R function
#' @param arguments named list of default arguments to be passed to an signal function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition,default NULL, only needed if you need special names to avoid argument collision
#' @param label arbitrary text label for signal output, default NULL
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the signal is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific signal, the index number in the $signals list to update
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @return if \code{strategy} was the name of a strategy, the name. It it was a strategy, the updated strategy. 
#' @seealso 
#' \code{\link{applySignals}}
#' \code{\link{add.indicator}}
#' \code{link{add.rule}}
#' \code{\link{sigComparison}}
#' \code{\link{sigCrossover}}
#' \code{\link{sigFormula}}
#' \code{\link{sigPeak}}
#' \code{\link{sigThreshold}}
#'  
#' @export
add.signal <- function(strategy, name, arguments, parameters=NULL, label=NULL, ..., enabled=TRUE, indexnum=NULL, store=FALSE) {
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object or the name of an object of type 'strategy'.")
        store=TRUE
    } 
    tmp_signal<-list()
    tmp_signal$name<-name
    if(is.null(label)) label = paste(name,"sig",sep='.')
    tmp_signal$label<-label
    tmp_signal$enabled<-enabled
    if (!is.list(arguments)) stop("arguments must be passed as a named list")
	arguments$label=label
    tmp_signal$arguments<-arguments
	if(!is.null(parameters)) tmp_signal$parameters = parameters
	if(length(list(...))) tmp_signal<-c(tmp_signal,list(...))

    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$signals)+1
    tmp_signal$call<-match.call()
	class(tmp_signal)<-'strat_signal'
    strategy$signals[[indexnum]]<-tmp_signal

    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
    strategy$name
}

#' apply the signals in the strategy to arbitrary market data
#' @param strategy an object of type 'strategy' to add the signal to
#' @param mktdata an xts object containing market data.  depending on signals, may need to be in OHLCV or BBO formats
#' @param indicators if indicator output is not contained in the mktdata object, it may be passed separately as an xts object or a list.
#' @param parameters named list of parameters to be applied during evaluation of the strategy
#' @param ... any other passthru parameters
#' @export
applySignals <- function(strategy, mktdata, indicators=NULL, parameters=NULL, ...) {
    #TODO add Date subsetting

    # TODO check for symbol name in mktdata using Josh's code:
    # symbol <- strsplit(colnames(mktdata)[1],"\\.")[[1]][1]

    # TODO handle indicator lists as well as indicators that were cbound to mktdata

    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object of type 'strategy'.")
    } 
    ret <- NULL
    
    for (signal in strategy$signals){
        #TODO check to see if they've already been calculated
        
        if(!is.function(get(signal$name))){
            if(!is.function(get(paste("sig",signal$name,sep='.')))){
                message(paste("Skipping signal",signal$name,"because there is no function by that name to call"))
                next()      
            } else {
                signal$name<-paste("sig",signal$name,sep='.')
            }
        }
 
        if(!isTRUE(signal$enabled)) next()
        
        # replace default function arguments with signal$arguments
        .formals <- formals(signal$name)
        .formals <- modify.args(.formals, signal$arguments, dots=TRUE)
        # now add arguments from parameters
        .formals <- modify.args(.formals, parameters, dots=TRUE)
        # now add dots
        .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
        # remove ... to avoid matching multiple args
        .formals$`...` <- NULL

        tmp_val <- do.call(signal$name, .formals)
		
		#add label
		if(is.null(colnames(tmp_val)))
			colnames(tmp_val) <- seq(ncol(tmp_val))
		if(!identical(colnames(tmp_val),signal$label)) 
			colnames(tmp_val) <- paste(colnames(tmp_val),signal$label,sep='.')
		
		if (nrow(mktdata)==nrow(tmp_val) | length(mktdata)==length(tmp_val)) {
            # the signal returned a time series, so we'll name it and cbind it
            mktdata<-cbind(mktdata,tmp_val)
        } else {
            # the signal returned something else, add it to the ret list
            if(is.null(ret)) ret<-list()
            ret[[signal$name]]<-tmp_val
        }
        #print(tmp_val)
    } #end signals loop
    mktdata<<-mktdata
    if(is.null(ret)) {
        return(mktdata)
    }
    else return(ret)
}


#' generate comparison signal
#' 
#' Currently, this function compares two columns.  
#' Patches to compare an arbitrary number of columns would be gladly accepted.
#' 
#' Comparison will be applied from the first to the second column in the \code{columns} vector.
#' 
#' Relationship 'op' means 'opposite' side.  Reasonable attempt will be made to match.
#' 
#' @param label text label to apply to the output
#' @param data data to apply comparison to
#' @param columns named columns to apply comparison to
#' @param relationship one of c("gt","lt","eq","gte","lte","op") or reasonable alternatives
#' @param offset1 numeric offset to be added to the first column prior to comparison
#' @param offset2 numeric offset to be added to the second column prior to comparison
#' @export
sigComparison <- function(label,data=mktdata, columns, relationship=c("gt","lt","eq","gte","lte"), offset1=0, offset2=0) {
    relationship=relationship[1] #only use the first one
    if (length(columns)==2){
        ret_sig=NULL
        if (relationship=='op'){
            # (How) can this support "Close"? --jmu
            if(columns[1] %in% c("Close","Cl","close"))
                stop("Close not supported with relationship=='op'")
            switch(columns[1],
                    Low =, 
                    low =, 
                    bid = { relationship = 'lt' },
                    Hi  =,
                    High=,
                    high=,
                    ask = {relationship = 'gt'}
            )
        }

        colNums <- match.names(columns,colnames(data))

		opr <- switch( relationship,
					   	gt = , '>' = '>', 
					 	lt =, '<' = '<', 
					 	eq =, "==" =, "=" = "==",
					 	gte =, gteq =, ge =, ">=" = ">=",
					 	lte =, lteq =, le =, "<=" = "<="
					 )

		ret_sig <- do.call( opr, list(data[,colNums[1]]+offset1, data[,colNums[2]]+offset2))

    } else {
        stop("comparison of more than two columns not supported, see sigFormula")
    }
    colnames(ret_sig)<-label
    return(ret_sig)
}

#' generate a crossover signal
#' 
#' This will generate a crossover signal, which is a dimension-reduced version 
#' of a comparison signal \code{\link{sigComparison}}.
#' 
#' It will return TRUE on the period in which there is a crossover in the 
#' direction specified by \code{relationship}, and NA otherwise.
#' 
#' If you want all the information, use a comparison instead.
#'  
#' @param label text label to apply to the output
#' @param data data to apply crossover to
#' @param columns named columns to apply crossover of the first against the second
#' @param relationship one of c("gt","lt","eq","gte","lte") or reasonable alternatives
#' @param offset1 numeric offset to be added to the first column prior to comparison
#' @param offset2 numeric offset to be added to the second column prior to comparison
#' @export
sigCrossover <- function(label,data=mktdata, columns, relationship=c("gt","lt","eq","gte","lte"), offset1=0, offset2=0) {
    ret_sig = FALSE
    lng<-length(columns)
    for (i in 1:(lng-1)) {
        ret_sig = suppressWarnings(ret_sig | diff(sigComparison(label=label,data=data,columns=columns[c(i,lng)],relationship=relationship,offset1=offset1, offset2=offset2))==1)
    }
    is.na(ret_sig) <- which(!ret_sig)
    colnames(ret_sig)<-label
    return(ret_sig)
}

#' signal function for peak/valley signals
#' 
#' This function tests to see if the mktdata or indicator \code{column} observations 
#' on either side are lower(higher) creating a local peak(bottom).
#' @param label text label to apply to the output
#' @param data data to apply comparison to
#' @param column named column to apply comparison to
#' @param direction one of "peak" or "bottom" to calculate  peaks for
#' @export
sigPeak <- function(label,data,column, direction=c("peak","bottom")){
    if(!is.numeric(column)){
        colNum<-match.names(column,colnames(data))   
    } else colNum<-column
    direction=direction[1] # only use the first]
    #(Lag(IBM[,4],2)<Lag(IBM[,4],1)) & Lag(IBM[,4],1) >IBM[,4]
    switch(direction,
           "peak"   = { ret_sig <- Lag(data[,colNum],2) < Lag(data[,colNum],1) & Lag(data[,colNum],1) > data[,colNum] } ,
           "bottom","valley" = { ret_sig <- Lag(data[,colNum],2) > Lag(data[,colNum],1) & Lag(data[,colNum],1) < data[,colNum] }
    )
    Lag(ret_sig,-1)
    colnames(ret_sig)<-paste(label,direction,"sig",sep='.')
    return(ret_sig)
}

#' generate a threshold signal
#' 
#' Many strategies, including RSI or MACD styles, make trading decisions when an indicator 
#' is over or under a specific threshold.  
#' This function generates the appropriate signal based on such a threshold.
#' 
#' @param label text label to apply to the output
#' @param data data to apply comparison to
#' @param column named column to apply comparison to
#' @param threshold numeric threshold to test for
#' @param relationship one of c("gt","lt","eq","gte","lte") or reasonable alternatives
#' @param cross if TRUE, will return TRUE only for the first observation to cross the threshold in a run
#' @export
sigThreshold <- function(label, data=mktdata, column, threshold=0, relationship=c("gt","lt","eq","gte","lte"),cross=FALSE) {
    relationship=relationship[1] #only use the first one
    ret_sig=NULL
    colNum <- match.names(column, colnames(data))
    switch(relationship,
            '>' =,
            'gt' = {ret_sig = data[,colNum] > threshold},
            '<' =,
            'lt' = {ret_sig = data[,colNum] < threshold},
            'eq'     = {ret_sig = data[,colNum] == threshold}, #FIXME any way to specify '='?
            'gte' =,
            'gteq'=,
            'ge'     = {ret_sig = data[,colNum] >= threshold}, #FIXME these fail with an 'unexpected =' error if you use '>='
            'lte' =,
            'lteq'=,
            'le'     = {ret_sig = data[,colNum] <= threshold}
    )
    if(isTRUE(cross)) ret_sig <- diff(ret_sig)==1
    if(!missing(label))  # colnames<- copies; avoid if possible
        colnames(ret_sig)<-label
    return(ret_sig)
}

#' @useDynLib quantstrat
.firstCross <- function(Data, threshold=0, relationship, start=1) {
    rel <- switch(relationship[1],
            '>'    =  ,
            'gt'   = 1,
            '<'    =  ,
            'lt'   = 2,
            'eq'   = 3, #FIXME any way to specify '='?
            'gte'  =  ,
            'gteq' =  ,
            'ge'   = 4, #FIXME these fail with an 'unexpected =' error if you use '>='
            'lte'  =  ,
            'lteq' =  ,
            'le'   = 5)
    .Call('firstCross', Data, threshold, rel, start)
}

#' generate a signal from a formula
#' 
#' This code takes advantage of some base R functionality that can treat an R object (in this case the internal mktdata object in quantstrat) as an environment or 'frame' using \code{\link{parent.frame}}.  
#' This allows the columns of the data to be addressed without any major manipulation, simply by column name.  In most cases in quantstrat, this will be either the price/return columns, or columns added by indicators or prior signals.
#' The formula will return TRUE/FALSE for each row comparison as a time series column which can then be used for rule execution.  The \code{formula} will be evaluated using \code{\link{eval}} as though in an if statement. 
#' 
#' This code is adapted from the approach used by Vijay Vaidyanthan in his PAST(AAII/SIPRO) code to construct arbitrary, formulaic, comparisons.  Many thanks to Vijay for sharing his expertise.
#'  
#' @param label text label to apply to the output
#' @param data data to apply formula to
#' @param formula a logical expression like that used in an if statement, will typically reference column names in \code{mktdata}
#' @param cross if TRUE, will return TRUE only for the first observation to match the formula in a run
#' @export
sigFormula <- function(label, data=mktdata, formula ,cross=FALSE){
	# Vijay's PAST/AAII/SIPRO example
	# fieldVals <- try(eval(parse(text=expression), data))
	ret_sig=NULL
	ret_sig <- try(.xts(eval(parse(text=formula), as.list(data)),index=.index(data)))
	if(is.xts(ret_sig)){
		if(isTRUE(cross)) ret_sig <- diff(ret_sig)==1
		colnames(ret_sig)<-label
	}
	return(ret_sig)
}

#' generate a signal on a timestamp
#' 
#' This will generate a signal on a specific timestamp or at a specific time every day, week, weekday, etc.
#' 
#' @param label text label to apply to the output
#' @param data data to apply formula to
#' @param timestamp either a POSIXct-based object, or a character string denoting a 24-hour time (e.g. "09:00", "16:00")
#' @param on only used if \code{timestamp} is character; passed to \code{\link[xts]{split.xts}}, therefore \code{on}
#' may be a character describing the time period as listed in \code{\link[xts]{endpoints}}, or a vector coercible to
#' factor (e.g. \code{\link[xts]{.indexday}})
#' @export
sigTimestamp <- function(label, data=mktdata, timestamp, on="days") {

   # default label
   if(missing(label))
     label <- "timestamp"

   # initialize ret$timestamp to the index of mktdata
   ret <- .xts(logical(nrow(data)), .index(data), dimnames=list(NULL,label))

   # default if timestamp and on are missing
   if(missing(timestamp) && missing(on)) {
     ret[end(data)] <- TRUE
     return(ret)
   }

   # timestamp can be a time-based timestamp
   if(is.timeBased(timestamp)) {
     after.sig <- .firstThreshold(index(data), timestamp, "lt")
     if(length(after.sig) != 0)
       ret$timestamp[after.sig] <- TRUE
   } else
   # timestamp can be a timestamp of day
   if(is.character(timestamp)) {
     time.str <- paste("T00:00/T",timestamp,sep="")
     funDay <- function(x, time.str) last(x[time.str])
     funOn  <- function(y, time.str) lapply(y, funDay, time.str)
     after.sig <- do.call(rbind, lapply(split(ret, on), funDay, time.str))
     if(nrow(after.sig) != 0)
       ret[index(after.sig)] <- TRUE
   } else {
     stop("don't know how to handle 'timestamp' of class ", class(timestamp))
   }
   return(ret)
}

#TODO Going Up/Going Down maybe better implemented as slope/diff() indicator, then coupled with threshold signal 
#TODO set/reset indicator/signal for n-periods since some other signal is set, or signal set for n periods

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2012
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
