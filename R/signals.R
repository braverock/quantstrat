
#' add a signal to a strategy
#' @param strategy an object of type 'strategy' to add the signal to
#' @param name name of the signal, must correspond to an R function
#' @param arguments default arguments to be passed to an signal function when executed
#' @param label arbitrary text label for signal output, NULL default will be converted to '<name>.sig'
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the signal is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific signal, the index number in the $signals list to update
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @export
add.signal <- function(strategy, name, arguments, label=NULL, ..., enabled=TRUE, indexnum=NULL, store=FALSE) {
    if(!is.strategy(strategy)) stop("You must pass in a strategy object to manipulate")
    tmp_signal<-list()
    tmp_signal$name<-name
    if(is.null(label)) label = paste(name,"sig",sep='.')
    tmp_signal$label<-label
    tmp_signal$enabled<-enabled
    if (!is.list(arguments)) stop("arguments must be passed as a named list")
    arguments$label=label
    tmp_signal$arguments<-arguments
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$signals)+1
    tmp_signal$call<-match.call()
    strategy$signals[[indexnum]]<-tmp_signal
    
    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
}

#' apply the signals in the strategy to arbitrary market data
#' @param strategy an object of type 'strategy' to add the signal to
#' @param mktdata an xts object containing market data.  depending on signals, may need to be in OHLCV or BBO formats
#' @param indicators if indicator output is not contained in the mktdata object, it may be passed separately as an xts object or a list.
#' @param ... any other passthru parameters
#' @export
applySignals <- function(strategy, mktdata, indicators=NULL, ...) {
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
    nargs <-list(...)
    if(length(nargs)==0) nargs=NULL
    if (length('...')==0 | is.null('...')) {
        rm('...')
        nargs=NULL
    }
    
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
        
        # see 'S Programming p. 67 for this matching
        fun<-match.fun(signal$name)

        .formals  <- formals(fun)
        onames <- names(.formals)
        
        pm <- pmatch(names(signal$arguments), onames, nomatch = 0L)
        if (any(pm == 0L))
            warning(paste("some arguments stored for",signal$name,"do not match"))
        names(signal$arguments[pm > 0L]) <- onames[pm]
        .formals[pm] <- signal$arguments[pm > 0L]
        #now add dots
        if (length(nargs)) {
            pm <- pmatch(names(nargs), onames, nomatch = 0L)
            names(nargs[pm > 0L]) <- onames[pm]
            .formals[pm] <- nargs[pm > 0L]
        }
        .formals$... <- NULL
        
        tmp_val<-do.call(fun,.formals)
        if(is.null(names(tmp_val)) & ncol(tmp_val)==1) names(tmp_val)<-signal$label
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
    if(is.null(ret)) return(mktdata)
    else return(ret)
}


#' generate comparison signal
#' 
#' Currently, this function compares two columns.  
#' Patches to compare an arbitrary number of columns would be gladly accepted.
#' 
#' Comparison will be applied from the first to the second column in the \code{columns} vector.
#' 
#' @param label text label to apply to the output
#' @param data data to apply comparison to
#' @param columns named columns to apply comparison to
#' @param relationship one of c("gt","lt","eq","gte","lte") or reasonable alternatives
#' @example 
#' getSymbols("IBM")
#' sigComparison(label="Cl.gt.Op",data=IBM,columns=c("Close","Open"),"gt")
#' @export
sigComparison <- function(label,data, columns, relationship=c("gt","lt","eq","gte","lte")) {
    relationship=relationship[1] #only use the first one
    if (length(columns==2)){
        ret_sig=NULL
        columns <- match.names(colnames(data),columns)
        switch(relationship,
                '>'  =,
                'gt' = {ret_sig = data[,columns[1]] > data[,columns[2]]},
                '<'  =,
                'lt' = {ret_sig = data[,columns[1]] < data[,columns[2]]},
                'eq'     = {ret_sig = data[,columns[1]] == data[,columns[2]]}, #FIXME any way to specify '='?
                'gte' =,
                'gteq'=,
                'ge'     = {ret_sig = data[,columns[1]] >= data[,columns[2]]}, #FIXME these fail with an 'unexpected =' error if you use '>=' 
                'lte' =,
                'lteq'=,
                'le' 	 = {ret_sig = data[,columns[1]] <= data[,columns[2]]}
        )
    } else {
        stop("comparison of more than two columns not supported yet, patches welcome")
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
#' @export
sigCrossover <- function(label,data, columns, relationship=c("gt","lt","eq","gte","lte")) {
    ret_sig = ifelse(diff(sigComparison(label=label,data=data,columns=columns,relationship=relationship))==1,TRUE,NA)
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
    #should we only do this for one column?
    column<-match.names(colnames(data),column)
    direction=direction[1] # only use the first]
    #(Lag(IBM[,4],2)<Lag(IBM[,4],1)) & Lag(IBM[,4],1) >IBM[,4]
    switch(direction,
           "peak"   = { Lag(data[,column],2) < Lag(data[,column],1) & Lag(data[,column],1) > data[,column] } ,
           "bottom","valley" = { Lag(data[,column],2) > Lag(data[,column],1) & Lag(data[,column],1) < data[,column] }
    )
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
#' @param threshold numeric threhold to test for
#' @param relationship one of c("gt","lt","eq","gte","lte") or reasonable alternatives
#' @export
sigThreshold <- function(label, data, column, threshold=0, relationship=c("gt","lt","eq","gte","lte")) {
    relationship=relationship[1] #only use the first one
    ret_sig=NULL
    column <- match.names(colnames(data),column)
    switch(relationship,
            '>' =,
            'gt' = {ret_sig = data[,column] > threshold},
            '<' =,
            'lt' = {ret_sig = data[,column] < threshold},
            'eq'     = {ret_sig = data[,column] == threshold}, #FIXME any way to specify '='?
            'gte' =,
            'gteq'=,
            'ge'     = {ret_sig = data[,column] >= threshold}, #FIXME these fail with an 'unexpected =' error if you use '>='
            'lte' =,
            'lteq'=,
            'le'     = {ret_sig = data[,column] <= threshold}
    )
    colnames(ret_sig)<-label
    return(ret_sig)
}

#TODO Going Up/Going Down maybe better implemented as slope/diff() indicator, then coupled with threshold signal 
#TODO set/reset indicator/signal for n-periods since some other signal is set, or signal set for n periods

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
