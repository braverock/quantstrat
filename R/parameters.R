#' @export
add.parameter <- 
function (strategy, 
          type = c('indicator','signal'), 
          add.to.name,
          method = c('lookup','lookup.range','calc'), 
          arguments = NULL, 
          label = NULL,
          ...,
          store=FALSE) 
{
    if(!is.strategy(strategy)) stop("You must pass in a strategy object to manipulate")
    # perhaps I should add parameters and parameter.args as arguments to the constructors...
    
    tmp.param<-list()
    
    type=type[1] #this should probably come out eventually
    
    method = method[1] #only use the first if the user didn't specify, or over-specified
    
    if(is.null(label)) {
        label<-method
    }
    tmp.param$label <- label
    tmp.param$method <- method
    tmp.param$call <- match.call()
    tmp.param$arguments <- arguments
    class(tmp.param)<-'quantstrat.parameter'
    
    switch(type,
            indicator = {type='indicators'},
            signal = {type='signals'},
            rule = {type='rules'}) #NOTE rules not supported yet, since they need a rule type too
    
    # need to think about how to create a 'parameters' list, and whether 
    # it should be at the strategy level or lower down, on the individual 
    # signal/indicator/rule
    
    if(!is.list(strategy[[type]][[add.to.name]]$parameters)){
        strategy[[type]][[add.to.name]]$parameters <- list()
    }
    strategy[[type]][[add.to.name]][['parameters']][[method]] <- tmp.param
    
    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
}


paramLookup <- function(strategy, symbol , type, name, parameter, ...) {
    # should take in a strategy and parameter object, and return an argument list for 'symbol'
    #as.pairlist(paramTable[,symbol]
    paramTable<-get(paste(strategy,type,name,'table',pos=.strategy))
    as.pairlist(paramTable[,symbol])
}

#' @export
add.paramLookupTable <- function(strategy, type, name, paramTable){
    assign(paste(strategy,type,name,'table',pos=.strategy),paramTable)
}

#' get parameterized arguments list out of the strategy environment
#' @param strategy 
#' @param symbol 
#' @param type 
#' @param name 
getParams <- function (strategy, symbol, type, name)
{
    
    params <- strategy[[type]][[name]]$parameters
    param.ret<-list()
    for (param in params) {
        switch(param$method,
                lookup = {param.ret<-c(param.ret,paramLookup(strategy,symbol,parameter=param))},
                lookup.range = {},
                calc = {},
                {warning("parameter method",param$method,'not recognized for',type,name); next()}
              )
    }
    # return an arguments list back to the 'apply*' fn
    return(param.ret)
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
