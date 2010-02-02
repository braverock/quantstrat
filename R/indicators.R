
#' add an indicator to a strategy
#' @param strategy an object of type 'strategy' to add the indicator to
#' @param name name of the indicator, must correspond to an R function
#' @param arguments default arguments to be passed to an indicator function when executed
#' @param ... any other passthru parameters
#' @param indexnum if you are updating a specific indicator, the index number in the $indicators list to update
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @export
add.indicator <- function(strategy, name, arguments, ..., indexnum=NULL,store=FALSE) {
    if(!is.strategy(strategy)) stop("You must pass in a strategy object to manipulate")
    tmp_indicator<-list()
    tmp_indicator$name<-name
    if (!is.list(arguments)) stop("arguments must be passed as a named list")
    tmp_indicator$arguments<-arguments
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$indicators)+1
    tmp_indicator$call<-match.call()
    strategy$indicators[[indexnum]]<-tmp_indicator
    
    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
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
