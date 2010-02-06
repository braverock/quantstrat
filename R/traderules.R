#' add a rule to a strategy
#' 
#' Rules will be processed in a very particular manner, so it bears going over.
#' 
#' First, rules are either path dependent or non-path-dependent.  path dependent rules 
#' will be processed in every time increment for the \code{mktdata} passed into
#' \code{\link{applyStrategy}}.  Non path dependent rules will likely be quite rare in real life, 
#' and will be applied after indicators and signals, and before path-dependent rules are processed.
#' 
#' All rules have a \code{type}.  These may be any of:
#' \itemize{
#'   \item{risk }{rules that check and react to risk of positions, may stop all other rule execution temporarily or permanently}
#'   \item{order }{order processing of any open orders at time t, always path-dependent}
#'   \item{rebalance }{rules executed specifically in a portfolio context, unnecessary in univariate strategies}
#'   \item{exit}{rules to determine whether to exit a position}
#'   \item{enter}{rules to determine whether to enter a position}
#' }  
#' The rules will be executed by type, in the order listed above.  
#' Multiple rules of each type may be defined, as with signals and indicators, 
#' they will be executed in order by index number with any other rules sharing the same 
#' type.
#' 
#' We anticipate that rules will be the part of a strategy most likely to 
#' not have suitable template code included with this package, as every strategy 
#' and environment are different, especially in this respect.  
#' We will attempt to provide enough examples and generic rules to give strategy
#' authors a place to start.
#'    
#' @param strategy an object of type 'strategy' to add the rule to
#' @param name name of the rule, must correspond to an R function
#' @param arguments default arguments to be passed to an rule function when executed
#' @param type 
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the rule is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific rule, the index number in the $rules list to update
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @export
add.rule <- function(strategy, name, arguments, label=NULL, type=c(NULL,"risk","order","rebalance","exit","entry"), ..., enabled=TRUE, indexnum=NULL, path.dep=TRUE, store=FALSE) {
    if(!is.strategy(strategy)) stop("You must pass in a strategy object to manipulate")
    type=type[1]
    if(is.null(type)) stop("You must specify a type")
    tmp_rule<-list()
    tmp_rule$name<-name
    tmp_rule$type<-type
    # TODO change this to a separate slot!!!!!
    tmp_rule$enabled<-enabled
    if (!is.list(arguments)) stop("arguments must be passed as a named list")
    arguments$label=label
    tmp_rule$arguments<-arguments
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$rules[type])+1
    tmp_rule$call<-match.call()
    strategy$rules[type][[indexnum]]<-tmp_rule
    
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
# $Id$
#
###############################################################################
