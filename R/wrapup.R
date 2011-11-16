#' Remove objects associated with a strategy
#'
#' Remove the order_book, account, and portfolio of given \code{name}.
#' @param name name of the portfolio/account/order book to clean up. (Default='default')
#' @param silent silence warnings about trying to remove objects that do not exist. (Default=TRUE)
#' @return invisible -- called for side-effect
#' @examples
#' \dontrun{
#' # make some things to remove
#' strategy("example", store=TRUE)
#' initPortf('example', stock('SPY', currency("USD")))
#' initAcct('example', 'example')
#' initOrders('example', 'SPY')
#' #Now remove them
#' rm.strat('example')
#' }
#' @export
rm.strat <- function(name='default', silent=TRUE) {
    if (is.strategy(name)) name <- name[['name']]
    if (silent) {
        suppressWarnings({
            try(rm(list=paste("order_book",name,sep="."), pos=.strategy))
            try(rm(list=paste(c("account", "portfolio"), name, sep="."), pos=.blotter))
            try(rm(list=name, pos=.strategy))
        })
    } else {
        try(rm(list=paste("order_book",name,sep="."), pos=.strategy))
        try(rm(list=paste(c("account", "portfolio"), name, sep="."), pos=.blotter))
        try(rm(list=name, pos=.strategy))
    }
}

#' run standard and custom strategy wrapup functions such as updating portfolio, account, and ending equity
#' 
#' \code{\link{updateStrategy}} will run a series of common wrapup functions at the 
#' beginning of an \code{\link{applyStrategy}} call.  This function allows the user to 
#' add arbitrary wrapup functions to the sequence.
#' 
#' These arbitrary functions will be added to the \code{update} slot of the strategy object
#' and when \code{applyStrategy} is evaluated, the arbitrary wrapup functions will
#' be evaluated \emph{before} the standardized functions.
#' 
#' For example, if you are working with high frequency data, it would be common 
#' to \emph{mark the book} on a lower frequency, perhaps minutes, hours, or even days,
#' rather than tick.  A custom wrapup function could take your high frequency 
#' data and transform it to lower frequency data before the call to \code{\link{updatePortf}}. 
#' 
#' @param portfolio string identifying a portfolio
#' @param account string identifying an account. Same as \code{portfolio} by default
#' @param Symbols character vector of names of symbols whose portfolios will be updated
#' @param Dates optional xts-style ISO-8601 time range to run updatePortf over, default NULL (will use times from Prices)
#' @param Prices optional xts object containing prices and timestamps to mark the book on, default NULL
#' @param update.Portf TRUE/FALSE if TRUE (default) a call will be made to \code{updatePortf}
#' @param update.Acct TRUE/FALSE if TRUE (default) a call will be made to \code{updateAcct}
#' @param update.EndEq TRUE/FALSE if TRUE (default) a call will be made to \code{updateEndEq}
#' @param showEq TRUE/FALSE if TRUE (default) ending equity will be printed to the screen
#' @param chart TRUE/FALSE if TRUE (default) a call will be made to \code{chart.Posn}
#' @param \dots any other passthrough parameters
#' @seealso \code{\link{updatePortf}}, \code{\link{updateAcct}}, \code{\link{updateEndEq}}, \code{\link{chart.Posn}}
#' @author Garrett See, Brian Peterson
#' @export
updateStrategy <- 
function(portfolio='default', 
         account=portfolio, 
         Symbols=NULL, 
         Dates=NULL, 
         Prices=NULL,
         update.Portf=TRUE,
         update.Acct=TRUE,
         update.EndEq=TRUE,
         showEq=TRUE,
         chart=TRUE,
         ...)
{
    
    #first do whatever the user stuck in this wrapup slot...
    for (wrapup_o in strategy$wrapup){
        if(!is.function(get(wrapup_o$name))){
            message(paste("Skipping wrapup",wrapup_o$name,"because there is no function by that name to call"))
            next()      
        }
        
        if(!isTRUE(wrapup_o$enabled)) next()
        
        # see 'S Programming p. 67 for this matching
        fun<-match.fun(wrapup_o$name)
        
        .formals  <- formals(fun)
        onames <- names(.formals)
        
        pm <- pmatch(names(wrapup_o$arguments), onames, nomatch = 0L)
        #if (any(pm == 0L))
        #    warning(paste("some arguments stored for",wrapup_o$name,"do not match"))
        names(wrapup_o$arguments[pm > 0L]) <- onames[pm]
        .formals[pm] <- wrapup_o$arguments[pm > 0L]       
        
        # now add arguments from parameters
        if(length(parameters)){
            pm <- pmatch(names(parameters), onames, nomatch = 0L)
            names(parameters[pm > 0L]) <- onames[pm]
            .formals[pm] <- parameters[pm > 0L]
        }
        
        #now add dots
        if (length(nargs)) {
            pm <- pmatch(names(nargs), onames, nomatch = 0L)
            names(nargs[pm > 0L]) <- onames[pm]
            .formals[pm] <- nargs[pm > 0L]
        }
        .formals$... <- NULL
        
        do.call(fun,.formals)
    }            
    
    out <- list()
    if(isTRUE(update.Portf)){
        out[[paste('portfolio',portfolio,sep='.')]] <- updatePortf(Portfolio=portfolio, Symbols=Symbols, Dates=Dates, Prices=Prices,...=...)
    }
    if(isTRUE(update.Acct)){
        out[[paste('account',portfolio,sep='.')]] <- updateAcct(name=account,Dates=Dates,...=...) 
    }
    if(isTRUE(update.EndEq)){
        updateEndEq(Account=account,Dates=Dates,...=...)
        if(showEq) cat('EndingEq: ', getEndEq(Account=account,Date=Sys.time()), '\n')
    }
    if(isTRUE(chart)){
        for (symbol in names(getPortfolio(portfolio)$symbols) ){
            dev.new()
            chart.Posn(Portfolio=portfolio, Symbol=symbol,...=...)
        }
    }
    
    if (out[[1]] == out[[2]]) out[[1]]
    else out
}

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2011
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, Garrett See, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
