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
#' The 'standard wrapup functions included are:
#' \describe{
#'      \item{update.Portf}{ if TRUE, will call \code{\link[blotter]{updatePortf}}
#'      to mark the book in the portfolio.
#'      }
#'      \item{update.Acct}{ if TRUE, will call \code{\link[blotter]{updateAcct}}
#'      to mark the blotter account for this test.
#'      }
#'      \item{update.EndEq}{ if TRUE, will call \code{\link[blotter]{updateEndEq}}
#'      to update the account equity after all other accounting has been completed.
#'      }
#' }
#' 
#' @param strategy object of type \code{strategy} to initialize data/containers for
#' @param portfolio string identifying a portfolio
#' @param account string identifying an account. Same as \code{portfolio} by default
#' @param Symbols character vector of names of symbols whose portfolios will be updated
#' @param parameters named list of parameters to be applied during evaluation of the strategy, default NULL
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
function(strategy,
         portfolio='default', 
         account=portfolio, 
         Symbols=NULL,
         parameters=NULL,
         Dates=NULL, 
         Prices=NULL,
         update.Portf=TRUE,
         update.Acct=TRUE,
         update.EndEq=TRUE,
         showEq=TRUE,
         chart=TRUE,
         ...)
{

    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object or the name of an object of type 'strategy'.")
        store=TRUE    
    } 
    
    out <- list()
    
    #first do whatever the user stuck in this wrapup slot...
    if(length(strategy$wrapup)>0){
        for (wrapup_o in strategy$wrapup){
            if(is.function(wrapup_o$name)) {
                wrapup_oFun <- wrapup_o$name
            } else {
                if(exists(wrapup_o$name, mode="function")) {
                    wrapup_oFun <- get(wrapup_o$name, mode="function")
                } else {
                    message("Skipping wrapup function ", wrapup_o$name,
                            " because there is no function by that name to call.")
                }
            }
            
            if(!isTRUE(wrapup_o$enabled)) next()
            
            # replace default function arguments wrapup_o$arguments
            .formals <- formals(wrapup_o$name)
            .formals <- modify.args(.formals, wrapup_o$arguments, dots=TRUE)
            # now add arguments from parameters
            .formals <- modify.args(.formals, parameters, dots=TRUE)
            # now add dots
            .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
            # remove ... to avoid matching multiple args
            .formals$`...` <- NULL
            
            out[[wrapup_o$name]] <- do.call(wrapup_oFun, .formals)
        }            
    }
    
    
    if(isTRUE(update.Portf)){
        out[[paste('portfolio',portfolio,sep='.')]] <- updatePortf(Portfolio=portfolio, Symbols=Symbols, Dates=Dates, Prices=Prices,...=...)
    }
    if(isTRUE(update.Acct)){
        out[[paste('account',account,sep='.')]] <- updateAcct(name=account,Dates=Dates,...=...) 
    }
    if(isTRUE(update.EndEq)){
        updateEndEq(Account=account,Dates=Dates,...=...)
        if(showEq) cat('Ending Account Equity: ', getEndEq(Account=account,Date=Sys.time()), '\n')
    }
    if(isTRUE(chart)){
        for (symbol in ls(getPortfolio(portfolio)$symbols) ){
            dev.new()
            chart.Posn(Portfolio=portfolio, Symbol=symbol,...=...)
        }
    }
    
    if (length(out)>1 && out[[1]] == out[[2]]) out[[1]]
    else out
}

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2015
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, Garrett See, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
