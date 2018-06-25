#' rule to base trade size on a percentage of available equity.
#' 
#' This rule works with \code{\link{applyStrategy.rebalancing}} to set the
#' maximum trade size by calling \code{\link{addPosLimit}}.  
#' 
#' To use it, you need to specify it as (part of) a rule of type 'rebalance'.
#' note that  \code{\link{applyStrategy.rebalancing}} will expect a 
#' 'rebalance_on' argument to be included in the \code{arguments=list(...)} 
#' of the rule definition. 
#' 
#'
#' 
#' @param trade.percent max percentage of equity to allow the strategy to trade in this symbol
#' @param longlevels numeric number of levels
#' @param shortlevels numeric number of short levels, default longlevels 
#' @param digits if not NULL(the default), will call \code{\link{round}} with specified number of digits
#' @param refprice if not NULL(the default), will divide the calculated trade size by the reference price
#' @param portfolio text name of the portfolio to place orders in, typically set automatically
#' @param account text name of the account to fetch initial equity from, defaults to initEq in the search path
#' @param symbol identifier of the instrument to cancel orders for, typically set automatically
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on, typically set automatically 
#' @param \dots any other passthrough parameters
#' @seealso \code{\link{osMaxPos}} , 
#' \code{\link{applyStrategy.rebalancing}}, 
#' \code{\link{addPosLimit}}, 
#' \code{\link{add.rule}}
#' 
#' @examples 
#' # example rule definition
#' \dontrun{
#' add.rule(strategy.name, 'rulePctEquity',
#'         arguments=list(rebalance_on='months',
#'                        trade.percent=.02,
#'                        refprice=quote(last(getPrice(mktdata)[paste('::',curIndex,sep='')])[,1]),
#'                        digits=0
#'         ),
#'         type='rebalance',
#'         label='rebalance')
#' }
#' @export
rulePctEquity <- function (trade.percent=.02,
                           ...,
                           longlevels=1, 
                           shortlevels=1, 
                           digits=NULL,
                           refprice=NULL,
                           portfolio,
                           account=NULL,
                           symbol,
                           timestamp)
{
    dummy <- updatePortf(Portfolio=portfolio,
            Dates=paste('::',timestamp,sep=''))
    trading.pl <- sum(.getPortfolio(portfolio)$summary$Net.Trading.PL)
    if(!is.null(account)){
      init.eq <- attributes(get(paste0('account.', account), .blotter))$initEq
    } else {
      init.eq <- initEq
    }
    total.equity <- init.eq+trading.pl
    tradeSize <- total.equity * trade.percent
    if(length(refprice)>1) refprice <- refprice[,1]
    if(!is.null(refprice)) tradeSize <- tradeSize/refprice
    if(!is.null(digits)) tradeSize<-round(tradeSize,digits)
    addPosLimit(portfolio = portfolio, 
                symbol = symbol, 
                timestamp = timestamp, 
                maxpos = tradeSize, 
                longlevels = longlevels, 
                minpos = -tradeSize, 
                shortlevels = shortlevels)
}

ruleWeights <- function (weights=NULL,
        ...,
        longlevels=1, 
        shortlevels=1, 
        digits=NULL,
        portfolio,
        symbol,
        account=NULL,
        timestamp)
{
    #update portfolio
    dummy <- updatePortf(Portfolio=portfolio,
            Dates=paste('::',timestamp,sep=''))
    
    #get total account equity
    if(!is.null(account)){
        dummy <- updateAcct(name=account,
                Dates=paste('::',timestamp,sep=''))
        dummy <- updateEndEq(Account=account,
                Dates=paste('::',timestamp,sep=''))
        total.equity<-getEndEq(account)
    } else {
        trading.pl <- sum(.getPortfolio(portfolio)$summary$Net.Trading.PL)
        total.equity <- initEq+trading.pl
    }
    
    if(!is.null(digits)) tradeSize<-round(tradeSize,digits)
    addPosLimit(portfolio = portfolio, 
            symbol = symbol, 
            timestamp = timestamp, 
            maxpos = tradeSize, 
            longlevels = longlevels, 
            minpos = -tradeSize, 
            shortlevels = shortlevels)
}

#TODO weights rule that takes or calculates max position based on weights
#TODO active weights rule that moved from current positions to target positions
#TODO PortfolioAnalytics sizing
#TODO LSPM sizing

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
