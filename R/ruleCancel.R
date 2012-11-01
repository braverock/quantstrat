#' rule to cancel an unfilled limit order on a signal
#' 
#' As described elsewhere in the documentation, quantstrat models 
#' \emph{orders}.  All orders in quantstrat are GTC orders, which means that
#' unfilled limit orders have to be cancelled manually or replaced by other orders.
#' 
#' This function is used for canceling the orders based on a signal.
#' 
#' @param data an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @param sigcol column name to check for signal
#' @param sigval signal value to match against
#' @param orderside one of either "long" or "short", default NULL, see details 
#' @param orderset tag to identify an orderset
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to cancel orders for
#' @param ruletype must be 'risk' for ruleCancel, see \code{\link{add.rule}}
#' @author Niklas Kolster
#' @seealso \code{\link{osNoOp}} , \code{\link{add.rule}}
#' @export
ruleCancel <- function(data=mktdata, timestamp, sigcol, sigval, orderside=NULL, orderset=NULL, portfolio, symbol, ruletype)
{
 

  if (ruletype!='risk') {
    stop(paste('Ruletype can only be risk'))
  }
  
  if(is.null(orderset)) orderset=NA
    
  updateOrder(portfolio=portfolio, 
              symbol=symbol, 
              timespan=timespan,
              orderset=orderset, 
              newstatus='canceled'
             )
  
}

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2012
# Niklas Kolster
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
