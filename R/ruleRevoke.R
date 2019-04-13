#' rule to revoke(cancel) an unfilled limit order on a signal
#' 
#' As described elsewhere in the documentation, quantstrat models 
#' \emph{orders}.  All orders in quantstrat are GTC orders, unless a
#' time in force is specified, which means that unfilled limit orders 
#' have to be canceled manually or replaced by other orders.
#' 
#' This function is used for revoking or canceling the orders based on a signal.  
#' Order status will be changed to 'revoked', to separate it from cancelations or
#' replacements from other causes.  This may make it easier it decipher the order book 
#' to figure out what the strategy ewas doing.
#' 
#' @param data an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @param sigcol column name to check for signal
#' @param sigval signal value to match against
#' @param orderside one of either "long" or "short", default NULL, see details 
#' @param orderset tag to identify an orderset
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to revoke orders for
#' @param ruletype must be 'risk' for ruleRevoke, see \code{\link{add.rule}}
#' @param ... any other passthru parameters
#' @author Niklas Kolster, Jan Humme
#' @seealso \code{\link{osNoOp}} , \code{\link{add.rule}}
#' @aliases ruleCancel
#' @export ruleRevoke
#' @export ruleCancel
ruleRevoke <- ruleCancel <- function(data=mktdata, timestamp, sigcol, sigval, orderside=NULL, orderset=NULL, portfolio, symbol, ruletype, ...)
{
    if (ruletype != 'risk') {
        stop('Ruletype for ruleRevoke or ruleCancel must be "risk".')
    }

    # Get row index of timestamp for faster subsetting
    if (hasArg(curIndex)) {
        curIndex <- eval(match.call(expand.dots=TRUE)$curIndex, parent.frame())
    } else {
        curIndex <- mktdata[timestamp,which.i=TRUE]
    }

    if (curIndex > 0 && curIndex <= nrow(mktdata) &&
        !is.na(mktdata[curIndex,sigcol]) && mktdata[curIndex,sigcol] == sigval)
    {
        updateOrders(portfolio=portfolio, 
                  symbol=symbol, 
                  timespan=timespan,
                  side=orderside,
                  orderset=orderset, 
                  oldstatus='open', 
                  newstatus='canceled',
                  statustimestamp=timestamp
        )
    }
}

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2015
# Niklas Kolster, Jan Humme
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: ruleRevoke.R 1233 2012-11-01 12:52:25Z braverock $
#
###############################################################################
