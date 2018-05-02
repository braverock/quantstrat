
#' default order sizing function 
#' 
#' default function performs no operation (NoOp), returns orderqty
#' 
#' if orderqty 'all', will only work on an exit rule type, otherwize orderqty is zero.
#'  
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @param orderqty numeric quantity of the desired order, modified by osFUN
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param ... any other passthru parameters
#' @param ruletype one of "risk","order","rebalance","exit","enter", see \code{\link{add.rule}}
#' @export
osNoOp <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...)
{
    if(orderqty == 'all' && !(ruletype %in% c('exit','risk'))
    || orderqty == 'trigger' && ruletype != 'chain')
    {
        stop(paste("orderqty 'all'/'trigger' would produce nonsense, maybe use osMaxPos instead?\n",
                   "Order Details:\n",
                   'Timestamp:',timestamp,
                   'Qty:',orderqty,
                   'Symbol:',symbol)
        )
    }
	return(orderqty)
}


#' add position and level limits at timestamp
#' 
#' Many strategies will not be allowed to trade unconstrained.
#' Typically, constraints will include position sizing limits.
#' 
#' \code{addPosLimit} works with \code{\link{osMaxPos}} to set 
#' and enforce position sizing limits.  If \code{levels=1},
#' then all order sizing will be in the complete control of
#' the strategy rules, up to the maximum position specified
#' using \code{addPosLimit}'s \code{maxpos} and \code{minpos} 
#' arguments.
#'   
#' Simply setting a position limit will not do anything.
#' The strategy entry rules also need to specify an
#' the use of order sizing function \code{\link{osMaxPos}}, 
#' most typically as an argument to \code{\link{ruleSignal}}.
#' 
#' levels are a simplification of more complex (proprietary) 
#' techniques sometimes used for order sizing.  
#' the max orderqty returned will be the limit/levels.
#' Obviously the strategy rules could ask for smaller order sizes, 
#' but this is the default.  If you don't want to use levels, set 
#' them to 1.
#' 
#' It is also important to note that position limits 
#' may be time-varying.  
#' If you only want one static maximum position limit, then
#' call \code{addPosLimit} with a \code{timestamp} argument
#' before your first trade.  If you want time varying limits,
#' typically in response to some rebalancing rule or risk
#' rule, set the \code{timestamp} at the time which you wish 
#' the limit to take effect.
#'  
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @param maxpos numeric maximum long position for symbol 
#' @param longlevels numeric number of levels
#' @param minpos numeric minimum position, default -minpos (short allowed use negative number)
#' @param shortlevels numeric number of short levels, default longlevels 
#' @seealso 
#' \code{\link{osMaxPos}}
#' \code{\link{getPosLimit}}
#' @export
addPosLimit <- function (portfolio, symbol, timestamp, maxpos, longlevels = 1, minpos = -maxpos, shortlevels = longlevels) 
{
	portf <- .getPortfolio(portfolio)
  #catch error where maxpos/minpos have length greater than 1
	if(length(maxpos)>1) maxpos <- maxpos[,1]
	if(length(minpos)>1) minpos <- minpos[,1]
	newrow <- xts(cbind(maxpos, longlevels, minpos, shortlevels), order.by = as.POSIXct(timestamp))
	tc <- try(colnames(newrow) <- c("MaxPos", "LongLevels", "MinPos", "ShortLevels"))
# 	if(inherits(tc, 'try-error')) {
#     print(symbol)
#     print(newrow)
#     browser()
# 	}
	if (is.null(portf$symbols[[symbol]]$PosLimit)) {
		portf$symbols[[symbol]]$PosLimit <- newrow
	} else {
		if (is.null(portf$symbols[[symbol]]$PosLimit[timestamp])) {
			portf$symbols[[symbol]]$PosLimit[timestamp] <- newrow
		} else {
			portf$symbols[[symbol]]$PosLimit <- rbind(portf$symbols[[symbol]]$PosLimit,	newrow)
		}
	}
	assign(paste("portfolio", portfolio, sep = "."), portf, envir = .blotter)
}

#' get position and level limits on timestamp
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @seealso \code{\link{addPosLimit}},\code{\link{osMaxPos}}
#' @export
getPosLimit <- function(portfolio, symbol, timestamp){
    portf<-getPortfolio(portfolio)
    # try to get on timestamp, otherwise find the most recent
    toDate = format(timestamp, '::%Y-%m-%d %H:%M:%OS6')
    PosLimit = last(portf$symbols[[symbol]]$PosLimit[toDate])
    return(PosLimit)
}

#' order sizing function for position limits and level sizing 
#' 
#' levels are a simplification of more complex (proprietary) 
#' techniques sometimes used for order sizing.  
#' the max orderqty returned will be the limit/levels
#' Obviously the strategy rules could ask for smaller order sizes, 
#' but this is the default.  If you don't want to use levels, set 
#' them to 1.
#' 
#' \code{orderqty='all'} in a risk rule will return an order size 
#' appropriate to flatten the current position.
#' 
#' @param data an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @param orderqty numeric quantity of the desired order, modified by osFUN
#' @param ordertype one of "market","limit","stoplimit", or "stoptrailing"
#' @param orderside one of either "long" or "short" 
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param ruletype one of "risk","order","rebalance","exit","enter", see \code{\link{add.rule}}
#' @param digits call \code{\link{round}} to round min/max clip size to specified number of digits after decimal place
#' @param ... any other passthru parameters
#' @seealso \code{\link{addPosLimit}},\code{\link{getPosLimit}}
#' @export
#' @note 
#' TODO integrate orderqty='all' into osMaxPos for non-risk exit orders by combining side and pos for exits
osMaxPos <- function(data, timestamp, orderqty, ordertype, orderside, portfolio, symbol, ruletype, digits=0, ...){
	# check for current position
    pos<-getPosQty(portfolio,symbol,timestamp)
    # check against max position
    PosLimit<-getPosLimit(portfolio,symbol,timestamp)
    if(is.null(PosLimit))
        stop(paste('no position limit defined for portfolio', portfolio))
	
	#TODO add handling for orderqty='all', and handle risk ruletype separately
    if(is.character(orderqty)) {
        if(ruletype == "risk" && orderqty == "all") {
            orderqty <- pos * -1
        } else {
            stop("orderqty ", orderqty, " is not supported for non-risk ruletypes")
        }
    }
	
	#check order side
	if(is.null(orderside) && !isTRUE(orderqty == 0)) {
		curqty<-pos
		if (curqty>0 ){
			#we have a long position
			orderside<-'long'
		} else if (curqty<0){
			#we have a short position
			orderside<-'short'
		} else {
			# no current position, which way are we going?
			if (orderqty>0) 
				orderside<-'long'
			else
				orderside<-'short'
		}
	}
    # TODO: need to ensure that orderside and pos align if orderside != NULL?
    # i.e. it's possible user passes orderside = "long" when pos < 0.
	
	# check levels
	# buy long
    if(orderqty>0 && orderside=='long') {
        # note no round lots for max clip
        clip <- round(PosLimit[,"MaxPos"] / PosLimit[,"LongLevels"], digits)

        if ((orderqty+pos) > PosLimit[,"MaxPos"]) {
            # this order would put us beyond the MaxPos limit
            orderqty <- PosLimit[,"MaxPos"] - pos
        }
        # check clip size
        orderqty <- min(orderqty, clip)

        return(as.numeric(orderqty))  # strip attributes
    }
    
    #sell long
    if(orderqty<0 && orderside=='long') {
        if(ruletype=='risk' || (orderqty+pos)>=0) {
            return(orderqty)
        } else {
            orderqty <- -pos #flatten position, don't cross through zero
            #TODO add code to break into two orders?
            return(orderqty)
        }
    }
    
    #sell short
    if(orderqty<0 && orderside=='short') {
        # note no round lots for max clip
        clip <- round(PosLimit[,"MinPos"] / PosLimit[,"ShortLevels"], digits)

        if ((orderqty+pos) < PosLimit[,"MinPos"]) {
            # this order would put us beyond the MinPos limit
            orderqty <- PosLimit[,"MinPos"] - pos
        }
        # check clip size
        orderqty <- max(orderqty, clip)

        return(as.numeric(orderqty))  # strip attributes
    }
    
    #buy cover short
    if(orderqty>0 && orderside=='short') {
        if(ruletype=='risk' || (orderqty+pos)<=0) {
            return(orderqty)
        } else {
            orderqty <- -pos #flatten position, don't cross through zero
            #TODO add code to break into two orders?
            return(orderqty)
        }
    }
    
    # fall through
    return(0)
}

#TODO ruleRiskPosLimits to check for overfilled position and scale back

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2015
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson,
# Jeffrey Ryan, Joshua Ulrich, and Garrett See
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
