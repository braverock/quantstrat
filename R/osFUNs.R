
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
osNoOp <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...){
	if(orderqty=='all'){
		if (ruletype=='exit') {
			orderqty=-1*getPosQty(Portfolio=portfolio,Symbol=symbol,Date=timestamp)
		} else {
			message("orderqty 'all' would produce nonsense, maybe use osMaxPos instead?")
			orderqty=0
		}
	} 
	return(orderqty)
}


#' add position and level limits at timestamp
#' 
#' levels are a simplification of more complex (proprietary) 
#' techniques sometimes used for order sizing.  
#' the max orderqty returned will be the limit/levels
#' Obviously the strategy rules could ask for smaller order sizes, 
#' but this is the default.  If you don't want to use levels, set 
#' them to 1.
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
	portf <- getPortfolio(portfolio)
	newrow <- xts(cbind(maxpos, longlevels, minpos, shortlevels), order.by = as.POSIXct(timestamp))
	colnames(newrow) <- c("MaxPos", "LongLevels", "MinPos", "ShortLevels")
	
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
    toDate = paste('::', timestamp, sep="")
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
#' @param ... any other passthru parameters
#' @seealso \code{\link{addPosLimit}},\code{\link{getPosLimit}}
#' @export
#' @note 
#' TODO integrate orderqty='all' into osMaxPos for non-risk exit orders by combining side and pos for exits
osMaxPos <- function(data, timestamp, orderqty, ordertype, orderside, portfolio, symbol, ruletype, ...){
	# check for current position
    pos<-getPosQty(portfolio,symbol,timestamp)
    # check against max position
    PosLimit<-getPosLimit(portfolio,symbol,timestamp)
	
	#TODO add handling for orderqty='all', and handle risk ruletype separately
	
	#check order side
	if(is.null(orderside) & !isTRUE(orderqty == 0)){
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
	
	# check levels
	# buy long
    if(orderqty>0 & orderside=='long'){
        if ((orderqty+pos)<PosLimit[,"MaxPos"]) {
            #we have room to expand the position
            if(orderqty<=(PosLimit[,"MaxPos"]/PosLimit[,"LongLevels"]) ) {
                orderqty=orderqty
            } else {
                orderqty = round(PosLimit[,"MaxPos"]/PosLimit[,"LongLevels"],0) #note no round lots
            }
        } else {
            # this order would put us over the MaxPos limit
            orderqty<-ifelse((PosLimit[,"MaxPos"]-pos)<=round(PosLimit[,"MaxPos"]/PosLimit[,"LongLevels"],0),PosLimit[,"MaxPos"]-pos, round(PosLimit[,"MaxPos"]/PosLimit[,"LongLevels"],0)) 
            if(orderqty+pos>PosLimit[,"MaxPos"]) orderqty <- PosLimit[,"MaxPos"]-pos
        }
        return(orderqty)
    }
    
    #sell long
    if(orderqty<0 & orderside=='long'){
		if(ruletype=='risk'){
          if(orderqty=='all') return(-1*pos)
          else return(orderqty)
        } 
		if ((orderqty+pos)>=0) {
            return(orderqty)
        } else {
			orderqty <- -pos #flatten position, don't cross through zero
            #TODO add code to break into two orders?
            return(orderqty)
        }
    }
    
    #sell short
    if(orderqty<0 & orderside=='short'){
        if ((orderqty+pos)>PosLimit[,"MinPos"]) {
            #we have room to expand the position
            if(orderqty<=(PosLimit[,"MinPos"]/PosLimit[,"ShortLevels"]) ) {
                orderqty=orderqty
            } else {
                orderqty = round(PosLimit[,"MinPos"]/PosLimit[,"ShortLevels"],0) #note no round lots
            }
        } else {
            # this order would put us over the MinPos limit
            orderqty<-ifelse((PosLimit[,"MinPos"]-pos)>=round(PosLimit[,"MinPos"]/PosLimit[,"ShortLevels"],0),PosLimit[,"MinPos"]-pos, round(PosLimit[,"MinPos"]/PosLimit[,"ShortLevels"],0)) 
            if(orderqty+pos>PosLimit[,"MaxPos"]) orderqty <- PosLimit[,"MinPos"]-pos
        }
        return(orderqty)
    }
    
    #buy cover short
    if(orderqty>0 & orderside=='short'){
        if(ruletype=='risk'){
            if(orderqty=='all') return(-1*pos)
            else return(orderqty)
        } 
        if ((orderqty+pos)<=0) {
            return(orderqty)
        } else {
            orderqty<-pos #flatten position, don't cross through zero
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
# Copyright (c) 2009-2011
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson,
# Jeffrey Ryan, Joshua Ulrich, and Garrett See
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
