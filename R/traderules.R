
#' default rule to generate a trade order on a signal
#' 
#' \code{pricemethod} may be one of 'market', 'opside', or 'maker' 
#' which will either try to get the price of the 'market' at \code{timestamp} and use this as the order price
#' or 'opside' which will use the 'ask' price if you're buying and the 'bid' price if you're selling, crossing 
#' the market at the time of order entry to attempt to set an aggressive price to get the trade.  
#' The 'maker' \code{pricemethod} will create a pair of orders for both bid and offer, modeling market making 
#' activities by having orders on both sides.  This will then create an Order.Set, and use the threshold to
#' set the prices for these orders.
#' 
#' If \code{threshold} is not numeric or \code{NULL} it should be the character string describing a function that can calculate a threshold.  
#' Ideally this will be a column lookup on a non-path-dependent indicator calculated in advance.
#' 
#' If \code{orderside} is NULL, the function will attempt to calculate the side from the current position 
#' (if any), the order quantity, and the order type.    
#'   
#' @param data an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @param sigcol column name to check for signal
#' @param sigval signal value to match against
#' @param orderqty numeric quantity of the desired order, or 'all', modified by osFUN
#' @param ordertype one of "market","limit","stoplimit", "stoptrailing", or "iceberg"
#' @param orderside one of either "long" or "short", default NULL, see details 
#' @param threshold numeric or function threshold to apply to trailing stop orders, default NULL, see Details
#' @param tmult if TRUE, threshold is a percent multiplier for \code{price}, not a scalar to be added/subtracted from price.  threshold will be dynamically converted to a scalar at time of order entry
#' @param replace TRUE/FALSE, whether to replace any other open order(s) on this portfolio symbol, default TRUE 
#' @param delay what delay to add to timestamp when inserting the order into the order book, in seconds
#' @param osFUN function or text descriptor of function to use for order sizing, default \code{\link{osNoOp}}
#' @param pricemethod one of 'market', 'opside', or 'maker', see Details
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param ... any other passthru parameters
#' @param ruletype one of "risk","order","rebalance","exit","entry", see \code{\link{add.rule}}
#' @param TxnFees numeric fees (usually negative) or function name for calculating TxnFees (processing happens later, not in this function)
#' @param prefer price method for getPrice
#' @param sethold boolean, puts entry Rule processing on hold, default FALSE
#' @seealso \code{\link{osNoOp}} , \code{\link{add.rule}}
#' @export
ruleSignal <- function(data=mktdata, timestamp, sigcol, sigval, orderqty=0, ordertype, orderside=NULL, threshold=NULL, tmult=FALSE, replace=TRUE, delay=0.0001, osFUN='osNoOp', pricemethod=c('market','opside','maker'), portfolio, symbol, ..., ruletype, TxnFees=0, prefer=NULL, sethold=FALSE)
{
    if(!is.function(osFUN)) osFUN<-match.fun(osFUN)
    #print(paste(symbol,timestamp))
    #print(data[timestamp][,sigcol])
    if (!is.na(data[timestamp][,sigcol]) && data[timestamp][,sigcol] == sigval) {
        #calculate order price using pricemethod
        pricemethod<-pricemethod[1] #only use the first if not set by calling function

		if(hasArg(prefer)) prefer=match.call(expand.dots=TRUE)$prefer
		else prefer = NULL

		#if(hasArg(TxnFees)) TxnFees=match.call(expand.dots=TRUE)$TxnFees
		#else TxnFees=0

		switch(pricemethod,
                opside = {
                    if (orderqty>0) 
                        prefer='ask'  # we're buying, so pay what they're asking
                    else
                        prefer='bid'  # we're selling, so give it to them for what they're bidding
                    orderprice <- try(getPrice(x=data, prefer=prefer))[timestamp]
				}, 
                market = {
                    if(is.BBO(mktdata)){
                        if (orderqty>0) 
                            prefer='bid'  # we're buying, so work the bid price
                        else
                            prefer='ask'  # we're selling, so work the ask price
                        
                    }
					orderprice <- try(getPrice(x=data, prefer=prefer))[timestamp] 
				},
				maker = {
					if(hasArg(price) & length(match.call(expand.dots=TRUE)$price)>1) {
						# we have prices, just use them
						orderprice <- try(match.call(expand.dots=TRUE)$price)
					} else {
						if(!is.null(threshold)) {
							baseprice<- last(getPrice(x=data)[timestamp]) # this should get either the last trade price or the Close
							if(hasArg(tmult) & isTRUE(match.call(expand.dots=TRUE)$tmult)) {
								baseprice<- last(getPrice(x=data)[timestamp]) # this should get either the last trade price or the Close
								# threshold is a multiplier of current price
								if (length(threshold)>1){
									orderprice <- baseprice * threshold # assume the user has set proper threshold multipliers for each side
								} else {
									orderprice <- c(baseprice*threshold,baseprice*(1+1-threshold)) #just bracket on both sides
								}
							} else {
								# tmult is FALSE or NULL, threshold is numeric
								if (length(threshold)>1){
									orderprice <- baseprice + threshold # assume the user has set proper threshold numerical offsets for each order
								} else {
									orderprice <- c(baseprice+threshold,baseprice+(-threshold)) #just bracket on both sides
								}
							}
						} else{
							# no threshold, put it on the averages?
							stop('maker orders without specified prices and without threholds not (yet?) supported')
							if(is.BBO(data)){

							} else {

							}
						}
					}
					if(length(orderqty)==1) orderqty <- c(orderqty,-orderqty) #create paired market maker orders at the same size
				}
        )
        if(inherits(orderprice,'try-error')) orderprice<-NULL
        if(length(orderprice>1) & !pricemethod=='maker') orderprice<-last(orderprice[timestamp])

        if(is.null(orderside) & !isTRUE(orderqty == 0)){
            curqty<-getPosQty(Portfolio=portfolio, Symbol=symbol, Date=timestamp)
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
        
        ## now size the order
        #TODO add fancy formals matching for osFUN
        orderqty <- osFUN(strategy=strategy, data=mktdata, timestamp=timestamp, orderqty=orderqty, ordertype=ordertype, orderside=orderside, portfolio=portfolio, symbol=symbol,...=...,ruletype=ruletype, orderprice=as.numeric(orderprice))
        
        
        if(!is.null(orderqty) & !orderqty == 0 & !is.null(orderprice)){
            addOrder(portfolio=portfolio, symbol=symbol, timestamp=timestamp, qty=orderqty, price=as.numeric(orderprice), ordertype=ordertype, side=orderside, threshold=threshold, status="open", replace=replace , delay=delay, tmult=tmult, ...=..., TxnFees=TxnFees)
        }
    }
    if(sethold) hold <<- TRUE
}

#TODO ruleORSignal
#TODO ruleANDSingnal
# perhaps this could be done using the approach of sigFormula, or perhaps we should advise users to use sigFormula to create a signal you can use ruleSignal on.  Thoughts?


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
#' @param minpos numeric minimum position, default 0 (short allowed use negative number)
#' @param shortlevels numeric number of short levels 
#' @seealso 
#' \code{\link{osMaxPos}}
#' \code{\link{getPosLimit}}
#' @export
addPosLimit <- function(portfolio, symbol, timestamp, maxpos, longlevels=1, minpos=0, shortlevels=0){
    portf<-getPortfolio(portfolio)
    newrow <- xts(cbind(maxpos, longlevels, minpos, shortlevels),order.by=as.POSIXct(timestamp))
	colnames(newrow)<-c("MaxPos","LongLevels","MinPos","ShortLevels")
	
    if(is.null(portf$symbols[[symbol]]$PosLimit)) {
        portf$symbols[[symbol]]$PosLimit <- newrow         
    } else {
        if(!is.null(portf[[symbol]]$PosLimit[timestamp])){
            # it exists already, so replace
            portf$symbols[[symbol]]$PosLimit[timestamp]<-newrow
        } else {
            # add a new row on timestamp
            portf$symbols[[symbol]]$PosLimit <- rbind(portf[[symbol]]$PosLimit,newrow)
        }
    }
    assign(paste("portfolio",portfolio,sep='.'),portf,envir=.blotter)
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
			orderqty<-pos #flatten position, don't cross through zero
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
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
