
#' default rule to generate a trade order on a signal
#' 
#' As described elsewhere in the documentation, quantstrat models 
#' \emph{orders}.  This function is the default provided rule function to 
#' generate those orders, which will be acted on later as they 
#' interact with your market data.
#' 
#' \code{pricemethod} may be one of 
#'      \describe{ 
#'          \item{'market', 'opside', or 'active'}{ will use the 'ask' price if you're buying and 
#'            the 'bid' price if you're selling, crossing the market at the time of 
#'            order entry to attempt to set an aggressive price to get the trade. }
#'         \item{'passive', 'work' or 'join'}{ which will join the 'bid' price if you are buying
#'            or join the 'ask' price if you are selling, passively working to make liquidity 
#'            at the prevailing market price without crossing the market at time of order entry}
#'         \item{'maker'}{will create a pair of orders for both bid and offer, modeling 
#'            market making activities by having orders on both sides.  
#'            This will then create an Order.Set, and use the \code{threshold} to set the prices for these orders.}
#'      } 
#' 
#' \code{orderqty} should be either numeric, or one of 'all'/'trigger'. 'all' can only be used with order of ruletype='exit' or 'risk', and will close the entire position. 'trigger' can only be used with ruletype='chain' and is exactly identical to 'all', except that the actual transaction is suppressed, and can be used to kick in a new order chain.
#' 
#' Where \code{ordertype} is a "stoptrailing" order and the market data is OHLC
#' data, the reference price used for determining whether an order price has
#' been crossed (ie. whether or not we can assume a trade would have occurred)
#' is the "Low" for negative quantity stoptrailing orders (ie. initial entry was
#' a long entry) and the bar's "High" for positive quantity stoptrailing orders
#' (ie. initial entry was a short entry).
#' For determining whether or not the stoptrailing order price needs to be re-
#' set, we look for any index which sets a higher "High" in the case of long
#' entries, and lower "Lows" in the case of short entries.
#' For BBO data, "bid" and "ask" are used in place of "Low" and "High"
#' 
#' If \code{threshold} is not numeric or \code{NULL} it should be the name of an indicator mktdata column holding the threshold values.
#' 
#' If \code{orderside} is NULL, the function will attempt to calculate the side from the current position 
#' (if any), the order quantity, and the order type.    
#'
#' If \code{prefer=NULL} then \link[quantmod]{getPrice} will grep for a column
#' name including "price" then "trade" then "close". If none are found, the
#' function will stop with an error message: "subscript out of bounds, no price
#' was discernible from the data". \link[quantmod]{getPrice} is attempting to
#' make a reasonable guess about the most likely 'price' the user might want,
#' though in practice this may lead to unexpected results.  It is often more 
#' prudent to specify \code{prefer} than to leave it to the default search order.  
#' 
#' @param mktdata an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @param sigcol column name to check for signal
#' @param sigval signal value to match against
#' @param orderqty numeric quantity of the desired order, or one of 'all'/'trigger', modified by osFUN
#' @param ordertype one of "market","limit","stoplimit", "stoptrailing", or "iceberg"
#' @param orderside one of either "long" or "short", default NULL, see details 
#' @param orderset tag to identify an orderset; if one order of the set is filled, all others are canceled
#' @param threshold numeric or name of indicator column in mktdata, default NULL, see Details
#' @param tmult if TRUE, threshold is a percent multiplier for \code{price}, not a scalar. Threshold is converted to a scalar by multiplying it with the price at the time of order entry (i.e. the scalar will not change if the order is updated, as in the case of a trailing stop), then it is added to the price just like a scalar threshold. 
#' @param replace TRUE/FALSE, whether to replace any other open order(s) on this portfolio symbol, default TRUE 
#' @param delay what delay to add to timestamp when inserting the order into the order book, in seconds
#' @param osFUN function or text descriptor of function to use for order sizing, default \code{\link{osNoOp}}
#' @param pricemethod determines how the order price will be calculated, see Details
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param ... any other passthru parameters
#' @param ruletype one of "risk","order","rebalance","exit","entry", see \code{\link{add.rule}}
#' @param TxnFees numeric fees (usually negative) or function name for calculating TxnFees (processing happens later, not in this function)
#' @param prefer price method for \link[quantmod]{getPrice} provided by \code{quantmod}, see Details
#' @param sethold boolean, puts entry Rule processing on hold, default FALSE
#' @param label rule label, default '', added by \code{\link{applyRules}}
#' @param order.price the order price to use, will overrule any mktdata lookup as well as chain.price (see below), meant to specify eg. a stop-loss price that is unrelated to the fill price (see chain.price)
#' @param chain.price the price that the parent order got filled for, used to pass to children in the order chain, will overrule all mktdata lookup, only meant for internal use really, default NULL
#' @param time.in.force timestamp time-in-force; either a time stamp, or a number of seconds, or 'GTC' / '', 'GTC' and '' both meaning 'Good Till Canceled'; order expires if still 'open' at this timestamp, default is ''
#' @seealso \code{\link{osNoOp}} , \code{\link{add.rule}}
#' @export

ruleSignal <- function(mktdata=mktdata, timestamp, sigcol, sigval, orderqty=0, ordertype, orderside=NULL, orderset=NULL, threshold=NULL, tmult=FALSE, replace=TRUE, delay=0.0001, osFUN='osNoOp', pricemethod=c('market','opside','active'), portfolio, symbol, ..., ruletype, TxnFees=0, prefer=NULL, sethold=FALSE, label='', order.price=NULL, chain.price=NULL, time.in.force='')
{
    if(!is.function(osFUN))
        osFUN<-match.fun(osFUN)

    # Get row index of timestamp for faster subsetting
    if(hasArg(curIndex))
        curIndex <- eval(match.call(expand.dots=TRUE)$curIndex, parent.frame())
    else
        curIndex <- mktdata[timestamp,which.i=TRUE]

    if(curIndex > 0 && curIndex <= nrow(mktdata) && (ruletype=='chain' || (!is.na(mktdata[curIndex,sigcol]) && mktdata[curIndex,sigcol]==sigval)))
    {
        #calculate order price using pricemethod
        pricemethod<-pricemethod[1] #only use the first if not set by calling function

        if(hasArg(prefer)) prefer=match.call(expand.dots=TRUE)$prefer
        else prefer = NULL

        #if(hasArg(TxnFees)) TxnFees=match.call(expand.dots=TRUE)$TxnFees
        #else TxnFees=0

        # compute threshold
        if(!is.null(threshold))
        {
            if(!is.numeric(threshold))
            {
                # threshold should be the name of an indicator column in mktdata

                col.idx <- grep(threshold, colnames(mktdata))

                if(length(col.idx) < 1)
                    stop(paste('no indicator column in mktdata matches threshold name "', threshold, '"', sep=''))
                if(length(col.idx) > 1)
                    stop(paste('more than one indicator column in mktdata matches threshold name "', threshold, '"', sep=''))

                threshold <- as.numeric(mktdata[curIndex,col.idx])
            }
        }

        if(is.null(orderside) & !isTRUE(orderqty == 0))
        {
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
        
        if(orderqty=='all'){
            if (orderside=='long'){
                #we're flattenting a long position
                tmpqty <-  1
            } else {
                tmpqty <- -1
            }
        } else {
            tmpqty <- orderqty
        }

	if(!is.null(order.price))
	{
		orderprice <- order.price
	}
	else if(!is.null(chain.price))
	{
		orderprice <- chain.price
	}
	else
	{
		switch(pricemethod,
			market = ,
			opside = ,
			active = {
			    if(is.BBO(mktdata)){
				if (tmpqty>0) 
				    prefer='ask'  # we're buying, so pay what they're asking
				else
				    prefer='bid'  # we're selling, so give it to them for what they're bidding  
			    } 
			    orderprice <- try(getPrice(x=mktdata[curIndex,], prefer=prefer)[,1]) 
			},
			passive =,
			work =,
			join = {
			    if(is.BBO(mktdata)){
				if (tmpqty>0) 
				    prefer='bid'  # we're buying, so work the bid price
				else
				    prefer='ask'  # we're selling, so work the ask price
			    }
			    orderprice <- try(getPrice(x=mktdata[curIndex,], prefer=prefer)[,1]) 
			},
			maker = {
			    if(hasArg(price) & length(match.call(expand.dots=TRUE)$price)>1) {
				# we have prices, just use them
				orderprice <- try(match.call(expand.dots=TRUE)$price)
			    } else {
				if(!is.null(threshold)) {
				    baseprice <- last(getPrice(x=mktdata[curIndex,])[,1]) # this should get either the last trade price or the Close
				    if(hasArg(tmult) & isTRUE(match.call(expand.dots=TRUE)$tmult)) {
				    baseprice <- last(getPrice(x=mktdata[curIndex,])[,1]) # this should get either the last trade price or the Close
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
				    if(is.BBO(mktdata)){

				    } else {

				    }
				}
			    }
			    if(length(orderqty)==1) orderqty <- c(orderqty,-orderqty) #create paired market maker orders at the same size
			}
		) # end switch

		if(inherits(orderprice,'try-error')) orderprice<-NULL
		if(length(orderprice)>1 && pricemethod!='maker') orderprice <- last(orderprice[timestamp])
		if(!is.null(orderprice) && !is.null(ncol(orderprice))) orderprice <- orderprice[,1]
	}

        if(is.null(orderset)) orderset=NA
        
        ## now size the order
        #TODO add fancy formals matching for osFUN
        if(orderqty!='all')
        {
            orderqty <- osFUN(strategy=strategy, 
                              data=mktdata, 
                              timestamp=timestamp, 
                              orderqty=orderqty, 
                              ordertype=ordertype, 
                              orderside=orderside, 
                              portfolio=portfolio, 
                              symbol=symbol,
                              ...=...,
                              ruletype=ruletype, 
                              orderprice=as.numeric(orderprice))
        }

        if(!is.null(orderqty) && orderqty!=0 && length(orderprice))
        {
                addOrder(portfolio=portfolio, 
                         symbol=symbol, 
                         timestamp=timestamp, 
                         qty=orderqty, 
                         price=as.numeric(orderprice), 
                         ordertype=ordertype, 
                         side=orderside, 
                         orderset=orderset, 
                         threshold=threshold, 
                         status="open", 
                         replace=replace , 
                         delay=delay, 
                         tmult=tmult, 
                         ...=..., 
                         prefer=prefer, 
                         TxnFees=TxnFees,
                         label=label,
			                   time.in.force=time.in.force)
        }
    }
    if(sethold) hold <<- TRUE
}

#TODO ruleORSignal
#TODO ruleANDSingnal
# perhaps this could be done using the approach of sigFormula, or perhaps we should advise users to use sigFormula to create a signal you can use ruleSignal on.  Thoughts?

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
