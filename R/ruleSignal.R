
#' default rule to generate a trade order on a signal
#' 
#' \code{pricemethod} may be one of 
#'      \describe{ 
#'          \item{'market', 'opside', or 'active'}{ will use the 'ask' price if you're buying and 
#'            the 'bid' price if you're selling, crossing the market at the time of 
#'            order entry to attempt to set an aggressive price to get the trade. }
#' 		   \item{'passive', 'work' or 'join'}{ which will join the 'bid' price if you are buying
#'      	  or join the 'ask' price if you are selling, passively working to make liquidity 
#'            at the prevailing market price without crossing the market at time of order entry}
#' 		   \item{'maker'}{will create a pair of orders for both bid and offer, modeling 
#' 		      market making activities by having orders on both sides.  
#'            This will then create an Order.Set, and use the \code{threshold} to set the prices for these orders.}
#'      } 
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
#' @param orderset tag to identify an orderset; if one order of the set is filled, all others are canceled
#' @param threshold numeric or function threshold to apply to trailing stop orders, default NULL, see Details
#' @param tmult if TRUE, threshold is a percent multiplier for \code{price}, not a scalar to be added/subtracted from price.  threshold will be dynamically converted to a scalar at time of order entry
#' @param replace TRUE/FALSE, whether to replace any other open order(s) on this portfolio symbol, default TRUE 
#' @param delay what delay to add to timestamp when inserting the order into the order book, in seconds
#' @param osFUN function or text descriptor of function to use for order sizing, default \code{\link{osNoOp}}
#' @param pricemethod determines how the order price will be calculated, see Details
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param ... any other passthru parameters
#' @param ruletype one of "risk","order","rebalance","exit","entry", see \code{\link{add.rule}}
#' @param TxnFees numeric fees (usually negative) or function name for calculating TxnFees (processing happens later, not in this function)
#' @param prefer price method for getPrice
#' @param sethold boolean, puts entry Rule processing on hold, default FALSE
#' @param label rule label, default '', added by \code{\link{applyRules}}
#' @seealso \code{\link{osNoOp}} , \code{\link{add.rule}}
#' @export
ruleSignal <- function(data=mktdata, timestamp, sigcol, sigval, orderqty=0, ordertype, orderside=NULL, orderset=NULL, threshold=NULL, tmult=FALSE, replace=TRUE, delay=0.0001, osFUN='osNoOp', pricemethod=c('market','opside','active'), portfolio, symbol, ..., ruletype, TxnFees=0, prefer=NULL, sethold=FALSE, label='')
{
    if(!is.function(osFUN)) osFUN<-match.fun(osFUN)
    #print(paste(symbol,timestamp, sigval))
    #print(data[timestamp][,sigcol])
    #browser()
    if (!is.na(timestamp) && !is.na(data[timestamp][,sigcol]) && data[timestamp][,sigcol] == sigval) {
        #calculate order price using pricemethod
        pricemethod<-pricemethod[1] #only use the first if not set by calling function

		if(hasArg(prefer)) prefer=match.call(expand.dots=TRUE)$prefer
		else prefer = NULL

		#if(hasArg(TxnFees)) TxnFees=match.call(expand.dots=TRUE)$TxnFees
		#else TxnFees=0

		switch(pricemethod,
                market = ,
				opside = ,
				active = {
                    if(is.BBO(data)){
                        if (orderqty>0) 
                            prefer='ask'  # we're buying, so pay what they're asking
                        else
                            prefer='bid'  # we're selling, so give it to them for what they're bidding  
                    } 
					orderprice <- try(getPrice(x=data, prefer=prefer))[timestamp] 
				},
				passive =,
				work =,
				join = {
					if(is.BBO(data)){
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
        if(length(orderprice>1) && !pricemethod=='maker') orderprice<-last(orderprice[timestamp])
        if(!is.null(orderprice) && !is.null(ncol(orderprice))) orderprice <- orderprice[,1]

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

	if(is.null(orderset)) orderset=NA

	## now size the order
	#TODO add fancy formals matching for osFUN
	if(orderqty!='all' || ordertype=='market' || (ruletype!='risk' && ruletype!='exit'))
	{
		orderqty <- osFUN(strategy=strategy, data=data, timestamp=timestamp, orderqty=orderqty, ordertype=ordertype, orderside=orderside, portfolio=portfolio, symbol=symbol,...=...,ruletype=ruletype, orderprice=as.numeric(orderprice))

		if(ruletype=='risk' || ruletype=='exit')
		{
			if(orderqty==0)	# cancel any open orders from orderset associated with this exit/risk order
				updateOrders(portfolio, symbol, oldstatus="open", newstatus='canceled', statustimestamp=timestamp, orderset=orderset)

			if(((orderqty>0 && orderside=='long') || (orderqty<0 && orderside=='short')))
				orderqty = NULL		# dirty trick to suppress adding order below JH; (why?)
		}

        }

	if(!is.null(orderqty) && orderqty!=0 && !is.null(orderprice)) #orderqty could have length > 1
	{
		addOrder(portfolio=portfolio, symbol=symbol, timestamp=timestamp, qty=orderqty, price=as.numeric(orderprice), ordertype=ordertype, side=orderside, orderset=orderset, threshold=threshold, status="open", replace=replace , delay=delay, tmult=tmult, ...=..., prefer=prefer, TxnFees=TxnFees,label=label)
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
