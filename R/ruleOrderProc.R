#' process open orders at time \emph{t}, generating transactions or new orders
#' 
#' The ruleOrderProc function is effectively the default fill simulator for quantstrat. 
#' This function is meant to be sufficient for backtesting most strategies, 
#' but would need to be replaced for production use.  It provides the interface 
#' for taking the order book and determining when orders become trades.
#'  
#' For the purposes of backtesting, and compatibility with the trade accounting in
#' \code{blotter}, this function will not allow a transaction to cross your current 
#' position through zero.  The accounting rules for realizing gains in such cases 
#' are more complicated than we wish to support.  Also, many brokers will break, revise,
#' or split such transactions for the same reason. If you wish to do a "stop and reverse" 
#' system, first stop (flatten), and then reverse (initiate a new position).
#' 
#' This function would need to be revised or replaced for connection to a live trading infrastructure.
#' In a production mode, you would replace the \code{\link{addOrder}} function 
#' with a custom function to connect to your market infrastructure.  
#' In that case, you might need to add additional code to your strategy, 
#' or overload functions for checking position.  
#'   
#' Note that this function is called by default in the 'orders' slot of the 
#' \code{\link{applyRules}} processing.  If you have defined another order 
#' processing rule, it will \emph{replace} this function.  If you want your 
#' custom order rule and ruleOrderProc to both be called, you will need
#' explicitly add a rule to call ruleOrderProc either before or after your 
#' custom order processing function. 
#' 
#' We would like to model slippage here via \code{slippageFUN}.  Code contributions, suggestions, 
#' and requests appreciated. 
#'
#' @concept fill simulator
#' @concept orders  
#' @concept backtest
#' @concept fills
#' 
#' This function is meant to be sufficient for backtesting many/most strategies, 
#' but would need to be replaced for production use.  It provides the interface 
#' for taking the order book and determining when orders become trades.
#'  
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC or BBO) should match these
#' @param mktdata an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats, default NULL
#' @param timespan xts-style character timespan to be the period to find orders to process in
#' @param ordertype one of NULL, "market","limit","stoplimit", or "stoptrailing" default NULL
#' @param ... any other passthru parameters
#' @param slippageFUN default  NULL, not yet implemented
#' @seealso add.rule
#' @seealso applyRules
#' @seealso getOrderBook
#' @seealso addOrder
#' @seealso updateOrders
#' @export
ruleOrderProc <- function(portfolio, symbol, mktdata, timespan=NULL, ordertype=NULL, ..., slippageFUN=NULL)
{
    if(is.null(timespan)) return()
    orderbook <- getOrderBook(portfolio)
    ordersubset <- orderbook[[portfolio]][[symbol]]
    
    # get open orders
    OpenOrders.i=NULL
    OpenOrders.i<-getOrders(portfolio=portfolio, symbol=symbol, status="open", timespan=timespan, ordertype=ordertype, which.i=TRUE)

    if(hasArg(prefer)) prefer=match.call(expand.dots=TRUE)$prefer
    else prefer = NULL

    # check for open orders
    if (length(OpenOrders.i)>=1){
        # get previous bar
        prevtime  <- time(mktdata[last(mktdata[timespan, which.i = TRUE])-1]) 
        timestamp <- time(last(mktdata[timespan]))
        #switch on frequency
        freq = periodicity(mktdata)
        neworders<-NULL
        mktdataTimestamp <- mktdata[timestamp]
        #str(mktdataTimestamp)
        # Should we only keep the last observation per time stamp?
        if( NROW(mktdataTimestamp) > 1 ) mktdataTimestamp <- last(mktdataTimestamp)
        isOHLCmktdata <- is.OHLC(mktdata)
        isBBOmktdata  <- is.BBO(mktdata)
        for (ii in OpenOrders.i ){
		if(ordersubset[ii, "Order.Status"] != "open")	# need to check this bc sideeffects may have changed order.status in this loop
		{
			#print("@@@@@@@@ status changed from open")
			next()
		}
            txnprice=NULL
            txnfees=ordersubset[ii,"Txn.Fees"]
            orderPrice <- as.numeric(ordersubset[ii,"Order.Price"])
            orderQty <- ordersubset[ii,"Order.Qty"]
            if(orderQty=='all') orderQty <- osNoOp(timestamp=timestamp, orderqty=orderQty, portfolio=portfolio, symbol=symbol,ruletype='exit' )
            orderQty<-as.numeric(orderQty)
            if(orderQty==0) next()
            orderThreshold <- as.numeric(ordersubset[ii,"Order.Threshold"])
            # mktdataTimestamp <- mktdata[timestamp]
            #FIXME Should we only keep the last observation per time stamp?
            #if( NROW(mktdataTimestamp) > 1 ) mktdataTimestamp <- last(mktdataTimestamp)
            orderType <- ordersubset[ii,"Order.Type"]

            switch(orderType,
                    market = {
                        switch(freq$scale,
                                yearly = ,
                                quarterly = ,
                                monthly = {
                                    txntime=as.character(index(ordersubset[ii,])) # transacts on this bar, e.g. in the intraday cross, or leading into the end of month, quarter, etc.
                                    # txntime=as.character(timestamp) # use this if you wanted to transact on the close of the next bar
                                    txnprice=as.numeric(getPrice(last(mktdata[txntime]), prefer=prefer)[,1])
                                }, #end daily
                                { 
                                    txntime = timestamp
                                    if (isBBOmktdata) {
                                        #An ordertype of market will *almost* trump pricemethod here. orderPrice was determined using pricemethod.
                                        #but, for buy orders you'll be filled at either orderPrice or the current mkt ask -- whichever is worse.
                                        #and, for sell orders you'll be filled at either orderPrice or the current mkt bid -- whichever is worse.
                                        if(orderQty > 0){ # positive quantity 'buy'
                                            #fill at max(orderPrice,newMktAsk price) 
                                            txnprice = max(orderPrice, as.numeric(getPrice(mktdataTimestamp,prefer='ask')[,1]))
                                        } else { # negative quantity 'sell'
                                            txnprice = min(orderPrice, as.numeric(getPrice(mktdataTimestamp,prefer='bid')[,1])) #presumes unique timestamp
                                        }
                                        #e.g. if pricemethod was opside, it sent a buy order at mktAsk. fill at greater of that ask, and current ask
                                    } else txnprice = as.numeric(getPrice(mktdataTimestamp, prefer=prefer)[,1]) #filled at 'price'
                                }) # end switch on frequency
                    },
                    limit= ,
                    stoplimit =,
                    iceberg = {
                        if (!isBBOmktdata) { #(isOHLCmktdata){
                            if( orderType == 'iceberg'){
                                stop("iceberg orders only supported for BBO data")
                            } 
                            # check to see if price moved through the limit                        
                            if((orderQty > 0 && orderType != 'stoplimit') || (orderQty < 0 && orderType == 'stoplimit') ) {  
                                # buy limit, or sell stoplimit
                                if( (has.Lo(mktdata) && orderPrice > as.numeric(Lo(mktdataTimestamp))) || 
                                    (!has.Lo(mktdata) && orderPrice >= as.numeric(getPrice(mktdataTimestamp, prefer=prefer))))
                                {
                                    txnprice = orderPrice
                                    txntime = timestamp
                                } else next() # price did not move through my order, should go to next order  
                            } else if((orderQty < 0 && orderType != 'stoplimit') || (orderQty > 0 && orderType == 'stoplimit')) { 
                                # sell limit or buy stoplimit
                                if ( (has.Hi(mktdata) && orderPrice < as.numeric(Hi(mktdataTimestamp))) ||
                                     (!has.Hi(mktdata) && orderPrice <= as.numeric(getPrice(mktdataTimestamp,prefer=prefer))) )
                                {
                                    txnprice = orderPrice
                                    txntime = timestamp
                                } else next() # price did not move through my order, should go to next order 
                            } else {
                                warning('ignoring order with quantity of zero')
                                next()
                            }
                        } else if(isBBOmktdata){
                            # check side/qty
                            if(orderQty > 0){ # positive quantity 'buy'
                                if (orderType == 'stoplimit') {
                                       if(orderPrice <= as.numeric(getPrice(mktdataTimestamp,prefer='ask')[,1])){
                                        # mktprice moved above our stop buy price 
                                        txnprice = orderPrice #assume we got filled at our stop price
                                        #txnprice = as.numeric(getPrice(mktdataTimestamp,prefer='ask')[,1]) #presumes unique timestamps
                                        txntime = timestamp
                                       } else next()
                                } else {
                                    if(orderPrice >= as.numeric(getPrice(mktdataTimestamp,prefer='ask')[,1])){
                                        # price we're willing to pay is higher than the offer price, so execute at the prevailing price
                                        #txnprice = orderPrice
                                        txnprice = as.numeric(getPrice(mktdataTimestamp,prefer='ask')[,1]) #presumes unique timestamps
                                        txntime = timestamp
                                    } else next()
                                }
                            } else { # negative quantity 'sell'
                                if (orderType == 'stoplimit') {
                                    if(orderPrice >= as.numeric(getPrice(mktdataTimestamp,prefer='bid')[,1])){
                                        # mktprice moved below our stop sell price
                                        txnprice = orderPrice #assumption is that we're filled at our stop price
                                        #txnprice = as.numeric(getPrice(mktdataTimestamp,prefer='bid')[,1]) #presumes unique timestamp
                                        txntime = timestamp
                                    } else next()
                                } else {
                                    if(orderPrice <= as.numeric(getPrice(mktdataTimestamp,prefer='bid')[,1])){
                                        # we're willing to sell at a better price than the bid, so execute at the prevailing price
                                        # txnprice = orderPrice
                                        txnprice = as.numeric(getPrice(mktdataTimestamp,prefer='bid')[,1]) #presumes unique timestamp
                                        txntime = timestamp
                                    } else next()
                               } 
                            }


                            if( orderType == 'iceberg'){
                                #we've transacted, so the old order was closed, put in a new one
                                neworder<-addOrder(portfolio=portfolio,
                                        symbol=symbol,
                                        timestamp=timestamp,
                                        qty=orderQty,
                                        price=as.numeric(getPrice(mktdataTimestamp,prefer=prefer)[,1]), 
                                        ordertype=orderType,
                                        side=ordersubset[ii,"Order.Side"],
                                        threshold=orderThreshold,
                                        status="open",
                                        replace=FALSE, return=TRUE,
                                        ,...=..., TxnFees=txnfees)
                                if (is.null(neworders)) neworders=neworder else neworders = rbind(neworders,neworder)
                                ordersubset[ii,"Order.Status"]<-'replaced'
                                ordersubset[ii,"Order.StatusTime"]<-as.character(timestamp)
                                next()
                            } 
                        }
                    },
                    stoptrailing = {
                        # if market moved through my price, execute
                        if(orderQty > 0){ # positive quantity 'buy'
                            if(isBBOmktdata) prefer='offer'
                            if(orderPrice >= getPrice(mktdataTimestamp,prefer=prefer)[,1]){ #TODO maybe use last(getPrice) to catch multiple prints on timestamp?
                                # price we're willing to pay is higher than the offer price, so execute at the prevailing price
                                #txnprice = orderPrice
                                txnprice = as.numeric(getPrice(mktdataTimestamp,prefer=prefer)[,1]) #presumes unique timestamps
                                txntime = timestamp
                            } 
                        } else { # negative quantity 'sell'
                            if(isBBOmktdata) prefer='bid'
                            if(orderPrice <= getPrice(mktdataTimestamp,prefer=prefer)[,1]){
                                # we're willing to sell at a better price than the bid, so execute at the prevailing price
                                # txnprice = orderPrice
                                txnprice = as.numeric(getPrice(mktdataTimestamp,prefer=prefer)[,1]) #presumes unique timestamp
                                txntime = timestamp
                            } 
                        } 
                        if(isOHLCmktdata){
                            # check to see if price moved through the limit
                            if( orderPrice > as.numeric(Lo(mktdataTimestamp)) &
                                orderPrice < as.numeric(Hi(mktdataTimestamp)) ) 
                            {
                                txnprice = orderPrice
                                txntime = timestamp
                            } 
                        }
                        # if market is beyond price+(-threshold), replace order
                        if(is.null(txnprice)) { 
                            #print("here")
                            # we didn't trade, so check to see if we need to move the stop
                            # first figure out how to find a price
                            if (isOHLCmktdata){
                                prefer='close'
                            } else if(isBBOmktdata) {
                                if(orderQty > 0){
                                    prefer='offer'
                                } else {
                                    prefer='bid'
                                }
                            } else {
                                prefer=NULL # see if getPrice can figure it out
                            }
                            # check if we need to move the stop
                            mvstop=FALSE
                            if(orderQty > 0){ # positive quantity 'buy'
                                if( as.numeric(last(getPrice(x=mktdataTimestamp,prefer=prefer)[,1]))+orderThreshold < orderPrice ) mvstop=TRUE
                            } else {  # negative quantity 'sell'
                                if( as.numeric(last(getPrice(x=mktdataTimestamp,prefer=prefer)[,1]))+orderThreshold > orderPrice ) mvstop=TRUE
                                
                            }
                            if( isTRUE(mvstop) ){
                                neworder<-addOrder(portfolio=portfolio,
                                         symbol=symbol,
                                         timestamp=timestamp,
                                         qty=orderQty,
                                         price=as.numeric(getPrice(mktdataTimestamp,prefer=prefer)[,1]), 
                                         ordertype=orderType,
                                         side=ordersubset[ii,"Order.Side"],
                                         threshold=orderThreshold,
                                         status="open",
                                         replace=FALSE, return=TRUE,
                                         ,...=..., TxnFees=txnfees)
                                if (is.null(neworders)) neworders=neworder else neworders = rbind(neworders,neworder)
                                ordersubset[ii,"Order.Status"]<-'replaced'
                                ordersubset[ii,"Order.StatusTime"]<-as.character(as.POSIXlt(statustimestamp, Sys.getenv('TZ')))
                                next()
                            }
                        }
                        # else next
                    }
            )
            if(!is.null(txnprice) && !isTRUE(is.na(txnprice))) {
                #make sure we don't cross through zero
                pos<-getPosQty(portfolio,symbol,timestamp)
                if ( (pos > 0 && orderQty < -pos) || (pos < 0 && orderQty > -pos) ) {
                    warning("orderQty of ",orderQty,
                            " would cross through zero, adjusting qty to ",-pos)
                    orderQty <- -pos
                }
                if (orderQty != 0) {
                    #now add the transaction
                    addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=txntime, 
                                 TxnQty=orderQty, TxnPrice=txnprice , ...=..., TxnFees=txnfees)
                    ordersubset[ii,"Order.Status"]<-'closed'
                    ordersubset[ii,"Order.StatusTime"]<-as.character(timestamp)

                    #close all other orders in the order set
                    OrdersetTag = toString(ordersubset[ii,"Order.Set"])
		    OpenInOrderset.i = which(ordersubset[,"Order.Status"] == 'open' & ordersubset[,"Order.Set"] == OrdersetTag)
                    ordersubset[OpenInOrderset.i, "Order.Status"] = 'canceled'
                    ordersubset[OpenInOrderset.i, "Order.StatusTime"]<-as.character(timestamp)
                } 
            }
        } #end loop over open orders  
        if(!is.null(neworders)) ordersubset=rbind(ordersubset,neworders)
        
        # now put the orders back in
        # assign order book back into place (do we need a non-exported "put" function?)
        orderbook[[portfolio]][[symbol]] <- ordersubset
        assign(paste("order_book",portfolio,sep='.'),orderbook,envir=.strategy)
    } # end check for open orders
}
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
