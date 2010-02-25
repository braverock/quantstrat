#' get the order book object
#' 
#' I don't think this should be exported, but it is for now while we're in test mode.
#' 
#' @param portfolio text name of the portfolio the order book is associated with
#' @export
getOrderBook <- function(portfolio) #should symbol subsets be supported too?  probably not.
{ 
    if(!grepl("order_book",portfolio)) orders<-try(get(paste("order_book",portfolio,sep='.'),envir=.strategy),silent=TRUE)
    else orders<-try(get(portfolio,envir=.strategy),silent=TRUE)
    if(inherits(orders,"try-error"))
        stop(paste("Orders for ",portfolio," not found, use initOrders() to create a new order book for this portfolio"))
    if(!inherits(orders,"order_book")) stop("Order Book for portfolio",portfolio,"does not appear to name an order book object.")
    return(orders)
}

#' initialize order container
#' 
#' This function sets up the order container by portfolio.
#' 
#' If no symbols list is provided (the default) the function will attempt 
#' to retrieve the symbols list from the portfolio in the trade blotter.
#' 
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbols a list of identfiers of the instruments to be contained in the Portfolio.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param initDate date (ISO8601) prior to the first close price given in mktdata, used to initialize the order book with a dummy order
#' @export
initOrders <- function(portfolio=NULL, symbols=NULL, initDate = '1999-12-31')
{
    # NOTE we could store all of these in one object, but I think that might get big
    orders<- try(getOrderBook(portfolio),silent=TRUE)
    if(inherits(orders,"order_book")) {
        stop(paste("Order Book for portfolio",portfolio,"already exists."))
    } else {
        orders<-list()
        orders[[portfolio]]<-list()
    }
    ordertemplate<-xts(as.matrix(t(c(0,NA,"init","long",0,"closed",as.POSIXct(initDate)))),order.by=as.POSIXct(initDate))
    colnames(ordertemplate) <- c("Order.Qty","Order.Price","Order.Type","Order.Side","Order.Threshold","Order.Status","Order.StatusTime")
    
    if(is.null(symbols)) {
        pfolio<-getPortfolio(portfolio)
        symbols<-names(pfolio)
    }
    if(!is.null(symbols)){
        for (symbol in symbols){
            orders[[portfolio]][[symbol]] <- ordertemplate
        }
    } else {
        stop("You must specify a symbols list or a valid portfolio to retrieve the list from.")
    }
    class(orders)<-"order_book"
    assign(paste("order_book",portfolio,sep='.'),orders,envir=.strategy)
}

#' get orders by time span, status, type, and side
#' 
#' This function exists so that other code can find open orders, potentially to update or cancel them.
#' 
#' It has some use as a reporting or post-hoc analytics tool, but it may not always be exported.
#' 
#' should this be symbols instead of symbol?
#' 
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param status one of "open", "closed", "canceled", or "replaced", default "open"
#' @param timespan xts-style character timespan to be the period to find orders of the given status and ordertype 
#' @param ordertype one of NULL, "market","limit","stoplimit", or "stoptrailing" default NULL
#' @param side one of NULL, "long" or "short", default NULL 
#' @param starttime difference to current timestamp to search, in seconds(numeric) or as a POSIXct timestamp, defaults to -86400 (one day) 
#' @export
getOrders <- function(portfolio,symbol,status="open",timespan=NULL,ordertype=NULL, side=NULL, starttime=-86400)
{
    if(is.null(timespan)) stop("timespan must be an xts style timestring")
    # get order book
    orderbook <- getOrderBook(portfolio)
    if(!length(grep(symbol,names(orderbook[[portfolio]])))==1) stop(paste("symbol",symbol,"does not exist in portfolio",portfolio,"having symbols",names(orderbook)))
    orderset<-NULL
    
    #data quality checks
    if(!is.null(status) & !length(grep(status,c("open", "closed", "canceled","replaced")))==1) stop(paste("order status:",status,' must be one of "open", "closed", "canceled", or "replaced"'))
    if(!is.null(ordertype)) {
        if(!length(grep(ordertype,c("market","limit","stoplimit","stoptrailing")))==1){
            stop(paste("ordertype:",ordertype,' must be one of "market","limit","stoplimit", or "stoptrailing"'))
        } 
    } 

    
    # extract
    orderset<-orderbook[[portfolio]][[symbol]][timespan]
    if(!is.null(status) & !is.null(orderset) & nrow(orderset)>=1 ){
        orderset<-orderset[which(orderset[,"Order.Status"]==status),]
    }
    if(!is.null(ordertype) & !is.null(orderset) & nrow(orderset)>=1 ) {
        orderset<-orderset[which(orderset[,"Order.Type"]==ordertype),]    
    }
    if(!is.null(side) & !is.null(orderset) & nrow(orderset)>=1 ) {
        orderset<-orderset[which(orderset[,"Order.Side"]==side),]    
    }
    return(orderset)
}

#' add an order to the order book
#' 
#' By default, this function will locate and replace any 'open' order(s) 
#' on the requested portfolio/symbol that have the same type and side.  
#' This is the equivalent of what is sometimes called an 
#' OCO (Order Cancels Other) order.  If you do not want the function to 
#' behave this way, set \code{replace=FALSE}.
#'  
#' We have modeled two types of stop orders, which should be sufficient to model most types of stops.  
#' We have modeled the simplest type, a 'stoplimit' order, which is just a limit order used to enter 
#' or exit a position at a specific price.  There is no functional different between a regular 'limit'
#' order and a 'stoplimit' order, but the distinction will likely be useful for reporting on when stops
#' have been triggered.
#' We have also modeled a 'stoptrailing' order, which may be used to model dynamic limit-based entry or exit.  
#' The 'stoptrailing' order type is the only order type that makes use of the order \code{threshold}, which 
#' is the difference either positive or negative from the current price when the order is entered.  
#' Some markets and brokers recognize a stop that triggers a market order, when the stop is triggered, 
#' a market order will be executed at the then-prevailing price.  We have not modeled this type of order.   
#' 
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param timespan xts-style character timespan to be the period to find orders of the given status and ordertype 
#' @param qty numeric quantity of the order
#' @param price numeric price at which the order is to be inserted
#' @param ordertype one of "market","limit","stoplimit", or "stoptrailing"
#' @param side one of either "long" or "short" 
#' @param threshold numeric threshold to apply to trailing stop orders, default NULL
#' @param status one of "open", "closed", "canceled", or "replaced", default "open"
#' @param replace TRUE/FALSE, whether to replace any other open order(s) on this portfolio symbol, default TRUE 
#' @param statustimestamp timestamp of a status update, will be blank when order is initiated 
#' @param delay what delay to add to timestamp when inserting the order into the order book, in seconds
#' @export
addOrder <- function(portfolio, symbol, timestamp, qty, price, ordertype, side, threshold=NULL, status="open", replace=TRUE, statustimestamp='' , delay=.00001)
{
    # get order book
    #orderbook <- getOrderBook(portfolio)
    #if(!length(grep(symbol,names(orderbook[[portfolio]])))==1) stop(paste("symbol",symbol,"does not exist in portfolio",portfolio,"having symbols",names(orderbook[[portfolio]])))
    
    #data quality checks
    if(!is.numeric(qty)) stop (paste("Quantity must be numeric:",qty))
    if(!is.numeric(price)) stop (paste("Price must be numeric:",price))
    if(!length(grep(side,c('long','short')))==1) stop(paste("side:",side," must be one of 'long' or 'short'"))
    if(!length(grep(ordertype,c("market","limit","stoplimit","stoptrailing")))==1) stop(paste("ordertype:",ordertype,' must be one of "market","limit","stoplimit", or "stoptrailing"'))
    if(!is.null(threshold) & !length(grep(ordertype,c("stoplimit","stoptrailing")))==1){ 
        stop(paste("Threshold may only be applied to a stop order type",ordertype,threshold))
    }
    if(is.null(threshold)) threshold=NA #NA is not ignored byc() like NULL is
    if(!length(grep(status,c("open", "closed", "canceled","replaced")))==1) stop(paste("order status:",status,' must be one of "open", "closed", "canceled", or "replaced"'))
    # TODO do we need to check for collision, and increment timestamp?  or alternately update?
    
    # subset by time and symbol
    if(!is.null(timestamp)& length(timestamp)>=1){
        timespan<-paste("::",timestamp,sep='')
    } else {
        # construct the timespan of the entire series
        timespan=paste(index(first(orderbook),index(last(orderbook)),sep='::'))
    }
    
    if(isTRUE(replace)) updateOrders(portfolio=portfolio, symbol=symbol,timespan=timespan, ordertype=ordertype, side=side, oldstatus="open", newstatus="replaced", statustimestamp=timestamp)
    # get order book
    orderbook <- getOrderBook(portfolio)
    statustimestamp=NA # new orders don't have a status time
    # insert new order
    if(is.timeBased(timestamp)) ordertime<-timestamp+delay
    else ordertime<-as.POSIXct(timestamp)+delay
    order<-xts(as.matrix(t(c(qty, price, ordertype, side, threshold, status, statustimestamp))),order.by=(ordertime))
    colnames(order) <- c("Order.Qty","Order.Price","Order.Type","Order.Side","Order.Threshold","Order.Status","Order.StatusTime")
    orderbook[[portfolio]][[symbol]]<-rbind(orderbook[[portfolio]][[symbol]],order)
    
    # assign order book back into place (do we need a non-exported "put" function?)
    assign(paste("order_book",portfolio,sep='.'),orderbook,envir=.strategy)
    rm(orderbook)
}

#' update an order or orders
#' 
#' When an order gets filled, it should have its status moved to 'closed'.
#' 
#' When an order is updated with a new order, the order status should change to 'replaced' 
#' with a StatusTime that is the same as the one for the new order.  This could happen in 
#' the case of a traditional Cancel/Replace, because of a trailing stop, or in the
#' case of a partial fill that needs to enter a replaced order for the remainder. 
#' 
#' When a risk event or over-limit event happens, typically open orders will be 'canceled'.  
#' Possibly new orders will be added to close open positions.  
#' Many models will also want to run a process at the close of market that will cancel all open orders. 
#' 
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param timestamp timestamp coercible to POSIXct that will be the time to search for orders before this time 
#' @param ordertype one of NULL, "market","limit","stoplimit", or "stoptrailing" default NULL
#' @param side one of NULL, "long" or "short", default NULL 
#' @param oldstatus one of NULL, "open", "closed", "canceled", or "replaced", default "open"
#' @param newstatus one of "open", "closed", "canceled", or "replaced"
#' @param statustimestamp timestamp of a status update, will be blank when order is initiated 
#' @export
updateOrders <- function(portfolio, symbol, timespan, ordertype=NULL, side=NULL, oldstatus="open", newstatus, statustimestamp) 
{ 
    #data quality checks
    if(!is.null(oldstatus) & !length(grep(oldstatus,c("open", "closed", "canceled","replaced")))==1) 
        stop(paste("old order status:",oldstatus,' must be one of "open", "closed", "canceled", or "replaced"'))
    if(!length(grep(newstatus,c("open", "closed", "canceled","replaced")))==1) 
        stop(paste("new order status:",newstatus,' must be one of "open", "closed", "canceled", or "replaced"'))
    if(!is.null(side) & !length(grep(side,c('long','short')))==1) 
        stop(paste("side:",side," must be one of 'long' or 'short'"))
    #if(is.null(side)) side<-NA
    if(!is.null(ordertype) & !length(grep(ordertype,c("market","limit","stoplimit","stoptrailing")))==1) 
        stop(paste("ordertype:",ordertype,' must be one of "market","limit","stoplimit", or "stoptrailing"'))
    
    # need the ability to pass a range like we do in blotter
    updatedorders<-getOrders(portfolio=portfolio, symbol=symbol, status=oldstatus, timespan=timespan, ordertype=ordertype, side=side) 
    if(nrow(updatedorders>=1)){
        
        # get order book 
        #TODO this gets the order book again after it was already retrieved by getOrdersByStatus.  
        # at some point, we should eliminate the) double get
        orderbook <- getOrderBook(portfolio)
        
        orderbook[[portfolio]][[symbol]][index(updatedorders),"Order.Status"]<-newstatus
        orderbook[[portfolio]][[symbol]][index(updatedorders),"Order.StatusTime"]<-statustimestamp
        
        # assign order book back into place (do we need a non-exported "put" function?)
        assign(paste("order_book",portfolio,sep='.'),orderbook,envir=.strategy)

    }
}

#' insert a block of updated orders
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param updatedorders time series containing updated orders 
#' @export
updateOrderMatrix<-function(portfolio, symbol, updatedorders){
    orderbook <- getOrderBook(portfolio)

    orderbook[[portfolio]][[symbol]][index(updatedorders)]<-updatedorders
    
    # assign order book back into place (do we need a non-exported "put" function?)
    assign(paste("order_book",portfolio,sep='.'),orderbook,envir=.strategy)
}

#' process open orders at time t, generating transactions or new orders
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param mktdata an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats, default NULL
#' @param timestamp timestamp coercible to POSIXct that will be the time to search for orders before this time 
#' @param ordertype one of NULL, "market","limit","stoplimit", or "stoptrailing" default NULL
#' @param ... any other passthru parameters
#' @param slippageFUN default  NULL, not yet implemented
#' @export
ruleOrderProc <- function(portfolio, symbol, mktdata, timestamp, ordertype=NULL, ..., slippageFUN=NULL)
{
    # get open orders
    procorders<-getOrders(portfolio=portfolio, symbol=symbol, status="open", timespan=timestamp, ordertype=ordertype)
    freq = periodicity(mktdata)
    if (!is.null(procorders)){ 
    if (nrow(procorders)>=1){
        # get previous bar
        prevtime=time(mktdata[mktdata[timestamp,which.i=TRUE]-1])
        #switch on frequency
        switch(freq$scale,
            yearly = ,
            quarterly = ,
            monthly = ,{
                # first process low frequencies with look-back assumption
                for (ii in 1:nrow(procorders) ){
                    if(procorders[[ii]]$Order.Type=='market'){
                        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=prevtime, TxnQty=procorders[[ii]]$Order.Qty, TxnPrice=Cl(mktdata) ,...=...)
                        procorders[[ii]]$Order.Status<-'closed'
                        procorders[[ii]]$Order.StatusTime<-timestamp
                    } else {
                        stop("order types other than market not (yet?) supported for low-frequency strategies")
                    }
                }
            }, # end low frequency processing
            daily = { 
                # next process daily
                for (ii in 1:nrow(procorders) ){
                    switch(procorders[[ii]]$Order.Type,
                        market = ,
                        limit = {
                            if (procorders[[ii]]$Order.Type == 'market' ){
                                txnprice=getPrice(mktdata[prevtime], prefer='close')
                                if(ncol(txnprice)>1) txnprice = getPrice(mktdata[timestamp], symbol=symbol, prefer='close')
                                txntime=prevtime
                            } else {
                                # check to see if price moved through the limit
                                if(procorders[[ii]]$Order.Price>Lo(mktdata[timestamp]) & procorders[[ii]]$Order.Price<Hi(mktdata[timestamp]) ) {
                                    txnprice=procorders[[ii]]$Order.Price
                                    txntime=timestamp
                                } else {
                                    # price did not move through my order
                                    next() # should go to next order
                                }   
                            }   
                        },
                        {
                            stop("order types other than market and limit not (yet?) supported for daily frequencies")
                        }
                    )
                    addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=txntime, TxnQty=procorders[[ii]]$Order.Qty, TxnPrice=Cl(mktdata) ,...=...)
                    procorders[[ii]]$Order.Status<-'closed'
                    procorders[[ii]]$Order.StatusTime<-timestamp
                } #end loop over open orders       
            }, #end daily processing
            {
                # now do higher frequencies
                for (ii in 1:nrow(procorders) ){
                    txnprice=NULL
                    switch(procorders[[ii]]$Order.Type,
                            market = {
                                txnprice = getPrice(mktdata[timestamp])
                                if(ncol(txnprice)>1) txnprice = getPrice(mktdata[timestamp], symbol=symbol)
                                txntime  = timestamp
                            },
                            limit= ,
                            stoplimit = {
                                if (is.OHLC(mktdata)){
                                    # check to see if price moved through the limit
                                    if(procorders[[ii]]$Order.Price>Lo(mktdata[timestamp]) & procorders[[ii]]$Order.Price<Hi(mktdata[timestamp]) ) {
                                        txnprice = procorders[[ii]]$Order.Price
                                        txntime  = timestamp
                                    } else {
                                        # price did not move through my order
                                        next() # should go to next order
                                    }   
                                } else if(is.BBO(mktdata)){
                                    # check side/qty
                                    if(procorders[[ii]]$Order.Qty>0){ # positive quantity 'buy'
                                        if(procorders[[ii]]$Order.Price>=getPrice(mktdata[timestamp],prefer='offer')){
                                            # price we're willing to pay is higher than the offer price, so execute at the limit
                                            txnprice = procorders[[ii]]$Order.Price
                                            txntime  = timestamp
                                        } else next()
                                    } else { # negative quantity 'sell'
                                        if(getPrice(procorders[[ii]]$Order.Price<=mktdata[timestamp],prefer='bid')){
                                            # we're willing to sell at a better price than the bid, so execute at the limit
                                            txnprice = procorders[[ii]]$Order.Price
                                            txntime  = timestamp
                                        } else next() 
                                    } 
                                } else {
                                    # no depth data, either OHLC or BBO, getPrice explicitly using symbol
                                    if(procorders[[ii]]$Order.Price==getPrice(mktdata[timestamp], symbol=symbol, prefer='Price')){
                                        txnprice = procorders[[ii]]$Order.Price
                                        txntime  = timestamp
                                    } else next()                                     
                                }
                                
                            },
                            stoptrailing = {
                                # if market moved through my price, execute
                                if(procorders[[ii]]$Order.Qty>0){ # positive quantity 'buy'
                                    if(procorders[[ii]]$Order.Price>=getPrice(mktdata[timestamp],prefer='offer')){
                                        # price we're willing to pay is higher than the offer price, so execute at the limit
                                        txnprice = procorders[[ii]]$Order.Price
                                        txntime  = timestamp
                                    } 
                                } else { # negative quantity 'sell'
                                    if(procorders[[ii]]$Order.Price<=getPrice(mktdata[timestamp],prefer='bid')){
                                        # we're willing to sell at a better price than the bid, so execute at the limit
                                        txnprice = procorders[[ii]]$Order.Price
                                        txntime  = timestamp
                                    }  
                                } 
                                # if market is beyond price+(-threshold), replace order
                                if(is.null(txnprice)){ 
                                    if(procorders[[ii]]$Order.Qty>0){
                                        prefer='offer'
                                    } else {
                                        prefer='bid'
                                    }
                                    # we didn't trade, so check to see if we need to move the stop
                                    if( getPrice(mktdata[timestamp],prefer=prefer)-procorders[[ii]]$Order.Threshold > procorders[[ii]]$Order.Price ){
                                        addOrder(portfolio=portfolio, 
                                                 symbol=symbol, 
                                                 timestamp=timestamp, 
                                                 qty=procorders[[ii]]$Order.Qty, 
                                                 price=getPrice(mktdata[timestamp],prefer=prefer)-procorders[[ii]]$Order.Threshold, 
                                                 ordertype=procorders[[ii]]$Order.Type, 
                                                 side=procorders[[ii]]$Order.Side, 
                                                 threshold=procorders[[ii]]$Order.Threshold, 
                                                 status="open", 
                                                 replace=TRUE)
                                        procorders[[ii]]$Order.Status<-'replaced'
                                        procorders[[ii]]$Order.StatusTime<-timestamp 
                                        next()
                                    }
                                }
                                # else next
                            }
                    )
                    if(!is.null(txnprice)){
                        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=txntime, TxnQty=procorders[[ii]]$Order.Qty, TxnPrice=Cl(mktdata) ,...=...)
                        procorders[[ii]]$Order.Status<-'closed'
                        procorders[[ii]]$Order.StatusTime<-timestamp
                    }
                } #end loop over open orders       
            } # end higher frequency processing
        ) # end switch on freq
    } # end check for open orders
    }
    # now put the orders back in
    updateOrderMatrix(portfolio=portfolio, symbol=symbol, updatedorders=procorders)
}
###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
