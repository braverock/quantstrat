#' get the order book object
#' 
#' I don't think this should be exported, but it is for now while we're in test mode.
#' 
#' @param portfolio text name of the portfolio the order book is associated with
#' @export
getOrderBook <- function(portfolio) #should symbol subsets be supported too?  probably not.
{ 
    if(!grepl("order_book",portfolio)) orders<-try(get(paste("order_book",portfolio,sep='.'),envir=.strategy))
    else orders<-try(get(portfolio,envir=.strategy))
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
initOrders <- function(portfolio=NULL, symbols=NULL, initDate = '1999-12-31')
{
    # NOTE we could stor all of these in one object, but I think that might get big
    orders<- try(getOrderBook(portfolio))
    if(inherits(orders,"order_book")) {
        stop(paste("Order Book for portfolio",portfolio,"already exists."))
    } else {
        orders<-list(portfolio=portfolio)
    }
    ordertemplate<-xts(c(0,NA,"init","long","closed",as.POSIXct(initDate)),order.by=as.POSIXct(initDate))
    colnames(ordertemplate) <- c("Order.Qty","Order.Price","Order.Type","Order.Side","Order.Status","Order.StatusTime")
    
    if(is.null(symbols)) {
        pfolio<-getPortfolio(portfolio)
        symbols<-names(pfolio)
    }
    if(!is.null(symbols)){
        for (symbol in symbols){
            orders[[portfolio]]$symbol <- ordertemplate
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
#' @param timestamp timestamp coercible to POSIXct that will be the period to find orders of the given status and ordertype 
#' @param ordertype one of NULL, "market","limit",or "stop", default NULL
#' @param side one of NULL, "long" or "short", default NULL 
#' @param starttime difference to current timestamp to search, in seconds(numeric) or as a POSIXct timestamp, defaults to -86400 (one day) 
#' @export
getOrders <- function(portfolio,symbol,status="open",timestamp=NULL,ordertype=NULL, side=NULL, starttime=-86400)
{
    # get order book
    orderbook <- getOrderBook(portfolio)
    if(!length(grep(symbol,names(orderbook)))==1) stop(paste("symbol",symbol,"does not exist in portfolio",portfolio,"having symbols",names(orderbook)))
    orderset<-NULL
    
    #data quality checks
    if(!is.null(status) & !length(grep(status,c("open", "closed", "canceled","replaced")))==1) stop(paste("order status:",status,' must be one of "open", "closed", "canceled", or "replaced"'))
    if(!is.null(ordertype) & !length(grep(ordertype,c("market","limit","stop")))==1) stop(paste("ordertype:",ordertype,' must be one of "market","limit",or "stop"'))

    # subset by time and symbol
    if(!is.null(timestamp)){
        if(!is.null(starttime)){
            timespan<-paste("::",timestamp,sep='')
        } else {
            if(!is.timeBased(starttime) & !is.numeric(starttime)) stop("starttime is not coercible to a time stamp")
            if(is.numeric(starttime)) starttime = starttime + timestamp
            timespan=paste(starttime,timestamp,sep='::')
        }
    } else {
        # construct the timespan of the entire series
        timespan=paste(index(first(orderbook[[symbol]]),index(last(orderbook[[symbol]])),sep='::'))
    }
    
    # extract
    orderset<-orderbook[[symbol]][timespan]
    if(!is.null(status)){
        orderset<-orderset[which(orderset[,"Order.Status"==status])]
    }
    if(!is.null(ordertype)) {
        orderset<-orderset[which(orderset[,"Order.Type"==ordertype])]    
    }
    if(!is.null(side)) {
        orderset<-orderset[which(orderset[,"Order.Side"==side])]    
    }
    return(orderset)
}

#' add an order to the order book
#' 
#' By default, this function will locate and replace any 'open' order(s) 
#' on the requested portfolio/symbol that have the same type and side.
#'  
#' we need to figure out how to handle stop entry and stop exit orders, maybe via a negative price to specify the pullback that would trigger the order at the market.
#' 
#' trailing stops should be modeled with replaced orders as prices change
#' 
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @param qty numeric quantity of the order
#' @param price numeric price at which the order is to be inserted
#' @param ordertype one of "market","limit",or "stop"
#' @param side one of either "long" or "short" 
#' @param status one of "open", "closed", "canceled", or "replaced", default "open"
#' @param replace TRUE/FALSE, whether to replace any other open order(s) on this portfolio symbol, default TRUE 
#' @param statustimestamp timestamp of a status update, will be blank when order is initiated 
#' @param delay what delay to add to timestamp when inserting the order into the order book, in seconds
#' @export
addOrder <- function(portfolio, symbol, timestamp, qty, price, ordertype, side, status="open", replace=TRUE, statustimestamp='' , delay=.00001)
{
    # get order book
    orderbook <- getOrderBook(portfolio)
    if(!length(grep(symbol,names(orderbook)))==1) stop(paste("symbol",symbol,"does not exist in portfolio",portfolio,"having symbols",names(orderbook)))
    
    #data quality checks
    if(!is.numeric(qty)) stop (paste("Quantity must be numeric:",qty))
    if(!is.numeric(price)) stop (paste("Price must be numeric:",price))
    if(!length(grep(side,c('long','short')))==1) stop(paste("side:",side," must be one of 'long' or 'short'"))
    if(!length(grep(ordertype,c("market","limit","stop")))==1) stop(paste("ordertype:",ordertype,' must be one of "market","limit",or "stop"'))
    if(!length(grep(status,c("open", "closed", "canceled","replaced")))==1) stop(paste("order status:",status,' must be one of "open", "closed", "canceled", or "replaced"'))
    # TODO do we need to check for collision, and increment timestamp?  or alternately update?
    
    if(isTRUE(replace)) updateOrders(portfolio=portfolio, symbol=symbol,timestamp=timestamp, ordertype=ordertype, side=side, oldstatus="open", newstatus="replaced", statustimestamp=timestamp)
    # insert new order
    order<-xts(c(qty, price, ordertype, side, status, statustimestamp),order.by=(as.POSIXct(timestamp)+delay))
    colnames(order) <- c("Order.Qty","Order.Price","Order.Type","Order.Side","Order.Status","Order.StatusTime")
    orderbook[[symbol]]<-rbind(orderbook[[symbol]],order)
    
    # assign order book back into place (do we need a non-exported "put" function?)
    assign(paste("order_book",portfolio,sep='.'),orderbook,envir=.strategy)
}

#' update an order or orders
#' 
#' When an order gets filled, it should have its status moved to 'closed'.
#' 
#' When an order is updated with a new order, the order status should change to 'replaced' with a StatusTime that is the same as the one for the new order.
#' 
#' When a risk event or over-limit event happens, typically open orders will be 'canceled'.  Possibly new orders will be added to close open positions.  
#' Many models will also want to run a process at the close of market that will cancel all open orders. 
#' 
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param timestamp timestamp coercible to POSIXct that will be the time to search for orders before this time 
#' @param ordertype one of NULL, "market","limit",or "stop", default NULL
#' @param side one of NULL, "long" or "short", default NULL 
#' @param oldstatus one of NULL, "open", "closed", "canceled", or "replaced", default "open"
#' @param newstatus one of "open", "closed", "canceled", or "replaced"
#' @param statustimestamp timestamp of a status update, will be blank when order is initiated 
#' @export
updateOrders <- function(portfolio, symbol, timestamp, ordertype=NULL, side=NULL, oldstatus="open", newstatus, statustimestamp) 
{ 
    #data quality checks
    if(!is.null(oldstatus) & !length(grep(oldstatus,c("open", "closed", "canceled","replaced")))==1) stop(paste("old order status:",oldstatus,' must be one of "open", "closed", "canceled", or "replaced"'))
    if(!length(grep(newstatus,c("open", "closed", "canceled","replaced")))==1) stop(paste("new order status:",newstatus,' must be one of "open", "closed", "canceled", or "replaced"'))
    if(!is.null(side) & !length(grep(side,c('long','short')))==1) stop(paste("side:",side," must be one of 'long' or 'short'"))
    if(!is.null(ordertype) & !length(grep(ordertype,c("market","limit","stop")))==1) stop(paste("ordertype:",ordertype,' must be one of "market","limit",or "stop"'))
    
    # need the ability to pass a range like we do in blotter
    updatedorders<-getOrders(portfolio=portfolio, symbol=symbol, status=oldstatus, timestamp=timestamp, ordertype=ordertype, side=side) 
    
    
    # get order book 
    #TODO this gets the order book again after it was already retrieved by getOrdersByStatus.  
    # at some point, we should eliminate the double get
    orderbook <- getOrderBook(portfolio)
    
    updatedorders[,"Order.Status"]<-newstatus
    updatedorders[,"Order.StatusTime"]<-statustimestamp
    
    #orderbook<-merge.xts(orderbook,updatedorders,join='left')
    orderbook[index(updatedorders)]<-updatedorders

    # assign order book back into place (do we need a non-exported "put" function?)
    assign(paste("order_book",portfolio,sep='.'),orderbook,envir=.strategy)
}

# TODO ruleOrderProc
# process orders at time t, generating transactions

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
