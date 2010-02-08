#' get the order book object
#' 
#' I don't think this should be exported. 
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
#' @export
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

#TODO getOrdersByStatus
#' get orders by status
#' 
#' This function exists so that other code can find open orders, potentially to update or cancel them.
#' 
#' It has some use as a reporting or post-hoc analytics tool, but it may not always be exported.
#' 
#' should this be symbols in stead of symbol?
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param status one of "open", "closed", "canceled", or "replaced"
#' @param timestamp timestamp coercible to POSIXct that will be the period to find orders of the given status and ordertype 
#' @param ordertype one of "market","limit",or "stop"
#' @param starttime difference to current timestamp to search, in seconds(numeric) or as a POSIXct timestamp, defaults to -86400 (one day) 
#' @export
getOrdersByStatus <- function(portfolio,symbol,status="open",timestamp=NULL,ordertype=NULL,starttime=-86400)
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
    return(orderset)
}

#' add an order to the order book
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
#' @param status one of "open", "closed", "canceled", or "replaced"
#' @param statustime timestamp of a status update, will be blank when order is initiated 
#' @param delay what delay to add to timestamp when inserting the order into the order book, in seconds
#' @export
addOrder <- function(portfolio, symbol, timestamp, qty, price, ordertype, side, status="open", statustime='' , delay=.00001)
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
    
    # insert new order
    order<-xts(c(qty, price, ordertype, side, status, statustime),order.by=(as.POSIXct(timestamp)+delay))
    colnames(order) <- c("Order.Qty","Order.Price","Order.Type","Order.Side","Order.Status","Order.StatusTime")
    orderbook[[symbol]]<-rbind(orderbook[[symbol]],order)
    
    # assign order book back into place (do we need a non-exported "put" function?)
    assign(paste("order_book",portfolio,sep='.'),orderbook,envir=.strategy)
}

# TODO update an order

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
