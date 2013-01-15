#' get the order book object
#' 
#' I don't think this should be exported, but it is for now while we're in test mode.
#' 
#' @param portfolio text name of the portfolio the order book is associated with
#' @seealso addOrder
#' @seealso getOrders
#' @concept order book
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

#' put an orderbook object in .strategy env
#' @param portfolio.st string identifying portfolio
#' @param orderbook orderbook object
#' @seealso getOrderBook
#' @concept order book
#' @export

put.orderbook <- function(portfolio.st, orderbook)
{
    strategy.orderbook.st <- paste('order_book', portfolio.st, sep='.')
    assign(strategy.orderbook.st, orderbook, envir=.strategy)
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2011 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

#' initialize order container
#' 
#' This function sets up the order container by portfolio.
#' 
#' If no symbols list is provided (the default) the function will attempt 
#' to retrieve the symbols list from the portfolio in the trade blotter.
#' 
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbols a list of identifiers of the instruments to be contained in the Portfolio.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param initDate date (ISO8601) prior to the first close price given in mktdata, used to initialize the order book with a dummy order
#' @param \dots any other passthrough parameters
#' @concept order book
#' @export
initOrders <- function(portfolio=NULL, symbols=NULL, initDate = '1999-12-31', ...)
{
    # NOTE we could store all of these in one object, but I think that might get big
    orders<- try(getOrderBook(portfolio),silent=TRUE)
    if(inherits(orders,"order_book")) {
        stop(paste("Order Book for portfolio",portfolio,"already exists."))
    } else {
        orders<-list()
        orders[[portfolio]]<-list()
    }
    ordertemplate<-xts(as.matrix(t(c(0,NA,"init","long",0,"closed",as.character(as.POSIXct(initDate)),'','',0,''))),order.by=as.POSIXct(initDate), ...=...)
    colnames(ordertemplate) <- c("Order.Qty","Order.Price","Order.Type","Order.Side","Order.Threshold","Order.Status","Order.StatusTime","Prefer", "Order.Set","Txn.Fees","Rule")

    if(is.null(symbols)) {
        pfolio<-getPortfolio(portfolio)
        symbols<-names(pfolio$symbols)
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
#' @param symbol identifier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param status one of "open", "closed", "canceled", "revoked", or "replaced", default "open"
#' @param timespan xts-style character timespan to be the period to find orders of the given status and ordertype
#' @param ordertype one of NULL, "market","limit","stoplimit", "stoptrailing" or "iceberg" default NULL
#' @param side one of NULL, "long" or "short", default NULL 
#' @param qtysign one of NULL, -1,0,1 ; could be useful when all qty's are reported as positive numbers and need to be identified other ways, default NULL
#' @param orderset a tag identifying the orderset
#' @param which.i if TRUE, return the row index numbers rather than the order rows matching the criteria, default FALSE
#' @seealso getOrderBook
#' @seealso addOrder
#' @concept order book
#' @export
getOrders <- function(portfolio,symbol,status="open",timespan=NULL,ordertype=NULL, side=NULL, qtysign=NULL, orderset=NULL, which.i=FALSE)
{
    #if(is.null(timespan)) stop("timespan must be an xts style timestring")
    # get order book
    orderbook <- getOrderBook(portfolio)
    if(!any(names(orderbook[[portfolio]]) == symbol)) stop(paste("symbol",symbol,"does not exist in portfolio",portfolio,"having symbols",names(orderbook[[portfolio]])))
	    ordersubset<-orderbook[[portfolio]][[symbol]]

    #data quality checks
    if(!is.null(status) & !length(grep(status,c("open", "closed", "canceled", "revoked","replaced")))==1) stop(paste("order status:",status,' must be one of "open", "closed", "canceled", "revoked", or "replaced"'))
    if(!is.null(ordertype)) {
        if(is.na(charmatch(ordertype,c("market","limit","stoplimit","stoptrailing","iceberg")))){
            stop(paste("ordertype:",ordertype,' must be one of "market","limit","stoplimit", "stoptrailing" or "iceberg"'))
        }
    }

    indices <- which(#if(!is.null(timespan)) ordersubset[timespan,which.i=TRUE] else TRUE &
                     (if(!is.null(status)) ordersubset[,"Order.Status"]==status else TRUE) &
                     (if(!is.null(ordertype)) ordersubset[,"Order.Type"]==ordertype else TRUE) &
                     (if(!is.null(side)) ordersubset[,"Order.Side"]==side else TRUE) &
                     (if(!is.null(orderset)) ordersubset[,"Order.Set"]==orderset else TRUE) &
                     (if(!is.null(qtysign)) sign(as.numeric(ordersubset[,"Order.Qty"]))==qtysign else TRUE)
                    )

    if(isTRUE(which.i)){
        return(indices)
    } else {
        # extract
        ordersubset<-orderbook[[portfolio]][[symbol]][indices,]
        #subset by time
        if(nrow(ordersubset)>1 && !is.null(timespan)) ordersubset<-ordersubset[timespan]
        return(ordersubset)
    }
}

#' add an order to the order book
#' 
#' It is important to understand that all the order functionality included in \code{quantstrat}
#' exists to more closely model a real trading environment both in backtesting and in production.
#' Many backtesting systems make a set of assumptions about instant execution, 
#' and we have chosen not to do this in quantstrat, because real quantitative 
#' trading systems do not have instant execution.  They make decisions 
#' (the Rules) and then enter orders (the province of this function in backtesting),
#' during which there is some \code{delay} between receiving the data that fires the 
#' Signal and Rule, and the time the order reaches the market, and then those orders 
#' \emph{MAY} become transactions if market prices and liquidity cooperate.  
#' 
#' By default, this function will locate and replace any 'open' order(s) 
#' on the requested portfolio/symbol that have the same order  
#' type and side.  If an orderset is also specified and replace=TRUE,
#' \emph{all open orders} for the orderset will be replaced.   
#' If you do not want open orders to be canceled and replaced with the new order,
#' set \code{replace=FALSE}.
#'  
#' We have modeled a 'limit' order, used to enter or exit a position at a specific price, determined by the
#' prefered price (see \code{prefer}) plus \code{threshold} (see below).
#' 
#' We have modeled two types of stop orders, which should be sufficient to model most types of stops.  
#' 
#' We have modeled the simplest type, a 'stoplimit' order, which is just a limit order used to enter 
#' or exit a position at a specific price, determined by the prefered price (see \code{prefer}) plus \code{threshold}
#' (see below). The stoplimit order type can be used to implement both stop-enter (long/buy or short/sell)
#' and stop-loss (long/sell or short/buy) style limit orders. There is no functional difference between a
#' regular 'limit' order and a 'stoplimit' order once entered into the order book, but the distinction will
#' likely be useful for reporting on when stops have been triggered.
#' 
#' We have also modeled a 'stoptrailing' order, which may be used to model dynamic limit-based exit. The
#' \code{threshold} will be calculated only once upon order entry (see below) and remain fixed for the life span
#' of the order. In this way, a 10 pct trailing exit will not change in size from the current price as the
#' price changes. Be aware that a stoptrailing order may be moved ("replaced") frequently.
#' 
#' Some markets and brokers recognize a stop that triggers a market order, when the stop is triggered, 
#' a market order will be executed at the then-prevailing price.  We have not modeled this type of order.   
#' 
#' We have also added the 'iceberg' order type.  This order type should
#' most often be paired with \code{delay} and \code{\link{osMaxPos}}.  The 
#' iceberg order when initially entered is treated like a limit 
#' order, with an optional \code{threshold} (which is applied at initial order 
#' entry, so be careful).  Right now, they will enter a new order at price+threshold
#' upon any execution of the prior iceberg order.  This process could 
#' be infinite if \code{\link{osMaxPos}} or an equivalent order sizing 
#' function is not used to limit total position size. An order \code{delay}
#' is also advisable to model the delay that occurs between getting the trade 
#' confirmation of the previous trade and entering the new order into the order book.
#'  
#' The 'limit', 'stoplimit', 'stoptrailing' and 'iceberg' order types are the only order types that make
#' use of the order \code{threshold}. Thresholds may be specified in one of 2 ways: as a scalar (\code{tmult=FALSE})
#' or as a multiplier for the current price (\code{tmult=TRUE}).
#' The threshold is then added to the prefered order price upon order entry. The correct sign for the threshold
#' (pos or neg, ie. add or subtract) is automagically figured out from the order side and the order quantity (buy or sell);
#' if the user provides the wrong sign for the threshold, then it will be reversed. In other words, the user may
#' provide all thresholds as a positive number, and the software will automagically figure out whether to add or
#' subtract the threshold amount from the price.
#'
#' If you ever wanted to move from a backtesting mode to a production mode, 
#' this function (and the linked funtion \code{\link{ruleOrderProc}}) would 
#' need to be replaced by functions that worked against your execution environment.  
#' Basically, the execution environment must provide three interfaces in a live 
#' trading environment:
#' 
#' \enumerate{
#'      \item a market data interface to provide updated market data, usually accessed in an event loop
#' 
#'      \item an order interface for sending orders (and canceling or updating them) to the market
#' 
#'      \item a fill interface that reports the transaction details when an order has been filled 
#' }
#' 
#' Conversion to a live trading environment will also likely require a new version of 
#' \code{\link{applyStrategy}} to provide the event loop interface and interact with \code{mktdata}.
#' 
#' @concept backtest
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param timestamp timestamp coercible to POSIXct that will be the time to search for orders before this time 
#' @param qty numeric quantity of the order, or "all" or "trigger"
#' @param price numeric price at which the order is to be inserted
#' @param ordertype one of "market","limit","stoplimit", "stoptrailing" or "iceberg"
#' @param side one of either "long" or "short" 
#' @param threshold numeric threshold to apply to limit, stoplimit, stoptrailing and iceberg orders, default NULL
#' @param orderset set a tag identifying the orderset
#' @param status one of "open", "closed", "canceled", "revoked", or "replaced", default "open"
#' @param statustimestamp timestamp of a status update, will be blank when order is initiated 
#' @param prefer the prefered order price (eg. 'Close')
#' @param delay what delay to add to timestamp when inserting the order into the order book, in seconds
#' @param tmult if TRUE, threshold is a percent multiplier for \code{price}, not a scalar. Threshold is converted to a scalar by multiplying it with the price, then added to the price just like a scalar threshold. 
#' @param replace TRUE/FALSE, whether to replace any other open order(s) on this symbol with the same properties as this order, default TRUE, see Details 
#' @param return if TRUE, return the row that makes up the order, default FALSE (will assign into the environment)
#' @param \dots any other passthru parameters
#' @param TxnFees numeric fees (usually negative) or function name for calculating TxnFees (processing happens later, not in this function)
#' @param label text label, default to '', set to rule label by \code{\link{ruleSignal}}
#' @seealso getOrderBook
#' @seealso updateOrders
#' @concept order book
#' @export
addOrder <- function(portfolio, 
                     symbol, 
                     timestamp, 
                     qty, 
                     price, 
                     ordertype, 
                     side, 
                     threshold=NULL, 
                     orderset='', 
                     status="open", 
                     statustimestamp='' , 
                     prefer=NULL, 
                     delay=.00001, 
                     tmult=FALSE, 
                     replace=TRUE, 
                     return=FALSE, 
                     ..., 
                     TxnFees=0,
                     label=''
             )
{
    # get order book
    #orderbook <- getOrderBook(portfolio)
    #if(!length(grep(symbol,names(orderbook[[portfolio]])))==1) stop(paste("symbol",symbol,"does not exist in portfolio",portfolio,"having symbols",names(orderbook[[portfolio]])))

    #data quality checks
    if(!is.numeric(qty) && !(qty=='all') && !(qty=='trigger')) stop (paste("Quantity must be numeric:",qty))
    if(qty==0) stop("qty",qty,"must positive, negative, or 'all' or 'trigger'")
    if(is.null(qty)) stop("qty",qty,"must not be NULL")
    if(is.na(qty)) stop("qty",qty,"must not be NA")
    if(!is.numeric(price)) stop (paste("Price must be numeric:",price))
    if(is.null(price)) stop("price ",price," must not be NULL")
    if(is.na(price)) stop("order at timestamp ", timestamp, " must not have price of NA")
    #spreads can have a zero price
    #if(price==0) warning(paste(ordertype, "order for", qty, "has a price of zero."))

    if(!is.null(side) & !length(grep(side,c('long','short')))==1) stop(paste("side:",side," must be one of 'long' or 'short'"))
    if(is.na(charmatch(ordertype,c("market","limit","stoplimit","stoptrailing","iceberg")))) stop(paste("ordertype:",ordertype,' must be one of "market","limit","stoplimit","stoptrailing" or "iceberg"'))
    if(!is.null(threshold) & length(price)>=1 )
    {
        if(length(grep(paste("^",ordertype,"$",sep=""),c("limit","stoplimit","stoptrailing","iceberg")))==1)
        {
            #we have a threshold set on a stop* order, process it
            switch(ordertype,
                limit =, 
                iceberg =, 
                stoplimit =, 
                stoptrailing = {
                    if(isTRUE(tmult))
                    {
                        threshold = price*threshold
                        tmult=FALSE
                    } 
                    if(!is.null(side))
                    {
                        switch(ordertype,
                            limit = {
                                if((qty %in% c('all','trigger')) && side == 'long' || !(qty %in% c('all','trigger')) && as.numeric(qty) < 0) # SELL
                                {
                                    #this is a limit exit, so it will sell *higher* than the current market
                                    if(threshold < 0) threshold = -threshold
                                }
                                else    # BUY
                                {
                                    #this is a limit exit, so it will buy *lower* than the current market
                                    if(threshold > 0) threshold = -threshold
                                }
                            },
                            stoplimit =,
                            stoptrailing = {
                                if((qty %in% c('all','trigger')) && side == 'long' || !(qty %in% c('all','trigger')) && as.numeric(qty) < 0) # SELL
                                {
                                    #this is a stop exit, so it will sell *lower* than the current market
                                    if(threshold > 0) threshold = -threshold
                                }
                                else    # BUY
                                {
                                    #this is a stop exit, so it will buy *higher* than the current market
                                    if(threshold < 0) threshold = -threshold
                                }
                            }
                        )
                    }
                    price = price+threshold                        
                }
            ) #end type switch
        } else {
            stop(paste("Threshold may only be applied to a limit, stop or iceberg order type",ordertype,threshold))
        }
    }

    if(is.null(threshold)) threshold=NA  #NA is not ignored like NULL is 

    if(!length(grep(status,c("open", "closed", "canceled", "revoked","replaced",'rejected')))==1) stop(paste("order status:",status,' must be one of "open", "closed", "canceled", "revoked", "replaced", or "rejected"'))
    # TODO do we need to check for collision, and increment timestamp?  or alternately update?

    # subset by time and symbol
    if(!is.null(timestamp)& length(timestamp)>=1){
        timespan <- format(timestamp, "::%Y-%m-%d %H:%M:%OS6")
    } else {
        # construct the timespan of the entire series
        timespan <- paste(format(index(first(orderbook)), "%Y-%m-%d %H:%M:%OS6"),
                          format(index( last(orderbook)), "%Y-%m-%d %H:%M:%OS6"), sep="::")
    }

    statustimestamp=NA # new orders don't have a status time

    #set up the other parameters
    if (!length(qty)==length(price)) qty <- rep(qty,length(price))
    if (!length(ordertype)==length(price)) ordertype <- rep(ordertype,length(price))
    if (!length(threshold)==length(price)) threshold <- rep(threshold,length(price))
    #if (!length(param)==length(price)) param <- rep(param,length(price))

    # insert new order
    if(is.timeBased(timestamp)) ordertime<-timestamp+delay
    else ordertime<-as.POSIXct(timestamp)+delay
    orders<-NULL
    for (i in 1:length(price)) {
        if(is.null(prefer[i])) prefer[i] = ''
        neworder<-xts(as.matrix(t(c(as.character(qty[i]), 
                                        price[i], 
                                        ordertype[i], 
                                        side, 
                                        threshold[i], 
                                        status, 
                                        statustimestamp, 
                                        prefer[i],
                                        orderset[i], 
                                        TxnFees, label))), 
                      order.by=(ordertime))
              
        if(is.null(orders)) orders<-neworder
        else orders <- rbind(orders,neworder)
    }

    if(ncol(orders)!=11) {
        print("bad order(s):")
        print(orders)
        return()
    }

    if(is.numeric(qty)) qtysign <- sign(drop(coredata(qty)))
    else qtysign <- NULL
    
    if(!isTRUE(return)){
        if(isTRUE(replace)) {
            updateOrders(portfolio=portfolio, 
                         symbol=symbol,
                         timespan=timespan, 
                         side=side, 
                         qtysign=qtysign,
                         orderset=orderset,
                         oldstatus="open", 
                         newstatus="replaced", 
                         statustimestamp=timestamp)
        }
        # get order book
        orderbook <- getOrderBook(portfolio)
        orderbook[[portfolio]][[symbol]]<-rbind(orderbook[[portfolio]][[symbol]],orders)
        # assign order book back into place (do we need a non-exported "put" function?)
        assign(paste("order_book",portfolio,sep='.'),orderbook,envir=.strategy)
        rm(orderbook)
        return()
    } else {
        return(orders)
    }
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
#' When a risk event or over-limit event happens, typically open orders will be 'revoked'.  
#' Possibly new orders will be added to close open positions.  
#' Many models will also want to run a process at the close of market that will cancel all open orders. 
#' 
#' If orderset is passed to updateOrders, all orders with oldstatus 
#' for the orderset will be updated, regardless of their other parameters.
#' 
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param timespan xts-style character timespan to be the period to find orders of the given status and ordertype 
#' @param ordertype one of NULL, "market","limit","stoplimit" or "stoptrailing" default NULL
#' @param side one of NULL, "long" or "short", default NULL 
#' @param qtysign one of NULL, -1,0,1 ; could be useful when all qty's are reported as positive numbers and need to be identified other ways, default NULL
#' @param orderset set a tag identifying the orderset
#' @param oldstatus one of NULL, "open", "closed", "canceled", "revoked", or "replaced", default "open"
#' @param newstatus one of "open", "closed", "canceled", "revoked", or "replaced"
#' @param statustimestamp timestamp of a status update, will be blank when order is initiated
#' @seealso addOrder
#' @seealso getOrders
#' @seealso getOrderBook 
#' @concept order book
#' @export
updateOrders <- function(portfolio, 
                         symbol, 
                         timespan, 
                         ordertype=NULL, 
                         side=NULL, 
                         qtysign=NULL, 
                         orderset=NULL,
                         oldstatus="open", 
                         newstatus, 
                         statustimestamp 
                 )
{ 
    #data quality checks
    if(!is.null(oldstatus) && !length(grep(oldstatus,c("open", "closed", "canceled", "revoked","replaced",'rejected')))==1) 
        stop(paste("old order status:",oldstatus,' must be one of "open", "closed", "canceled", "revoked", "replaced", or "rejected"'))
    if(!length(grep(newstatus,c("open", "closed", "canceled", "revoked","replaced",'rejected')))==1) 
        stop(paste("new order status:",newstatus,' must be one of "open", "closed", "canceled", "revoked", "replaced", or "rejected"'))
    if(!is.null(side) && !length(grep(side,c('long','short')))==1) 
        stop(paste("side:",side," must be one of 'long' or 'short'"))
    if(!is.null(qtysign) && (qtysign != -1 && qtysign != 1 && qtysign != 0))
        stop(paste("qtysign:",qtysign," must be one of NULL, -1, 0, or 1"))
    if(!is.null(ordertype) && is.na(charmatch(ordertype,c("market","limit","stoplimit","stoptrailing","iceberg")))) 
        stop(paste("ordertype:",ordertype,' must be one of "market","limit","stoplimit","stoptrailing" or "iceberg"'))
    if(!is.null(orderset) && newstatus=='replaced'){
        #replace any outstanding orders for this orderset
        ordertype=NULL
        side=NULL
        qtysign=NULL    
    }
    # need the ability to pass a range like we do in blotter
    updatedorders<-getOrders(portfolio=portfolio, 
                             symbol=symbol, 
                             status=oldstatus, 
                             timespan=timespan, 
                             ordertype=ordertype, 
                             side=side, 
                             qtysign=qtysign,
                             orderset=orderset,
                             which.i=TRUE) 
    if(length(updatedorders)>=1){
        # get order book 
        #TODO this gets the order book again after it was already retrieved by getOrdersByStatus.  
        # at some point, we should eliminate the double get
        orderbook <- getOrderBook(portfolio)
        
        orderbook[[portfolio]][[symbol]][updatedorders,"Order.Status"]<-newstatus
        orderbook[[portfolio]][[symbol]][updatedorders,"Order.StatusTime"]<- as.character(as.POSIXlt(statustimestamp, Sys.getenv('TZ')))
        # assign order book back into place (do we need a non-exported "put" function?)
        assign(paste("order_book",portfolio,sep='.'),orderbook,envir=.strategy)
    }
}

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2012
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, 
# Jeffrey Ryan, Joshua Ulrich, and Garrett See 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
