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
#' @param \dots any other passthrough parameters
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
    ordertemplate<-xts(as.matrix(t(c(0,NA,"init","long",0,"closed",as.character(as.POSIXct(initDate)),1,0))),order.by=as.POSIXct(initDate), ...=...)
    colnames(ordertemplate) <- c("Order.Qty","Order.Price","Order.Type","Order.Side","Order.Threshold","Order.Status","Order.StatusTime","Order.Set","Txn.Fees")

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
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param status one of "open", "closed", "canceled", or "replaced", default "open"
#' @param timespan xts-style character timespan to be the period to find orders of the given status and ordertype
#' @param ordertype one of NULL, "market","limit","stoplimit", "stoptrailing", or "iceberg" default NULL
#' @param side one of NULL, "long" or "short", default NULL 
#' @param qtysign one of NULL, -1,0,1 ; could be useful when all qty's are reported as positive numbers and need to be identified other ways, default NULL
#' @param which.i if TRUE, return the row index numbers rather than the order rows matching the criteria, default FALSE
#' @export
getOrders <- function(portfolio,symbol,status="open",timespan=NULL,ordertype=NULL, side=NULL, qtysign=NULL, which.i=FALSE)
{
    #if(is.null(timespan)) stop("timespan must be an xts style timestring")
    # get order book
    orderbook <- getOrderBook(portfolio)
    if(!any(names(orderbook[[portfolio]]) == symbol)) stop(paste("symbol",symbol,"does not exist in portfolio",portfolio,"having symbols",names(orderbook[[portfolio]])))
    ordersubset<-orderbook[[portfolio]][[symbol]]

    #data quality checks
    if(!is.null(status) & !length(grep(status,c("open", "closed", "canceled","replaced")))==1) stop(paste("order status:",status,' must be one of "open", "closed", "canceled", or "replaced"'))
    if(!is.null(ordertype)) {
        if(is.na(charmatch(ordertype,c("market","limit","stoplimit","stoptrailing","iceberg")))){
            stop(paste("ordertype:",ordertype,' must be one of "market","limit","stoplimit", "stoptrailing", or "iceberg"'))
        }
    }

    indices <- which(#if(!is.null(timespan)) ordersubset[timespan,which.i=TRUE] else TRUE &
                     (if(!is.null(status)) ordersubset[,"Order.Status"]==status else TRUE) &
                     (if(!is.null(ordertype)) ordersubset[,"Order.Type"]==ordertype else TRUE) &
                     (if(!is.null(side)) ordersubset[,"Order.Side"]==side else TRUE) &
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
#' on the requested portfolio/symbol that have the same type and side.  
#' This is the equivalent of what is sometimes called an 
#' OCO (Order Cancels Other) order.  If you do not want the function to 
#' behave this way, set \code{replace=FALSE}.
#'  
#' We have modeled two types of stop orders, which should be sufficient to model most types of stops.  
#' 
#' We have modeled the simplest type, a 'stoplimit' order, which is just a limit order used to enter 
#' or exit a position at a specific price.  
#' Threshold multipliers have also been added for stoplimit.  These allow a threshold to be set as a multiplier 
#' of the current price. For example, to set a stoplimit order at the current price 
#' plus ten percent, you would set the threshold multiplier to 1.10.  The threshold multiplier (or scalar) on a 
#' stoplimit order will be converted to a price at order entry.   
#' There is no functional different between a regular 'limit'
#' order and a 'stoplimit' order once entered into the order book, but the distinction will likely 
#' be useful for reporting on when stops have been triggered.
#' 
#' We have also modeled a 'stoptrailing' order, which may be used to model dynamic limit-based entry or exit.  
#' If you set \code{tmult=TRUE} on a stoptrailing order, the size of the threshold will be set as a 
#' difference between the multiplier times the price and the current price at order entry.  in this way, a 10%
#' trailing entry (exit) will not change in size from the current price as the price changes.  It is effectively 
#' converted to a scalar at order entry.  While this functionality could change in the future, 
#' we believe this is more conservative than growing or shrinking the threshold distance from the current market price 
#' in relation to the threshold, and will result in fewer unintended consequences and more understandable behavior. Comments Welcome.
#' 
#' The 'stop*' or 'iceberg' order types are the only order type that makes use of the order \code{threshold}.   
#' Scalar thresholds \code{tmult=FALSE} on stoplimit or stoptrailing
#' orders will be added to the current market price to set the limit price.  In other worlds, a
#' scalar threshold is the difference either positive or negative from the current price when 
#' the order is entered. With a stoptrailing order, the order may be moved (via cancel/replace) frequently.
#' 
#' Some markets and brokers recognize a stop that triggers a market order, when the stop is triggered, 
#' a market order will be executed at the then-prevailing price.  We have not modeled this type of order.   
#' 
#' We have also added the 'iceberg' order type.  This order type should
#' most often be paired with \code{delay} and \code{\link{osMaxPos}}.  The 
#' iceberg order when initially entered is treated like a limit 
#' order, with an optional threshold (which is applied at initial order 
#' entry, so be careful).  Right now, they will enter a new order at price+threshold
#' upon any execution of the prior iceberg order.  This process could 
#' be infinite if \code{\link{osMaxPos}} or an equivalent order sizing 
#' function is not used to limit total position size. An order \code{delay}
#' is also advisable to model the delay that occurs between getting the trade 
#' confirmation of the previous trade and entering the new order into the order book.
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
#' @param qty numeric quantity of the order
#' @param price numeric price at which the order is to be inserted
#' @param ordertype one of "market","limit","stoplimit", "stoptrailing",or "iceberg"
#' @param side one of either "long" or "short" 
#' @param threshold numeric threshold to apply to trailing stop orders, default NULL
#' @param status one of "open", "closed", "canceled", or "replaced", default "open"
#' @param statustimestamp timestamp of a status update, will be blank when order is initiated 
#' @param delay what delay to add to timestamp when inserting the order into the order book, in seconds
#' @param tmult if TRUE, threshold is a percent multiplier for \code{price}, not a scalar to be added/subtracted from price.  threshold will be dynamically converted to a scalar at time of order entry
#' @param replace TRUE/FALSE, whether to replace any other open order(s) on this portfolio symbol, default TRUE 
#' @param return if TRUE, return the row that makes up the order, default FALSE (will assign into the environment)
#' @param \dots any other passthru parameters
#' @param TxnFees numeric fees (usually negative) or function name for calculating TxnFees (processing happens later, not in this function)
#' @export
addOrder <- function(portfolio, symbol, timestamp, qty, price, ordertype, side, threshold=NULL, status="open", statustimestamp='' , delay=.00001, tmult=FALSE, replace=TRUE, return=FALSE, ..., TxnFees=0)
{
    # get order book
    #orderbook <- getOrderBook(portfolio)
    #if(!length(grep(symbol,names(orderbook[[portfolio]])))==1) stop(paste("symbol",symbol,"does not exist in portfolio",portfolio,"having symbols",names(orderbook[[portfolio]])))

    #data quality checks
    if(!is.numeric(qty)) stop (paste("Quantity must be numeric:",qty))
    if(qty==0) stop("qty",qty,"must be positive or negative")
    if(is.null(qty)) stop("qty",qty,"must not be NULL")
    if(is.na(qty)) stop("qty",qty,"must not be NA")
    if(!is.numeric(price)) stop (paste("Price must be numeric:",price))
    if(is.null(price)) stop("price ",price," must not be NULL")
    if(is.na(price)) stop("order at timestamp ", timestamp, " must not have price of NA")
    if(price==0) warning(paste(ordertype, "order for", qty, "has a price of zero."))

    if(!is.null(side) & !length(grep(side,c('long','short')))==1) stop(paste("side:",side," must be one of 'long' or 'short'"))
    if(is.na(charmatch(ordertype,c("market","limit","stoplimit","stoptrailing","iceberg")))) stop(paste("ordertype:",ordertype,' must be one of "market","limit","stoplimit","stoptrailing", or"iceberg"'))
    if(!is.null(threshold) & length(price)>=1 ) {
        if(length(grep(ordertype,c("stoplimit","stoptrailing","iceberg")))==1) {
            #we have a threshold set on a stop* order, process it
            switch(ordertype,
                    stoplimit =, 
                    iceberg = {
                        # handle setting the stop limit price
                        if(isTRUE(tmult)){
                            price = price*threshold
                        } else {
                            price = price+threshold
                        }
                    },
                    stoptrailing = {
                        if(isTRUE(tmult)){
                            #get the difference between the threshold*price and the price
                            threshold = (price*threshold)-price
                            tmult=FALSE
                            #this sets the threshold as a fixed number for later trailing orders 
                        } 
                        price = price+threshold                        
                    }
            ) #end type switch
        } else {
            stop(paste("Threshold may only be applied to a stop or iceberg order type",ordertype,threshold))
        }
    }

    if(is.null(threshold)) threshold=NA  #NA is not ignored like NULL is 

    if(!length(grep(status,c("open", "closed", "canceled","replaced")))==1) stop(paste("order status:",status,' must be one of "open", "closed", "canceled", or "replaced"'))
    # TODO do we need to check for collision, and increment timestamp?  or alternately update?

    # subset by time and symbol
    if(!is.null(timestamp)& length(timestamp)>=1){
        timespan<-paste("::",timestamp,sep='')
    } else {
        # construct the timespan of the entire series
        timespan=paste(index(first(orderbook),index(last(orderbook)),sep='::'))
    }

    statustimestamp=NA # new orders don't have a status time

    #handle order sets
    #get the order set if length(price)>1
    if(length(price)>1) {
        order.set<-max(na.omit(getOrders(portfolio=portfolio, symbol=symbol, status='open', timespan=timespan, ordertype=NULL, side=NULL,which.i=FALSE)$Order.Set))
        if(is.na(order.set)) order.set<-1
    } else {    
        order.set=NA
    }

    #set up the other parameters
    if (!length(qty)==length(price)) qty <- rep(qty,length(price))
    if (!length(ordertype)==length(price)) ordertype <- rep(ordertype,length(price))
    if (!length(threshold)==length(price)) threshold <- rep(threshold,length(price))
    #if (!length(param)==length(price)) param <- rep(param,length(price))

    # insert new order
    if(is.timeBased(timestamp)) ordertime<-timestamp+delay
    else ordertime<-as.POSIXct(timestamp)+delay

    order<-NULL
    for (i in 1:length(price)){
        neworder<-xts(as.matrix(t(c(as.numeric(qty[i]), price[i], ordertype[i], side, threshold[i], status, statustimestamp, order.set,TxnFees))),order.by=(ordertime))
        if(is.null(order)) order<-neworder
        else order <- rbind(order,neworder)
    }

    if(ncol(order)!=9) {
        print("bad order(s):")
        print(order)
        next()
    }

    qtysign <- sign(drop(coredata(qty)))
    
    if(!isTRUE(return)){
        if(isTRUE(replace)) updateOrders(portfolio=portfolio, symbol=symbol,timespan=timespan, ordertype=ordertype, side=side, qtysign=qtysign, oldstatus="open", newstatus="replaced", statustimestamp=timestamp)
        # get order book
        orderbook <- getOrderBook(portfolio)
        orderbook[[portfolio]][[symbol]]<-rbind(orderbook[[portfolio]][[symbol]],order)
        # assign order book back into place (do we need a non-exported "put" function?)
        assign(paste("order_book",portfolio,sep='.'),orderbook,envir=.strategy)
        rm(orderbook)
        return()
    } else {
        return(order)
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
#' When a risk event or over-limit event happens, typically open orders will be 'canceled'.  
#' Possibly new orders will be added to close open positions.  
#' Many models will also want to run a process at the close of market that will cancel all open orders. 
#' 
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param timespan xts-style character timespan to be the period to find orders of the given status and ordertype 
#' @param ordertype one of NULL, "market","limit","stoplimit", or "stoptrailing" default NULL
#' @param side one of NULL, "long" or "short", default NULL 
#' @param qtysign one of NULL, -1,0,1 ; could be useful when all qty's are reported as positive numbers and need to be identified other ways, default NULL
#' @param oldstatus one of NULL, "open", "closed", "canceled", or "replaced", default "open"
#' @param newstatus one of "open", "closed", "canceled", or "replaced"
#' @param statustimestamp timestamp of a status update, will be blank when order is initiated 
#' @export
updateOrders <- function(portfolio, symbol, timespan, ordertype=NULL, side=NULL, qtysign=NULL, oldstatus="open", newstatus, statustimestamp) 
{ 
    #data quality checks
    if(!is.null(oldstatus) & !length(grep(oldstatus,c("open", "closed", "canceled","replaced")))==1) 
        stop(paste("old order status:",oldstatus,' must be one of "open", "closed", "canceled", or "replaced"'))
    if(!length(grep(newstatus,c("open", "closed", "canceled","replaced")))==1) 
        stop(paste("new order status:",newstatus,' must be one of "open", "closed", "canceled", or "replaced"'))
    if(!is.null(side) & !length(grep(side,c('long','short')))==1) 
        stop(paste("side:",side," must be one of 'long' or 'short'"))
    if(!is.null(qtysign) && (qtysign != -1 && qtysign != 1 && qtysign != 0))
        stop(paste("qtysign:",qtysign," must be one of NULL, -1, 0, or 1"))
    if(!is.null(ordertype) & is.na(charmatch(ordertype,c("market","limit","stoplimit","stoptrailing","iceberg")))) 
        stop(paste("ordertype:",ordertype,' must be one of "market","limit","stoplimit","stoptrailing", or "iceberg"'))
    
    # need the ability to pass a range like we do in blotter
    updatedorders<-getOrders(portfolio=portfolio, symbol=symbol, status=oldstatus, timespan=timespan, ordertype=ordertype, side=side, qtysign=qtysign, which.i=TRUE) 
    if(length(updatedorders)>=1){
        # get order book 
        #TODO this gets the order book again after it was already retrieved by getOrdersByStatus.  
        # at some point, we should eliminate the double get
        orderbook <- getOrderBook(portfolio)
        
        orderbook[[portfolio]][[symbol]][updatedorders,"Order.Status"]<-newstatus
        orderbook[[portfolio]][[symbol]][updatedorders,"Order.StatusTime"]<-as.character(statustimestamp)
        # assign order book back into place (do we need a non-exported "put" function?)
        assign(paste("order_book",portfolio,sep='.'),orderbook,envir=.strategy)
    }
}


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
#' 
#' This function is meant to be sufficient for backtesting most strategies, 
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
#' @export
ruleOrderProc <- function(portfolio, symbol, mktdata, timespan=NULL, ordertype=NULL, ..., slippageFUN=NULL)
{
    if(is.null(timespan)) return()
    orderbook <- getOrderBook(portfolio)
    ordersubset <- orderbook[[portfolio]][[symbol]]
    
    # get open orders
    procorders=NULL
    procorders<-getOrders(portfolio=portfolio, symbol=symbol, status="open", timespan=timespan, ordertype=ordertype, which.i=TRUE)

    if(hasArg(prefer)) prefer=match.call(expand.dots=TRUE)$prefer
    else prefer = NULL

    # check for open orders
    if (length(procorders)>=1){
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
        for (ii in procorders ){
            txnprice=NULL
            txnfees=ordersubset[ii,"Txn.Fees"]
            orderPrice <- as.numeric(ordersubset[ii,"Order.Price"])
            orderQty <- as.numeric(ordersubset[ii,"Order.Qty"])
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
                                monthly = ,
                                daily = {
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
                            if (orderQty > 0) { # positive quantity 'buy'
                                if( (has.Lo(mktdata) && orderprice > as.numeric(Lo(mktdataTimestamp))) || 
                                    (!has.Lo(mktdata) && orderprice >= as.numeric(getPrice(mktdataTimestamp, prefer=prefer))))
                                {
                                    txnprice = orderPrice
                                    txntime = timestamp
                                } else next() # price did not move through my order, should go to next order  
                            } else if(orderQty < 0) { #negative quantity 'sell'
                                if ( (has.Hi(mktdata) && orderprice < as.numeric(Hi(mktdataTimestamp))) ||
                                     (!has.Hi(mktdata) && orderprice <= as.numeric(getPrice(mktdataTimestamp,prefer=prefer))) )
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
                                ordersubset[ii,"Order.StatusTime"]<-as.character(timestamp)
                                next()
                            }
                        }
                        # else next
                    }
            )
            #if(!is.null(txnprice) & !isTRUE(is.na(txnprice)))
            if(ifelse(is.null(txnprice),FALSE,!is.na(txnprice))) {  # eliminate warning for is.na(NULL) -- jmu
                #make sure we don't cross through zero

                pos<-getPosQty(portfolio,symbol,timestamp)
                              pos<-getPosQty(portfolio,symbol,timestamp)
                if ( (pos > 0 && orderQty < -pos) || (pos < 0 && orderQty > -pos) ){
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
