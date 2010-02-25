
#' default rule to generate a trade order on a signal
#' 
#' \code{pricemethod} may be one of 'market' or 'opside' 
#' which will either try to get the price of the 'market' at \code{timestamp} and use this as the order price
#' or 'opside' which will use the 'ask' price if you're buying and the 'bid' price if you're selling
#' 
#' If \code{threshold} is not numeric or \code{NULL} it should be the character string describing a function that can calculate a threshold.  
#' Ideally this will be a column lookup on a non-path-dependent indicator calculated in advance.
#' 
#' If \code{orderside} is NULL, the function will attempt to calculate the side from the current position 
#' (if any) and the order quantity.    
#'   
#' @param mktdata an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @param sigcol column name to check for signal
#' @param sigval signal value to match against
#' @param orderqty numeric quantity of the desired order, modified by osFUN
#' @param ordertype one of "market","limit","stoplimit", or "stoptrailing"
#' @param orderside one of either "long" or "short", default NULL, see details 
#' @param threshold numeric or function threshold to apply to trailing stop orders, default NULL, see Details
#' @param replace TRUE/FALSE, whether to replace any other open order(s) on this portfolio symbol, default TRUE 
#' @param delay what delay to add to timestamp when inserting the order into the order book, in seconds
#' @param osFUN function or text descriptor of function to use for order sizing, default \code{\link{osNoOp}}
#' @param pricemethod one of 'market' or 'opside', see Details
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param ... any other passthru parameters
#' @seealso \code{\link{osNoOp}}
#' @export
ruleSignal <- function(mktdata, timestamp, sigcol, sigval, orderqty=0, ordertype, orderside, threshold=NULL, replace=TRUE, delay=0.0001, osFUN='osNoOp', pricemethod=c('market','opside'), portfolio, symbol, ... ) {
    if(!is.function(osFUN)) osFUN<-match.fun(osFUN)
    if (!is.na(mktdata[timestamp][,sigcol]) & mktdata[timestamp][,sigcol] == sigval) {
        #TODO add fancy formals matching for osFUN
         
        orderqty <- osFUN(strategy=strategy, mktdata=mktdata, timestamp=timestamp, orderqty=orderqty, ordertype=ordertype, orderside=orderside, portfolio=portfolio, symbol=symbol)
        #calculate order price using pricemethod
        pricemethod<-pricemethod[1] #only use the first if not set by calling function
        switch(pricemethod,
               opside = {
                   if (orderqty>0) 
                       prefer='ask'  # we're buying, so pay what they're asking
                   else
                       prefer='bid'  # we're selling, so give it to them for what they're bidding
                   orderprice <- try(getPrice(x=mktdata,symbol=symbol,prefer=prefer))
               }, 
               market = { 
                   orderprice <- try(getPrice(x=mktdata,symbol=symbol,prefer=NULL)) 
               }  
        )
        if(inherits(orderprice,'try-error')) orderprice<-NULL
        if(length(orderprice>1)) orderprice<-last(orderprice[timestamp])
        if(is.null(orderside) & !orderqty == 0){
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
        if(!is.null(orderqty) & !orderqty == 0 & !is.null(orderprice)){
            addOrder(portfolio=portfolio, symbol=symbol, timestamp=timestamp, qty=orderqty, price=orderprice, ordertype=ordertype, side=orderside, threshold=threshold, status="open", replace=replace , delay=delay, ...)
        }
    }
}

#TODO ruleORSignal
#TODO ruleANDSingnal


#' default order sizing function 
#' 
#' default function performs no operation (NoOp), returns orderqty
#'  
#' @param mktdata an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @param orderqty numeric quantity of the desired order, modified by osFUN
#' @param ordertype one of "market","limit","stoplimit", or "stoptrailing"
#' @param orderside one of either "long" or "short" 
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @export
osNoOp <- function(orderqty, ...){
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
    newrow <- xts(c(maxpos, longlevels, minpos, shortlevels),order.by=as.POSIXct(timestamp))
    if(is.null(portf[[symbol]]$PosLimit)) {
        portf[[symbol]]$PosLimit <- newrow 
        colnames(portf[[symbol]]$PosLimit)<-c("MaxPos","LongLevels","MinPos","ShortLevels")
    } else {
        if(!is.null(portf[[symbol]]$PosLimit[timestamp])){
            # it exists already, so replace
            portf[[symbol]]$PosLimit[timestamp]<-newrow
        } else {
            # add a new row on timestamp
            portf[[symbol]]$PosLimit <- rbind(portf[[symbol]]$PosLimit,newrow)
        }
    }
    assign(paste("portfolio",portfolio,sep='.'),portf,envir=.blotter)
}

#' get position and level limits on timestamp
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @seealso \code{\link{addPosLimit}},\code{\link{osMaxPos}}
getPosLimit <- function(portfolio, symbol, timestamp){
    portf<-getPortfolio(portfolio)
    # try to get on timestamp, otherwise find the most recent
    toDate = paste('::', timestamp, sep="")
    PosLimit = last(portf[[symbol]]$PosLimit[toDate])
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
#' @param mktdata an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be inserted on 
#' @param orderqty numeric quantity of the desired order, modified by osFUN
#' @param ordertype one of "market","limit","stoplimit", or "stoptrailing"
#' @param orderside one of either "long" or "short" 
#' @param portfolio text name of the portfolio to place orders in
#' @param symbol identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @seealso \code{\link{addPosLimit}},\code{\link{getPosLimit}}
#' @export
osMaxPos <- function(mktdata, timestamp, orderqty, ordertype, orderside, portfolio, symbol){
    # check for current position
    pos<-getPosQty(portoflio,symbol,timestamp)
    # check against max position
    PosLimit<-getPosLimit(portoflio,symbol,timestamp)
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
            if(oderqty+pos>PosLimit[,"MaxPos"]) orderqty <- PosLimit[,"MaxPos"]-pos
        }
        return(orderqty)
    }
    
    #sell long
    if(orderqty<0 & orderside=='long'){
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
                orderqty = round(PosLimit[,"MinPos"]/PosLimit[,"LongLevels"],0) #note no round lots
            }
        } else {
            # this order would put us over the MinPos limit
            orderqty<-ifelse((PosLimit[,"MinPos"]-pos)>=round(PosLimit[,"MinPos"]/PosLimit[,"ShortLevels"],0),PosLimit[,"MinPos"]-pos, round(PosLimit[,"MinPos"]/PosLimit[,"ShortLevels"],0)) 
            if(oderqty+pos>PosLimit[,"MaxPos"]) orderqty <- PosLimit[,"MinPos"]-pos
        }
        return(orderqty)
    }
    
    #buy cover short
    if(orderqty>0 & orderside=='short'){
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
# Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
