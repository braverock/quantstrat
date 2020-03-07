#' process open orders at time \emph{t}, generating transactions or new orders
#' 
#' The ruleOrderProc function is effectively the default fill simulator for quantstrat. 
#' This function is meant to be sufficient for backtesting most strategies, 
#' but would need to be replaced for production use.  It provides the interface 
#' for taking the order book and determining when orders become trades.
#'  
#' In this version, in contrast with an earlier version, 
#' this function will allow a transaction to cross your current 
#' position through zero.  The accounting rules for realizing gains in such cases 
#' are quite complicated, so blotter will split this transaction into two transactions.  
#' Many brokers will break, revise, or split such transactions for the same reason.
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
#' @param timestamp timestamp coercible to POSIXct that will be the time the order will be processed on 
#' @param ordertype one of NULL, "market","limit","stoplimit", or "stoptrailing" default NULL
#' @param ... any other passthru parameters
#' @param slippageFUN default  NULL, not yet implemented
#' @seealso add.rule
#' @seealso applyRules
#' @seealso getOrderBook
#' @seealso addOrder
#' @seealso updateOrders
#' @export
ruleOrderProc <- function(portfolio, symbol, mktdata, timestamp=NULL, ordertype=NULL, ..., slippageFUN=NULL)
{
  if(is.null(timestamp)) return()
  # Get row index of timestamp for faster subsetting
  if(hasArg(curIndex))
    curIndex <- eval(match.call(expand.dots=TRUE)$curIndex, parent.frame())
  else
    curIndex <- mktdata[timestamp,which.i=TRUE]
  
  orderbook <- getOrderBook(portfolio)
  ordersubset <- orderbook[[portfolio]][[symbol]]
  
  ### retrieve open orders
  OpenOrders.i<-getOrders(portfolio=portfolio, symbol=symbol, status="open", timespan=timespan, ordertype=ordertype, which.i=TRUE)

  #extract time in force for open orders
  tif.xts <- ordersubset[OpenOrders.i, 'Time.In.Force']
  if(any(!tif.xts==''))
  {
    if (any(tclass(ordersubset)=='Date'))
        tif <- as.Date(coredata(tif.xts))
    else
      {
        tif <- strptime(coredata(tif.xts), format='%Y-%m-%d %H:%M:%OS')
        tif.na <- is.na(tif)
        if(any(tif.na))
          tif[tif.na] <- strptime(coredata(tif.xts[tif.na]), format='%Y-%m-%d %H:%M:%S')
      }
    
    #check which ones should be expired
    ExpiredOrders.i<-which(tif<timestamp)

    ordersubset[OpenOrders.i[ExpiredOrders.i], "Order.Status"] = 'expired'  
    ordersubset[OpenOrders.i[ExpiredOrders.i], "Order.StatusTime"]<-ordersubset[OpenOrders.i[ExpiredOrders.i], "Time.In.Force"]
  }
  
  if(hasArg(prefer)) prefer=match.call(expand.dots=TRUE)$prefer
  else prefer = NULL
  
  # check for open orders
  if (!(length(OpenOrders.i)>=1)){
    return(NULL)  
    } else {
      mktdataTimestamp <- mktdata[curIndex]
      # only keep the last observation per time stamp
      if( NROW(mktdataTimestamp) > 1 ) mktdataTimestamp <- last(mktdataTimestamp)
      isOHLCmktdata <- is.OHLC(mktdata)
      isBBOmktdata  <- is.BBO(mktdata)
    
      for (ii in OpenOrders.i )
        {
        if(ordersubset[ii, "Order.Status"] != "open")   # need to check this bc side effects may have changed order.status in this loop
        next()
      
      txnprice=NULL
      
      txnfees=ordersubset[ii,"Txn.Fees"]
      
      orderPrefer=ordersubset[ii, "Prefer"]
      if(!orderPrefer=="") prefer=orderPrefer
      
      orderPrice <- as.numeric(ordersubset[ii,"Order.Price"])
      
      orderQty <- ordersubset[ii,"Order.Qty"]
      if(orderQty %in% c('all','trigger'))
        {
        # this has to be an exit or risk order, so: 
        orderQty=-1*getPosQty(Portfolio=portfolio,Symbol=symbol,Date=timestamp)
        orderside<-ordersubset[ii, "Order.Side"]
        if(((orderQty>0 && orderside=='long') || (orderQty<0 && orderside=='short')))
          {
          # this condition may occur if (for example) a signal triggers an 'increase LONG pos' and 'close all SHORT pos' simultaneously
          # hence this is legal condition, and we must 0 the orderQty to reject the order
          
          orderQty = 0
        }
      }
      orderQty<-as.numeric(orderQty)
      
      orderThreshold <- as.numeric(ordersubset[ii,"Order.Threshold"])
      # mktdataTimestamp <- mktdata[timestamp]
      #FIXME Should we only keep the last observation per time stamp?
      #if( NROW(mktdataTimestamp) > 1 ) mktdataTimestamp <- last(mktdataTimestamp)
      
      orderType <- ordersubset[ii,"Order.Type"]
      
      if(hasArg(allowMagicalThinking)) allowMagicalThinking=list(...)$allowMagicalThinking
      else allowMagicalThinking = FALSE
      
      # Get cached frequency, if it's available
      if(hasArg(periodicity))
          freq <- eval(match.call(expand.dots=TRUE)$periodicity, envir=parent.frame())
      else
          freq <- periodicity(mktdata)
      #switch on frequency
      switch(orderType,
             market = {
               switch(freq$scale,
                      yearly = ,
                      quarterly = ,
                      monthly = {
                        txntime=as.character(index(ordersubset[ii,])) # transacts on this bar, e.g. in the intraday cross, or leading into the end of month, quarter, etc.
                        # txntime=as.character(timestamp) # use this if you wanted to transact on the close of the next bar
                        # txnprice=as.numeric(getPrice(mktdataTimestamp, prefer=prefer)[,1])
                        txnprice = orderPrice
                      },
                      daily = {
                        if(isTRUE(allowMagicalThinking)){
                          txntime=as.character(index(ordersubset[ii,])) # transacts on this bar, e.g. in the intraday cross, or leading into the end of month, quarter, etc.
                          #txnprice=as.numeric(getPrice(mktdataTimestamp, prefer=prefer)[,1])
                          txnprice = orderPrice
                        } else {
                          txntime = timestamp
                          txnprice = as.numeric(getPrice(mktdataTimestamp, prefer=prefer)[,1]) #filled at now-prevailing 'price'
                        }
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
                      }
                    ) # end switch on frequency
             },
             limit= ,
             stoplimit =,
             iceberg = {
               if (!isBBOmktdata) {
                 if( orderType == 'iceberg'){
                   stop("iceberg orders only supported for BBO data")
                 } 
                 # check to see if price moved through the limit                        
                 if((orderQty > 0 && orderType != 'stoplimit') || (orderQty < 0 && (orderType=='stoplimit'))) {
                   # buy limit, or sell stoplimit
                   if( (has.Lo(mktdata) && orderPrice > as.numeric(Lo(mktdataTimestamp)[,1])) || 
                         (!has.Lo(mktdata) && orderPrice > as.numeric(getPrice(mktdataTimestamp, prefer=prefer)[,1])))
                   {
                     if(orderType == 'stoplimit')
                         txnprice <- min(orderPrice, Op(mktdataTimestamp)[,1])
                     else
                         txnprice <- orderPrice
                     txntime = timestamp
                   } else next() # price did not move through my order, should go to next order  
                 } else if((orderQty < 0 && orderType != 'stoplimit') || (orderQty > 0 && (orderType=='stoplimit'))) { 
                   # sell limit or buy stoplimit
                   if ( (has.Hi(mktdata) && orderPrice < as.numeric(Hi(mktdataTimestamp)[,1])) ||
                          (!has.Hi(mktdata) && orderPrice < as.numeric(getPrice(mktdataTimestamp,prefer=prefer)[,1])) )
                   {
                     if(orderType == 'stoplimit')
                         txnprice <- max(orderPrice, Op(mktdataTimestamp)[,1])
                     else
                         txnprice <- orderPrice
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
                                      replace=FALSE, 
                                      return=TRUE,
                                      ...=..., 
                                      TxnFees=txnfees)
                   
                   ordersubset<-rbind(ordersubset, neworder)
                   
                   ordersubset[ii,"Order.Status"]<-'replaced'
                   ordersubset[ii,"Order.StatusTime"]<-format(timestamp, "%Y-%m-%d %H:%M:%S")
                   next()
                 } 
               }
             },
             stoptrailing = {
               if(isBBOmktdata)
               {
                 
                 order.side <- ordersubset[ii, "Order.Side"]
                 mvstop <- FALSE
                 absOrderThreshold <- abs(orderThreshold)
                 # +++++++++ stop order price - buy
                 # ~~~~~~~~~ market price
                 # --------- stop order price - sell
                 if(orderQty > 0) {  # positive quantity 'buy'
                   prefer <- 'ask'
                   mktPrice <- as.numeric(getPrice(mktdataTimestamp,prefer=prefer)[,1])
                   # check to see if price moved through the limit
                   if(mktPrice >= orderPrice) {  # buy when price >= stop
                     txnprice <- mktPrice
                     txntime <- timestamp
                   }
                   # move stop if price < stop - thresh
                   else {
                     mvstop <- orderPrice - absOrderThreshold > mktPrice
                     new.order.price <- min(orderPrice, mktPrice + absOrderThreshold)
                     #new.order.price <- mktPrice + absOrderThreshold
                   }
                 } else {  # negative quantity 'sell'
                   prefer <- 'bid'
                   mktPrice <- as.numeric(getPrice(mktdataTimestamp,prefer=prefer)[,1])
                   # check to see if price moved through the limit
                   if(mktPrice <= orderPrice) {  # sell when price <= stop
                     txnprice <- mktPrice
                     txntime <- timestamp
                   }
                   # move stop if price > stop + thresh
                   else {
                     mvstop <- orderPrice + absOrderThreshold < mktPrice
                     new.order.price <- max(orderPrice, mktPrice - absOrderThreshold)
                     #new.order.price <- mktPrice - absOrderThreshold
                   }
                 }
                 if( isTRUE(mvstop) ){
                   # if ordersubset[ii, "Order.Qty"] was character, we must recover it
                   new.order.qty <- ordersubset[ii, "Order.Qty"]
                   if(!is.na(suppressWarnings(as.numeric(new.order.qty))))
                     new.order.qty <- as.numeric(new.order.qty)
                   
                   neworder<-addOrder(portfolio=portfolio,
                                      symbol=symbol,
                                      timestamp=timestamp,
                                      qty=new.order.qty,
                                      price=new.order.price-orderThreshold,
                                      ordertype=orderType,
                                      prefer=prefer,
                                      side=order.side,
                                      threshold=orderThreshold,
                                      status="open",
                                      replace=FALSE, return=TRUE,
                                      orderset=ordersubset[ii,"Order.Set"],
                                      label=ordersubset[ii,"Rule"],
                                      ...=..., TxnFees=txnfees)
                   
                   ordersubset<-rbind(ordersubset, neworder)
                   
                   ordersubset[ii,"Order.Status"]<-'replaced'
                   ordersubset[ii,"Order.StatusTime"]<-format(timestamp, "%Y-%m-%d %H:%M:%S")
                   next()
                 }
                 # else next
               }
               else if(isOHLCmktdata)
               {
                 # check to see if price moved through the limit
                 
                 order.side <- ordersubset[ii, "Order.Side"]
                 
                 if(order.side == 'long'  && as.numeric(Lo(mktdataTimestamp)[,1]) < orderPrice
                    || order.side == 'short' && as.numeric(Hi(mktdataTimestamp)[,1]) > orderPrice)
                 {
                   if (order.side == 'long') {
                     txnprice <- ifelse(orderPrice < Op(mktdataTimestamp)[,1], orderPrice, Op(mktdataTimestamp)[,1]) # assume we can unwind long position at the open price if open price < order price
                     } else if (order.side == 'short') {
                       txnprice <- ifelse(orderPrice > Op(mktdataTimestamp)[,1], orderPrice, Op(mktdataTimestamp)[,1]) # assume we can unwind short position at the open price if open price > order price
                       }
                   ordersubset[ii,"Order.Price"] <- txnprice
                   txntime <- timestamp
                   } else { # do we need to change the trailing stop?
                     order.threshold <- as.numeric(ordersubset[ii, "Order.Threshold"])
                     
                     if(order.side == 'long')
                       new.order.price <- max(orderPrice, as.numeric(Hi(mktdataTimestamp)[,1]) + order.threshold)
                     if(order.side == 'short')
                       new.order.price <- min(orderPrice, as.numeric(Lo(mktdataTimestamp)[,1]) + order.threshold)
                     if(new.order.price != orderPrice) { # adjust trailing stop
                       order.qty <- ordersubset[ii, "Order.Qty"]   # if orderQty='all' we must recover it
                       neworder<-addOrder(portfolio=portfolio,
                                          symbol=symbol,
                                          timestamp=timestamp,
                                          qty=order.qty,
                                          price=new.order.price - order.threshold,
                                          ordertype=orderType,
                                          side=order.side,
                                          threshold=order.threshold,
                                          status="open",
                                          replace=FALSE, return=TRUE,
                                          orderset=ordersubset[ii,"Order.Set"],
                                          label=ordersubset[ii,"Rule"],
                                          ...=..., TxnFees=txnfees)
                       ordersubset<-rbind(ordersubset, neworder)
                       
                       ordersubset[ii,"Order.Status"]<-'replaced'
                       ordersubset[ii,"Order.StatusTime"]<-format(timestamp, "%Y-%m-%d %H:%M:%S")
                       
                       next()
                     }
                   }
                 }
               }# end stoptrailing
      )
      
      if(!is.null(txnprice) && !isTRUE(is.na(txnprice)))
      {
        #make sure we don't cross through zero
        pos<-getPosQty(portfolio,symbol,timestamp)
        
        if (orderQty == 0)  # reject the order (should be exit/market/all)
        {
          ordersubset[ii,"Order.Status"]<-'rejected'
        }
        else    #add the transaction
        {
          if(ordersubset[ii,"Order.Qty"] != 'trigger')
          {
            addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=txntime, 
                   TxnQty=orderQty, TxnPrice=txnprice , ...=..., TxnFees=txnfees)
          }
          ordersubset[ii,"Order.Status"]<-'closed'
        }
        ordersubset[ii,"Order.StatusTime"]<-format(timestamp, "%Y-%m-%d %H:%M:%S")
        
        #close all other orders in the same order set
        OrdersetTag = toString(ordersubset[ii,"Order.Set"])
        OpenInOrderset.i = which(ordersubset[,"Order.Status"] == 'open' & ordersubset[,"Order.Set"] == OrdersetTag)
        
        # skip this if there are no orders
        if(length(OpenInOrderset.i) > 0)
        {
          ordersubset[OpenInOrderset.i, "Order.Status"] = 'canceled'
          ordersubset[OpenInOrderset.i, "Order.StatusTime"]<-format(timestamp, "%Y-%m-%d %H:%M:%S")
          
        } 
      }
    } #end loop over open orders  
    
    # now put the orders back in
    # assign order book back into place (do we need a non-exported "put" function?)
    orderbook[[portfolio]][[symbol]] <- ordersubset
    assign(paste("order_book",portfolio,sep='.'),orderbook,envir=.strategy)
  } # end check for open orders
  
  # return list of orers filled in this call for order chain processing
  if(length(OpenOrders.i) > 0)
  {
    OpenOrders <- ordersubset[OpenOrders.i,]
    JustClosedOrders.i <- which(OpenOrders[,"Order.Status"]=="closed")
    
    if(length(JustClosedOrders.i) > 0)
      return( OpenOrders[JustClosedOrders.i,] )
    
  }
  return(NULL)
}

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
