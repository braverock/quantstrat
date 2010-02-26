\name{addOrder}
\alias{addOrder}
\title{add an order to the order book...}
\usage{addOrder(portfolio, symbol, timestamp, qty, price, ordertype, side, threshold, status="open", replace=TRUE, statustimestamp="", delay=1e-05)}
\description{add an order to the order book}
\details{By default, this function will locate and replace any 'open' order(s) 
on the requested portfolio/symbol that have the same type and side.  
This is the equivalent of what is sometimes called an 
OCO (Order Cancels Other) order.  If you do not want the function to 
behave this way, set \code{replace=FALSE}.

We have modeled two types of stop orders, which should be sufficient to model most types of stops.  
We have modeled the simplest type, a 'stoplimit' order, which is just a limit order used to enter 
or exit a position at a specific price.  There is no functional different between a regular 'limit'
order and a 'stoplimit' order, but the distinction will likely be useful for reporting on when stops
have been triggered.
We have also modeled a 'stoptrailing' order, which may be used to model dynamic limit-based entry or exit.  
The 'stoptrailing' order type is the only order type that makes use of the order \code{threshold}, which 
is the difference either positive or negative from the current price when the order is entered.  
Some markets and brokers recognize a stop that triggers a market order, when the stop is triggered, 
a market order will be executed at the then-prevailing price.  We have not modeled this type of order.}
\arguments{\item{portfolio}{text name of the portfolio to associate the order book with}
\item{symbol}{identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}
\item{timestamp}{timestamp coercible to POSIXct that will be the time to search for orders before this time}
\item{qty}{numeric quantity of the order}
\item{price}{numeric price at which the order is to be inserted}
\item{ordertype}{one of "market","limit","stoplimit", or "stoptrailing"}
\item{side}{one of either "long" or "short"}
\item{threshold}{numeric threshold to apply to trailing stop orders, default NULL}
\item{status}{one of "open", "closed", "canceled", or "replaced", default "open"}
\item{replace}{TRUE/FALSE, whether to replace any other open order(s) on this portfolio symbol, default TRUE}
\item{statustimestamp}{timestamp of a status update, will be blank when order is initiated}
\item{delay}{what delay to add to timestamp when inserting the order into the order book, in seconds}}
