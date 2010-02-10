\name{updateOrders}
\alias{updateOrders}
\title{update an order or orders...}
\usage{updateOrders(portfolio, symbol, timestamp, ordertype, side, oldstatus="open", newstatus, statustimestamp)}
\description{update an order or orders}
\details{When an order gets filled, it should have its status moved to 'closed'.

When an order is updated with a new order, the order status should change to 'replaced' 
with a StatusTime that is the same as the one for the new order.  This could happen in 
the case of a traditional Cancel/Replace, because of a trailing stop, or in the
case of a partial fill that needs to enter a replaced order for the remainder. 

When a risk event or over-limit event happens, typically open orders will be 'canceled'.  
Possibly new orders will be added to close open positions.  
Many models will also want to run a process at the close of market that will cancel all open orders.}
\arguments{\item{portfolio}{text name of the portfolio to associate the order book with}
\item{symbol}{identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}
\item{timestamp}{timestamp coercible to POSIXct that will be the time to search for orders before this time}
\item{ordertype}{one of NULL, "market","limit","stoplimit", or "stoptrailing" default NULL}
\item{side}{one of NULL, "long" or "short", default NULL}
\item{oldstatus}{one of NULL, "open", "closed", "canceled", or "replaced", default "open"}
\item{newstatus}{one of "open", "closed", "canceled", or "replaced"}
\item{statustimestamp}{timestamp of a status update, will be blank when order is initiated}}
