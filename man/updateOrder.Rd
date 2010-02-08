\name{updateOrder}
\alias{updateOrder}
\title{update an order...}
\usage{updateOrder(portfolio, symbol, timestamp, oldstatus, newstatus, statustimestamp)}
\description{update an order}
\details{When an order gets filled, it should have its status moved to 'closed'.

When an order is updated with a new order, the order status should change to 'replaced' with a StatusTime that is the same as the one for the new order.

When a risk event or over-limit event happens, typically open orders will be 'canceled'.  Possibly new orders will be added to close open positions.  
Many models will also want to run a process at the close of market that will cancel all open orders.}
\arguments{\item{portfolio}{text name of the portfolio to associate the order book with}
\item{symbol}{identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}
\item{timestamp}{timestamp coercible to POSIXct that will be the time the order will be inserted on}
\item{oldstatus}{one of NULL, "open", "closed", "canceled", or "replaced"}
\item{newstatus}{one of "open", "closed", "canceled", or "replaced"}
\item{statustimestamp}{timestamp of a status update, will be blank when order is initiated}}
