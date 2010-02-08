\name{addOrder}
\alias{addOrder}
\title{add an order to the order book...}
\usage{addOrder(portfolio, symbol, timestamp, qty, price, ordertype, side, status="open", statustimestamp="", delay=1e-05)}
\description{add an order to the order book}
\details{we need to figure out how to handle stop entry and stop exit orders, maybe via a negative price to specify the pullback that would trigger the order at the market.

trailing stops should be modeled with replaced orders as prices change}
\arguments{\item{portfolio}{text name of the portfolio to associate the order book with}
\item{symbol}{identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}
\item{timestamp}{timestamp coercible to POSIXct that will be the time the order will be inserted on}
\item{qty}{numeric quantity of the order}
\item{price}{numeric price at which the order is to be inserted}
\item{ordertype}{one of "market","limit",or "stop"}
\item{side}{one of either "long" or "short"}
\item{status}{one of "open", "closed", "canceled", or "replaced"}
\item{statustimestamp}{timestamp of a status update, will be blank when order is initiated}
\item{delay}{what delay to add to timestamp when inserting the order into the order book, in seconds}}
