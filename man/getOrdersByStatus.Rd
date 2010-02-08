\name{getOrdersByStatus}
\alias{getOrdersByStatus}
\title{get orders by status...}
\usage{getOrdersByStatus(portfolio, symbol, status="open", timestamp, ordertype, starttime=-86400)}
\description{get orders by status}
\details{This function exists so that other code can find open orders, potentially to update or cancel them.

It has some use as a reporting or post-hoc analytics tool, but it may not always be exported.

should this be symbols in stead of symbol?}
\arguments{\item{portfolio}{text name of the portfolio to associate the order book with}
\item{symbol}{identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}
\item{status}{one of "open", "closed", "canceled", or "replaced"}
\item{timestamp}{timestamp coercible to POSIXct that will be the period to find orders of the given status and ordertype}
\item{ordertype}{one of "market","limit",or "stop"}
\item{starttime}{difference to current timestamp to search, in seconds(numeric) or as a POSIXct timestamp, defaults to -86400 (one day)}}
