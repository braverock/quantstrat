\name{getOrders}
\alias{getOrders}
\title{get orders by time span, status, type, and side...}
\usage{getOrders(portfolio, symbol, status="open", timespan, ordertype, side,
    which.i=FALSE)}
\description{get orders by time span, status, type, and side}
\details{This function exists so that other code can find open orders, potentially to update or cancel them.

It has some use as a reporting or post-hoc analytics tool, but it may not always be exported.

should this be symbols instead of symbol?}
\arguments{\item{portfolio}{text name of the portfolio to associate the order book with}
\item{symbol}{identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}
\item{status}{one of "open", "closed", "canceled", or "replaced", default "open"}
\item{timespan}{xts-style character timespan to be the period to find orders of the given status and ordertype}
\item{ordertype}{one of NULL, "market","limit","stoplimit", "stoptrailing", or "iceberg" default NULL}
\item{side}{one of NULL, "long" or "short", default NULL}
\item{which.i}{if TRUE, return the row index numbers rather than the order rows matching the criteria, default FALSE}}
