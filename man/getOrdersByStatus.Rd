\name{getOrdersByStatus}
\alias{getOrdersByStatus}
\title{get orders by status...}
\usage{getOrdersByStatus(portfolio, symbol, status="open", date, ordertype)}
\description{get orders by status}
\details{should this be symbols in stead of symbol?}
\arguments{\item{portfolio}{text name of the portfolio to associate the order book with}
\item{symbol}{identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}
\item{status}{one of "open", "closed", "canceled", or "replaced"}
\item{date}{timestamp coercible to POSIXct that will be the period to find orders of the given status and ordertype}
\item{ordertype}{one of "market","limit",or "stop"}}
