\name{ruleOrderProc}
\alias{ruleOrderProc}
\title{process open orders at time t, generating transactions or new orders...}
\usage{ruleOrderProc(portfolio, symbol, mktdata, timestamp, ordertype, ..., slippageFUN)}
\description{process open orders at time t, generating transactions or new orders}
\arguments{\item{portfolio}{text name of the portfolio to associate the order book with}
\item{symbol}{identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}
\item{mktdata}{an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats, default NULL}
\item{timestamp}{timestamp coercible to POSIXct that will be the time to search for orders before this time}
\item{ordertype}{one of NULL, "market","limit","stoplimit", or "stoptrailing" default NULL}
\item{...}{any other passthru parameters}
\item{slippageFUN}{default  NULL, not yet implemented}}
