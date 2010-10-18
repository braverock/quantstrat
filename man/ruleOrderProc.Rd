\name{ruleOrderProc}
\alias{ruleOrderProc}
\title{process open orders at time t, generating transactions or new orders...}
\usage{ruleOrderProc(portfolio, symbol, mktdata, timespan, ordertype, ...,
    slippageFUN)}
\description{process open orders at time \emph{t}, generating transactions or new orders}
\details{This function is meant to be sufficient for backtesting most strategies, 
but would need to be replaced for production use.  It provides the interface 
for taking the order book and determining when orders become trades.

For the purposes of backtesting, and compatibility with the trade accounting in
\code{blotter}, this function will not allow a transaction to cross your current 
position through zero.  The accounting rules for realizing gains in such cases 
are more complicated than we wish to support.  Also, many brokers will break, revise,
or split such transactions for the same reason. If you wish to do a "stop and reverse" 
system, first stop (flatten), and then reverse (initiate a new position).

This function would need to be revised or replaced for connection to a live trading infrastructure.

We would like to model slippage here via \code{slippageFUN}.  Code contributions, suggestions, 
and requests appreciated.}
\arguments{\item{portfolio}{text name of the portfolio to associate the order book with}
\item{symbol}{identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}
\item{mktdata}{an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats, default NULL}
\item{timespan}{xts-style character timespan to be the period to find orders to process in}
\item{ordertype}{one of NULL, "market","limit","stoplimit", or "stoptrailing" default NULL}
\item{...}{any other passthru parameters}
\item{slippageFUN}{default  NULL, not yet implemented}}
