\name{ruleOrderProc}
\alias{ruleOrderProc}
\title{process open orders at time t, generating transactions or new orders...}
\usage{ruleOrderProc(portfolio, symbol, mktdata, timespan, ordertype, ..., slippageFUN)}
\description{process open orders at time t, generating transactions or new orders}
\arguments{\item{portfolio}{text name of the portfolio to associate the order book with}
\item{symbol}{identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}
\item{mktdata}{an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats, default NULL}
\item{timespan}{xts-style character timespan to be the period to find orders to process in}
\item{ordertype}{one of NULL, "market","limit","stoplimit", or "stoptrailing" default NULL}
\item{...}{any other passthru parameters}
\item{slippageFUN}{default  NULL, not yet implemented}}
