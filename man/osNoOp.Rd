\name{osNoOp}
\alias{osNoOp}
\title{default order sizing function...}
\usage{osNoOp(mktdata, timestamp, orderqty, ordertype, orderside, portfolio, symbol)}
\description{default order sizing function}
\details{default function performs no operation (NoOp), returns orderqty}
\arguments{\item{mktdata}{an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information}
\item{timestamp}{timestamp coercible to POSIXct that will be the time the order will be inserted on}
\item{orderqty}{numeric quantity of the desired order, modified by osFUN}
\item{ordertype}{one of "market","limit","stoplimit", or "stoptrailing"}
\item{orderside}{one of either "long" or "short"}
\item{portfolio}{text name of the portfolio to place orders in}
\item{symbol}{identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}}
