\name{ruleSignal}
\alias{ruleSignal}
\title{default rule to execute a trade on a signal...}
\usage{ruleSignal(mktdata, timestamp, sigcol, sigval, orderqty=0, ordertype, orderside, threshold, replace=TRUE, delay=1e-04, osFUN="osNoOp",
    pricemethod=c("market", "opside"), portfolio, symbol, ...)}
\description{default rule to execute a trade on a signal}
\details{\code{pricemethod} may be one of 'market' and 'opside' which will either try to get the price of the market at \code{timestamp} and use this as the order price}
\seealso{\code{\link{osNoOp}}}
\arguments{\item{mktdata}{an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information}
\item{timestamp}{timestamp coercible to POSIXct that will be the time the order will be inserted on}
\item{sigcol}{column name to check for signal}
\item{sigval}{signal value to match against}
\item{orderqty}{numeric quantity of the desired order, modified by osFUN}
\item{ordertype}{one of "market","limit","stoplimit", or "stoptrailing"}
\item{orderside}{one of either "long" or "short"}
\item{threshold}{numeric threshold to apply to trailing stop orders, default NULL}
\item{replace}{TRUE/FALSE, whether to replace any other open order(s) on this portfolio symbol, default TRUE}
\item{delay}{what delay to add to timestamp when inserting the order into the order book, in seconds}
\item{osFUN}{function or text descriptor of function to use for order sizing, default \code{\link{osNoOp}}}
\item{pricemethod}{one of 'market' or 'opside', see Details}
\item{portfolio}{text name of the portfolio to place orders in}
\item{symbol}{identifier of the instrument to place orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}
\item{...}{any other passthru parameters}}
