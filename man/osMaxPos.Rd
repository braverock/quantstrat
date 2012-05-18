\name{osMaxPos}
\alias{osMaxPos}
\title{order sizing function for position limits and level sizing}
\usage{
  osMaxPos(data, timestamp, orderqty, ordertype, orderside,
    portfolio, symbol, ruletype, ...)
}
\arguments{
  \item{data}{an xts object containing market data.
  depending on rules, may need to be in OHLCV or BBO
  formats, and may include indicator and signal
  information}

  \item{timestamp}{timestamp coercible to POSIXct that will
  be the time the order will be inserted on}

  \item{orderqty}{numeric quantity of the desired order,
  modified by osFUN}

  \item{ordertype}{one of "market","limit","stoplimit", or
  "stoptrailing"}

  \item{orderside}{one of either "long" or "short"}

  \item{portfolio}{text name of the portfolio to place
  orders in}

  \item{symbol}{identifier of the instrument to place
  orders for.  The name of any associated price objects
  (xts prices, usually OHLC) should match these}

  \item{ruletype}{one of
  "risk","order","rebalance","exit","enter", see
  \code{\link{add.rule}}}

  \item{...}{any other passthru parameters}
}
\description{
  levels are a simplification of more complex (proprietary)
  techniques sometimes used for order sizing. the max
  orderqty returned will be the limit/levels Obviously the
  strategy rules could ask for smaller order sizes, but
  this is the default.  If you don't want to use levels,
  set them to 1.
}
\details{
  \code{orderqty='all'} in a risk rule will return an order
  size appropriate to flatten the current position.
}
\note{
  TODO integrate orderqty='all' into osMaxPos for non-risk
  exit orders by combining side and pos for exits
}
\seealso{
  \code{\link{addPosLimit}},\code{\link{getPosLimit}}
}

