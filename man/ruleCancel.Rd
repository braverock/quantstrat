\name{ruleCancel}
\alias{ruleCancel}
\title{rule to cancel an unfilled limit order on a signal}
\usage{
  ruleCancel(data = mktdata, timestamp, sigcol, sigval,
    orderside = NULL, orderset = NULL, portfolio, symbol,
    ruletype)
}
\arguments{
  \item{data}{an xts object containing market data.
  depending on rules, may need to be in OHLCV or BBO
  formats, and may include indicator and signal
  information}

  \item{timestamp}{timestamp coercible to POSIXct that will
  be the time the order will be inserted on}

  \item{sigcol}{column name to check for signal}

  \item{sigval}{signal value to match against}

  \item{orderside}{one of either "long" or "short", default
  NULL, see details}

  \item{orderset}{tag to identify an orderset}

  \item{portfolio}{text name of the portfolio to place
  orders in}

  \item{symbol}{identifier of the instrument to cancel
  orders for}

  \item{ruletype}{must be 'risk' for ruleCancel, see
  \code{\link{add.rule}}}
}
\description{
  As described elsewhere in the documentation, quantstrat
  models \emph{orders}.  All orders in quantstrat are GTC
  orders, which means that unfilled limit orders have to be
  cancelled manually or replaced by other orders.
}
\details{
  This function is used for canceling the orders based on a
  signal.
}
\author{
  Niklas Kolster
}
\seealso{
  \code{\link{osNoOp}} , \code{\link{add.rule}}
}

