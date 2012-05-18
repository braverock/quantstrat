\name{osNoOp}
\alias{osNoOp}
\title{default order sizing function}
\usage{
  osNoOp(timestamp, orderqty, portfolio, symbol, ruletype,
    ...)
}
\arguments{
  \item{timestamp}{timestamp coercible to POSIXct that will
  be the time the order will be inserted on}

  \item{orderqty}{numeric quantity of the desired order,
  modified by osFUN}

  \item{portfolio}{text name of the portfolio to place
  orders in}

  \item{symbol}{identifier of the instrument to place
  orders for.  The name of any associated price objects
  (xts prices, usually OHLC) should match these}

  \item{...}{any other passthru parameters}

  \item{ruletype}{one of
  "risk","order","rebalance","exit","enter", see
  \code{\link{add.rule}}}
}
\description{
  default function performs no operation (NoOp), returns
  orderqty
}
\details{
  if orderqty 'all', will only work on an exit rule type,
  otherwize orderqty is zero.
}

