\name{getPosLimit}
\alias{getPosLimit}
\title{get position and level limits on timestamp}
\usage{
  getPosLimit(portfolio, symbol, timestamp)
}
\arguments{
  \item{portfolio}{text name of the portfolio to place
  orders in}

  \item{symbol}{identifier of the instrument to place
  orders for.  The name of any associated price objects
  (xts prices, usually OHLC) should match these}

  \item{timestamp}{timestamp coercible to POSIXct that will
  be the time the order will be inserted on}
}
\description{
  get position and level limits on timestamp
}
\seealso{
  \code{\link{addPosLimit}},\code{\link{osMaxPos}}
}

