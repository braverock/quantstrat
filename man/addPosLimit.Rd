\name{addPosLimit}
\alias{addPosLimit}
\title{add position and level limits at timestamp}
\usage{
  addPosLimit(portfolio, symbol, timestamp, maxpos,
    longlevels = 1, minpos = -maxpos,
    shortlevels = longlevels)
}
\arguments{
  \item{portfolio}{text name of the portfolio to place
  orders in}

  \item{symbol}{identifier of the instrument to place
  orders for.  The name of any associated price objects
  (xts prices, usually OHLC) should match these}

  \item{timestamp}{timestamp coercible to POSIXct that will
  be the time the order will be inserted on}

  \item{maxpos}{numeric maximum long position for symbol}

  \item{longlevels}{numeric number of levels}

  \item{minpos}{numeric minimum position, default -minpos
  (short allowed use negative number)}

  \item{shortlevels}{numeric number of short levels,
  default longlevels}
}
\description{
  levels are a simplification of more complex (proprietary)
  techniques sometimes used for order sizing. the max
  orderqty returned will be the limit/levels Obviously the
  strategy rules could ask for smaller order sizes, but
  this is the default.  If you don't want to use levels,
  set them to 1.
}
\seealso{
  \code{\link{osMaxPos}} \code{\link{getPosLimit}}
}

