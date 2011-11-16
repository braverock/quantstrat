\name{rm.strat}
\alias{rm.strat}
\title{Remove objects associated with a strategy}
\usage{
  rm.strat(name = "default", silent = TRUE)
}
\arguments{
  \item{name}{name of the portfolio/account/order book to
  clean up. (Default='default')}

  \item{silent}{silence warnings about trying to remove
  objects that do not exist. (Default=TRUE)}
}
\value{
  invisible -- called for side-effect
}
\description{
  Remove the order_book, account, and portfolio of given
  \code{name}.
}
\examples{
\dontrun{
# make some things to remove
strategy("example", store=TRUE)
initPortf('example', stock('SPY', currency("USD")))
initAcct('example', 'example')
initOrders('example', 'SPY')
#Now remove them
rm.strat('example')
}
}

