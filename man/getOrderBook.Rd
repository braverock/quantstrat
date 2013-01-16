\name{getOrderBook}
\alias{getOrderBook}
\title{get the order book object}
\usage{
  getOrderBook(portfolio, envir = .strategy)
}
\arguments{
  \item{portfolio}{text name of the portfolio the order
  book is associated with}

  \item{envir}{the environment to retrieve the orderbook
  object from, defaults to .strategy}
}
\description{
  I don't think this should be exported, but it is for now
  while we're in test mode.
}
\seealso{
  addOrder

  getOrders
}
\concept{
  order book
}

