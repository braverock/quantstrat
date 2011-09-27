\name{updateStrategy}
\alias{updateStrategy}
\title{run standard and custom strategy wrapup functions such as updating portfolio, account, and ending equity}
\usage{
  updateStrategy(portfolio = "default", account =
  portfolio, Symbols = NULL, Dates = NULL, Prices = NULL,
  update.Portf = TRUE, update.Acct = TRUE, update.EndEq =
  TRUE, showEq = TRUE, chart = TRUE, ...)
}
\arguments{
  \item{portfolio}{string identifying a portfolio}

  \item{account}{string identifying an account. Same as
  \code{portfolio} by default}

  \item{Symbols:}{character vector of names of symbols
  whose portfolios will be updated}

  \item{Dates}{optional xts-style ISO-8601 time range to
  run updatePortf over, default NULL (will use times from
  Prices)}

  \item{Prices}{optional xts object containing prices and
  timestamps to mark the book on, default NULL}

  \item{update.Portf}{TRUE/FALSE if TRUE (default) a call
  will be made to \code{updatePortf}}

  \item{update.Acct}{TRUE/FALSE if TRUE (default) a call
  will be made to \code{updateAcct}}

  \item{update.EndEq}{TRUE/FALSE if TRUE (default) a call
  will be made to \code{updateEndEq}}

  \item{showEq}{TRUE/FALSE if TRUE (default) ending equity
  will be printed to the screen}

  \item{chart}{TRUE/FALSE if TRUE (default) a call will be
  made to \code{chart.Posn}}

  \item{\dots}{any other passthrough parameters}
}
\description{
  \code{\link{updateStrategy}} will run a series of common
  wrapup functions at the beginning of an
  \code{\link{applyStrategy}} call.  This function allows
  the user to add arbitrary wrapup functions to the
  sequence.
}
\details{
  These arbitrary functions will be added to the
  \code{update} slot of the strategy object and when
  \code{applyStrategy} is evaluated, the arbitrary wrapup
  functions will be evaluated \emph{before} the
  standardized functions.

  For example, if you are working with high frequency data,
  it would be common to \empgh{mark the book} on a lower
  frequency, perhaps minutes, hours, or even days, rather
  than tick.  A custom wrapup function could take your high
  frequency data and transform it to lower frequency data
  before the call to \code{\link{updatePortf}}.
}
\author{
  Garrett See, Brian Peterson
}
\seealso{
  \code{\link{updatePortf}}, \code{\link{updateAcct}},
  \code{\link{updateEndEq}}, \code{\link{chart.Posn}}
}

